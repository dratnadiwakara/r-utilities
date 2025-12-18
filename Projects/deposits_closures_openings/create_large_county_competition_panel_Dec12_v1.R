# =============================================================================
# File: create_county_competition_panel_LARGE_BANKS.R
# Purpose: Analyze impact of "Large Bank Closures" on "Large Incumbents".
#          Denominator remains Total Market (All Banks).
# =============================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)
library(zoo) 
library(stringr)

# --- Helper Function for NA-safe Summing ---
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path   <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/county_large_competition_panel_Dec14_v1.rds"


# ---------------------------------------------------------------------
# 1. Load Banking Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
sod <- readRDS("C:/data/fdic_sod_2000_2025_simple.rds")
sod <- sod[YEAR==2019,.(RSSDID,ASSET)]
sod <- unique(sod)
sod <- sod[ASSET>10e6]

closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)
closure_opening_data[,county:=str_pad(STCNTYBR,5,"left","0")]

closure_opening_data[,is_large:=ifelse(RSSDID %in% unique(sod$RSSDID),1,0)]

# Keep essential columns (ADDED is_large)
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, county, YEAR, DEPSUMBR, closed, new_branch, CERT, is_large)
]

setorder(closure_opening_data, UNINUMBR, YEAR)

# Create Shifts
closure_opening_data[, `:=`(
  dep_lead3   = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3  = shift(YEAR, 3L, type = "lead"),
  dep_lead1   = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1  = shift(YEAR, 1L, type = "lead"),
  dep_lag1    = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1   = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]

# Align Shifts
closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lag1_aligned  = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_)
)]


# ---------------------------------------------------------------------
# 2. Aggregation to BANK-COUNTY Level
# ---------------------------------------------------------------------
# NOTE: Added 'is_large' to the grouping to preserve it
bank_zip_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, county, YEAR, is_large)]

bank_zip_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 3. Future Closer Logic
# ---------------------------------------------------------------------
setorder(bank_zip_panel, RSSDID, county, YEAR)
bank_zip_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  type_lead1 = shift(bank_type, 1L, type = "lead")
), by = .(RSSDID, county)] 

bank_zip_panel[, future_closer_1yr := as.integer(
  !is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER"
)]

bank_zip_panel[, bank_type_clean := bank_type]
bank_zip_panel[bank_type == "INCUMBENT" & future_closer_1yr == 1, bank_type_clean := "FUTURE_CLOSER"]
bank_zip_panel[, c("year_lead1", "type_lead1") := NULL]


# ---------------------------------------------------------------------
# 4. Market Denominators (TOTAL MARKET - All Banks)
# ---------------------------------------------------------------------
# We calculate this using the full 'bank_zip_panel' BEFORE filtering for large banks.
# This ensures the denominator represents the entire market (Small + Large).
zip_market_totals <- bank_zip_panel[YEAR<2025, .(
  branches_zip_curr = sum(n_branches),
  total_deps_zip_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(county, YEAR)]

setorder(zip_market_totals, county, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = county]


# ---------------------------------------------------------------------
# 5. Aggregate INCUMBENTS (LARGE ONLY)
# ---------------------------------------------------------------------
# We filter for 'is_large == 1' (or TRUE) here.
incumbent_agg <- bank_zip_panel[, .(
  # Standard (Large Only)
  incumbent_deps_curr_std  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lead3_std = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lead1_std = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lag1_std  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT" & is_large == 1]),
  
  # Clean (Large Only)
  incumbent_deps_curr_clean  = sum_na_safe(bank_deps_curr[bank_type_clean == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lead3_clean = sum_na_safe(bank_deps_lead3[bank_type_clean == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lead1_clean = sum_na_safe(bank_deps_lead1[bank_type_clean == "INCUMBENT" & is_large == 1]),
  incumbent_deps_lag1_clean  = sum_na_safe(bank_deps_lag1[bank_type_clean == "INCUMBENT" & is_large == 1])
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 6. Aggregate CLOSERS (LARGE ONLY)
# ---------------------------------------------------------------------
# Identify closers that are LARGE banks
closer_keys <- bank_zip_panel[bank_type == "CLOSER" & is_large == 1, .(RSSDID, county, YEAR)]

# Merge back to branch data to get branch-specific deposit info
relevant_branches <- merge(closure_opening_data, closer_keys, by = c("RSSDID", "county", "YEAR"))

closer_agg_final <- relevant_branches[, .(
  # --- Total (Large Banks Only) ---
  closer_closed_vol_lag1    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  
  # --- Other ---
  closer_opened_vol_curr    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  closer_remaining_vol_lag1 = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 7. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- zip_market_totals 
final_panel <- merge(final_panel, incumbent_agg, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_final, by = c("county", "YEAR"), all.x = TRUE)

# Fill NAs
cols_to_zero <- c("closer_closed_vol_lag1", "closer_opened_vol_curr", "closer_remaining_vol_lag1")
for(j in cols_to_zero){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

final_panel[, `:=`(
  # --- Independent Variables ---
  # Numerator: Closed Deposits of Large Banks
  # Denominator: Total Market Deposits (Large + Small)
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 0),
  
  # --- Dependent Variables ---
  # Growth of LARGE incumbents
  incumbent_dep_gr_1yr = fifelse(total_deps_zip_lag1 > 0,
                                 (incumbent_deps_lead1_std - incumbent_deps_curr_std) / incumbent_deps_curr_std,
                                 NA_real_),
  
  # Growth of LARGE incumbents relative to TOTAL Market
  incumbent_dep_gr_1yr_cd = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead1_std - incumbent_deps_curr_std) / total_deps_zip_lag1,
                                    NA_real_),
  
  incumbent_dep_gr_1yr_cd_lag = fifelse(total_deps_zip_lag1 > 0,
                                        (incumbent_deps_lead1_std - incumbent_deps_lag1_std) / total_deps_zip_lag1,
                                        NA_real_),
  
  incumbent_dep_gr_3yr = fifelse(total_deps_zip_lag1 > 0,
                                 (incumbent_deps_lead3_std - incumbent_deps_curr_std) / incumbent_deps_curr_std,
                                 NA_real_),
  
  incumbent_dep_gr_3yr_cd = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead3_std - incumbent_deps_curr_std) / total_deps_zip_lag1,
                                    NA_real_),
  
  incumbent_dep_gr_3yr_lag = fifelse(total_deps_zip_lag1 > 0,
                                     (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / incumbent_deps_lag1_std,
                                     NA_real_),
  
  incumbent_dep_gr_3yr_cd_lag = fifelse(total_deps_zip_lag1 > 0,
                                        (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / total_deps_zip_lag1,
                                        NA_real_),
  
  # --- Controls ---
  # Share of LARGE incumbents in TOTAL market
  incumbent_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0, incumbent_deps_lag1_std / total_deps_zip_lag1, 0),
  
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0, closer_remaining_vol_lag1 / total_deps_zip_lag1, 0),
  log_total_market_size_lag1 = log(total_deps_zip_lag1 + 1),
  relocation_openings_vol_share = fifelse(total_deps_zip_lag1 > 0, closer_opened_vol_curr / total_deps_zip_lag1, 0)
)]


# ---------------------------------------------------------------------
# 8. Save
# ---------------------------------------------------------------------
output_columns <- c("county", "YEAR", 
                    
                    # Dependent (Large Incumbent Growth)
                    "incumbent_dep_gr_1yr",
                    "incumbent_dep_gr_1yr_cd",
                    "incumbent_dep_gr_1yr_cd_lag",
                    "incumbent_dep_gr_3yr",
                    "incumbent_dep_gr_3yr_lag",
                    "incumbent_dep_gr_3yr_cd", 
                    "incumbent_dep_gr_3yr_cd_lag",
                    
                    # Main Shocks (Large Closures / Total Market)
                    "share_deps_closed",
                    
                    # Controls
                    "log_total_market_size_lag1",
                    "incumbent_mkt_share_lag1", 
                    "closer_remaining_mkt_share_lag1",
                    "relocation_openings_vol_share",
                    "total_deps_zip_lag1",
                    "branches_zip_lag1")

final_panel_clean <- final_panel[, ..output_columns]

saveRDS(final_panel_clean, output_path)
