# =============================================================================
# File: create_county_competition_panel_COEXISTENCE.R
# Purpose: Analyze impact of "Large Bank Closures" on "Large Incumbents".
#          SAMPLE RESTRICTION: Only County-Years where BOTH types exist.
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
output_path <- "C:/data/county_coexistence_panel_Dec14_v2.rds"


# ---------------------------------------------------------------------
# 1. Load Banking Data & Define 'Large' vs 'Small'
# ---------------------------------------------------------------------
sod <- readRDS("C:/data/fdic_sod_2000_2025_simple.rds")

# Identify Small Banks: Assets < $10 Billion in 2019
# Note: SOD ASSET column is in Thousands ($000s). 
# $10 Billion = 10,000,000 thousands.
sod_small_def <- unique(sod[YEAR == 2019 & ASSET < 10000000, .(RSSDID)])
sod_large_def <- unique(sod[YEAR == 2019 & ASSET >= 10000000, .(RSSDID)])

closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)
closure_opening_data[, county := str_pad(STCNTYBR, 5, "left", "0")]

# Flag Small Banks (is_small = 1). Large Banks (is_large = 1).
closure_opening_data[, is_small := ifelse(RSSDID %in% sod_small_def$RSSDID, 1, 0)]
# Note: Some banks might be neither if they didn't exist in 2019 or data mismatch, 
# so we use explicit lists for both.
closure_opening_data[, is_large := ifelse(RSSDID %in% sod_large_def$RSSDID, 1, 0)]

# Keep essential columns
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
# Group by is_large to allow separate aggregation later
bank_county_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, county, YEAR, is_large)]

bank_county_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 3. Future Closer Logic (Commented out per your template)
# ---------------------------------------------------------------------
# setorder(bank_county_panel, RSSDID, county, YEAR)
# ...


# ---------------------------------------------------------------------
# 4. Market Denominators (TOTAL MARKET - All Banks)
# ---------------------------------------------------------------------
# Aggregating across ALL banks (Large + Small) for the denominator
county_market_totals <- bank_county_panel[YEAR < 2025, .(
  branches_county_curr = sum(n_branches),
  total_deps_county_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(county, YEAR)]

setorder(county_market_totals, county, YEAR)
county_market_totals[, branches_county_lag1 := shift(branches_county_curr, 1L, type = "lag"), by = county]


# ---------------------------------------------------------------------
# 5A. Aggregate INCUMBENTS (LARGE ONLY - is_large == 1)
# ---------------------------------------------------------------------
incumbent_agg_large <- bank_county_panel[, .(
  # Standard (Large)
  inc_deps_curr_lg  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lead3_lg = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lead1_lg = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT" & is_large == 1]),
  inc_deps_lag1_lg  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT" & is_large == 1])
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 5B. Aggregate INCUMBENTS (SMALL ONLY - is_large == 0)
# ---------------------------------------------------------------------
incumbent_agg_small <- bank_county_panel[, .(
  # Standard (Small)
  inc_deps_curr_sm  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT" & is_large == 0]),
  inc_deps_lead3_sm = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT" & is_large == 0]),
  inc_deps_lead1_sm = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT" & is_large == 0])
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 6. Aggregate CLOSERS (LARGE ONLY - is_large == 1)
# ---------------------------------------------------------------------
closer_keys_lg <- bank_county_panel[bank_type == "CLOSER" & is_large == 1, .(RSSDID, county, YEAR)]
relevant_branches_lg <- merge(closure_opening_data, closer_keys_lg, by = c("RSSDID", "county", "YEAR"))

closer_agg_large <- relevant_branches_lg[, .(
  # Total Closed Volume (Large Banks)
  closer_closed_vol_lag1    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  
  # Other Metrics
  closer_opened_vol_curr    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  closer_remaining_vol_lag1 = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR)]


# ---------------------------------------------------------------------
# 7. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- county_market_totals 
final_panel <- merge(final_panel, incumbent_agg_large, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, incumbent_agg_small, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_large, by = c("county", "YEAR"), all.x = TRUE)

# --- 7a. Fill NAs with 0 (CRITICAL UPDATE) ---
# We must fill NAs with 0 for BOTH types before filtering. 
# This handles cases where a type is missing (NA) -> treats as 0 deposits.
cols_to_zero <- c(
  "closer_closed_vol_lag1", "closer_opened_vol_curr", "closer_remaining_vol_lag1",
  "inc_deps_curr_sm", "inc_deps_lead3_sm", "inc_deps_lead1_sm",
  "inc_deps_curr_lg", "inc_deps_lead3_lg", "inc_deps_lead1_lg", "inc_deps_lag1_lg"
) 
for(j in cols_to_zero){
  if (j %in% names(final_panel)) {
    set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
  }
}

# --- 7b. COEXISTENCE FILTER (CRITICAL UPDATE) ---
# Keep only markets where Large Incumbents exist.
final_panel <- final_panel[inc_deps_curr_lg > 0 ]


final_panel[, `:=`(
  # --- Independent Variables ---
  share_deps_closed = fifelse(total_deps_county_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_county_lag1, 0),
  
  # --- Dependent Variables: LARGE Incumbents ---
  incumbent_dep_gr_1yr = fifelse(total_deps_county_lag1 > 0,
                                 (inc_deps_lead1_lg - inc_deps_curr_lg) / inc_deps_curr_lg, NA_real_),
  
  incumbent_dep_gr_1yr_cd = fifelse(total_deps_county_lag1 > 0,
                                    (inc_deps_lead1_lg - inc_deps_curr_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_1yr_cd_lag = fifelse(total_deps_county_lag1 > 0,
                                        (inc_deps_lead1_lg - inc_deps_lag1_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr = fifelse(total_deps_county_lag1 > 0,
                                 (inc_deps_lead3_lg - inc_deps_curr_lg) / inc_deps_curr_lg, NA_real_),
  
  incumbent_dep_gr_3yr_cd = fifelse(total_deps_county_lag1 > 0,
                                    (inc_deps_lead3_lg - inc_deps_curr_lg) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_lag = fifelse(total_deps_county_lag1 > 0,
                                     (inc_deps_lead3_lg - inc_deps_lag1_lg) / inc_deps_lag1_lg, NA_real_),
  
  incumbent_dep_gr_3yr_cd_lag = fifelse(total_deps_county_lag1 > 0,
                                        (inc_deps_lead3_lg - inc_deps_lag1_lg) / total_deps_county_lag1, NA_real_),
  
  # --- Dependent Variables: SMALL Incumbents ---
  incumbent_dep_gr_1yr_cd_small = fifelse(total_deps_county_lag1 > 0 & !is.na(incumbent_dep_gr_1yr_cd),
                                          (inc_deps_lead1_sm - inc_deps_curr_sm) / total_deps_county_lag1, NA_real_),
  
  incumbent_dep_gr_3yr_cd_small = fifelse(total_deps_county_lag1 > 0 & !is.na(incumbent_dep_gr_3yr_cd),
                                          (inc_deps_lead3_sm - inc_deps_curr_sm) / total_deps_county_lag1, NA_real_),
  
  # --- Controls ---
  incumbent_mkt_share_lag1 = fifelse(total_deps_county_lag1 > 0, inc_deps_lag1_lg / total_deps_county_lag1, 0),
  
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_county_lag1 > 0, closer_remaining_vol_lag1 / total_deps_county_lag1, 0)
)]


# ---------------------------------------------------------------------
# 8. Save
# ---------------------------------------------------------------------
output_columns <- c("county", "YEAR", 
                    
                    # Dependent (Large)
                    "incumbent_dep_gr_1yr",
                    "incumbent_dep_gr_1yr_cd",
                    "incumbent_dep_gr_1yr_cd_lag",
                    "incumbent_dep_gr_3yr",
                    "incumbent_dep_gr_3yr_lag",
                    "incumbent_dep_gr_3yr_cd", 
                    "incumbent_dep_gr_3yr_cd_lag",
                    
                    # Dependent (Small)
                    "incumbent_dep_gr_1yr_cd_small",
                    "incumbent_dep_gr_3yr_cd_small",
                    
                    # Shocks
                    "share_deps_closed",
                    
                    # Controls
                    "log_total_market_size_lag1",
                    "incumbent_mkt_share_lag1", 
                    "closer_remaining_mkt_share_lag1",
                    "relocation_openings_vol_share",
                    "total_deps_county_lag1",
                    "branches_county_lag1")

final_panel_clean <- final_panel[, ..output_columns]

saveRDS(final_panel_clean, output_path)