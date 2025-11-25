# =============================================================================
# File: create_zip_competition_panel_Nov24_v6.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks.
# Update V6: Adds "Common Denominator" (CD) growth variables.
#            Normalizes Incumbent Growth by Total Market Size (t-1) rather than
#            Incumbent Size (t), solving the "Small Denominator" bias.
# =============================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)

# --- Helper Function for NA-safe Summing ---
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path   <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/zip_competition_churn_panel_Nov24_v6.rds"

# ---------------------------------------------------------------------
# 1. Load Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)

# Keep essential columns
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, closed, new_branch)
]

setorder(closure_opening_data, UNINUMBR, YEAR)

# Create Shifts (Branch Level)
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
# 2. Aggregation to BANK-ZIP Level (For Bank Classification)
# ---------------------------------------------------------------------
bank_zip_panel <- closure_opening_data[, .(
  # Deposits & Leads
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  
  # Counts
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, ZIPBR, YEAR)]

# --- Define Current Status ---
bank_zip_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 3. Future Closer Logic (Look-Ahead)
# ---------------------------------------------------------------------
setorder(bank_zip_panel, RSSDID, ZIPBR, YEAR)

# Create leads of the Bank Type
bank_zip_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  year_lead2 = shift(YEAR, 2L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  type_lead1 = shift(bank_type, 1L, type = "lead"),
  type_lead2 = shift(bank_type, 2L, type = "lead"),
  type_lead3 = shift(bank_type, 3L, type = "lead")
), by = .(RSSDID, ZIPBR)] 

# --- Identify Future Closers ---
bank_zip_panel[, `:=`(
  future_closer_3yr = (
    (!is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER") |
      (!is.na(year_lead2) & year_lead2 == YEAR + 2L & type_lead2 == "CLOSER") |
      (!is.na(year_lead3) & year_lead3 == YEAR + 3L & type_lead3 == "CLOSER")
  )
)]
bank_zip_panel[, future_closer_3yr := as.integer(future_closer_3yr)]

# --- Create "Clean" Bank Type ---
bank_zip_panel[, bank_type_clean := bank_type]
bank_zip_panel[bank_type == "INCUMBENT" & future_closer_3yr == 1, bank_type_clean := "FUTURE_CLOSER"]

# Cleanup
cols_to_remove <- c("year_lead1", "year_lead2", "year_lead3", "type_lead1", "type_lead2", "type_lead3")
bank_zip_panel[, (cols_to_remove) := NULL]


# ---------------------------------------------------------------------
# 4. Calculate Market Denominators (ZIP Totals)
# ---------------------------------------------------------------------
zip_market_totals <- bank_zip_panel[, .(
  branches_zip_curr = sum(n_branches),
  total_deps_zip_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(ZIPBR, YEAR)]

setorder(zip_market_totals, ZIPBR, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]


# ---------------------------------------------------------------------
# 5. Aggregate INCUMBENTS (Dual Versions)
# ---------------------------------------------------------------------
incumbent_agg <- bank_zip_panel[, .(
  
  # --- STANDARD (Includes unstable incumbents) ---
  incumbent_deps_curr_std  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT"]),
  incumbent_deps_lead3_std = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT"]),
  incumbent_deps_lag1_std  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT"]),
  
  # --- CLEAN (Excludes future closers) ---
  incumbent_deps_curr_clean  = sum_na_safe(bank_deps_curr[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lead3_clean = sum_na_safe(bank_deps_lead3[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lag1_clean  = sum_na_safe(bank_deps_lag1[bank_type_clean == "INCUMBENT"])
  
), by = .(ZIPBR, YEAR)]

# Note: We compute the specific Growth variables in Section 7 (Final Merge) 
# so we have access to total_deps_zip_lag1


# ---------------------------------------------------------------------
# 6. Aggregate CLOSERS (Precise Branch Level)
# ---------------------------------------------------------------------
# Revert to branch level for precision on closed volumes
branch_level_closers <- closure_opening_data[RSSDID %in% bank_zip_panel[bank_type=="CLOSER"]$RSSDID, ]

closer_agg_precise <- branch_level_closers[, .(
  # Note: Need to filter for Year/Zip where they are actually a closer
  dummy = 1 
)] 
# ...Actually, cleaner to use the logic from V5 directly:

# Get RSSDIDs of banks that are closers in specific Zip-Years
closer_keys <- bank_zip_panel[bank_type == "CLOSER", .(RSSDID, ZIPBR, YEAR)]

# Filter branch data to just these relevant bank-zip-years
relevant_branches <- merge(closure_opening_data, closer_keys, by = c("RSSDID", "ZIPBR", "YEAR"))

closer_agg_final <- relevant_branches[, .(
  # 1. Shock Volume (Branches actually closed)
  closer_closed_vol_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  
  # 2. Relocation Volume (New branches opened)
  closer_opened_vol_curr = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  
  # 3. Cannibalization Base (Branches kept open)
  closer_remaining_vol_lag1 = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_)),
  closer_n_remaining        = sum(closed == 0L & new_branch == 0L, na.rm = TRUE),
  
  closer_n_closed = sum(closed == 1L, na.rm=TRUE),
  closer_n_opened = sum(new_branch == 1L, na.rm=TRUE)
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 7. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- zip_market_totals 
final_panel <- merge(final_panel, incumbent_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_final, by = c("ZIPBR", "YEAR"), all.x = TRUE)

# Fill NAs for Closers with 0
cols_to_zero <- c("closer_closed_vol_lag1", "closer_opened_vol_curr", 
                  "closer_remaining_vol_lag1", "closer_n_remaining", 
                  "closer_n_closed", "closer_n_opened")
for(j in cols_to_zero){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

# --- CALCULATE VARIABLES ---
final_panel[, `:=`(
  # --- Independent Variable (The Shock) ---
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 0),
  
  # --- Standard Growth Variables (Original) ---
  incumbent_dep_gr_3yr = fifelse(incumbent_deps_curr_std > 0, 
                                 (incumbent_deps_lead3_std - incumbent_deps_curr_std) / incumbent_deps_curr_std, 
                                 NA_real_),
  incumbent_dep_gr_3yr_clean = fifelse(incumbent_deps_curr_clean > 0,
                                          (incumbent_deps_lead3_clean - incumbent_deps_curr_clean) / incumbent_deps_curr_clean,
                                          NA_real_),
  
  # --- NEW: Common Denominator (CD) Growth (Standard) ---
  # Numerator: Dollar Growth of Incumbents
  # Denominator: Total Market Size at t-1
  incumbent_dep_gr_3yr_cd = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead3_std - incumbent_deps_curr_std) / total_deps_zip_lag1,
                                    NA_real_),
  
  # --- NEW: Common Denominator (CD) Growth (Clean) ---
  incumbent_dep_gr_3yr_clean_cd = fifelse(total_deps_zip_lag1 > 0,
                                          (incumbent_deps_lead3_clean - incumbent_deps_curr_clean) / total_deps_zip_lag1,
                                          NA_real_),
  
  # --- Market Shares ---
  incumbent_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                     incumbent_deps_lag1_std / total_deps_zip_lag1, 0),
  
  incumbent_mkt_share_lag1_clean = fifelse(total_deps_zip_lag1 > 0,
                                           incumbent_deps_lag1_clean / total_deps_zip_lag1, 0),
  
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                            closer_remaining_vol_lag1 / total_deps_zip_lag1, 0),
  
  # --- Controls ---
  relocation_openings_vol_share = fifelse(total_deps_zip_lag1 > 0, 
                                          closer_opened_vol_curr / total_deps_zip_lag1, 0)
)]

# ---------------------------------------------------------------------
# 8. Save
# ---------------------------------------------------------------------
output_columns <- c("ZIPBR", "YEAR", 
                    # Dependent Vars (Standard)
                    "incumbent_dep_gr_3yr", 
                    # Dependent Vars (Clean)
                    "incumbent_dep_gr_3yr_clean",
                    
                    # New Market-Normalized Growth
                    "incumbent_dep_gr_3yr_cd", 
                    "incumbent_dep_gr_3yr_clean_cd",
                    
                    # Controls
                    "share_deps_closed", 
                    "incumbent_mkt_share_lag1",       # Standard
                    "incumbent_mkt_share_lag1_clean", # Clean
                    "closer_remaining_mkt_share_lag1",
                    "closer_n_remaining",
                    "relocation_openings_vol_share",
                    "branches_zip_lag1")



final_panel_clean <- final_panel[, ..output_columns]

cat("\nSummary of New Variables:\n")
print(summary(final_panel_clean[, .(incumbent_dep_gr_3yr_cd, incumbent_dep_gr_3yr_clean_cd)]))

saveRDS(final_panel_clean, output_path)
