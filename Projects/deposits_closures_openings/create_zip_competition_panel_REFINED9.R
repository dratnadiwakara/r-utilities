# =====================================================================
# File: create_zip_competition_panel_FINAL.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks.
#
# Features:
#   1. Shock Variable: Share of deposits closed in t-1.
#   2. Response Variable: Incumbent growth (normalized by t-1 market size).
#   3. Relocation Control: Tracks new openings/deposits by Closing banks.
#   4. Clean/Strict Identification: Identifies isolated shocks and full market exits.
# =====================================================================

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
output_path <- "C:/data/zip_competition_churn_panel_refined_9.rds"


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

# Create Shifts (Leads and Lags at Branch Level)
closure_opening_data[, `:=`(
  # For Growth Calculation
  dep_lead3        = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3       = shift(YEAR, 3L, type = "lead"),
  dep_lead1        = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1       = shift(YEAR, 1L, type = "lead"),
  
  # For Closure Share Calculation
  dep_lag1         = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1        = shift(YEAR, 1L, type = "lag"),
  dep_lag3         = shift(DEPSUMBR, 3L, type = "lag"),
  year_lag3        = shift(YEAR, 3L, type = "lag")
), by = UNINUMBR]

# Time Alignment: Ensure the shift actually corresponds to the correct year
closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lag1_aligned  = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_),
  dep_lag3_aligned  = fifelse(year_lag3 == YEAR - 3L, dep_lag3, NA_real_)
)]


# ---------------------------------------------------------------------
# 2. Classify Banks: "Incumbent" vs "Closer"
# ---------------------------------------------------------------------
bank_zip_status <- closure_opening_data[, .(
  n_closed = sum(closed == 1L, na.rm = TRUE)
), by = .(RSSDID, ZIPBR, YEAR)]

bank_zip_status[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]

closure_opening_data <- merge(closure_opening_data, 
                              bank_zip_status[, .(RSSDID, ZIPBR, YEAR, bank_type)],
                              by = c("RSSDID", "ZIPBR", "YEAR"),
                              all.x = TRUE)


# ---------------------------------------------------------------------
# 3. Calculate Market Context (The "Shock" and the "Denominator")
# ---------------------------------------------------------------------
zip_market_stats <- closure_opening_data[, .(
  branches_zip_curr    = uniqueN(UNINUMBR),
  total_deps_zip_curr  = sum(DEPSUMBR, na.rm = TRUE), 
  n_closed_zip_yr      = sum(closed, na.rm=T),
  
  # Volume of closures in current year
  total_closed_vol_zip_curr = sum_na_safe(fifelse(closed == 1L, DEPSUMBR, NA_real_)),
  
  # Volume of closures in previous year (The t-1 Shock Base)
  total_closed_vol_zip_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_))
  
), by = .(ZIPBR, YEAR)]

# --- LAG & GROWTH CALCULATIONS ---
setorder(zip_market_stats, ZIPBR, YEAR)
zip_market_stats[, `:=`(
  branches_zip_lag1   = shift(branches_zip_curr, 1L, type = "lag"),
  total_deps_zip_lag1 = shift(total_deps_zip_curr, 1L, type = "lag"),
  total_deps_zip_lag3 = shift(total_deps_zip_curr, 3L, type = "lag")
), by = ZIPBR]

# Calculate ZIP Growth Rates
zip_market_stats[, zip_growth := (total_deps_zip_curr - total_deps_zip_lag1) / total_deps_zip_lag1]
zip_market_stats[, zip_growth_3yr := (total_deps_zip_lag1 - total_deps_zip_lag3) / total_deps_zip_lag3]

# Handle NAs for volume
zip_market_stats[, total_closed_vol_zip_lag1 := ifelse(is.na(total_closed_vol_zip_lag1), 0, total_closed_vol_zip_lag1)]


# ---------------------------------------------------------------------
# 4. Aggregate to BANK-ZIP-Year Level
# ---------------------------------------------------------------------
bank_zip_panel <- closure_opening_data[RSSDID > 0, .(
  # Bank Status
  bank_type = unique(bank_type)[1], 
  
  # Bank Volumes
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  
  no_branches_bank_zip_yr = .N,
  
  # --- NEW: Count Openings AND Deposits of New Branches per Bank ---
  n_opened_bank_zip_yr = sum(new_branch, na.rm=TRUE),
  deps_new_branches_bank_zip_yr = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_))
  
), by = .(RSSDID, ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 4b. Calculate Future Variables & Exit Logic (Look-Ahead)
# ---------------------------------------------------------------------
setorder(bank_zip_panel, RSSDID, ZIPBR, YEAR)

# Create leads for logic checks
bank_zip_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  year_lead2 = shift(YEAR, 2L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  
  type_lead1 = shift(bank_type, 1L, type = "lead"),
  type_lead2 = shift(bank_type, 2L, type = "lead"),
  type_lead3 = shift(bank_type, 3L, type = "lead")
), by = .(RSSDID, ZIPBR)] 

bank_zip_panel[, `:=`(
  # Logic: Is this bank a CLOSER in future years?
  future_closer_3yr = (
    (!is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER") |
      (!is.na(year_lead2) & year_lead2 == YEAR + 2L & type_lead2 == "CLOSER") |
      (!is.na(year_lead3) & year_lead3 == YEAR + 3L & type_lead3 == "CLOSER")
  ),
  
  # Logic: Does this bank disappear from the data?
  future_exit_3yr = (
    (is.na(year_lead1) | year_lead1 != YEAR + 1L) |  
      (is.na(year_lead2) | year_lead2 != YEAR + 2L) | 
      (is.na(year_lead3) | year_lead3 != YEAR + 3L)   
  ),
  
  # --- NEW: Strict Immediate Exit Check ---
  # Returns 1 if this bank has NO record in this ZIP in t+1
  exit_next_year = (is.na(year_lead1) | year_lead1 != YEAR + 1L)
)]

# Cleanup
cols_to_remove <- c("year_lead1", "year_lead2", "year_lead3", "type_lead1", "type_lead2", "type_lead3")
bank_zip_panel[, (cols_to_remove) := NULL]

# Convert Booleans
bank_zip_panel[, `:=`(
  future_closer_3yr = as.integer(future_closer_3yr),
  future_exit_3yr   = as.integer(future_exit_3yr),
  exit_next_year    = as.integer(exit_next_year)
)]

# Update Bank Type for Future Closers (exclusion logic)
bank_zip_panel[, bank_type := ifelse(future_closer_3yr == 1, "FUTURE CLOSER", bank_type)]


# ---------------------------------------------------------------------
# 4c. Aggregate "Closer" Attributes to ZIP Level
# ---------------------------------------------------------------------
# We aggregate specifically for banks that are CLOSERS in this ZIP-Year
closers_agg <- bank_zip_panel[bank_type == "CLOSER", .(
  # Openings logic: Did the closers also open branches?
  n_openings_by_closers_curr    = sum(n_opened_bank_zip_yr, na.rm=TRUE),
  deps_openings_by_closers_curr = sum_na_safe(deps_new_branches_bank_zip_yr),
  
  # Exit Logic: Did ALL closers leave the market?
  # If min(exit) is 1, it means every single closer in this ZIP-Year exited next year.
  all_closers_exited_zip = min(exit_next_year, na.rm=TRUE) 
  
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 5. Aggregate INCUMBENTS to ZIP Level
# ---------------------------------------------------------------------
incumbent_zip_agg <- bank_zip_panel[bank_type == "INCUMBENT", .(
  incumbent_deps_curr   = sum_na_safe(bank_deps_curr),
  incumbent_deps_lag1   = sum_na_safe(bank_deps_lag1),
  incumbent_deps_lead1  = sum_na_safe(bank_deps_lead1),
  incumbent_deps_lead3  = sum_na_safe(bank_deps_lead3),
  n_incumbent_banks     = .N
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 6. Merge and Construct Variables
# ---------------------------------------------------------------------
final_zip_panel <- merge(incumbent_zip_agg, 
                         zip_market_stats[, .(ZIPBR, YEAR, total_deps_zip_lag1, 
                                              total_closed_vol_zip_lag1, branches_zip_lag1,
                                              zip_growth, zip_growth_3yr)],
                         by = c("ZIPBR", "YEAR"))

# Merge Closer Stats (Openings & Exits)
final_zip_panel <- merge(final_zip_panel, 
                         closers_agg, 
                         by = c("ZIPBR", "YEAR"), 
                         all.x = TRUE)

# Fill NAs for Closer Stats
final_zip_panel[is.na(n_openings_by_closers_curr), n_openings_by_closers_curr := 0]
final_zip_panel[is.na(deps_openings_by_closers_curr), deps_openings_by_closers_curr := 0]

# --- Lags and Main Variables ---
# We lag the closer stats to match the shock timing (t-1)
setorder(final_zip_panel, ZIPBR, YEAR)
final_zip_panel[, `:=`(
  openings_by_closers_lag1    = shift(n_openings_by_closers_curr, 1L, type="lag", fill=0),
  deps_opened_by_closers_lag1 = shift(deps_openings_by_closers_curr, 1L, type="lag", fill=0),
  all_closers_exited_zip_lag1 = shift(all_closers_exited_zip, 1L, type="lag", fill=0)
), by=ZIPBR]

# --- STEP 1: Construct Base Variables ---
final_zip_panel[, `:=`(
  # INDEPENDENT VARIABLE (The Shock): % of market closed in t-1
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              total_closed_vol_zip_lag1 / total_deps_zip_lag1, 0),
  
  # DEPENDENT VARIABLES (The Capture): Growth relative to t-1 market size
  incumbent_growth_share_3yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  incumbent_growth_share_1yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead1 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  incumbent_growth_share_3yr_own_deposits = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr,
                                       NA_real_),
  incumbent_growth_share_1yr_own_deposits = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr,
                                       NA_real_)
)]

# --- STEP 2: Construct Derived Dummy (Requires Step 1 to be complete) ---
# 1 if Share > 0 AND the bank(s) fully exited the ZIP in t+1
final_zip_panel[, is_closer_full_exit := as.integer(
  share_deps_closed > 0 & all_closers_exited_zip_lag1 == 1
)]


# ---------------------------------------------------------------------
# 7. Robust Window Logic (Defining "Clean" Observations)
# ---------------------------------------------------------------------
# A. Identify Shock Years
shock_events <- final_zip_panel[share_deps_closed > 0, .(ZIPBR, shock_year = YEAR)]

# B. Define Windows (t-3 to t+3)
final_zip_panel[, `:=`(win_start = YEAR - 3L, win_end = YEAR + 3L)]

# C. Non-Equi Join to Count Shocks in Window (Robust to missing years)
shock_counts <- shock_events[final_zip_panel, 
                             on = .(ZIPBR, shock_year >= win_start, shock_year <= win_end), 
                             .(n_shocks_in_window = .N), 
                             by = .EACHI]

final_zip_panel[, n_shocks_3yr_window := shock_counts$n_shocks_in_window]
final_zip_panel[, c("win_start", "win_end") := NULL]


# ---------------------------------------------------------------------
# 8. Final Dummies (Clean & Strict)
# ---------------------------------------------------------------------
final_zip_panel[, `:=`(
  is_clean_observation = 0L,
  is_clean_strict_exit = 0L
)]

# --- Definition 1: Standard Clean (Isolated Shock OR Pure Control) ---
# Shock: Must be the ONLY shock in the 7-year window.
# Control: Must have NO shocks in the 7-year window.
final_zip_panel[share_deps_closed > 0 & n_shocks_3yr_window == 1, is_clean_observation := 1L]
final_zip_panel[share_deps_closed == 0 & n_shocks_3yr_window == 0, is_clean_observation := 1L]

# --- Definition 2: Strict Clean (Isolated Shock + FULL EXIT) OR (Pure Control) ---
# Case A: Strict Shock (Isolated AND Closer Exited)
final_zip_panel[share_deps_closed > 0 & n_shocks_3yr_window == 1 & is_closer_full_exit == 1, 
                is_clean_strict_exit := 1L]

# Case B: Pure Control (Same as above)
final_zip_panel[share_deps_closed == 0 & n_shocks_3yr_window == 0, 
                is_clean_strict_exit := 1L]


# ---------------------------------------------------------------------
# 9. Final Cleanup & Save
# ---------------------------------------------------------------------

# Toxic Threshold logic (Small vs Large Shocks)
TOXIC_THRESHOLD <- 100000 

final_zip_panel[,`:=`(
  share_closed_small = fifelse(total_closed_vol_zip_lag1 < TOXIC_THRESHOLD, share_deps_closed, 0),
  share_closed_large = fifelse(total_closed_vol_zip_lag1 >= TOXIC_THRESHOLD, share_deps_closed, 0)
)]

final_zip_panel[, share_deps_closed := ifelse(is.na(share_deps_closed), 0, share_deps_closed)]

saveRDS(final_zip_panel, output_path)