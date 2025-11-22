# =====================================================================
# File: create_zip_competition_panel_REFINED.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks.
#
# Structure:
#   1. Identify "Incumbents" (Banks with 0 closures in ZIP-Year)
#   2. Identify "Closers" (Banks with >0 closures in ZIP-Year)
#   3. Dependent Var: Aggregated deposit growth of Incumbents.
#      ** UPDATE: Normalized by Total Market Size (Common Denominator) **
#   4. Independent Vars: Closure intensity of Closers (Rate & Share).
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
data_path <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/zip_competition_churn_panel_refined_3.rds"


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

# Create Shifts
closure_opening_data[, `:=`(
  # For Growth Calculation
  dep_lead3        = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3       = shift(YEAR, 3L, type = "lead"),
  dep_lead1        = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1       = shift(YEAR, 1L, type = "lead"),
  
  # For Closure Share Calculation
  dep_lag1         = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1        = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]


closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lag1_aligned = fifelse(year_lag1 == YEAR - 1L, dep_lead1, NA_real_)
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
  branches_zip_curr   = uniqueN(UNINUMBR),
  total_deps_zip_curr = sum(DEPSUMBR, na.rm = TRUE), # User requested sum(DEPSUMBR)
  
  # Critical: We must still calculate the 'Closer' volume here to build the X variable later
  total_closed_vol_zip_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1, NA_real_))
), by = .(ZIPBR, YEAR)]

# --- LAG & GROWTH CALCULATIONS ---
setorder(zip_market_stats, ZIPBR, YEAR)
zip_market_stats[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]
zip_market_stats[, total_deps_zip_lag1 := shift(total_deps_zip_curr, 1L, type = "lag"), by = ZIPBR]

# Calculate One-Year ZIP Growth Rate
zip_market_stats[, zip_growth := (total_deps_zip_curr - total_deps_zip_lag1) / total_deps_zip_lag1]

# --- FILTERING STEP (User Logic) ---

# 1. Identify Bad ZIPs (Outliers)
# If a ZIP has ANY year with growth < -70% or > 300%, exclude the ZIP entirely.
exclude_zips <- unique(zip_market_stats[zip_growth < -0.7 | zip_growth > 3.0]$ZIPBR)
zip_market_stats <- zip_market_stats[!ZIPBR %in% exclude_zips]

# 2. Identify Gaps (Consecutive Years)
# Ensure the previous row is actually the previous year (t-1)
zip_market_stats[, lag_YEAR := shift(YEAR, 1L, type = "lag"), by = ZIPBR]
zip_market_stats <- zip_market_stats[YEAR == (lag_YEAR + 1)]

# Clean up
zip_market_stats[, c("zip_growth", "lag_YEAR") := NULL]





# ---------------------------------------------------------------------
# 4. Aggregate to BANK-ZIP-Year Level (The Panel Unit)
# ---------------------------------------------------------------------
# Now we collapse branches to a single row per Bank per ZIP per Year.

bank_zip_panel <- closure_opening_data[RSSDID>0, .(
  # Bank Status (Constant within Group)
  bank_type = unique(bank_type)[1], 
  
  # Bank Volume at t (Current)
  bank_deps_curr = sum_na_safe(DEPSUMBR),
  
  # Bank Volume at t-1 (Lag) - Needed for Immediate Capture Y
  bank_deps_lag1 = sum_na_safe(dep_lag1),
  
  # Bank Volume Leads (for Future Growth Y)
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned)
  
), by = .(RSSDID, ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 4. Aggregate INCUMBENTS to ZIP Level (Using the Bank Panel)
# ---------------------------------------------------------------------
# Input: bank_zip_panel (One row per Bank-ZIP-Year)
# Output: incumbent_zip_agg (One row per ZIP-Year)

incumbent_zip_agg <- bank_zip_panel[bank_type == "INCUMBENT", .(
  # Summing the Bank-level totals to get ZIP-level Incumbent totals
  incumbent_deps_curr  = sum_na_safe(bank_deps_curr),
  incumbent_deps_lag1  = sum_na_safe(bank_deps_lag1),
  incumbent_deps_lead3 = sum_na_safe(bank_deps_lead3),
  
  # Metadata: Count how many distinct incumbent banks are in this ZIP
  n_incumbent_banks = .N
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 5. Merge and Construct Regression Variables
# ---------------------------------------------------------------------
# We merge with 'zip_market_stats' (created in Step 3) to get the 
# Total Market Denominators and the Closure Volumes.

final_zip_panel <- merge(incumbent_zip_agg, 
                         zip_market_stats[, .(ZIPBR, YEAR, total_deps_zip_lag1, 
                                              total_closed_vol_zip_lag1, branches_zip_lag1)],
                         by = c("ZIPBR", "YEAR"))

final_zip_panel[, `:=`(
  # ---------------------------------
  # INDEPENDENT VARIABLE (The Shock)
  # ---------------------------------
  # "What % of the total market closed down?"
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              total_closed_vol_zip_lag1 / total_deps_zip_lag1, 
                              0),
  
  # ---------------------------------
  # DEPENDENT VARIABLE (The Capture)
  # ---------------------------------
  # 1. Immediate Capture (t-1 to t)
  # "What % of the total market did Incumbents gain immediately?"
  # Numerator: (Aggregate Incumbent Current - Aggregate Incumbent Lag)
  # Denominator: Total Market Size Lag (Common Denominator)
  incumbent_growth_share_1yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead1 - incumbent_deps_lag1) / total_deps_zip_lag1,
                                       NA_real_),
  
  # 2. Medium Term Capture (t-1 to t+3)
  incumbent_growth_share_3yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_lag1) / total_deps_zip_lag1,
                                       NA_real_),
  
  # 2. Medium Term Capture (t-1 to t+3)
  incumbent_growth_share_3yr_alt = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr,
                                       NA_real_),
  
  incumbent_growth_share_1yr_alt = fifelse(total_deps_zip_lag1 > 0,
                                           (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr,
                                           NA_real_)
)]


---------------------------------------------------------------------
  # 6. Final Cleanup: Handle NAs for "Zero Closure" ZIPs
  # ---------------------------------------------------------------------

# Since 'sum_na_safe' returns NA when no branches close, 
# share_deps_closed becomes NA. We convert these to 0.
final_zip_panel[, share_deps_closed := ifelse(is.na(share_deps_closed),0,share_deps_closed)]

# Optional: You might also want to ensure the volume column is 0
final_zip_panel[, total_closed_vol_zip_lag1 := ifelse(is.na(total_closed_vol_zip_lag1),0,total_closed_vol_zip_lag1)]


saveRDS(final_zip_panel, output_path )
