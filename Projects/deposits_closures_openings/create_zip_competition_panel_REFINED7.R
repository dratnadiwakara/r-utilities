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
output_path <- "C:/data/zip_competition_churn_panel_refined_7.rds"


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
  year_lag1        = shift(YEAR, 1L, type = "lag"),
  dep_lag3         = shift(DEPSUMBR, 3L, type = "lag"),
  year_lag3        = shift(YEAR, 3L, type = "lag")
), by = UNINUMBR]


closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_),
  dep_lag1_aligned = fifelse(year_lag1 == YEAR - 1L, dep_lag1, NA_real_),
  dep_lag3_aligned = fifelse(year_lag3 == YEAR - 3L, dep_lag3, NA_real_)
)]

# t <- (closure_opening_data[RSSDID==1146 & ZIPBR=="72212"])


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
  n_closed_zip_yr = sum(closed,na.rm=T),
  
  # Critical: We must still calculate the 'Closer' volume here to build the X variable later
  total_closed_vol_zip_curr = sum_na_safe(fifelse(closed == 1L, DEPSUMBR, NA_real_)),
  
  total_closed_vol_zip_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_))
  
), by = .(ZIPBR, YEAR)]

# --- LAG & GROWTH CALCULATIONS ---
setorder(zip_market_stats, ZIPBR, YEAR)
zip_market_stats[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]
zip_market_stats[, total_deps_zip_lag1 := shift(total_deps_zip_curr, 1L, type = "lag"), by = ZIPBR]
zip_market_stats[, total_deps_zip_lag3 := shift(total_deps_zip_curr, 3L, type = "lag"), by = ZIPBR]

# Calculate One-Year ZIP Growth Rate
zip_market_stats[, zip_growth := (total_deps_zip_curr - total_deps_zip_lag1) / total_deps_zip_lag1]
zip_market_stats[, zip_growth_3yr := (total_deps_zip_lag1 - total_deps_zip_lag3) / total_deps_zip_lag3]
# --- FILTERING STEP (User Logic) ---

# 1. Identify Bad ZIPs (Outliers)
# If a ZIP has ANY year with growth < -70% or > 300%, exclude the ZIP entirely.
# exclude_zips <- unique(zip_market_stats[zip_growth < -0.7 | zip_growth > 3.0]$ZIPBR)
# zip_market_stats <- zip_market_stats[!ZIPBR %in% exclude_zips]

# 2. Identify Gaps (Consecutive Years)
# Ensure the previous row is actually the previous year (t-1)
# zip_market_stats[, lag_YEAR := shift(YEAR, 1L, type = "lag"), by = ZIPBR]
# zip_market_stats <- zip_market_stats[YEAR == (lag_YEAR + 1)]


# zip_year_count <- zip_market_stats[,.N,by=.(ZIPBR)]
# zips_with_10_or_more_years <- zip_year_count[N>=10]$ZIPBR
# 
# zip_market_stats <- zip_market_stats[ZIPBR %in% zips_with_10_or_more_years]
# 
# 
# zip_market_stats[,deposits_per_closed_br:=total_closed_vol_zip_lag1/n_closed_zip_yr]
# zip_market_stats[,deposits_per_branch:=total_deps_zip_curr/branches_zip_curr]
# 
# zip_market_stats[,deposits_closed_tot:=deposits_per_closed_br/deposits_per_branch]
# 
# zip_market_stats <- zip_market_stats[deposits_closed_tot<3 | is.na(deposits_closed_tot)]

zip_market_stats[,total_closed_vol_zip_lag1:=ifelse(is.na(total_closed_vol_zip_lag1),0,total_closed_vol_zip_lag1)]

# Clean up
# zip_market_stats[, c("lag_YEAR", "deposits_per_closed_br", "deposits_per_branch", "deposits_closed_tot") := NULL]




# ---------------------------------------------------------------------
# 4. Aggregate to BANK-ZIP-Year Level
# ---------------------------------------------------------------------
bank_zip_panel <- closure_opening_data[RSSDID > 0, .(
  # Bank Status (Constant within Group)
  bank_type = unique(bank_type)[1], 
  
  # Bank Volume at t (Current)
  bank_deps_curr = sum_na_safe(DEPSUMBR),
  
  # Bank Volume at t-1 (Lag)
  bank_deps_lag1 = sum_na_safe(dep_lag1_aligned),
  
  # Bank Volume Leads
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  
  no_branches_bank_zip_yr = .N
  
), by = .(RSSDID, ZIPBR, YEAR)]

# ---------------------------------------------------------------------
# 4b. Calculate Future Variables (Look-Ahead)
# ---------------------------------------------------------------------
# We must sort by Bank-ZIP-Year to ensure 'shift' looks at the future of the SAME bank-zip
setorder(bank_zip_panel, RSSDID, ZIPBR, YEAR)

# Create temporary lead variables for the next 3 years
bank_zip_panel[, `:=`(
  # Lead Years (to check existence/gaps)
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  year_lead2 = shift(YEAR, 2L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  
  # Lead Types (to check if they become a closer)
  type_lead1 = shift(bank_type, 1L, type = "lead"),
  type_lead2 = shift(bank_type, 2L, type = "lead"),
  type_lead3 = shift(bank_type, 3L, type = "lead")
), by = .(RSSDID, ZIPBR)] # Grouping ensures we don't shift into a different bank/zip

# --- Define the Indicator Variables ---

bank_zip_panel[, `:=`(
  # Variable 1: Is this bank a CLOSER in the same ZIP in t+1, t+2, or t+3?
  # Logic: The record must exist (Year matches) AND the type must be "CLOSER"
  future_closer_3yr = (
    (!is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER") |
      (!is.na(year_lead2) & year_lead2 == YEAR + 2L & type_lead2 == "CLOSER") |
      (!is.na(year_lead3) & year_lead3 == YEAR + 3L & type_lead3 == "CLOSER")
  ),
  
  # Variable 2: Does this bank NOT EXIST in the data in any of the next 3 years?
  # Logic: If the next chronological record is NOT t+1, then t+1 is missing (gap/exit).
  #        If t+1 exists but t+2 is missing, then it's missing in the next 3 years.
  #        We check if the record for Year+1, Year+2, or Year+3 is absent.
  future_exit_3yr = (
    (is.na(year_lead1) | year_lead1 != YEAR + 1L) |  # Missing t+1
      (is.na(year_lead2) | year_lead2 != YEAR + 2L) |  # Missing t+2
      (is.na(year_lead3) | year_lead3 != YEAR + 3L)    # Missing t+3
  )
)]

# --- Cleanup Intermediate Columns ---
cols_to_remove <- c("year_lead1", "year_lead2", "year_lead3", 
                    "type_lead1", "type_lead2", "type_lead3")
bank_zip_panel[, (cols_to_remove) := NULL]

# Convert Booleans to Integer (0/1) if preferred
bank_zip_panel[, `:=`(
  future_closer_3yr = as.integer(future_closer_3yr),
  future_exit_3yr   = as.integer(future_exit_3yr)
)]

bank_zip_panel[,bank_type:=ifelse(future_closer_3yr==1,"FUTURE CLOSER",bank_type)]
# t <- (bank_zip_panel[ ZIPBR=="72212"])


# ---------------------------------------------------------------------
# 4. Aggregate INCUMBENTS to ZIP Level (Using the Bank Panel)
# ---------------------------------------------------------------------
# Input: bank_zip_panel (One row per Bank-ZIP-Year)
# Output: incumbent_zip_agg (One row per ZIP-Year)

incumbent_zip_agg <- bank_zip_panel[bank_type == "INCUMBENT", .(
  # Summing the Bank-level totals to get ZIP-level Incumbent totals
  incumbent_deps_curr  = sum_na_safe(bank_deps_curr),
  incumbent_deps_lag1  = sum_na_safe(bank_deps_lag1),
  incumbent_deps_lead1  = sum_na_safe(bank_deps_lead1),
  incumbent_deps_lead3  = sum_na_safe(bank_deps_lead3),
  
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
                                              total_closed_vol_zip_lag1, branches_zip_lag1,
                                              zip_growth,zip_growth_3yr)],
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
                                       (incumbent_deps_lead1 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  
  # 2. Medium Term Capture (t-1 to t+3)
  incumbent_growth_share_3yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  
  incumbent_growth_share_1yr_from_last = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead1 - incumbent_deps_lag1) / total_deps_zip_lag1,
                                       NA_real_),
  
  # 2. Medium Term Capture (t-1 to t+3)
  incumbent_growth_share_3yr_from_last = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_lag1) / total_deps_zip_lag1,
                                       NA_real_),
  
  # 2. Medium Term Capture (t-1 to t+3)
  incumbent_growth_share_3yr_own_denominator = fifelse(incumbent_deps_curr > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr,
                                       NA_real_),
  
  incumbent_growth_share_1yr_own_denominator = fifelse(incumbent_deps_curr > 0,
                                           (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr,
                                           NA_real_)
)]


TOXIC_THRESHOLD <- 100000

final_zip_panel[,`:=`(
                # Variable A: The "Pure Opportunity" (Small/Normal closures)
                share_closed_small = fifelse(total_closed_vol_zip_lag1 < TOXIC_THRESHOLD, 
                                             share_deps_closed, 
                                             0),
                
                # Variable B: The "Toxic Signal" (Massive/Distress closures)
                share_closed_large = fifelse(total_closed_vol_zip_lag1 >= TOXIC_THRESHOLD, 
                                             share_deps_closed, 
                                             0))]


# ---------------------------------------------------------------------
# 6. Final Cleanup: Handle NAs for "Zero Closure" ZIPs
# ---------------------------------------------------------------------

# Since 'sum_na_safe' returns NA when no branches close, 
# share_deps_closed becomes NA. We convert these to 0.

final_zip_panel[, share_deps_closed := ifelse(is.na(share_deps_closed),0,share_deps_closed)]

# Optional: You might also want to ensure the volume column is 0
final_zip_panel[, total_closed_vol_zip_lag1 := ifelse(is.na(total_closed_vol_zip_lag1),0,total_closed_vol_zip_lag1)]


saveRDS(final_zip_panel, output_path )
