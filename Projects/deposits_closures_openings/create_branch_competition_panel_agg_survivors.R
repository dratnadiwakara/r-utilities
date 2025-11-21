# =====================================================================
# File: create_zip_competition_panel_REFINED.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks.
#
# Structure:
#   1. Identify "Incumbents" (Banks with 0 closures in ZIP-Year)
#   2. Identify "Closers" (Banks with >0 closures in ZIP-Year)
#   3. Dependent Var: Aggregated deposit growth of Incumbents.
#   4. Independent Vars: Closure intensity of Closers (Rate & Share),
#      adjusted by their Opening activity (Relocation proxies).
# =====================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)

# --- Helper Function for NA-safe Summing ---
# Returns NA if all inputs are NA, otherwise sums non-NA values.
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  # Force the sum to be a double (numeric) to match NA_real_
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/zip_competition_churn_panel.rds"


# ---------------------------------------------------------------------
# 1. Load Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)

# Keep essential columns
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, closed, new_branch)
]

# Sort for time-series operations
setorder(closure_opening_data, UNINUMBR, YEAR)

# Create Shifts (Leads for Growth, Lags for Closure Share)
closure_opening_data[, `:=`(
  # For Growth Calculation (Incumbents)
  dep_lead3        = shift(DEPSUMBR, 3L, type = "lead"),
  year_lead3       = shift(YEAR, 3L, type = "lead"),
  dep_lead1        = shift(DEPSUMBR, 1L, type = "lead"),
  year_lead1       = shift(YEAR, 1L, type = "lead"),
  
  # For Closure Share Calculation (Closers)
  dep_lag1         = shift(DEPSUMBR, 1L, type = "lag"),
  year_lag1        = shift(YEAR, 1L, type = "lag")
), by = UNINUMBR]

# Align Leads (Ensure strictly 3 years or 1 year)
closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_)
)]


# ---------------------------------------------------------------------
# 2. Classify Banks: "Incumbent" vs "Closer"
# ---------------------------------------------------------------------
# Group by Bank-ZIP-Year to see if they closed ANY branch
bank_zip_status <- closure_opening_data[, .(
  n_closed = sum(closed == 1L, na.rm = TRUE)
), by = .(RSSDID, ZIPBR, YEAR)]

# Tag the bank status
bank_zip_status[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]

# Merge status back to main data
closure_opening_data <- merge(closure_opening_data, 
                              bank_zip_status[, .(RSSDID, ZIPBR, YEAR, bank_type)],
                              by = c("RSSDID", "ZIPBR", "YEAR"),
                              all.x = TRUE)


# ---------------------------------------------------------------------
# 3. Calculate Market Denominators (ZIP Totals)
# ---------------------------------------------------------------------
# We need total ZIP branches (t-1) and total ZIP deposits (t-1) 
# regardless of whether banks were closers or incumbents.

zip_market_totals <- closure_opening_data[, .(
  branches_zip_curr = uniqueN(UNINUMBR),
  total_deps_zip_lag1 = sum_na_safe(dep_lag1)
), by = .(ZIPBR, YEAR)]

# Get lagged branch count
setorder(zip_market_totals, ZIPBR, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]


# ---------------------------------------------------------------------
# 4. Aggregate GROUP A: Incumbents (The Dependent Variable)
# ---------------------------------------------------------------------
# Filter: Only banks that DID NOT close branches in this ZIP-Year
incumbent_agg <- closure_opening_data[bank_type == "INCUMBENT", .(
  
  # Current Deposits (t)
  incumbent_deps_curr = sum_na_safe(DEPSUMBR),
  
  # Future Deposits (t+1, t+3)
  incumbent_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  incumbent_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  
  # Count for sanity check
  n_incumbent_branches = .N
  
), by = .(ZIPBR, YEAR)]

# Calculate Incumbent Growth Rates
incumbent_agg[, `:=`(
  incumbent_dep_gr_1yr = (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr,
  incumbent_dep_gr_3yr = (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr
)]


# ---------------------------------------------------------------------
# 5. Aggregate GROUP B: Closers (The Independent Variables)
# ---------------------------------------------------------------------
# Filter: Only banks that CLOSED at least one branch in this ZIP-Year
closer_agg <- closure_opening_data[bank_type == "CLOSER", .(
  
  # 1. CLOSURE METRICS
  # How many they closed
  closer_n_closed = sum(closed == 1L, na.rm = TRUE),
  
  # Volume of closed branches at t-1
  closer_closed_vol_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1, NA_real_)),
  
  # 2. RELOCATION/OFFSET METRICS
  # How many they OPENED in the same year
  closer_n_opened = sum(new_branch == 1L, na.rm = TRUE),
  
  # Volume of NEW branches at t (How much did they recapture immediately?)
  closer_opened_vol_curr = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_))
  
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 6. Final Merge and Rate Calculations
# ---------------------------------------------------------------------
# Start with ZIP Market Totals (defines the universe of ZIP-Years)
final_panel <- zip_market_totals[, .(ZIPBR, YEAR, branches_zip_lag1, total_deps_zip_lag1)]

# Merge Incumbents
final_panel <- merge(final_panel, incumbent_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)

# Merge Closers
final_panel <- merge(final_panel, closer_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)

# Fill NAs for Closers with 0 
# (If no record in closer_agg, it means NO closures occurred in that ZIP-Year)
cols_to_zero <- c("closer_n_closed", "closer_closed_vol_lag1", 
                  "closer_n_opened", "closer_opened_vol_curr")

for(j in cols_to_zero){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

# --- FINAL VARIABLES ---

final_panel[, `:=`(
  # A. Fraction of branches closed (Closer Count / Total Market Branches t-1)
  frac_branches_closed = fifelse(branches_zip_lag1 > 0, 
                                 closer_n_closed / branches_zip_lag1, 
                                 0),
  
  # B. Share of total deposits of closed branches (Closer Lost Vol t-1 / Total Market Vol t-1)
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 
                              0),
  
  # C. Relocation/Offset Count (Number of branches opened by Closers)
  relocation_openings_frac = fifelse(branches_zip_lag1 > 0, 
                                     closer_n_opened/ branches_zip_lag1, 
                                     0),
  
  # D. Relocation/Offset Volume (Deposits of new branches by Closers at t)
  # (Optional: Normalize this by total market deposits if desired, keeping as raw level per request)
  relocation_openings_vol_share = fifelse(total_deps_zip_lag1 > 0, 
                                          closer_opened_vol_curr / total_deps_zip_lag1, 
                                          0)
)]

# ---------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------
# Remove intermediate calculation columns if desired to keep it clean
output_columns <- c("ZIPBR", "YEAR", 
                    # Dependent Vars
                    "incumbent_dep_gr_1yr", "incumbent_dep_gr_3yr", 
                    # Independent Vars (Closure)
                    "frac_branches_closed", "share_deps_closed",
                    # Independent Vars (Relocation/Offset)
                    "relocation_openings_frac", "relocation_openings_vol_share",
                    # Metadata
                    "incumbent_deps_curr", "n_incumbent_branches","branches_zip_lag1")

final_panel_clean <- final_panel[, ..output_columns]

cat("\nPreview of Clean Panel:\n")
print(head(final_panel_clean))

saveRDS(final_panel_clean, output_path)