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
output_path <- "C:/data/zip_competition_churn_panel_refined.rds"


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

# Align Leads
closure_opening_data[, `:=`(
  dep_lead3_aligned = fifelse(year_lead3 == YEAR + 3L, dep_lead3, NA_real_),
  dep_lead1_aligned = fifelse(year_lead1 == YEAR + 1L, dep_lead1, NA_real_)
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
# 3. Calculate Market Denominators (ZIP Totals) & Filter Outliers
# ---------------------------------------------------------------------
# We need total ZIP branches (t-1) and total ZIP deposits (t-1).
# We also calculate Current deposits to check for extreme growth outliers.

zip_market_totals <- closure_opening_data[, .(
  branches_zip_curr   = uniqueN(UNINUMBR),
  total_deps_zip_curr = sum_na_safe(DEPSUMBR) # Added to calculate growth
), by = .(ZIPBR, YEAR)]

setorder(zip_market_totals, ZIPBR, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]
zip_market_totals[, total_deps_zip_lag1 := shift(total_deps_zip_curr, 1L, type = "lag"), by = ZIPBR]
# --- FILTERING STEP ---
# Calculate One-Year ZIP Growth Rate
zip_market_totals[, zip_growth := (total_deps_zip_curr - total_deps_zip_lag1) / total_deps_zip_lag1]

# Keep only ZIP-Years with realistic growth (-50% to +200%)
# We uses !is.na() check to ensure we don't drop the first year of data if distinct handling isn't desired,
# though typically growth is NA for the first year, so those might get dropped. 
# If you want to KEEP the first year (where growth is NA), use: 
# zip_market_totals[is.na(zip_growth) | (zip_growth >= -0.5 & zip_growth <= 2.0)]
# Assuming strict filtering:
exclude_zips <- unique(zip_market_totals[zip_growth < -0.5 | zip_growth > 2.0]$ZIPBR)
zip_market_totals <- zip_market_totals[!ZIPBR %in% exclude_zips]
zip_market_totals[,lag_YEAR:=lag(YEAR),by=ZIPBR]

zip_market_totals <- zip_market_totals[YEAR==(lag_YEAR+1)]



# ---------------------------------------------------------------------
# 4. Aggregate GROUP A: Incumbents (The Numerator for Y)
# ---------------------------------------------------------------------
incumbent_agg <- closure_opening_data[bank_type == "INCUMBENT" & ZIPBR %in% unique(zip_market_totals$ZIPBR), .(
  
  # Current Deposits (t)
  incumbent_deps_curr = sum_na_safe(DEPSUMBR),

  n_incumbent_branches = .N
  
), by = .(ZIPBR, YEAR)]


setorder(incumbent_agg,ZIPBR,YEAR)


incumbent_agg[,incumbent_deps_lag1 := lag(incumbent_deps_curr,1), by=ZIPBR]
incumbent_agg[,incumbent_deps_lead1 := lead(incumbent_deps_curr,1), by=ZIPBR]
incumbent_agg[,incumbent_deps_lead3 := lead(incumbent_deps_curr,3), by=ZIPBR]



# Note: We calculate the normalized growth rates in Section 6
# so that we can use the Total Market denominator.


# ---------------------------------------------------------------------
# 5. Aggregate GROUP B: Closers (The Numerator for X)
# ---------------------------------------------------------------------
closer_agg <- closure_opening_data[bank_type == "CLOSER" & ZIPBR %in% unique(zip_market_totals$ZIPBR), .(
  closer_n_closed = sum(closed == 1L, na.rm = TRUE),
  closer_closed_vol_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1, NA_real_)),
  closer_n_opened = sum(new_branch == 1L, na.rm = TRUE),
  closer_opened_vol_curr = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_))
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 6. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- zip_market_totals[, .(ZIPBR, YEAR, branches_zip_lag1, total_deps_zip_lag1)]

final_panel <- merge(final_panel, incumbent_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)

# Fill NAs for Closers with 0
cols_to_zero <- c("closer_n_closed", "closer_closed_vol_lag1", 
                  "closer_n_opened", "closer_opened_vol_curr")
for(j in cols_to_zero){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

# --- FINAL VARIABLES ---

final_panel[, `:=`(
  # --- Independent Variable (X) ---
  # Share of Total Market Deposits that were Closed
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 
                              0),
  
  # --- Dependent Variables (Y) ---
  # CO-AUTHOR FIX: Normalize Incumbent Growth by TOTAL MARKET SIZE (same denom as X)
  # Interpretation: % of the Total Market captured by incumbents
  
  incumbent_growth_share_1yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead1 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  
  incumbent_growth_share_3yr = fifelse(total_deps_zip_lag1 > 0,
                                       (incumbent_deps_lead3 - incumbent_deps_curr) / total_deps_zip_lag1,
                                       NA_real_),
  
  # --- Alternative Dependent Variables (Robustness) ---
  # Standard growth rate (Growth relative to Incumbent Size)
  incumbent_dep_gr_1yr = (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr,
  incumbent_dep_gr_3yr = (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr,
  
  # --- Other Controls ---
  frac_branches_closed = fifelse(branches_zip_lag1 > 0, 
                                 closer_n_closed / branches_zip_lag1, 
                                 0),
  
  relocation_openings_frac = fifelse(branches_zip_lag1 > 0, 
                                     closer_n_opened/ branches_zip_lag1, 
                                     0)
)]

# ---------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------
output_columns <- c("ZIPBR", "YEAR", 
                    # New Common-Denominator Ys
                    "incumbent_growth_share_1yr", "incumbent_growth_share_3yr",
                    # Old Percentage Growth Ys (Keep for robustness)
                    "incumbent_dep_gr_1yr", "incumbent_dep_gr_3yr",
                    # Xs
                    "share_deps_closed", "frac_branches_closed",
                    "relocation_openings_frac", 
                    # Metadata
                    "total_deps_zip_lag1", "n_incumbent_branches","branches_zip_lag1")

final_panel_clean <- final_panel[, ..output_columns]

cat("\nPreview of Clean Panel:\n")
print(head(final_panel_clean))

saveRDS(final_panel_clean, output_path)