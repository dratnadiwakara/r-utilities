rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)

sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path <- "C:/data/closure_opening_data_simple.rds"
output_path <- "C:/data/zip_competition_churn_panel_Nov24_v4.rds"

# ---------------------------------------------------------------------
# 1. Load Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)

closure_opening_data <- closure_opening_data[, .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, closed, new_branch)]
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
# 2. Classify Banks
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
# 3. Calculate Market Denominators (ZIP Totals)
# ---------------------------------------------------------------------
zip_market_totals <- closure_opening_data[, .(
  branches_zip_curr = uniqueN(UNINUMBR),
  total_deps_zip_lag1 = sum_na_safe(dep_lag1_aligned)
), by = .(ZIPBR, YEAR)]

setorder(zip_market_totals, ZIPBR, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]

# ---------------------------------------------------------------------
# 4. Aggregate INCUMBENTS
# ---------------------------------------------------------------------
incumbent_agg <- closure_opening_data[bank_type == "INCUMBENT", .(
  incumbent_deps_curr  = sum_na_safe(DEPSUMBR),
  incumbent_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  incumbent_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  
  # --- NEW: Get the lagged deposits of these incumbents ---
  incumbent_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  
  n_incumbent_branches = .N
), by = .(ZIPBR, YEAR)]

# Calculate Growth (Added NA check to prevent Inf if current deposits are 0)
incumbent_agg[, `:=`(
  incumbent_dep_gr_1yr = fifelse(incumbent_deps_curr > 0, 
                                 (incumbent_deps_lead1 - incumbent_deps_curr) / incumbent_deps_curr, NA_real_),
  incumbent_dep_gr_3yr = fifelse(incumbent_deps_curr > 0, 
                                 (incumbent_deps_lead3 - incumbent_deps_curr) / incumbent_deps_curr, NA_real_)
)]

# ---------------------------------------------------------------------
# 5. Aggregate CLOSERS
# ---------------------------------------------------------------------
closer_agg <- closure_opening_data[bank_type == "CLOSER", .(
  closer_n_closed = sum(closed == 1L, na.rm = TRUE),
  closer_closed_vol_lag1 = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  closer_n_opened = sum(new_branch == 1L, na.rm = TRUE),
  closer_opened_vol_curr = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_))
), by = .(ZIPBR, YEAR)]

# ---------------------------------------------------------------------
# 6. Final Merge and Calculations
# ---------------------------------------------------------------------
final_panel <- zip_market_totals 
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
  # Closure Share (using Market Deposits as denominator)
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 0),
  
  # --- NEW VARIABLE: Incumbent Market Share (t-1) ---
  incumbent_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                     incumbent_deps_lag1 / total_deps_zip_lag1, 0),
  
  # Other Controls
  frac_branches_closed = fifelse(branches_zip_lag1 > 0, 
                                 closer_n_closed / branches_zip_lag1, 0),
  relocation_openings_frac = fifelse(branches_zip_lag1 > 0, 
                                     closer_n_opened/ branches_zip_lag1, 0),
  relocation_openings_vol_share = fifelse(total_deps_zip_lag1 > 0, 
                                          closer_opened_vol_curr / total_deps_zip_lag1, 0)
)]





# ---------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------
# Subset to clean columns
output_columns <- c("ZIPBR", "YEAR", 
                    "incumbent_dep_gr_1yr", "incumbent_dep_gr_3yr", 
                    "share_deps_closed", "frac_branches_closed",
                    "incumbent_mkt_share_lag1", # <--- Included in output
                    "relocation_openings_frac", "relocation_openings_vol_share",
                    "incumbent_deps_curr", "n_incumbent_branches", "branches_zip_lag1")

final_panel_clean <- final_panel[, ..output_columns]

print(head(final_panel_clean))
saveRDS(final_panel_clean, output_path)