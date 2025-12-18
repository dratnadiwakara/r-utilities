# =============================================================================
# File: create_cz_competition_panel.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks at CZ Level.
# Update: Aggregates to CZ, Includes Primary State FIPS.
# =============================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)
library(zoo) # For na.locf
library(stringr)

# --- Helper Function for NA-safe Summing ---
sum_na_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  return(as.numeric(sum(x, na.rm = TRUE))) 
}

# --- Define Paths ---
data_path   <- "C:/data/closure_opening_data_simple.rds"
cz_map_path <- "C:/data/czone_county.csv" 
output_path <- "C:/data/cz_competition_panel_Dec13_v1.rds"


# ---------------------------------------------------------------------
# 1. Load Maps & Define CZ State
# ---------------------------------------------------------------------
# Load CZ Map
cz_map <- fread(cz_map_path)
cz_map[, county := str_pad(county, 5, "left", "0")]

# Create State FIPS from County Code
cz_map[, state_fips := substr(county, 1, 2)]

# Define "Primary State" for each CZ
# Logic: Assign the state that has the most counties within the CZ.
# If a CZ spans states (e.g., NY/NJ), this picks the dominant one to maintain unique CZ rows.
cz_state_counts <- cz_map[, .N, by = .(czone, state_fips)]
setorder(cz_state_counts, czone, -N) # Sort by count descending
cz_primary_state <- cz_state_counts[, .(state = first(state_fips)), by = czone]


# ---------------------------------------------------------------------
# 2. Load Banking Data & Merge CZ
# ---------------------------------------------------------------------
closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)

# Create padded FIPS for merging
closure_opening_data[, county := str_pad(STCNTYBR, 5, "left", "0")]

# Merge CZ info into banking data
closure_opening_data <- merge(closure_opening_data, cz_map[, .(county, czone)], by = "county", all.x = TRUE)

# Filter out records that didn't match a CZ
closure_opening_data <- closure_opening_data[!is.na(czone)]

# Keep essential columns
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, czone, YEAR, DEPSUMBR, closed, new_branch, CERT)
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
# 3. Aggregation to BANK-CZ Level
# ---------------------------------------------------------------------
bank_cz_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, czone, YEAR)]

bank_cz_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 4. Future Closer Logic
# ---------------------------------------------------------------------
setorder(bank_cz_panel, RSSDID, czone, YEAR)
bank_cz_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  year_lead2 = shift(YEAR, 2L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  type_lead1 = shift(bank_type, 1L, type = "lead"),
  type_lead2 = shift(bank_type, 2L, type = "lead"),
  type_lead3 = shift(bank_type, 3L, type = "lead")
), by = .(RSSDID, czone)] 

bank_cz_panel[, future_closer_1yr := as.integer(
  !is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER"
)]

bank_cz_panel[, bank_type_clean := bank_type]
bank_cz_panel[bank_type == "INCUMBENT" & future_closer_1yr == 1, bank_type_clean := "FUTURE_CLOSER"]
bank_cz_panel[, c("year_lead1", "year_lead2", "year_lead3", "type_lead1", "type_lead2", "type_lead3") := NULL]


# ---------------------------------------------------------------------
# 5. Market Denominators (CZ Level)
# ---------------------------------------------------------------------
cz_market_totals <- bank_cz_panel[YEAR<2025, .(
  branches_cz_curr = sum(n_branches),
  total_deps_cz_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(czone, YEAR)]

setorder(cz_market_totals, czone, YEAR)
cz_market_totals[, branches_cz_lag1 := shift(branches_cz_curr, 1L, type = "lag"), by = czone]


# ---------------------------------------------------------------------
# 6. Aggregate INCUMBENTS
# ---------------------------------------------------------------------
incumbent_agg <- bank_cz_panel[, .(
  incumbent_deps_curr_std  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT"]),
  incumbent_deps_lead3_std = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT"]),
  incumbent_deps_lead1_std = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT"]),
  incumbent_deps_lag1_std  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT"]),
  
  incumbent_deps_curr_clean  = sum_na_safe(bank_deps_curr[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lead3_clean = sum_na_safe(bank_deps_lead3[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lead1_clean = sum_na_safe(bank_deps_lead1[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lag1_clean  = sum_na_safe(bank_deps_lag1[bank_type_clean == "INCUMBENT"])
), by = .(czone, YEAR)]


# ---------------------------------------------------------------------
# 7. Aggregate CLOSERS
# ---------------------------------------------------------------------
closer_keys <- bank_cz_panel[bank_type == "CLOSER", .(RSSDID, czone, YEAR)]
relevant_branches <- merge(closure_opening_data, closer_keys, by = c("RSSDID", "czone", "YEAR"))

closer_agg_final <- relevant_branches[, .(
  closer_closed_vol_lag1    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  closer_opened_vol_curr    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  closer_remaining_vol_lag1 = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_)),
  closer_n_remaining = sum(closed == 0L & new_branch == 0L, na.rm = TRUE),
  closer_n_closed    = sum(closed == 1L, na.rm=TRUE),
  closer_n_opened    = sum(new_branch == 1L, na.rm=TRUE)
), by = .(czone, YEAR)]


# ---------------------------------------------------------------------
# 8. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- cz_market_totals 
final_panel <- merge(final_panel, incumbent_agg, by = c("czone", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_final, by = c("czone", "YEAR"), all.x = TRUE)

# --- NEW: Merge Primary State back into Final Panel ---
final_panel <- merge(final_panel, cz_primary_state, by = "czone", all.x = TRUE)

# Fill NAs
cols_to_zero <- c("closer_closed_vol_lag1", "closer_opened_vol_curr", 
                  "closer_remaining_vol_lag1", "closer_n_remaining", "closer_n_closed", "closer_n_opened")

for(j in cols_to_zero){
  if (j %in% names(final_panel)) {
    set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
  }
}

final_panel[, `:=`(
  # --- Independent Variables ---
  share_deps_closed = fifelse(total_deps_cz_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_cz_lag1, 0),
  
  # --- Dependent Variables ---
  incumbent_dep_gr_1yr = fifelse(total_deps_cz_lag1 > 0,
                                 (incumbent_deps_lead1_std - incumbent_deps_curr_std) / incumbent_deps_curr_std, NA_real_),
  incumbent_dep_gr_1yr_cd = fifelse(total_deps_cz_lag1 > 0,
                                    (incumbent_deps_lead1_std - incumbent_deps_curr_std) / incumbent_deps_curr_std, NA_real_),
  incumbent_dep_gr_1yr_cd_lag = fifelse(total_deps_cz_lag1 > 0,
                                        (incumbent_deps_lead1_std - incumbent_deps_lag1_std) / total_deps_cz_lag1, NA_real_),
  incumbent_dep_gr_3yr = fifelse(total_deps_cz_lag1 > 0,
                                 (incumbent_deps_lead3_std - incumbent_deps_curr_std) / incumbent_deps_curr_std, NA_real_),
  incumbent_dep_gr_3yr_cd = fifelse(total_deps_cz_lag1 > 0,
                                    (incumbent_deps_lead3_std - incumbent_deps_curr_std) / total_deps_cz_lag1, NA_real_),
  incumbent_dep_gr_3yr_lag = fifelse(total_deps_cz_lag1 > 0,
                                     (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / incumbent_deps_lag1_std, NA_real_),
  incumbent_dep_gr_3yr_cd_lag = fifelse(total_deps_cz_lag1 > 0,
                                        (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / total_deps_cz_lag1, NA_real_),
  
  # --- Controls ---
  incumbent_mkt_share_lag1 = fifelse(total_deps_cz_lag1 > 0, incumbent_deps_lag1_std / total_deps_cz_lag1, 0),
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_cz_lag1 > 0, closer_remaining_vol_lag1 / total_deps_cz_lag1, 0),
  log_total_market_size_lag1 = log(total_deps_cz_lag1 + 1),
  relocation_openings_vol_share = fifelse(total_deps_cz_lag1 > 0, closer_opened_vol_curr / total_deps_cz_lag1, 0)
)]


# ---------------------------------------------------------------------
# 9. Save
# ---------------------------------------------------------------------
output_columns <- c("czone", "state", "YEAR", # Added 'state'
                    
                    "incumbent_dep_gr_1yr",
                    "incumbent_dep_gr_1yr_cd",
                    "incumbent_dep_gr_1yr_cd_lag",
                    "incumbent_dep_gr_3yr",
                    "incumbent_dep_gr_3yr_lag",
                    "incumbent_dep_gr_3yr_cd", 
                    "incumbent_dep_gr_3yr_cd_lag",
                    
                    "share_deps_closed",
                    
                    "log_total_market_size_lag1",
                    "incumbent_mkt_share_lag1", 
                    "closer_remaining_mkt_share_lag1",
                    "relocation_openings_vol_share",
                    "total_deps_cz_lag1",
                    "branches_cz_lag1")

final_panel_clean <- final_panel[, ..output_columns]

saveRDS(final_panel_clean, output_path)