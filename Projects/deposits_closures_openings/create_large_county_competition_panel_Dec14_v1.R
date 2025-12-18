# =============================================================================
# File: create_county_competition_panel_BY_TYPE.R
# Purpose: Analyze impact of Closures on Incumbents by Bank Type.
#          Denominator for shares/CD-growth remains Total Market (All Banks).
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
output_path <- "C:/data/county_competition_panel_by_type_Dec14_v2.rds"


# ---------------------------------------------------------------------
# 1. Load Banking Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
sod <- readRDS("C:/data/fdic_sod_2000_2025_simple.rds")
sod <- sod[YEAR==2019,.(RSSDID,ASSET)]
sod <- unique(sod)



closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)
closure_opening_data[, county := str_pad(STCNTYBR, 5, "left", "0")]

closure_opening_data[,bank_type:=ifelse(RSSDID %in% sod[ASSET>=100e6]$RSSDID,"large",
                                        ifelse(RSSDID %in% sod[ASSET>=10e6 & ASSET <100e6]$RSSDID,"medium",
                                               ifelse(RSSDID %in% sod[ASSET>1e6 & ASSET<10e6 ]$RSSDID,"small","cbo")))]

closure_opening_data <- closure_opening_data[
  !is.na(bank_type), 
  .(RSSDID, UNINUMBR, county, YEAR, DEPSUMBR, closed, new_branch, CERT, bank_type)
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
bank_county_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, county, YEAR, bank_type)]

bank_county_panel[, type_status := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 3. Future Closer Logic
# ---------------------------------------------------------------------
setorder(bank_county_panel, RSSDID, county, YEAR)
bank_county_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  status_lead1 = shift(type_status, 1L, type = "lead")
), by = .(RSSDID, county)] 

bank_county_panel[, future_closer_1yr := as.integer(
  !is.na(year_lead1) & year_lead1 == YEAR + 1L & status_lead1 == "CLOSER"
)]

bank_county_panel[, type_status_clean := type_status]
bank_county_panel[type_status == "INCUMBENT" & future_closer_1yr == 1, type_status_clean := "FUTURE_CLOSER"]
bank_county_panel[, c("year_lead1", "status_lead1") := NULL]


# ---------------------------------------------------------------------
# 4. Market Denominators (TOTAL MARKET - All Banks)
# ---------------------------------------------------------------------
county_market_totals <- bank_county_panel[YEAR < 2025, .(
  branches_curr = sum(n_branches),
  total_deps_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(county, YEAR)]

setorder(county_market_totals, county, YEAR)
county_market_totals[, branches_lag1 := shift(branches_curr, 1L, type = "lag"), by = county]


# ---------------------------------------------------------------------
# 5. Aggregate INCUMBENTS (Pivoted by Type)
# ---------------------------------------------------------------------
incumbent_agg_long <- bank_county_panel[, .(
  # Standard
  inc_curr_std  = sum_na_safe(bank_deps_curr[type_status == "INCUMBENT"]),
  inc_lead3_std = sum_na_safe(bank_deps_lead3[type_status == "INCUMBENT"]),
  inc_lead1_std = sum_na_safe(bank_deps_lead1[type_status == "INCUMBENT"]),
  inc_lag1_std  = sum_na_safe(bank_deps_lag1[type_status == "INCUMBENT"]),
  
  # Clean
  inc_curr_clean  = sum_na_safe(bank_deps_curr[type_status_clean == "INCUMBENT"]),
  inc_lead3_clean = sum_na_safe(bank_deps_lead3[type_status_clean == "INCUMBENT"]),
  inc_lead1_clean = sum_na_safe(bank_deps_lead1[type_status_clean == "INCUMBENT"]),
  inc_lag1_clean  = sum_na_safe(bank_deps_lag1[type_status_clean == "INCUMBENT"])
), by = .(county, YEAR, bank_type)]

# Reshape Long to Wide
incumbent_agg_wide <- dcast(incumbent_agg_long, 
                            county + YEAR ~ bank_type, 
                            value.var = c("inc_curr_std", "inc_lead3_std", 
                                          "inc_lead1_std", "inc_lag1_std",
                                          "inc_curr_clean", "inc_lead3_clean",
                                          "inc_lead1_clean", "inc_lag1_clean"),
                            sep = "_")


# ---------------------------------------------------------------------
# 6. Aggregate CLOSERS (Pivoted by Type)
# ---------------------------------------------------------------------
closer_keys <- bank_county_panel[type_status == "CLOSER", .(RSSDID, county, YEAR, bank_type)]
relevant_branches <- merge(closure_opening_data, closer_keys, by = c("RSSDID", "county", "YEAR", "bank_type"))

closer_agg_long <- relevant_branches[, .(
  cl_closed_lag1    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  cl_opened_curr    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  cl_rem_lag1       = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_))
), by = .(county, YEAR, bank_type)]

# Reshape Long to Wide
closer_agg_wide <- dcast(closer_agg_long, 
                         county + YEAR ~ bank_type, 
                         value.var = c("cl_closed_lag1", "cl_opened_curr", "cl_rem_lag1"),
                         sep = "_")


# ---------------------------------------------------------------------
# 7. Final Merge and Variable Construction (Short Names)
# ---------------------------------------------------------------------
final_panel <- county_market_totals 
final_panel <- merge(final_panel, incumbent_agg_wide, by = c("county", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_wide, by = c("county", "YEAR"), all.x = TRUE)

# Calculate rates dynamically for each type
bank_types <- unique(na.omit(bank_county_panel$bank_type))

# Fill NAs
vars_to_fill <- grep(paste(bank_types, collapse="|"), names(final_panel), value=TRUE)
for(j in vars_to_fill){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

for (bt in bank_types) {
  sfx <- paste0("_", bt)
  
  # Raw Columns from Pivoting
  raw_cl_closed   <- paste0("cl_closed_lag1", sfx)
  raw_cl_rem      <- paste0("cl_rem_lag1", sfx)
  raw_cl_open     <- paste0("cl_opened_curr", sfx)
  
  raw_inc_curr    <- paste0("inc_curr_std", sfx)
  raw_inc_lag1    <- paste0("inc_lag1_std", sfx)
  raw_inc_lead1   <- paste0("inc_lead1_std", sfx)
  raw_inc_lead3   <- paste0("inc_lead3_std", sfx)
  
  # --- 1. Independent Variable: Share of Deposits Closed ---
  # Name: share_cl_[Type]
  final_panel[, (paste0("share_cl", sfx)) := fifelse(total_deps_lag1 > 0,
                                                     get(raw_cl_closed) / total_deps_lag1, 0)]
  
  # --- 2. Control: Market Share of Incumbents ---
  # Name: mkt_share_[Type]
  final_panel[, (paste0("mkt_share", sfx)) := fifelse(total_deps_lag1 > 0,
                                                      get(raw_inc_lag1) / total_deps_lag1, 0)]
  
  # --- 3. Dependent Variables: Growth ---
  # Name: gr_1y_[Type]
  final_panel[, (paste0("gr_1y", sfx)) := fifelse(get(raw_inc_curr) > 0,
                                                  (get(raw_inc_lead1) - get(raw_inc_curr)) / get(raw_inc_curr), 
                                                  NA_real_)]
  
  # Name: gr_3y_[Type]
  final_panel[, (paste0("gr_3y", sfx)) := fifelse(get(raw_inc_curr) > 0,
                                                  (get(raw_inc_lead3) - get(raw_inc_curr)) / get(raw_inc_curr),
                                                  NA_real_)]
  
  # Name: gr_1y_cd_[Type] (Common Denominator)
  final_panel[, (paste0("gr_1y_cd", sfx)) := fifelse(total_deps_lag1 > 0,
                                                     (get(raw_inc_lead1) - get(raw_inc_curr)) / total_deps_lag1,
                                                     NA_real_)]
  
  # Name: gr_3y_cd_[Type] (Common Denominator)
  final_panel[, (paste0("gr_3y_cd", sfx)) := fifelse(total_deps_lag1 > 0,
                                                     (get(raw_inc_lead3) - get(raw_inc_curr)) / total_deps_lag1,
                                                     NA_real_)]
  
  # Name: gr_1y_cd_lag_[Type] (Lagged Common Denominator - for robustness)
  final_panel[, (paste0("gr_1y_cd_lag", sfx)) := fifelse(total_deps_lag1 > 0,
                                                         (get(raw_inc_lead1) - get(raw_inc_lag1)) / total_deps_lag1,
                                                         NA_real_)]
  
  # Raw Columns (Already defined in previous code)
  raw_cl_rem <- paste0("cl_rem_lag1", sfx)
  
  # --- [NEW] Equivalent of closer_remaining_mkt_share_lag1 ---
  # Name: share_rem_[Type]
  final_panel[, (paste0("share_rem", sfx)) := fifelse(total_deps_lag1 > 0,
                                                      get(raw_cl_rem) / total_deps_lag1, 0)]
}

# --- Global Controls ---
final_panel[, log_mkt_size_lag1 := log(total_deps_lag1 + 1)]

# Save
saveRDS(final_panel, output_path)