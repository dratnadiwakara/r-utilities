rm(list=ls())
library(data.table)
library(dplyr)

# Load branch-year data
closure_opening_data <- readRDS("C:/data/closure_opening_data.rds") # sod_identiy_closures_and_openings.R
setDT(closure_opening_data)

# 1. Bank-year summary
bank_year_summary <- closure_opening_data[
  ,
  .(
    num_branches_t = .N,
    closed_t       = sum(closed == 1, na.rm = TRUE),
    opened_t       = sum(new_branch == 1, na.rm = TRUE),
    deps_closed_t  = sum(DEPSUMBR[closed == 1], na.rm = TRUE),
    deps_opened_t  = sum(DEPSUMBR[new_branch == 1], na.rm = TRUE),
    # New: counts by closure type
    old_closed_t   = sum(closed_old_branch == 1, na.rm = TRUE),
    acq_closed_t   = sum(closed_acq_branch == 1, na.rm = TRUE)
  ),
  by = .(RSSDID, YEAR)
]

setorder(bank_year_summary, RSSDID, YEAR)

# 2. Create 3-year lags for events and for number of branches
bank_year_summary[
  ,
  `:=`(
    closed_t_lag1      = shift(closed_t,      1, type = "lag"),
    closed_t_lag2      = shift(closed_t,      2, type = "lag"),
    closed_t_lag3      = shift(closed_t,      3, type = "lag"),
    opened_t_lag1      = shift(opened_t,      1, type = "lag"),
    opened_t_lag2      = shift(opened_t,      2, type = "lag"),
    opened_t_lag3      = shift(opened_t,      3, type = "lag"),
    deps_closed_lag1   = shift(deps_closed_t, 1, type = "lag"),
    deps_closed_lag2   = shift(deps_closed_t, 2, type = "lag"),
    deps_closed_lag3   = shift(deps_closed_t, 3, type = "lag"),
    deps_opened_lag1   = shift(deps_opened_t, 1, type = "lag"),
    deps_opened_lag2   = shift(deps_opened_t, 2, type = "lag"),
    deps_opened_lag3   = shift(deps_opened_t, 3, type = "lag"),
    num_branches_lag3  = shift(num_branches_t, 3, type = "lag"),
    # New: lags for type splits
    old_closed_lag1    = shift(old_closed_t,  1, type = "lag"),
    old_closed_lag2    = shift(old_closed_t,  2, type = "lag"),
    old_closed_lag3    = shift(old_closed_t,  3, type = "lag"),
    acq_closed_lag1    = shift(acq_closed_t,  1, type = "lag"),
    acq_closed_lag2    = shift(acq_closed_t,  2, type = "lag"),
    acq_closed_lag3    = shift(acq_closed_t,  3, type = "lag")
  ),
  by = RSSDID
]

# 3. Aggregate over previous 3 years
bank_year_summary[
  ,
  `:=`(
    closed_last3      = rowSums(cbind(closed_t_lag1,
                                      closed_t_lag2,
                                      closed_t_lag3),
                                na.rm = TRUE),
    opened_last3      = rowSums(cbind(opened_t_lag1,
                                      opened_t_lag2,
                                      opened_t_lag3),
                                na.rm = TRUE),
    deps_closed_last3 = rowSums(cbind(deps_closed_lag1,
                                      deps_closed_lag2,
                                      deps_closed_lag3),
                                na.rm = TRUE),
    deps_opened_last3 = rowSums(cbind(deps_opened_lag1,
                                      deps_opened_lag2,
                                      deps_opened_lag3),
                                na.rm = TRUE),
    # New: totals by type over last 3 years
    old_closed_last3   = rowSums(cbind(old_closed_lag1,
                                       old_closed_lag2,
                                       old_closed_lag3), na.rm = TRUE),
    acq_closed_last3   = rowSums(cbind(acq_closed_lag1,
                                       acq_closed_lag2,
                                       acq_closed_lag3), na.rm = TRUE)
  )
]

# 4. Fractions using num_branches_lag3 as denominator
bank_year_summary[
  ,
  `:=`(
    frac_closed_last3 = fifelse(
      num_branches_lag3 > 0,
      closed_last3 / num_branches_lag3,
      NA_real_
    ),
    frac_opened_last3 = fifelse(
      num_branches_lag3 > 0,
      opened_last3 / num_branches_lag3,
      NA_real_
    ),
    # New: requested shares
    share_existing_closed_last3 = fifelse(num_branches_lag3 > 0,
                                          old_closed_last3 / num_branches_lag3, NA_real_),
    share_recent_acq_closed_last3 = fifelse(num_branches_lag3 > 0,
                                            acq_closed_last3 / num_branches_lag3, NA_real_)
  )
]

# 5. Final bank-year panel
bank_year_panel <- bank_year_summary[
  ,
  .(
    RSSDID,
    YEAR,
    num_branches          = num_branches_t,
    frac_closed_last3,
    frac_opened_last3,
    # New outputs
    share_existing_closed_last3,
    share_recent_acq_closed_last3,
    # Keep deposit sums if needed downstream
    deps_closed_last3,
    deps_opened_last3
  )
]

# Save output
saveRDS(bank_year_panel, "C:/data/bank_year_panel_branch_dynamics.rds")
