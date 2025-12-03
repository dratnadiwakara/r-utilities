# =============================================================================
# File: create_zip_competition_panel_Nov24_v9.R
# Purpose: Analyze impact of "Closer" banks on "Incumbent" banks.
# Update V9: Adds weighted average online usage intensity for closed branches.
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
app_path    <- "C:/data/data_bank_mobile_app_updates_2007_2021.csv"
output_path <- "C:/data/zip_competition_churn_panel_Dec01_v1.rds"


# ---------------------------------------------------------------------
# 0. PROCESS APP DATA
# ---------------------------------------------------------------------
# Goal: Create bank-year flags for "Has App" and "High Updates"
app_data <- fread(app_path)

# 1. Clean and Metric Calculation
app_data[, n_updates := (gp_app_updates + as_app_updates) / 2]

# 2. Balance Panel & Fill Forward (2007-2025)
min_yr <- min(app_data$year)
max_yr <- 2025 
all_certs <- unique(app_data$FDIC_certificate_id)

app_grid <- CJ(FDIC_certificate_id = all_certs, year = min_yr:max_yr)
app_panel <- merge(app_grid, app_data, by = c("FDIC_certificate_id", "year"), all.x = TRUE)

setorder(app_panel, FDIC_certificate_id, year)

cols_to_fill <- c("first_app_available", "n_updates")
app_panel[, (cols_to_fill) := lapply(.SD, function(x) zoo::na.locf(x, na.rm = FALSE)), 
          by = FDIC_certificate_id, .SDcols = cols_to_fill]

app_panel[is.na(first_app_available), first_app_available := 0]
app_panel[is.na(n_updates), n_updates := 0]

# 3. Calculate 3-Year Rolling Updates
app_panel[, updates_3yr_roll := frollsum(n_updates, n = 3, align = "right", fill = 0), 
          by = FDIC_certificate_id]

# 4. Calculate Annual Medians 
medians_by_year <- app_panel[first_app_available == 1, 
                             .(median_updates = median(updates_3yr_roll, na.rm = TRUE)), 
                             by = year]

app_panel <- merge(app_panel, medians_by_year, by = "year", all.x = TRUE)

# 5. Define Categories
app_panel[, `:=`(
  has_app = first_app_available,
  is_high_update = as.integer(first_app_available == 1 & updates_3yr_roll > median_updates)
)]

app_panel_clean <- app_panel[, .(CERT = FDIC_certificate_id, YEAR = year, has_app, is_high_update)]


# ---------------------------------------------------------------------
# 0.5 PROCESS WEB VISITS DATA (NEW SECTION)
# ---------------------------------------------------------------------
# web_data <- readRDS("C:/data/web_visits_2021.rds")
visit_data <- readRDS("C:/data/bank_branch_visits_count_2019_2022.rds")
sod <- readRDS("C:/data/fdic_sod_2000_2025_simple.rds")
sod <- sod[YEAR==2021,.(RSSDID,UNINUMBR,DEPSUMBR)]
sod <- unique(sod)
visit_data <- merge(visit_data,sod,by="UNINUMBR")
visit_data <- visit_data[,.(UNINUMBR,DATE_RANGE_START,RAW_VISIT_COUNTS,DEPSUMBR,RSSDID)]

visit_data[, year := year(DATE_RANGE_START)]

# Filter for only the relevant years
visit_data <- visit_data[year %in% c(2019, 2021)]

# 2. Calculate Branch-Level Means (Annualize)
# We average the monthly visits and deposits to get one observation per branch per year
branch_annual <- visit_data[, .(
  avg_monthly_visits = mean(RAW_VISIT_COUNTS, na.rm = TRUE),
  avg_deposits = mean(DEPSUMBR, na.rm = TRUE)
), by = .(RSSDID, UNINUMBR, year)]

# 3. Calculate Bank-Level Deposit-Weighted Averages
# Formula: Sum(Branch_Visits * Branch_Deposits) / Sum(Total_Bank_Deposits)
# This gives the "Visits for the average dollar of deposits" at that bank.
bank_weighted <- branch_annual[, .(
  weighted_visits = sum(avg_monthly_visits * avg_deposits, na.rm = TRUE) / sum(avg_deposits, na.rm = TRUE)
), by = .(RSSDID, year)]

# 4. Reshape to Wide Format to Calculate Change
bank_change <- dcast(bank_weighted, RSSDID ~ year, value.var = "weighted_visits")

# Rename columns (Columns will be '2019' and '2021')
setnames(bank_change, c("2019", "2021"), c("w_visits_2019", "w_visits_2021"))

# 5. Calculate the Change
bank_change[, `:=`(
  visit_change_raw = w_visits_2021 - w_visits_2019,
  visit_change_pct = (w_visits_2021 - w_visits_2019) / w_visits_2019
)]


bank_change[,visit_change_pct:=Winsorize(visit_change_pct, val = quantile(visit_change_pct, probs = c(0.05, 0.95), na.rm = T))]

# ---------------------------------------------------------------------
# 1. Load Banking Data & Compute Branch-Level Leads/Lags
# ---------------------------------------------------------------------
closure_opening_data <- readRDS(data_path)
setDT(closure_opening_data)

# Keep essential columns 
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, closed, new_branch, CERT)
]

# --- MERGE APP DATA ---
closure_opening_data <- merge(closure_opening_data, app_panel_clean, 
                              by = c("CERT", "YEAR"), all.x = TRUE)

# --- MERGE WEB VISITS DATA (NEW) ---
# Merging on RSSDID. Assuming web intensity is constant across years or provided as cross-section.
# If web_visits_assets varies by year, you need to ensure 'YEAR' is in web_data and add to 'by'.
closure_opening_data <- merge(closure_opening_data, bank_change[,.(RSSDID,visit_change_pct)], 
                              by = "RSSDID", all.x = TRUE)

# Fill NAs
closure_opening_data[YEAR >= 2007 & is.na(has_app), has_app := 0]
closure_opening_data[YEAR >= 2007 & is.na(is_high_update), is_high_update := 0]
# For web visits, if missing, we assume 0 for the weighted calc, or keep NA based on preference.
# Here setting to 0 to avoid dropping branches from weighted average entirely.
closure_opening_data[is.na(visit_change_pct), visit_change_pct := 0] 


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
# 2. Aggregation to BANK-ZIP Level
# ---------------------------------------------------------------------
bank_zip_panel <- closure_opening_data[, .(
  bank_deps_curr  = sum_na_safe(DEPSUMBR),
  bank_deps_lead1 = sum_na_safe(dep_lead1_aligned),
  bank_deps_lead3 = sum_na_safe(dep_lead3_aligned),
  bank_deps_lag1  = sum_na_safe(dep_lag1_aligned),
  n_closed = sum(closed == 1L, na.rm = TRUE),
  n_opened = sum(new_branch == 1L, na.rm = TRUE),
  n_branches = .N
), by = .(RSSDID, ZIPBR, YEAR)]

bank_zip_panel[, bank_type := fifelse(n_closed > 0, "CLOSER", "INCUMBENT")]


# ---------------------------------------------------------------------
# 3. Future Closer Logic
# ---------------------------------------------------------------------
setorder(bank_zip_panel, RSSDID, ZIPBR, YEAR)
bank_zip_panel[, `:=`(
  year_lead1 = shift(YEAR, 1L, type = "lead"),
  year_lead2 = shift(YEAR, 2L, type = "lead"),
  year_lead3 = shift(YEAR, 3L, type = "lead"),
  type_lead1 = shift(bank_type, 1L, type = "lead"),
  type_lead2 = shift(bank_type, 2L, type = "lead"),
  type_lead3 = shift(bank_type, 3L, type = "lead")
), by = .(RSSDID, ZIPBR)] 

bank_zip_panel[, future_closer_3yr := as.integer(
  (!is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER") |
    (!is.na(year_lead2) & year_lead2 == YEAR + 2L & type_lead2 == "CLOSER") |
    (!is.na(year_lead3) & year_lead3 == YEAR + 3L & type_lead3 == "CLOSER")
)]

bank_zip_panel[, future_closer_1yr := as.integer(
  !is.na(year_lead1) & year_lead1 == YEAR + 1L & type_lead1 == "CLOSER"
)]

bank_zip_panel[, bank_type_clean := bank_type]
bank_zip_panel[bank_type == "INCUMBENT" & future_closer_1yr == 1, bank_type_clean := "FUTURE_CLOSER"]
bank_zip_panel[, c("year_lead1", "year_lead2", "year_lead3", "type_lead1", "type_lead2", "type_lead3") := NULL]


# ---------------------------------------------------------------------
# 4. Calculate Market Denominators
# ---------------------------------------------------------------------
zip_market_totals <- bank_zip_panel[YEAR<2025, .(
  branches_zip_curr = sum(n_branches),
  total_deps_zip_lag1 = sum_na_safe(bank_deps_lag1)
), by = .(ZIPBR, YEAR)]

setorder(zip_market_totals, ZIPBR, YEAR)
zip_market_totals[, branches_zip_lag1 := shift(branches_zip_curr, 1L, type = "lag"), by = ZIPBR]


# ---------------------------------------------------------------------
# 5. Aggregate INCUMBENTS
# ---------------------------------------------------------------------
incumbent_agg <- bank_zip_panel[, .(
  # Standard
  incumbent_deps_curr_std  = sum_na_safe(bank_deps_curr[bank_type == "INCUMBENT"]),
  incumbent_deps_lead3_std = sum_na_safe(bank_deps_lead3[bank_type == "INCUMBENT"]),
  incumbent_deps_lead1_std = sum_na_safe(bank_deps_lead1[bank_type == "INCUMBENT"]),
  incumbent_deps_lag1_std  = sum_na_safe(bank_deps_lag1[bank_type == "INCUMBENT"]),
  
  # Clean
  incumbent_deps_curr_clean  = sum_na_safe(bank_deps_curr[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lead3_clean = sum_na_safe(bank_deps_lead3[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lead1_clean = sum_na_safe(bank_deps_lead1[bank_type_clean == "INCUMBENT"]),
  incumbent_deps_lag1_clean  = sum_na_safe(bank_deps_lag1[bank_type_clean == "INCUMBENT"])
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 6. Aggregate CLOSERS (With Tech Splits & Web Intensity)
# ---------------------------------------------------------------------
# Get RSSDIDs of banks that are closers in specific Zip-Years
closer_keys <- bank_zip_panel[bank_type == "CLOSER", .(RSSDID, ZIPBR, YEAR)]
relevant_branches <- merge(closure_opening_data, closer_keys, by = c("RSSDID", "ZIPBR", "YEAR"))

closer_agg_final <- relevant_branches[, .(
  # --- Original Total ---
  closer_closed_vol_lag1    = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned, NA_real_)),
  
  # --- NEW: Weighted Web Intensity Numerator ---
  # Sum of (Deposit_Weight * Web_Score) for closed branches
  closer_closed_visit_wtd_vol = sum_na_safe(fifelse(closed == 1L, dep_lag1_aligned * visit_change_pct, NA_real_)),

  # --- SPLIT 1: APP vs NO APP ---
  closer_closed_vol_lag1_app    = sum_na_safe(fifelse(closed == 1L & has_app == 1, dep_lag1_aligned, NA_real_)),
  closer_closed_vol_lag1_no_app = sum_na_safe(fifelse(closed == 1L & has_app == 0, dep_lag1_aligned, NA_real_)),
  
  # --- SPLIT 2: HIGH UPDATE vs LOW UPDATE ---
  closer_closed_vol_lag1_high_up = sum_na_safe(fifelse(closed == 1L & is_high_update == 1, dep_lag1_aligned, NA_real_)),
  closer_closed_vol_lag1_low_up  = sum_na_safe(fifelse(closed == 1L & is_high_update == 0, dep_lag1_aligned, NA_real_)),
  
  # --- Other Metrics ---
  closer_opened_vol_curr    = sum_na_safe(fifelse(new_branch == 1L, DEPSUMBR, NA_real_)),
  closer_remaining_vol_lag1 = sum_na_safe(fifelse(closed == 0L & new_branch == 0L, dep_lag1_aligned, NA_real_)),
  
  closer_n_remaining = sum(closed == 0L & new_branch == 0L, na.rm = TRUE),
  closer_n_closed    = sum(closed == 1L, na.rm=TRUE),
  closer_n_opened    = sum(new_branch == 1L, na.rm=TRUE)
), by = .(ZIPBR, YEAR)]


# ---------------------------------------------------------------------
# 7. Final Merge and Variable Construction
# ---------------------------------------------------------------------
final_panel <- zip_market_totals 
final_panel <- merge(final_panel, incumbent_agg, by = c("ZIPBR", "YEAR"), all.x = TRUE)
final_panel <- merge(final_panel, closer_agg_final, by = c("ZIPBR", "YEAR"), all.x = TRUE)

# Fill NAs
cols_to_zero <- c("closer_closed_vol_lag1", "closer_opened_vol_curr", 
                  "closer_remaining_vol_lag1", "closer_n_remaining", "closer_n_closed", "closer_n_opened",
                  "closer_closed_vol_lag1_app", "closer_closed_vol_lag1_no_app",
                  "closer_closed_vol_lag1_high_up", "closer_closed_vol_lag1_low_up",
                  "closer_closed_visit_wtd_vol") # Added new column to zero fill
for(j in cols_to_zero){
  set(final_panel, i = which(is.na(final_panel[[j]])), j = j, value = 0)
}

# --- CALCULATE VARIABLES ---
final_panel[, `:=`(
  # --- Independent Variables (Total Shock) ---
  share_deps_closed = fifelse(total_deps_zip_lag1 > 0, 
                              closer_closed_vol_lag1 / total_deps_zip_lag1, 0),
  
  # --- NEW: Weighted Average Web Intensity of Closed Branches ---
  # Formula: Sum(Deps * WebScore) / Sum(Deps)
  weighted_visit_intensity_closed = fifelse(closer_closed_vol_lag1 > 0,
                                           closer_closed_visit_wtd_vol / closer_closed_vol_lag1,
                                          0),

  # --- Independent Variables (Split 1: App vs No App) ---
  share_deps_closed_app     = fifelse(total_deps_zip_lag1 > 0, 
                                      closer_closed_vol_lag1_app / total_deps_zip_lag1, 0),
  share_deps_closed_no_app = fifelse(total_deps_zip_lag1 > 0, 
                                     closer_closed_vol_lag1_no_app / total_deps_zip_lag1, 0),
  
  # --- Independent Variables (Split 2: High vs Low Updates) ---
  share_deps_closed_high_up = fifelse(total_deps_zip_lag1 > 0, 
                                      closer_closed_vol_lag1_high_up / total_deps_zip_lag1, 0),
  share_deps_closed_low_up  = fifelse(total_deps_zip_lag1 > 0, 
                                      closer_closed_vol_lag1_low_up / total_deps_zip_lag1, 0),
  
  # --- Dependent Variables ---
  incumbent_dep_gr_3yr = fifelse(total_deps_zip_lag1 > 0,
                                 (incumbent_deps_lead3_std - incumbent_deps_curr_std) / incumbent_deps_curr_std,
                                 NA_real_),
  
  incumbent_dep_gr_3yr_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                 (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / incumbent_deps_curr_std,
                                 NA_real_),
  
  incumbent_dep_gr_1yr = fifelse(total_deps_zip_lag1 > 0,
                                 (incumbent_deps_lead1_std - incumbent_deps_curr_std) / incumbent_deps_curr_std,
                                 NA_real_),
  
  incumbent_dep_gr_3yr_cd = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead3_std - incumbent_deps_curr_std) / total_deps_zip_lag1,
                                    NA_real_),
  incumbent_dep_gr_3yr_cd_lag = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead3_std - incumbent_deps_lag1_std) / total_deps_zip_lag1,
                                    NA_real_),
  incumbent_dep_gr_1yr_cd = fifelse(total_deps_zip_lag1 > 0,
                                    (incumbent_deps_lead1_std - incumbent_deps_curr_std) / total_deps_zip_lag1,
                                    NA_real_),
  
  incumbent_dep_gr_3yr_clean_cd = fifelse(total_deps_zip_lag1 > 0,
                                          (incumbent_deps_lead3_clean - incumbent_deps_curr_clean) / total_deps_zip_lag1,
                                          NA_real_),
  incumbent_dep_gr_3yr_clean_cd_lag = fifelse(total_deps_zip_lag1 > 0,
                                          (incumbent_deps_lead3_clean - incumbent_deps_lag1_clean) / total_deps_zip_lag1,
                                          NA_real_),
  incumbent_dep_gr_1yr_clean_cd = fifelse(total_deps_zip_lag1 > 0,
                                          (incumbent_deps_lead1_clean - incumbent_deps_curr_clean) / total_deps_zip_lag1,
                                          NA_real_),
  
  # --- Controls ---
  incumbent_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                     incumbent_deps_lag1_std / total_deps_zip_lag1, 0),
  
  closer_remaining_mkt_share_lag1 = fifelse(total_deps_zip_lag1 > 0,
                                            closer_remaining_vol_lag1 / total_deps_zip_lag1, 0),
  
  log_total_market_size_lag1 = log(total_deps_zip_lag1 + 1),
  
  relocation_openings_vol_share = fifelse(total_deps_zip_lag1 > 0, 
                                          closer_opened_vol_curr / total_deps_zip_lag1, 0)
)]

# ---------------------------------------------------------------------
# 8. Save
# ---------------------------------------------------------------------
output_columns <- c("ZIPBR", "YEAR", 
                    
                    # Dependent
                    "incumbent_dep_gr_1yr",
                    "incumbent_dep_gr_3yr",
                    "incumbent_dep_gr_3yr_lag1",
                    "incumbent_dep_gr_3yr_cd", 
                    "incumbent_dep_gr_3yr_clean_cd",
                    "incumbent_dep_gr_3yr_cd_lag",
                    "incumbent_dep_gr_1yr_clean_cd",
                    "incumbent_dep_gr_1yr_cd",
                    
                    # Main Shocks (Total)
                    "share_deps_closed",
                    
                    # NEW VARIABLE
                    "weighted_visit_intensity_closed",
                    
                    # Split Shocks
                    "share_deps_closed_app", "share_deps_closed_no_app",
                    "share_deps_closed_high_up", "share_deps_closed_low_up",
                    
                    # Controls
                    "log_total_market_size_lag1",
                    "incumbent_mkt_share_lag1", 
                    "closer_remaining_mkt_share_lag1",
                    "relocation_openings_vol_share",
                    "total_deps_zip_lag1",
                    "branches_zip_lag1")

final_panel_clean <- final_panel[, ..output_columns]

cat("\nSummary of Web Intensity Variable:\n")
print(summary(final_panel_clean$weighted_web_intensity_closed))

saveRDS(final_panel_clean, output_path)

