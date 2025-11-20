# =====================================================================
# File: create_branch_competition_panel_with_apps.R
# =====================================================================

rm(list = ls())
library(data.table)
library(dplyr)
library(DescTools)

# ---------------------------------------------------------------------
# 0. Load data
# ---------------------------------------------------------------------
closure_opening_data <- readRDS("C:/data/closure_opening_data_simple.rds")
closure_opening_data <- closure_opening_data[UNINUMBR>0]


app_df <- fread("C:/data/data_bank_mobile_apps_2000_2021.csv")
# sod  <- readRDS("C:/data/fdic_sod_2000_2025.rds") 
# sod <- sod[,.(CERT,RSSDID,YEAR)]
# sod <- sod[!duplicated(sod)]
# saveRDS(sod,"C:/data/cert_rssd_yr.rds")
sod <- readRDS("C:/data/cert_rssd_yr.rds")
app_df <- merge(app_df[,.(FDIC_certificate_id,year,first_app_available)],
                sod,
                by.x=c("FDIC_certificate_id","year"),
                by.y=c("CERT","YEAR"))

temp <- app_df[year==2021]
for(yr in 2022:2025) {
  temp[,year:=yr]
  app_df <- rbind(app_df,temp)
}
setDT(app_df)

# harmonize column name and types
app_df[, YEAR := as.integer(year)]
app_df[, year := NULL]
app_df[, FDIC_certificate_id:= NULL]
app_df[, RSSDID := as.integer(RSSDID)]
app_df[, first_app_available := as.integer(first_app_available)]




# ---------------------------------------------------------------------
# 1. Keep relevant columns and attach app flag
# ---------------------------------------------------------------------
closure_opening_data <- closure_opening_data[
  , .(RSSDID, UNINUMBR, ZIPBR, YEAR, DEPSUMBR, STCNTY,
      closed, new_branch)
]

setDT(closure_opening_data)
closure_opening_data[, RSSDID := as.integer(RSSDID)]
closure_opening_data[, YEAR   := as.integer(YEAR)]

# merge app info, treat missing as 0 (no app)
closure_opening_data <- merge(
  closure_opening_data,
  app_df[, .(RSSDID, YEAR, first_app_available)],
  by = c("RSSDID", "YEAR"),
  all.x = TRUE
)
closure_opening_data[
  is.na(first_app_available), first_app_available := 0L
]


# ---------------------------------------------------------------------
# 2. 3-year and 1-year forward deposit growth (unchanged)
# ---------------------------------------------------------------------
setorder(closure_opening_data, UNINUMBR, YEAR)

closure_opening_data[
  , `:=`(
    year_lead3 = shift(YEAR, 3L, type = "lead"),
    year_lead1 = shift(YEAR, 1L, type = "lead"),
    dep_lead3  = shift(DEPSUMBR, 3L, type = "lead"),
    dep_lead1  = shift(DEPSUMBR, 1L, type = "lead")
  ),
  by = UNINUMBR
]

closure_opening_data[
  ,
  dep_gr_3yr := fifelse(
    !is.na(dep_lead3) & !is.na(DEPSUMBR) & DEPSUMBR > 0 &
      year_lead3 == YEAR + 3L,
    (dep_lead3 - DEPSUMBR) / DEPSUMBR,
    NA_real_
  )
]

closure_opening_data[
  ,
  dep_gr_1yr := fifelse(
    !is.na(dep_lead1) & !is.na(DEPSUMBR) & DEPSUMBR > 0 &
      year_lead1 == YEAR + 1L,
    (dep_lead1 - DEPSUMBR) / DEPSUMBR,
    NA_real_
  )
]

closure_opening_data[
  , dep_gr_3yr := Winsorize(dep_gr_3yr,
                            val = quantile(dep_gr_3yr, probs = c(0.01, 0.99),
                                           na.rm = TRUE))
]

closure_opening_data[
  , dep_gr_1yr := Winsorize(dep_gr_1yr,
                            val = quantile(dep_gr_1yr, probs = c(0.01, 0.99),
                                           na.rm = TRUE))
]

# ---------------------------------------------------------------------
# 3. Bank–ZIP–year activity (own) including app split
# ---------------------------------------------------------------------
bank_zip_year_activity <- closure_opening_data[
  ,
  .(
    own_closures_zip       = sum(closed == 1L, na.rm = TRUE),
    own_openings_zip       = sum(new_branch == 1L, na.rm = TRUE),
    own_closures_zip_app   = sum(closed == 1L & first_app_available == 1L, na.rm = TRUE),
    own_closures_zip_noapp = sum(closed == 1L & first_app_available == 0L, na.rm = TRUE)
  ),
  by = .(RSSDID, ZIPBR, YEAR)
]

# ---------------------------------------------------------------------
# 4. ZIP–year totals (all banks) including app split
# ---------------------------------------------------------------------
zip_year_totals <- closure_opening_data[
  ,
  .(
    total_closures_zip       = sum(closed == 1L, na.rm = TRUE),
    total_openings_zip       = sum(new_branch == 1L, na.rm = TRUE),
    total_closures_zip_app   = sum(closed == 1L & first_app_available == 1L, na.rm = TRUE),
    total_closures_zip_noapp = sum(closed == 1L & first_app_available == 0L, na.rm = TRUE)
  ),
  by = .(ZIPBR, YEAR)
]

# ---------------------------------------------------------------------
# 5. Competitor activity (counts) + app/no-app split
# ---------------------------------------------------------------------
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_year_totals,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  ,
  `:=`(
    competitor_closures_zip       = total_closures_zip       - own_closures_zip,
    competitor_openings_zip       = total_openings_zip       - own_openings_zip,
    competitor_closures_zip_app   = total_closures_zip_app   - own_closures_zip_app,
    competitor_closures_zip_noapp = total_closures_zip_noapp - own_closures_zip_noapp
  )
]

# ---------------------------------------------------------------------
# 5B. Competitor closure deposit share (t−1), split by app status
# ---------------------------------------------------------------------
# branch-level lag deposits
closure_opening_data[
  , dep_lag1 := shift(DEPSUMBR, 1L, type = "lag"),
  by = UNINUMBR
]

# ZIP-year total dep_{t-1}
zip_dep_tminus1 <- closure_opening_data[
  , .(zip_dep_tminus1 = sum(dep_lag1, na.rm = TRUE)),
  by = .(ZIPBR, YEAR)
]

# By bank–ZIP–year: dep_{t-1} for branches that close in t (all, app, no app)
close_dep_tminus1_by_bank <- closure_opening_data[
  closed == 1L,
  .(
    own_close_dep_tminus1       = sum(dep_lag1, na.rm = TRUE),
    own_close_dep_tminus1_app   = sum(dep_lag1[first_app_available == 1L], na.rm = TRUE),
    own_close_dep_tminus1_noapp = sum(dep_lag1[first_app_available == 0L], na.rm = TRUE)
  ),
  by = .(RSSDID, ZIPBR, YEAR)
]

# ZIP totals of dep_{t-1} associated with closures (all/app/noapp)
zip_close_dep_tminus1_total <- close_dep_tminus1_by_bank[
  ,
  .(
    zip_close_dep_tminus1_total       = sum(own_close_dep_tminus1,       na.rm = TRUE),
    zip_close_dep_tminus1_total_app   = sum(own_close_dep_tminus1_app,   na.rm = TRUE),
    zip_close_dep_tminus1_total_noapp = sum(own_close_dep_tminus1_noapp, na.rm = TRUE)
  ),
  by = .(ZIPBR, YEAR)
]

# merge own closure deposit info into bank_zip_year_activity
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  close_dep_tminus1_by_bank,
  by = c("RSSDID", "ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  is.na(own_close_dep_tminus1),       own_close_dep_tminus1       := 0
][
  is.na(own_close_dep_tminus1_app),   own_close_dep_tminus1_app   := 0
][
  is.na(own_close_dep_tminus1_noapp), own_close_dep_tminus1_noapp := 0
]

# attach ZIP totals
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_close_dep_tminus1_total,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

bank_zip_year_activity[
  is.na(zip_close_dep_tminus1_total),       zip_close_dep_tminus1_total       := 0
][
  is.na(zip_close_dep_tminus1_total_app),   zip_close_dep_tminus1_total_app   := 0
][
  is.na(zip_close_dep_tminus1_total_noapp), zip_close_dep_tminus1_total_noapp := 0
]

# competitor dep_{t-1} tied to closures (all/app/noapp)
bank_zip_year_activity[
  ,
  `:=`(
    competitor_close_dep_tminus1       = pmax(zip_close_dep_tminus1_total       - own_close_dep_tminus1,       0),
    competitor_close_dep_tminus1_app   = pmax(zip_close_dep_tminus1_total_app   - own_close_dep_tminus1_app,   0),
    competitor_close_dep_tminus1_noapp = pmax(zip_close_dep_tminus1_total_noapp - own_close_dep_tminus1_noapp, 0)
  )
]

# denominator: total ZIP dep_{t-1}
bank_zip_year_activity <- merge(
  bank_zip_year_activity,
  zip_dep_tminus1,
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

# shares: all competitors, competitors with apps, competitors without apps
bank_zip_year_activity[
  ,
  `:=`(
    competitor_close_dep_share_prev =
      fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
              competitor_close_dep_tminus1 / zip_dep_tminus1,
              NA_real_),
    
    competitor_close_dep_share_prev_app =
      fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
              competitor_close_dep_tminus1_app / zip_dep_tminus1,
              NA_real_),
    
    competitor_close_dep_share_prev_noapp =
      fifelse(!is.na(zip_dep_tminus1) & zip_dep_tminus1 > 0,
              competitor_close_dep_tminus1_noapp / zip_dep_tminus1,
              NA_real_)
  )
]

# ---------------------------------------------------------------------
# 6. Attach variables back to branch-year observations
# ---------------------------------------------------------------------
closure_opening_data <- merge(
  closure_opening_data,
  bank_zip_year_activity[
    , .(RSSDID, ZIPBR, YEAR,
        own_closures_zip, own_openings_zip,
        competitor_closures_zip, competitor_openings_zip,
        competitor_closures_zip_app, competitor_closures_zip_noapp,
        competitor_close_dep_share_prev,
        competitor_close_dep_share_prev_app,
        competitor_close_dep_share_prev_noapp)
  ],
  by = c("RSSDID", "ZIPBR", "YEAR"),
  all.x = TRUE
)

# ---------------------------------------------------------------------
# 7. ZIP branch counts and rates (including app/noapp competitor rates)
# ---------------------------------------------------------------------
zip_year_branch_counts <- closure_opening_data[
  , .(branches_zip = uniqueN(UNINUMBR)),
  by = .(ZIPBR, YEAR)
]

setorder(zip_year_branch_counts, ZIPBR, YEAR)
zip_year_branch_counts[
  , branches_zip_lag := shift(branches_zip, 1L, type = "lag"),
  by = ZIPBR
]

closure_opening_data <- merge(
  closure_opening_data,
  zip_year_branch_counts[, .(ZIPBR, YEAR, branches_zip_lag)],
  by = c("ZIPBR", "YEAR"),
  all.x = TRUE
)

closure_opening_data[
  ,
  `:=`(
    competitor_closures_rate =
      fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
              competitor_closures_zip / branches_zip_lag, NA_real_),
    
    competitor_openings_rate =
      fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
              competitor_openings_zip / branches_zip_lag, NA_real_),
    
    competitor_closures_rate_app =
      fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
              competitor_closures_zip_app / branches_zip_lag, NA_real_),
    
    competitor_closures_rate_noapp =
      fifelse(!is.na(branches_zip_lag) & branches_zip_lag > 0,
              competitor_closures_zip_noapp / branches_zip_lag, NA_real_)
  )
]

# ---------------------------------------------------------------------
# 8. Save final dataset
# ---------------------------------------------------------------------
saveRDS(closure_opening_data,
        "C:/data/branch_competition_panel_with_apps.rds")
# =====================================================================
