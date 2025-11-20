rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

# 1. Load the full dataset ----------------------------------------------------
branch_year <- readRDS("C:/data/fdic_sod_2000_2025.rds") # sod_download_all_data_to_rds.R




# Step 1. First and last observed year per bank
bank_years <- branch_year[DEPSUMBR>0] %>%
  group_by(RSSDID) %>%
  summarize(
    bank_first_year = min(YEAR, na.rm = TRUE),
    bank_last_year  = max(YEAR, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2. Build exclude_2011 list (banks that only show "new branches" in 2011)
tmp_open <- branch_year %>%
  arrange(UNINUMBR, YEAR) %>%
  group_by(UNINUMBR) %>%
  mutate(
    prev_year_tmp = lag(YEAR),
    new_branch_tmp = ifelse(is.na(prev_year_tmp), 1, 0)
  ) %>%
  ungroup() %>%
  data.table()

tt <- tmp_open[
  new_branch_tmp == 1 & YEAR %in% 2000:2011,
  .(no_new_branches = .N),
  by = .(RSSDID, YEAR)
]

tt_cast <- dcast(tt, RSSDID ~ YEAR)
exclude_2011 <- tt_cast[
  is.na(`2004`) &
    is.na(`2005`) &
    is.na(`2006`) &
    is.na(`2007`) &
    is.na(`2008`) &
    is.na(`2009`) &
    is.na(`2010`) &
    `2011` > 0,
  RSSDID
]

exclude_2003 <- tt_cast[
  is.na(`2002`) &
    is.na(`2001`) &
    `2003` > 0,
  RSSDID
]

# Step 3. Main closure_data construction
closure_opening_data <- branch_year %>%
  arrange(UNINUMBR, YEAR) %>%
  group_by(UNINUMBR) %>%
  mutate(
    prev_year = lag(YEAR),
    next_year = lead(YEAR),
    
    # raw branch-new indicator (before bank filter)
    branch_new_raw = ifelse(is.na(prev_year) , 1, 0)
  ) %>%
  ungroup() %>%
  # attach bank_first_year, bank_last_year
  left_join(bank_years, by = "RSSDID") %>%
  mutate(
    # new_branch: branch_new_raw must be 1 AND YEAR > bank_first_year.
    # then overwrite with NA if bank is in exclude_2011
    new_branch = ifelse(branch_new_raw == 1 & YEAR > bank_first_year, 1, 0),
    new_branch = ifelse(RSSDID %in% c(exclude_2011,exclude_2003), NA, new_branch),
    
    # closed definition:
    # - last observed year for this branch (next_year is NA)
    # - YEAR < 2025
    # - bank continues after this year (bank_last_year > YEAR)
    closed = ifelse(
      is.na(next_year) &
        YEAR < 2025 &
        bank_last_year > YEAR,
      1, 0
    )
  ) %>%
  # select columns
  select(
    RSSDHCR,
    RSSDID,
    UNINUMBR,
    DEPSUMBR,
    ZIPBR,
    STCNTY,
    YEAR,
    closed,
    new_branch
  ) %>%
  data.table()


# 1. Create lags at the branch level ---------------------------------

closure_opening_data[, `:=`(
  lag1_bank  = shift(RSSDID, 1L, type = "lag"),   # bank last year
  lag3_bank  = shift(RSSDID, 3L, type = "lag"),   # bank 3 years ago
  lag1_bhc   = shift(RSSDHCR, 1L, type = "lag"),  # BHC last year
  dep_lag1   = shift(DEPSUMBR, 1L, type = "lag"), # deposits t-1
  dep_lag3   = shift(DEPSUMBR, 3L, type = "lag")  # deposits t-3
), by = UNINUMBR]

# 2. merged_1_year, merged_2_year, merged_3_year, merged_last_3_yrs ----
# "merged" means ownership changed vs prior year (RSSDID or BHC changed)

closure_opening_data[
  ,
  merged_1_year := as.integer(
    !is.na(lag1_bank) &
      (RSSDID != lag1_bank | RSSDHCR != lag1_bhc)
  )
]

# merged_2_year is merged_1_year lagged 1 more year within same branch
# merged_3_year is merged_1_year lagged 2 more years within same branch
closure_opening_data[
  ,
  `:=`(
    merged_2_year = shift(merged_1_year, 1L, type = "lag"),
    merged_3_year = shift(merged_1_year, 2L, type = "lag")
  ),
  by = UNINUMBR
]

closure_opening_data[
  ,
  merged_last_3_yrs := as.integer(
    (merged_1_year == 1L) |
      (merged_2_year == 1L) |
      (merged_3_year == 1L)
  )
]

# 3. deposit_gr_3yrs_branch -------------------------------------------
# your formula: lag_branch_deposit_amount*100/lag_branch_deposit_amount_3yr
# That is effectively DEPSUMBR(t-1) / DEPSUMBR(t-3) * 100.
# If you instead want growth, replace with (dep_lag1 - dep_lag3)/dep_lag3.

closure_opening_data[
  ,
  deposit_gr_3yrs_branch := fifelse(
    !is.na(dep_lag1) & !is.na(dep_lag3) & dep_lag3 != 0,
    dep_lag1 * 100 / dep_lag3,
    NA_real_
  )
]

# 4. legacy_branch -----------------------------------------------------
# legacy_branch = 1 if same bank has owned this same branch for >=3 years
# i.e. bank 3 years ago equals current bank

closure_opening_data[
  ,
  legacy_branch := as.integer(
    !is.na(lag3_bank) & lag3_bank == RSSDID
  )
]

# 5. same_zip_prior_branches ------------------------------------------
# Logic: for branches that were "merged" (i.e. newly acquired),
# check if, in the prior 1-3 years, the same bank already had
# another branch in the same ZIPBR.

# closure_opening_data[, same_zip_prior_branches := 0L]
# 
# merged_rows <- which(closure_opening_data$merged_1_year == 1L)
# pb <- txtProgressBar(min = 0, max = length(merged_rows), style = 3, width = 50)
# count=0
# for (i in merged_rows) {
#   count = count+1
#   setTxtProgressBar(pb, count)
#   temp <- closure_opening_data[
#     YEAR <= (closure_opening_data[i, YEAR] - 1L) &
#       YEAR >= (closure_opening_data[i, YEAR] - 3L) &
#       ZIPBR ==  closure_opening_data[i, ZIPBR] &
#       RSSDID == closure_opening_data[i, RSSDID] &
#       UNINUMBR != closure_opening_data[i, UNINUMBR]
#   ]
# 
#   if (nrow(temp) > 0L) {
#     closure_opening_data[i, same_zip_prior_branches := 1L]
#   }
# }

# 6. closed_old_branch and closed_acq_branch ---------------------------
# closed_old_branch = 1 if branch closed and it's a long-held (legacy) branch
# closed_acq_branch = 1 if branch closed and it's NOT legacy

closure_opening_data[
  ,
  `:=`(
    closed_old_branch = as.integer(closed == 1L & legacy_branch == 1L),
    closed_acq_branch = as.integer(closed == 1L & merged_last_3_yrs == 1L)
  )
]

# 7. Drop helper columns you do not want to keep -----------------------
closure_opening_data[, c("lag1_bank","lag3_bank","lag1_bhc","dep_lag1","dep_lag3",
                         "merged_2_year","merged_3_year") := NULL]

# 8. Zip code level characteristics -----------------------

library(readxl)
library(stringr)

acs_data <- readRDS("C:/data/acs_data_2010_2022_tract.rds")

tract_zip_crosswalk <- read_excel('C:/data/ZIP_TRACT_122019.xlsx')
tract_zip_crosswalk <- data.table(tract_zip_crosswalk)
tract_zip_crosswalk[,zip:=str_pad(ZIP,5,"left","0")]
tract_zip_crosswalk[,tract:=as.character(TRACT)]
tract_zip_crosswalk[,tract:=str_pad(tract,11,"left","0")]
tract_zip_crosswalk[,tot_ratio:=TOT_RATIO]


acs_with_zip <- acs_data %>%
  left_join(tract_zip_crosswalk[,c("zip","tract","tot_ratio")], by = c("tract" = "tract"))



zip_aggregated_data <- acs_with_zip %>%
  group_by(zip,yr) %>%
  summarize(
    # Weighted average of median income
    median_income = sum(median_income * tot_ratio, na.rm = T) ,
    
    # Weighted average of percentage of population with a bachelor's degree
    pct_college_educated = sum(pct_college_educated * tot_ratio, na.rm=T),
    
    # Weighted average of white population percentage
    # white_population_pct = sum(white_populationE * RES_RATIO,na.rm=T),
    
    # Weighted average of median age
    median_age = sum(median_age * tot_ratio,na.rm = T)
    
    # Total population is weighted by RES_RATIO
    # total_population = sum(total_populationE * RES_RATIO)
  )

zip_aggregated_data <- data.table(zip_aggregated_data)

irs_data <- readRDS("C:/data/irs_data_2010_2022_zip.rds")


irs_data <- irs_data[,c("zipcode","yr","dividend_frac","capital_gain_frac")]
irs_data[,zipcode:=str_pad(zipcode,5,"left","0")]
# 
zip_demo_data <- merge(zip_aggregated_data,irs_data,by.x=c("zip","yr"),by.y=c("zipcode","yr"),all.x=T)


key_chars <- c("median_income","median_age","pct_college_educated","capital_gain_frac","dividend_frac")

zip_demo_data <- data.table(zip_demo_data)
zip_demo_data[,paste0(key_chars,"_q"):=lapply(.SD, function(x) ntile(x,2)),by=yr,.SDcols = key_chars]

zip_demo_data[,sophisticated_ed_sm:=ifelse(!is.na(dividend_frac)  & pct_college_educated_q==2 & (dividend_frac_q==2 | capital_gain_frac_q==2),1,
                                           ifelse(!is.na(dividend_frac),0,NA))]


zip_demo_data <- zip_demo_data[,c("zip","yr","median_income","median_age","pct_college_educated","capital_gain_frac","dividend_frac","sophisticated_ed_sm")]

zip_demo_data[,age_bin:=ntile(median_age,4),by=yr]

temp1 <- zip_demo_data[yr==2010]
for(y in 2000:2009) {
  temp <- copy(temp1)
  temp[,yr:=y]
  zip_demo_data <- rbind(zip_demo_data,temp)
}

temp1 <- zip_demo_data[yr==2022]
for(y in 2023:2024) {
  temp <- copy(temp1)
  temp[,yr:=y]
  zip_demo_data <- rbind(zip_demo_data,temp)
}

temp1 <- zip_demo_data[yr==2010]
for(y in 2011:2012) {
  temp <- copy(temp1)
  temp[,yr:=y]
  zip_demo_data <- rbind(zip_demo_data,temp)
}

temp1 <- zip_demo_data[yr==2013]
for(y in 2014:2015) {
  temp <- copy(temp1)
  temp[,yr:=y]
  zip_demo_data <- rbind(zip_demo_data,temp)
}

temp1 <- zip_demo_data[yr==2019]
for(y in 2020:2021) {
  temp <- copy(temp1)
  temp[,yr:=y]
  zip_demo_data <- rbind(zip_demo_data,temp)
}

closure_opening_data <- merge(closure_opening_data,
                              zip_demo_data[!is.na(zip),.(zip,yr,sophisticated_ed_sm)],
                              by.x=c("ZIPBR","YEAR"),
                              by.y=c("zip","yr"),
                              all.x=T)

closure_opening_data[
  ,
  `:=`(
    closed_sophisticated_branch = as.integer(closed == 1L & sophisticated_ed_sm == 1L)
  )
]


saveRDS(closure_opening_data,'C:/data/closure_opening_data.rds')


# temp <- closure_opening_data[,.(closed_frac=mean(closed,na.rm=T),
#                         new_frac = mean(new_branch,na.rm=T)),
#                      by=YEAR]
# ggplot(temp[YEAR<2025],aes(x=YEAR,y=closed_frac))+geom_line()
# ggplot(temp[YEAR>2000],aes(x=YEAR,y=new_frac))+geom_line()

