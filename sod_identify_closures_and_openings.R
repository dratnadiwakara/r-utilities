rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

# 1. Load the full dataset ----------------------------------------------------
branch_year <- readRDS("C:/data/fdic_sod_2000_2025.rds") # sod_download_all_data_to_rds.R




# Step 1. First and last observed year per bank
bank_years <- branch_year %>%
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
    RSSDID,
    UNINUMBR,
    DEPSUMBR,
    YEAR,
    closed,
    new_branch
  ) %>%
  data.table()

saveRDS(closure_opening_data,'C:/data/closure_opening_data.rds')


temp <- closure_opening_data[,.(closed_frac=mean(closed,na.rm=T),
                        new_frac = mean(new_branch,na.rm=T)),
                     by=YEAR]
ggplot(temp[YEAR<2025],aes(x=YEAR,y=closed_frac))+geom_line()
ggplot(temp[YEAR>2000],aes(x=YEAR,y=new_frac))+geom_line()

