rm(list=ls())
library(data.table)
library(stringr)
library(tidycensus)
library(dplyr)


irs_data <- list()

for(yr in seq(2010,2022,by=1)) {

  print(yr)

  irs <-fread(paste0("C:/data/irs_zip/",substr(yr,3,4),"zpallagi.csv"))  #no_returns,no_returns_salaries,no_returns_dividend,no_returns_business,no_returns_captial_gain
  names(irs) <- tolower(names(irs))
  irs <- irs[,c("zipcode","n1","n00200","n00600","n00900","n01000")]
  setnames(irs,
           c("n1","n00200","n00600","n00900","n01000"),
           c("no_returns","no_returns_salaries","no_returns_dividend","no_returns_business","no_returns_captial_gain"))

  irs <- irs[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = zipcode]


  irs[,salary_frac:=no_returns_salaries/no_returns]
  irs[,dividend_frac:=no_returns_dividend/no_returns]
  irs[,business_frac:=no_returns_business/no_returns]
  irs[,capital_gain_frac:=no_returns_captial_gain/no_returns]

  irs <- irs[,c("zipcode","dividend_frac","capital_gain_frac")]
  irs[,yr:=yr]

  irs_data[[paste0(yr,"_")]] <- copy(irs)
}

irs_data <- bind_rows(irs_data)


saveRDS(irs_data,"C:/data/irs_data_2010_2022_zip.rds")
