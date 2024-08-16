rm(list=ls())
library(data.table)
source("functions.R")
library(RSQLite)
library(DBI)  
library(dplyr)

db_path <- "D:/OneDrive/research-data/HMDA/db/hmda.db"
con <- dbConnect(RSQLite::SQLite(), db_path)

cols_to_drop <- c(paste0("applicant_ethnicity_",2:5),
                  paste0("co_applicant_ethnicity_",2:5),
                  paste0("applicant_race_",2:5),
                  paste0("co_applicant_race_",2:5),
                  paste0("aus_",2:5),
                  paste0("denial_reason_",2:5),
                  "manufactured_home_secured_property_type",
                  "manufactured_home_land_property_interest")



fn = "2018_lar.txt"
temp <- fread(paste0('C:/Users/dimut/Downloads/','2018_lar.txt'))
gc()
temp <- temp[, (cols_to_drop) := NULL, with = FALSE]


dbWriteTable(con,sub("\\..*$", "", fn),hmda,append=T)