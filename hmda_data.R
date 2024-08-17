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


#####################

# 
# rm(list=ls())
# 
# library(data.table)
# library(RSQLite)
# library(DBI)
# library(dplyr)
# library(stringr)
# 
# cols_to_drop <- c(paste0("applicant_ethnicity_", 2:5),
#                   paste0("co_applicant_ethnicity_", 2:5),
#                   paste0("applicant_race_", 2:5),
#                   paste0("co_applicant_race_", 2:5),
#                   paste0("applicantrace", 2:5),
#                   paste0("coapplicantrace", 2:5),
#                   paste0("aus_", 2:5),
#                   paste0("denial_reason_", 2:5),
#                   paste0("denialreason", 2:5),
#                   "manufactured_home_secured_property_type",
#                   "manufactured_home_land_property_interest")
# 
# yrs <- 2012:2013
# file_names <- paste0(yrs, "_lar.dat")
# 
# # yrs <- 2017
# # file_names <- paste0(yrs, "_lar.txt")
# 
# # yrs <- 2014:2016
# # file_names <- paste0(yrs, "_lar.csv")
# 
# # yrs <- 2018:2023
# # file_names <- paste0(2018:2023, "_lar.txt")
# 
# for(fn in file_names) {
#   print(fn)
#   
#   if(identical(yrs, 2012:2013)) {
#     temp <- fread(paste0('/raw_data_to_delete/',fn), sep="|", header = F)
#     
#     split_positions <- c(4, 14, 15, 16, 17, 18, 23, 24, 29, 31, 34, 39, 40, 42, 43, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 6 ...)
#     
#     
#     col_names <- c("asofyear", "respondentid", "agencycode", "typeofloan", "purposeofloan",
#                    "occupancy", "amountofloan", "actiontaken", "msa", "state", "countycode", 
#                    "censustract", "applicantsex", "applicantincome",
#                    "typeofpurchaser", "denialreason1", "denialreason2", "denialreason3",
#                    "editstatus", "propertytype", "preapprovals", "applicantethnicity",
#                    "coapplicantethnicity", "applicantrace1", "applicantrace2", "applicantrace3",
#                    "applicantrace4", "applicantrace5", "coapplicantrace1", "coapplicantrace2",
#                    "coapplicantrace3", "coapplicantrace4", "coapplicantrace5", "ratespread",
#                    "hoepastatus", "lienstatus", "seqno")
#     
#     cols_to_numeric <- col_names[!col_names %in% c("state", "countycode", "censustract", "respondentid")]
#     
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     
#     temp[, V1 := NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     temp[, censustract := trimws(censustract)]
#     temp[, censustract := gsub("[.]", "", censustract)]
#     temp[, censustract := str_pad(censustract, 6, "left", "0")]
#     
#   } else {
#     temp <- fread(paste0('/raw_data_to_delete/',fn))
#   }
#   
#   gc()
#   
#   if(identical(yrs, 2014:2016)) {
#     names(temp) <- c("asofyear", "respondentid", "agencycode", "typeofloan", "propertytype", "purposeofloan",
#                      "occupancy", "amountofloan", "preapprovals", "actiontaken", "msa", "state", "countycode",
#                      "censustract", "applicantsex", "applicantethnicity", "coapplicantethnicity", 
#                      "applicantrace1", "applicantrace2", "applicantrace3", "applicantrace4", "applicantrace5",
#                      "coapplicantrace1", "coapplicantrace2", "coapplicantrace3", "coapplicantrace4",
#                      "coapplicantrace5", "applicantincome", "denialreason1", "denialreason2", "denialreason3",
#                      "editstatus", "hoepastatus", "lienstatus", "seqno", "ratespread",
#                      "ct_population", "ct_minoritypct", "ct_median_income", "ct_tract_to_msa_income", 
#                      "ct_no_occupied_unites", "ct_no_of_1to4_family_unit", "application_date_indicator")
#   }
#   
#   if(identical(yrs, 2017)) {
#     names(temp) <- c("recordid", "respondentid", "agencycode", "typeofloan", "propertytype", "purposeofloan",
#                      "occupancy", "amountofloan", "preapprovals", "actiontaken", "msa", "state", "countycode",
#                      "censustract", "applicantethnicity", "coapplicantethnicity", 
#                      "applicantrace1", "applicantrace2", "applicantrace3", "applicantrace4", "applicantrace5",
#                      "coapplicantrace1", "coapplicantrace2", "coapplicantrace3", "coapplicantrace4",
#                      "coapplicantrace5", "applicantsex", "applicantincome", "denialreason1", "denialreason2",
#                      "denialreason3", "ratespread", "hoepastatus", "lienstatus", "seqno", 
#                      "ct_population", "ct_minoritypct", "ct_median_income", "ct_tract_to_msa_income",
#                      "ct_no_occupied_unites", "ct_no_of_1to4_family_unit", "application_date_indicator")
#     temp[, asofyear := 2017]
#   }
#   
#   temp <- temp[, (cols_to_drop) := NULL, with = FALSE]
#   
#   db_path <- paste0("/processed_data/hmda/", sub("\\..*$", "", fn), ".db")
#   con <- dbConnect(RSQLite::SQLite(), db_path)
#   dbWriteTable(con, sub("\\..*$", "", fn), temp)
#   dbDisconnect(con)
# }


# old code
# ---
#   title: "Clean HMDA Data to SQLite"
# date: "07/21/2023"
# output:
#   html_document:
#   toc: true
# toc_float: true
# toc_collapsed: true
# theme: united
# number_sections: true
# code_folding: hide
# ---
#   <style type="text/css">
#   .main-container {
#     max-width: 100%;
#     margin-left: auto;
#     margin-right: auto;
#   }
# 
# div.main-container {
#   max-width: 100%;
# }
# </style>
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE,warning = FALSE,message = FALSE)
# ```
# 
# 
# ```{r}
# rm(list=ls())
# library(data.table)
# library(fst)
# # library(MonetDBLite)
# library(RSQLite)
# library(DBI)  
# library(dplyr)
# library(stringr)
# dbdir <- "C:/Users/dratnadiwakara2/Downloads/HMDA/hmda.db"
# Sys.time()
# con <- dbConnect(RSQLite::SQLite(), dbdir)
# Sys.time()
# ```
# 
# # Links
# 1981-2014: https://catalog.archives.gov/id/2456161
# 2015-2016: https://www.ffiec.gov/hmda/hmdaproducts.htm
# https://ffiec.cfpb.gov/data-publication/snapshot-national-loan-level-dataset/2022
# 
# # Panel
# 
# ```{r}
# panel_files <- list.files("C:/Users/dratnadiwakara2/Downloads/HMDA/raw_data/panel/",full.names = T)
# ```
# 
# ## Panel 2018-2022
# ```{r}
# yrs <- as.character(2018:2022)
# 
# for(lf in panel_files) {
#   if(substr(lf,56,59) %in% yrs) {
#     print(substr(lf,56,59))
#     del = "|"
#     if(substr(lf,56,59)=="2022") {
#       del=","
#     }
#     temp <- fread(lf,header = T,sep = del)
#     setnames(temp,
#              c("activity_year","upper","respondent_rssd","agency_code","assets"),
#              c("asofdate","lei","rssd","agencycode","totalassets"),
#              skip_absent = T)
#   }
#   
#   dbWriteTable(con,paste0("panel_",substr(lf,56,59)),temp,overwrite =T)
# }
# ```
# 
# 
# ## Panel 1990 to 2003
# 
# ```{r}
# yrs <- as.character(1990:2003)
# split_positions <- c(10,14,15,17,47,72,74,76,86,87,97,127,152,154,158)
# 
# col_names <- c("respondentid","msa","agencycode","agencygroupcode","respondentname","respondenthomecity","respondenthomestate","respondentfipsstatenumber","totalassets","otherlendercode","parentidentifier","parentname","parentcity","parentstate","asofdate")
# 
# cols_to_numeric <- c("msa","agencycode","totalassets","asofdate")
# 
# for(lf in panel_files) {
#   if(substr(lf,56,59) %in% yrs) {
#     print(substr(lf,56,59))
#     temp <- fread(lf,header = F,sep = "}")
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     temp[,V1:=NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     for (coln in names(temp)) {
#       if (is.character(temp[[coln]])) {
#         temp[, (coln) := trimws(temp[[coln]])]
#       }
#     }
#     
#     msa_branch <- copy(temp)
#     msa_branch <- msa_branch[,c("respondentid","agencycode","msa","asofdate")]
#     msa_branch <- msa_branch[!duplicated(msa_branch)]
#     
#     temp[,msa:=NULL]
#     temp <- temp[!duplicated(temp)]
#     # dbWriteTable(con,paste0("msa_branch_",substr(lf,56,59)),msa_branch,overwrite =T)
#     dbWriteTable(con,paste0("panel_",substr(lf,56,59)),temp,overwrite =T)
#   } 
#   
#   
# }
# 
# ```
# 
# ```{r}
# for(yr in 2007:2022) {
#   t <- dbGetQuery(con,paste0("select * from panel_",yr," LIMIT 5 "))
#   print(t$asofdate)
# }
# ```
# 
# 
# 
# ## Panel 2004 to 2014
# ```{r}
# yrs <- as.character(2004:2014)
# split_positions <- c(10,15,16,18,48,73,75,77,87,88,98,128,153,155,159,169)
# 
# col_names <- c("respondentid","msa","agencycode","agencygroupcode","respondentname","respondenthomecity","respondenthomestate","respondentfipsstatenumber","totalassets","otherlendercode","parentidentifier","parentname","parentcity","parentstate","asofdate","rssd")
# 
# cols_to_numeric <- c("msa","agencycode","totalassets","asofdate","rssd")
# 
# for(lf in panel_files) {
#   if(substr(lf,56,59) %in% yrs) {
#     print(substr(lf,56,59))
#     temp <- fread(lf,header = F,sep = "}")
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     temp[,V1:=NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     for (coln in names(temp)) {
#       if (is.character(temp[[coln]])) {
#         temp[, (coln) := trimws(temp[[coln]])]
#       }
#     }
#     
#     
#     msa_branch <- copy(temp)
#     msa_branch <- msa_branch[,c("respondentid","agencycode","msa","asofdate")]
#     msa_branch <- msa_branch[!duplicated(msa_branch)]
#     
#     temp[,msa:=NULL]
#     temp <- temp[!duplicated(temp)]
#     dbWriteTable(con,paste0("msa_branch_",substr(lf,56,59)),msa_branch,overwrite =T)
#     dbWriteTable(con,paste0("panel_",substr(lf,56,59)),temp,overwrite =T)
#   } 
# }
# ```
# 
# ## Panel 2015 to 2016
# 
# ```{r}
# 
# yrs <- as.character(2015:2016)
# 
# split_positions <- c(4,14,15,25,55,80,82,84,94,95,125,165,190,192,202,212,222,252,277,279,319,329,339,341)
# 
# col_names <- c("asofdate","respondentid","agencycode","parentidentifier","parentname","parentcity","parentstate","region","totalassets","otherlendercode","respondentname","filler","respondenthomecity","respondenthomestate","filler2","filler3","topholderrssd","topholdername","topholdercity","topholderstate","topholdercounty","rssd","parentrssd","respondentfipsstatenumber")
# 
# cols_to_numeric <- c("msa","agencycode","totalassets","asofdate","rssd","topholderrssd","parentrssd")
# 
# for(lf in panel_files) {
#   if(substr(lf,56,59) %in% yrs) {
#     print(substr(lf,56,59))
#     temp <- fread(lf,header = F,sep = "}")
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     temp[,V1:=NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     for (coln in names(temp)) {
#       if (is.character(temp[[coln]])) {
#         temp[, (coln) := trimws(temp[[coln]])]
#       }
#     }
#     
#     
#     dbWriteTable(con,paste0("panel_",substr(lf,56,59)),temp,overwrite =T)
#   } 
#   
#   
# }
# ```
# 
# 
# ## MSA Office 2015-2016
# 
# ```{r}
# msaoffice_files <- list.files("C:/Users/dratnadiwakara2/Downloads/HMDA/raw_data/msa_office/",full.names = T)
# yrs <- as.character(2015:2016)
# 
# for(lf in msaoffice_files) {
#   if(substr(lf,61,64) %in% yrs) {
#     temp <- fread(lf)
#     names(temp) <- c("asofdate","agencycode","respondentid","msa","msa_name")
#     
#     dbWriteTable(con,paste0("msa_branch_",substr(lf,61,64)),temp,overwrite =T)
#   }
# }
# ```
# 
# 
# # LAR
# 
# ```{r}
# lar_files <- list.files("C:/Users/dratnadiwakara2/Downloads/HMDA/raw_data/lar/",full.names = T)
# ```
# 
# 
# ## LAR 2018 to 2022
# ```{r}
# yrs <- as.character(2022)
# 
# for(lf in lar_files) {
#   if(substr(lf,54,57) %in% yrs) {
#     print(substr(lf,54,57))
#     temp <- fread(lf,header = T,sep = "|")
#     setnames(temp,
#              c("activity_year","derived_msa_md","county_code","census_tract","purchaser_type","action_taken","preapproval",
#                "loan_type","loan_purpose","lien_status","loan_amount","income",
#                "applicant_ethnicity_1","co_applicant_ethnicity_1",
#                "applicant_race_1","applicant_race_2","applicant_race_3","applicant_race_4","applicant_race_5",
#                "co_applicant_race_1","co_applicant_race_2","co_applicant_race_3","co_applicant_race_4", "co_applicant_race_5",
#                "applicant_sex","co_applicant_sex",
#                "denial_reason_1","denial_reason_2","denial_reason_3",
#                "tract_population","tract_minority_population_percent","ffiec_msa_md_median_family_income",
#                "tract_to_msa_income_percentage","tract_owner_occupied_units","tract_one_to_four_family_homes",
#                "tract_median_age_of_housing_units"),
#              c("asofdate","msa","countycode","censustract","typeofpurchaser","actiontaken","preapprovals",
#                "typeofloan","purposeofloan","lienstatus","amountofloan","applicantincome",
#                "applicantethnicity","coapplicantenthnicity",
#                "applicantrace1","applicantrace2","applicantrace3","applicantrace4","applicantrace5",
#                "coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4", "coapplicantrace5",
#                "applicantsex","coapplicantsex",
#                "denialreason1","denialreason2","denialreason3",
#                "ct_population","ct_minoritypct","ct_median_income","ct_tract_to_msa_income","ct_no_occupied_unites",
#                "ct_no_of_1to4_family_unit","ct_median_age_housing_units"))
#     temp[,amountofloan:=floor(amountofloan/1000)]
#     temp[,property_value:=floor(as.numeric(property_value)/1000)]
#     
#     temp <-  temp %>% 
#       mutate(age = substr(applicant_age,nchar(applicant_age)-1,nchar(applicant_age))) %>% 
#       as.data.table()
#     temp[,age:=as.numeric(age)]
#     temp[,age:=ifelse(age %in% c(88,99),NA,age)]
#     temp[,applicant_age:=age]
#     temp[,age:=NULL]
#     
#     temp <-  temp %>% 
#       mutate(age = substr(co_applicant_age,nchar(co_applicant_age)-1,nchar(co_applicant_age))) %>% 
#       as.data.table()
#     temp[,age:=as.numeric(age)]
#     temp[,age:=ifelse(age %in% c(88,99),NA,age)]
#     temp[,co_applicant_age:=age]
#     temp[,age:=NULL]
#     
#     temp <-  temp %>% 
#       mutate(dti = substr(debt_to_income_ratio,nchar(debt_to_income_ratio)-2,nchar(debt_to_income_ratio))) %>% 
#       as.data.table()
#     temp[,dti:=str_replace(string = dti,pattern = "%","")]
#     temp[,dti:=as.numeric(dti)]
#     temp[,debt_to_income_ratio:=dti]
#     temp[,dti:=NULL]
#     
#     temp[,combined_loan_to_value_ratio:=as.numeric(combined_loan_to_value_ratio)]
#     temp[,interest_rate:=as.numeric(interest_rate)]
#     temp[,total_loan_costs:=as.numeric(total_loan_costs)]
#     temp[,origination_charges:=as.numeric(origination_charges)]
#     
#     temp[,countycode:=str_pad(countycode,5,"left","0")]
#     temp[,state:=substr(countycode,1,2)]
#     temp[,countycode:=substr(countycode,3,5)]
#     
#     temp[,censustract:=as.character(censustract)]
#     temp[,censustract:=str_pad(censustract,11,"left","0")]
#     
#     temp[,loan_term:=as.numeric(loan_term)]
#     
#     dbWriteTable(con,paste0("lar_",substr(lf,54,57)),temp,overwrite=T)
#   }
# }
# 
# 
# ```
# 
# ## LAR 2015 to 2016
# 
# ```{r}
# yrs <- as.character(2015:2016)
# 
# split_positions <- c(4,14,15,16,17,18,19,24,25,26,31,33,36,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,61,62,63,64,65,70,71,72,73,80,88,94,102,108,116,124,125)
# 
# col_names <- c("asofdate","respondentid","agencycode","typeofloan","propertytype","purposeofloan",
#                "occupancy","amountofloan","preapprovals","actiontaken","msa","state","countycode",
#                "censustract","applicantethnicity","coapplicantenthnicity",
#                "applicantrace1","applicantrace2","applicantrace3","applicantrace4","applicantrace5",
#                "coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4", "coapplicantrace5",
#                "applicantsex","coapplicantsex","applicantincome",
#                "typeofpurchaser","denialreason1","denialreason2","denialreason3",
#                "ratespread","hoepastatus","lienstatus","editstatus","seqno",
#                "ct_population","ct_minoritypct","ct_median_income","ct_tract_to_msa_income","ct_no_occupied_unites",
#                "ct_no_of_1to4_family_unit","application_date_indicator"
# )
# 
# for(lf in lar_files) {
#   print(lf)
#   if(substr(lf,54,57) %in% yrs) {
#     print(substr(lf,54,57))
#     temp <- fread(lf,header = F,sep = ",")
#     names(temp) <- col_names
#     temp[,state:=str_pad(state,2,"left","0")]
#     temp[,countycode:=str_pad(countycode,3,"left","0")]
#     temp[,censustract:=as.character(censustract)]
#     temp[,censustract:=trimws(censustract)]
#     temp[,censustract:=gsub("[.]","",censustract)]
#     temp[,censustract:=str_pad(censustract,6,"left","0")]
#     temp[,censustract:= paste(state,countycode,censustract,sep="")]
#     temp[,respondentid:=str_pad(respondentid,10,"left","0")]
#     dbWriteTable(con,paste0("lar_",substr(lf,54,57)),temp,overwrite=T)
#   } 
#   
# }				
# 
# ```
# 
# 
# ## LAR 2017
# 
# ```{r}
# lar <- fread("C:/Users/dratnadiwakara2/Downloads/2017_public_lar.csv")
# 
# col_names <- c("asofdate","respondentid","agencycode","typeofloan","propertytype","purposeofloan",
#                "occupancy","amountofloan","preapprovals","actiontaken","msa","state","countycode",
#                "censustract","applicantethnicity","coapplicantenthnicity",
#                "applicantrace1","applicantrace2","applicantrace3","applicantrace4","applicantrace5",
#                "coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4", "coapplicantrace5",
#                "applicantsex","coapplicantsex","applicantincome",
#                "typeofpurchaser","denialreason1","denialreason2","denialreason3",
#                "ratespread","hoepastatus","lienstatus","editstatus","seqno",
#                "ct_population","ct_minoritypct","ct_median_income","ct_tract_to_msa_income","ct_no_occupied_unites",
#                "ct_no_of_1to4_family_unit","application_date_indicator"
# )
# 
# names(lar) <- col_names
# 
# lar[,state:=str_pad(as.character(state),width = 2,pad="0",side = "left")]
# lar[,countycode:=str_pad(as.character(countycode),width = 3,pad="0",side = "left")]
# lar[,censustract:=as.character(censustract)]
# lar[,censustract:=gsub("[.]","",censustract)]
# lar[,censustract:=str_pad(censustract,6,"left","0")]
# lar[,censustract:= paste(state,countycode,censustract,sep="")]
# 
# dbWriteTable(con,paste0("lar_",2017),lar,overwrite=T)
# 
# ```
# 
# ## Panel 2017
# ```{r}
# panel <- fread("C:/Users/dratnadiwakara2/Downloads/2017_public_panel.csv")
# 
# col_names <- c("asofdate","respondentid","agencycode","parentidentifier","parentname","parentcity","parentstate","region","totalassets","otherlendercode","respondentname","respondenthomecity","respondenthomestate","topholderrssd","topholdername","topholdercity","topholderstate","topholdercounty","rssd","parentrssd","respondentfipsstatenumber")
# 
# names(panel) <- col_names
# 
# dbWriteTable(con,paste0("panel_",2017),panel,overwrite =T)
# ```
# 
# 
# ## arid2017_to_lei_xref_csv
# 
# ```{r}
# arid <- fread("C:/Users/dratnadiwakara2/Downloads/arid2017_to_lei_xref_csv.csv")
# dbWriteTable(con,paste0("arid_",2017),arid,overwrite =T)
# ```
# 
# 
# ## LAR 2004 to 2014
# ```{r}
# 
# yrs <- as.character(2004:2014)
# 
# split_positions <- c(4,14,15,16,17,18,23,24,29,31,34,41,42,43,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,71,72,73,80)
# 
# col_names <- c("asofdate","respondentid","agencycode","typeofloan","purposeofloan",
#                "occupancy","amountofloan","actiontaken","msa","state","countycode",
#                "censustract","applicantsex","coapplicantsex","applicantincome",
#                "typeofpurchaser","denialreason1","denialreason2","denialreason3",
#                "editstatus","propertytype","preapprovals","applicantethnicity",
#                "coapplicantenthnicity","applicantrace1","applicantrace2","applicantrace3",
#                "applicantrace4","applicantrace5",
#                "coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4",
#                "coapplicantrace5",
#                "ratespread","hoepastatus","lienstatus","seqno")
# 
# cols_to_numeric <- col_names[!col_names %in% c("state","countycode","censustract","respondentid")]
# 
# for(lf in lar_files) {
#   if(substr(lf,54,57) %in% yrs) {
#     print(substr(lf,54,57))
#     temp <- fread(lf,header = F,sep = "}")
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     temp[,V1:=NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     temp[,censustract:=trimws(censustract)]
#     temp[,censustract:=gsub("[.]","",censustract)]
#     temp[,censustract:=str_pad(censustract,6,"left","0")]
#     temp[,censustract:= paste(state,countycode,censustract,sep="")]
#     
#     
#     dbWriteTable(con,paste0("lar_",substr(lf,54,57)),temp,append=T)
#   } 
#   
# }
# ```
# 
# 
# ## LAR 1990 to 2003
# ```{r}
# yrs <- as.character(1990:2003)
# 
# split_positions <- c(4,14,15,16,17,18,23,24,28,30,33,40,41,42,43,44,48,49,50,51,52,53,60)
# 
# col_names <- c("asofdate","respondentid","agencycode","typeofloan","purposeofloan","occupancy","amountofloan","actiontaken","msa","state","countycode","censustract","applicantrace1","coapplicantrace1","applicantsex","coapplicantsex","applicantincome","typeofpurchaser","denialreason1","denialreason2","denialreason3","editstatus","seqno")
# 
# cols_to_numeric <- col_names[!col_names %in% c("state","countycode","censustract","respondentid")]
# 
# 
# for(lf in lar_files) {
#   if(substr(lf,54,57) %in% yrs) {
#     print(substr(lf,54,57))
#     temp <- fread(lf,header = F,sep = "}")
#     for (i in 1:length(split_positions)) {
#       start_pos <- ifelse(i == 1, 1, split_positions[i - 1] + 1)
#       end_pos <- split_positions[i]
#       new_col_name <- col_names[i]
#       temp[, (new_col_name) := substr(V1, start_pos, end_pos)]
#     }
#     temp[,V1:=NULL]
#     for (coln in cols_to_numeric) {
#       if (!is.numeric(temp[[coln]])) {
#         temp[, (coln) := as.numeric(temp[[coln]])]
#       }
#     }
#     
#     temp[,censustract:=trimws(censustract)]
#     temp[,censustract:=gsub("[.]","",censustract)]
#     temp[,censustract:=str_pad(censustract,6,"left","0")]
#     temp[,censustract:= paste(state,countycode,censustract,sep="")]
#     
#     
#     dbWriteTable(con,paste0("lar_",substr(lf,54,57)),temp,overwrite =T)
#   } 
#   
#   
# }
# ```
# 
# 
# 
# ## LAR 1983 to 1989
# Only aggregage data
# ```{r}
# dbDisconnect(con)
# ```
# 
# 
# 
# # MSA-County match
# MSA – Metropolitan Statistical Area
# For purposes of HMDA, the term is interchangeable with "metropolitan area." The underlying concept of an MSA is that of a core area containing a large population nucleus, together with adjacent communities having a high degree of economic and social integration with that core. MSAs are composed of entire counties or county equivalents. Every MSA has at least one urbanized area with a population of 50,000 or more.
# 
# 
# 
# The terms MSA and CBSA are often used interchangeably, but there is a subtle difference between the two. A CBSA (Core-Based Statistical Area) is a geographic entity that consists of a central county (or urban area) and the surrounding counties that have a high degree of social and economic integration with the urban core. An MSA (Metropolitan Statistical Area) is a type of CBSA that has a population of at least 50,000 people in its urban core.
# 
# In other words, all MSAs are CBSAs, but not all CBSAs are MSAs.
# 
# As of 2023, there are 962 CBSAs in the United States, of which 384 are MSAs. The remaining 578 CBSAs are micropolitan statistical areas (µSAs), which are CBSAs with a population of at least 10,000 but less than 50,000 in their urban core.
# 
# ```{r}
# yrs <- 1990:2016
# 
# msa_county <- list()
# 
# i=1
# for(yr in yrs) {
#   print(yr)
#   msa_county[[i]] <- dbGetQuery(con,paste0("select state,countycode,msa,asofdate from lar_",yr))
#   i=i+1
# }
# 
# temp <- rbindlist(msa_county)
# temp <- temp[!duplicated(temp)]
# temp <- temp[!is.na(state) & !is.na(countycode) & !is.na(msa) & state != "  " & countycode != "   "]
# temp[,fips:=paste0(state,countycode)]
# temp2 <- dcast(temp,fips~asofdate,value.var = "msa",fun.aggregate = median)
# temp2 <- temp2[nchar(fips)==5]
# ```
# 
# 
# # Broker
# 
# Question: Our investor reviews the loan before closing. Please let us know whether we are supposed to report the loan as a brokered or correspondent loan on the HMDA-LAR?
#   
#   Answer: Under Regulation C, the implementing regulation of the Home Mortgage Disclosure Act, a special broker rule applies to the reporting of loans on the Loan Application Register (HMDA-LAR) made by a broker and sold to an investor, where the investor reviews the loan before closing. Before determining who has the responsibility to report, it is necessary to define the terms “broker” and “investor” as they are used in Regulation C.
# 
# For purposes of the special broker rule, a “broker” is an institution that takes and processes a loan application and arranges for another institution to acquire the loan at or after closing; and an “investor” is an institution that acquires a loan from a broker at or after closing. [12 CFR, Part 203, Supplement I § 203.1(c)-2]
# 
# A broker that makes a credit decision in connection with a loan application reports that decision, and a broker that does not make a credit decision in connection with a loan application does not report the application. [12 CFR, Part 203, Supplement I § 203.1(c)-2]
