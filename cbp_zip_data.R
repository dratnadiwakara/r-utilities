# https://www.census.gov/programs-surveys/cbp/data/datasets.html


if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")

library(data.table)

# Define the folder path
folder_path <- "C:/Users/dimut/Downloads/zbp"

# Get list of all text files in the folder
file_list <- list.files(folder_path, pattern = "^zbp[0-9]{2}totals\\.txt$", full.names = TRUE)

# Initialize an empty list to store the data.tables
data_list <- list()

# Loop through each file
for (file in file_list) {
  # Extract the year from the file name
  file_name <- basename(file)
  year_suffix <- as.numeric(sub("^zbp([0-9]{2})totals\\.txt$", "\\1", file_name))
  year <- 2000 + year_suffix
  
  # Read the file into a data.table
  dt <- fread(file)
  
  # Normalize column names to uppercase for consistency
  setnames(dt, toupper(names(dt)))
  
  # Keep only required columns
  dt <- dt[, .(ZIP, EST, AP)]
  
  # Add the year column
  dt[, Year := year]
  
  saveRDS(dt,paste0(folder_path,"/zbp",year,".rds"))
  
  # Add the data.table to the list
  data_list[[as.character(year)]] <- dt
}

# Print the structure of the list
# print(data_list)



# 
# if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
# if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
# 
# library(httr)
# library(jsonlite)
# 
# # base_url <- "https://api.census.gov/data"
# # dataset <- "zbp"
# # years <- 2012:2023
# # api_key <- 'c8809455cbebf7aa64ff3f98b394c8f25fdf8846'
# # 
# # year=2017
# # url <- sprintf("%s/%d/%s", base_url, year, dataset)
# # 
# # response <- GET(url, query = list(
# #   get = "ESTAB",      # Number of establishments
# #   for = "zip code:*", # ZIP-code level data
# #   NAICS2017 = "00",   # NAICS Sector 00
# #   key = api_key,
# #   EMPSZES = "000", 
# # ))
# # 
# # response <- GET('https://api.census.gov/data/2018/zbp?get=ESTAB,EMPSZES&for=zipcode:20002&NAICS2017=72')
# # 
# # 
# # response <- GET('https://api.census.gov/data/2018/zbp?get=EMPSZES,ESTAB&for=zip%20code:*&NAICS2017=00&key=c8809455cbebf7aa64ff3f98b394c8f25fdf8846')
# 
# results <- list()
# 
# for(year in 2020:2022) {
#   print(year)
#   response <- GET(paste0('https://api.census.gov/data/',year,'/zbp?get=PAYANN,EMPSZES,ESTAB&for=zip%20code:*&NAICS2017=00&key=c8809455cbebf7aa64ff3f98b394c8f25fdf8846&EMPSZES=001'))
#   
#   data <- fromJSON(content(response, as = "text"))
#   # Convert response to data frame and append year
#   df <- as.data.frame(data[-1, ], stringsAsFactors = FALSE)
#   colnames(df) <- data[1, ]
#   df$year <- year
#   results[[as.character(year)]] <- df
# }
# 
