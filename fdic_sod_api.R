# install.packages(c("httr2", "data.table"))
library(httr2)
library(data.table)

# Function to get one page
get_sod_page <- function(offset = 0, limit = 10000) {
  req <- request("https://banks.data.fdic.gov/api/sod") |>
    req_url_query(
      # api_key = '9k0hdtov3afqiZNxad4CMMuFihouBNJ2DDs0yLXj',  # optional; works even without key
      filters = 'YEAR:2024',
      fields  = "RSSDID,RSSDHCR,CERT,NAME,UNINUMBR,BRNAME,DEPSUMBR,STCNTYBR,YEAR,ZIPBR",
      limit   = limit,
      offset  = offset,
      format  = "json"
    )
  
  resp <- req_perform(req)
  data <- resp_body_json(resp, simplifyVector = TRUE)
  
  # Adjust depending on FDIC JSON structure
  as.data.table(data$data$data)
}

# Pull all pages
page_size <- 10000
all_data <- data.table()
offset <- 0

repeat {
  cat("Downloading rows", offset + 1, "to", offset + page_size, "...\n")
  page <- get_sod_page(offset, page_size)
  
  if (nrow(page) == 0) break  # no more data
  
  all_data <- rbind(all_data, page)
  offset <- offset + page_size
}


# Inspect data
print(head(all_data))




# MINIMAL EXAMPLE
# # install.packages(c("httr2", "data.table"))
# library(httr2)
# library(data.table)
# 
# # Define the request: SOD data for 2024, North Carolina branches
# req <- request("https://banks.data.fdic.gov/api/sod") |>
#   req_url_query(
#     api_key = '9k0hdtov3afqiZNxad4CMMuFihouBNJ2DDs0yLXj',
#     filters = 'YEAR:2024',   # 2024 data, state = NC
#     fields  = "RSSDID,RSSDHCR,CERT,NAME,UNINUMBR,BRNAME,DEPSUMBR,STCNTYBR,YEAR,ZIPBR",  # choose fields you want
#     limit   = 10000,                            # just first 10 rows
#     format  = "json"
#   )
# 
# # Perform the request
# resp <- req_perform(req)
# 
# # Parse JSON to R list
# data <- resp_body_json(resp, simplifyVector = TRUE)
# 
# # Extract the data table
# sod <- data$data$data
# 
# # View the results
# print(sod)

