rm(list=ls())
library(httr2)
library(data.table)

# Function to get one quarter of data
get_financials_quarter <- function(date_str, limit = 10000) {
  out_all <- data.table()
  offset  <- 0L
  print(date_str)
  repeat {
    req_q <- request("https://api.fdic.gov/banks/financials") |>
      req_url_query(
        format    = "json",
        filters   = paste0('REPDTE:"',date_str,'"'),  # quarter-end Call Report date
        fields    = "RSSDHCR,RSSDID,CERT,REPDTE,ASSET,DEPDOM",  # total assets is ASSET
        limit     = limit,
        offset    = offset
      )
    
    resp_q <- req_perform(req_q)
    j <- resp_body_json(resp_q, simplifyVector = TRUE)
    
    page_dt <- as.data.table(j$data$data)
    if (nrow(page_dt) == 0L) break
    
    out_all <- rbind(out_all, page_dt, fill = TRUE)
    offset  <- offset + limit
  }
  
  out_all[, REPDTE := as.Date(REPDTE, "%Y%m%d")]
  out_all[]
}

# Generate sequence of quarter-end dates from 2000-03-31 onward
years <- 2000:2025

# Generate quarter-end dates explicitly
quarters <- as.Date(c(
  outer(years, c("-03-31", "-06-30", "-09-30", "-12-31"), paste0)
))
quarters_fmt <- format(quarters, "%Y%m%d")
quarters_fmt <- quarters_fmt[order(quarters_fmt)]
quarters_fmt <- quarters_fmt[!quarters_fmt %in% c("20250930","20251231")]

# Download and stack all quarters
all_quarters <- rbindlist(
  lapply(quarters_fmt, get_financials_quarter),
  fill = TRUE
)

# Save full panel
saveRDS(all_quarters, "C:/data/fdic_call_report_2000_2025_assets_deposits.rds")

# Quick preview
head(all_quarters)






# library(data.table)
# 
# req_assets <- request("https://api.fdic.gov/banks/financials") |>
#   req_url_query(
#     format    = "json",
#     filters   = paste0('REPDTE:"',date_str,'"'),  # quarter-end Call Report date
#     fields    = "RSSDHCR,RSSDID,CERT,REPDTE,ASSET,DEPDOM",  # total assets is ASSET
#     limit     = 10,
#     offset    = offset
#   )
# 
# resp_assets <- req_perform(req_assets)
# assets_json <- resp_body_json(resp_assets, simplifyVector = TRUE)
# 
# assets_dt <- as.data.table(assets_json$data$data)
# head(assets_dt)
