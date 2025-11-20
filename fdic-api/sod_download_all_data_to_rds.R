rm(list=ls())
# install.packages(c("httr2", "data.table"))
library(httr2)
library(data.table)

#----------------------------
# Helper: download one page
#----------------------------
get_sod_page <- function(year, offset = 0, limit = 10000) {
  req <- request("https://banks.data.fdic.gov/api/sod") |>
    req_url_query(
      # api_key is optional for FDIC; you can include it or drop it
      # api_key = '9k0hdtov3afqiZNxad4CMMuFihouBNJ2DDs0yLXj',
      filters = paste0("YEAR:", year),
      # fields  = paste(
      #   "RSSDID",     # Bank RSSD ID
      #   "RSSDHCR",    # Top-holder RSSD
      #   "CERT",       # FDIC Cert
      #   "NAME",       # Bank name
      #   "UNINUMBR",   # Branch (unique) ID
      #   "BRNAME",     # Branch name
      #   "DEPSUMBR",   # Branch deposits ($000s)
      #   "STCNTYBR",   # State+county code
      #   "YEAR",       # Survey year
      #   "ZIPBR",      # Branch ZIP
      #   "CBSA_DIV_NAMB",
      #   "BRSERTYP",
      #   "SPECDESC",
      #   "UNIT",
      #   "SPECGRP",
      #   "REGAGNT",
      #   sep = ","
      # ),
      limit   = limit,
      offset  = offset,
      format  = "json"
    )
  
  resp <- req_perform(req)
  j <- resp_body_json(resp, simplifyVector = TRUE)
  
  # The FDIC SOD API returns data nested. Your working code used data$data$data.
  # We'll follow that.
  dt <- as.data.table(j$data$data)
  
  return(dt)
}

#------------------------------------------------
# Helper: download ALL pages for a given year
#------------------------------------------------
get_sod_year_all <- function(year, page_size = 10000) {
  message("Year ", year, ": downloading...")
  out <- data.table()
  offset <- 0L
  
  repeat {
    page_dt <- get_sod_page(year = year, offset = offset, limit = page_size)
    
    # stop if no more rows
    if (nrow(page_dt) == 0L) break
    
    out <- rbind(out, page_dt, fill = TRUE)
    offset <- offset + page_size
  }
  
  message("Year ", year, ": ", nrow(out), " rows")
  out[]
}

#------------------------------------------------
# Main loop: 2000 through 2025
#------------------------------------------------
years <- 2000:2025

all_sod <- rbindlist(
  lapply(years, get_sod_year_all),
  fill = TRUE
)

# optional: convert deposits to dollars instead of $000
all_sod[, deposits_dollars := as.numeric(DEPSUMBR) * 1000]

#------------------------------------------------
# Save as .Rds
#------------------------------------------------
saveRDS(all_sod, file = "C:/data/fdic_sod_2000_2025.rds")

# Quick sanity check
print(dim(all_sod))
print(head(all_sod))
