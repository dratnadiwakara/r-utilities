library(httr2)
library(data.table)

base_url <- "https://api.fdic.gov/banks/history"

# ---- Filters ----
# FI-level only + EFFDATE >= 1995-01-01 + CHANGECODE in required set
filter_str <- 'ORG_ROLE_CDE:"FI" AND EFFDATE:[1995-01-01 TO 9999-12-31] AND (CHANGECODE:810 OR CHANGECODE:223 OR CHANGECODE:221 OR CHANGECODE:811 OR CHANGECODE:240 OR CHANGECODE:222)'

# ---- Fields ----
fields_str <- paste(
  c("ACQ_CERT",
    "ACQ_INSTNAME",
    "OUT_CERT",
    "SUR_CERT",
    "EFFDATE",
    "CHANGECODE_DESC",
    "CHANGECODE",
    "TRANSNUM",
    "CERT",
    "FRM_CERT"),
  collapse = ","
)

# ---- Paging ----
limit  <- 10000     # API max
offset <- 0
all_dt <- data.table()

repeat {
  
  req <- request(base_url) |>
    req_url_query(
      filters    = filter_str,
      fields     = fields_str,
      limit      = limit,
      offset     = offset,
      format     = "json",
      download   = "false",
      filename   = "data_file"
    )
  
  resp <- req_perform(req)
  j    <- resp_body_json(resp, simplifyVector = TRUE)
  
  page_dt <- as.data.table(j$data$data)
  
  if (nrow(page_dt) == 0L) break
  
  all_dt <- rbind(all_dt, page_dt, fill = TRUE)
  
  if (nrow(page_dt) < limit) break
  
  offset <- offset + limit
}

all_dt <- all_dt[!duplicated(all_dt[,.(TRANSNUM)])]

saveRDS(all_dt, "C:/data/m_and_a_data.rds")

