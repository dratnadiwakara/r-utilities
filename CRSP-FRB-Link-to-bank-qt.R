#https://www.newyorkfed.org/research/banking_research/crsp-frb

library(data.table)
library(lubridate)

convert_crsp_frb_to_bank_qt <- function(file_path, start_date = "2000-03-31") {
  
  df <- fread(file_path)
  
  # Convert date columns
  df[, dt_start := ymd(dt_start)]
  df[, dt_end := ymd(dt_end)]
  
  # Define panel start date and most recent quarter-end
  panel_start_date <- ymd(start_date)
  most_recent_qtr_end <- floor_date(Sys.Date(), "quarter") + months(3) - days(1)
  
  # Extend dt_end if it equals max(dt_end)
  max_dt_end <- max(df$dt_end, na.rm = TRUE)
  df[dt_end == max_dt_end, dt_end := most_recent_qtr_end]
  
  # Safe quarter generator
  generate_quarters <- function(start_date, end_date) {
    start <- max(start_date, panel_start_date)
    if (start > end_date) return(NULL)  # avoid invalid range
    
    # Get first day of quarters, then convert to ends
    quarter_starts <- seq(floor_date(start, "quarter"), floor_date(end_date, "quarter"), by = "3 months")
    quarter_ends <- quarter_starts + months(3) - days(1)
    quarter_ends[quarter_ends >= start & quarter_ends <= end_date]
  }
  
  # Build the panel
  panel_dt <- df[, {
    qtrs <- generate_quarters(dt_start, dt_end)
    if (!is.null(qtrs) && length(qtrs) > 0) {
      .(name = name, inst_type = inst_type, entity = entity, permco = permco, quarter_end = qtrs)
    } else NULL
  }, by = .I][, -"I"]
  
  setnames(panel_dt,"entity","RSSD")
  return(panel_dt)
}



# Example usage:
# op <- convert_crsp_frb_to_bank_qt("C:/Users/dimut/Downloads/crsp_20240930.csv","2022-01-01")

