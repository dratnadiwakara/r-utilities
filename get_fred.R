# install.packages("fredr")   # if needed
library(fredr)

#' Get a FRED series with flexible frequency/aggregation (defaults: quarterly, end-of-quarter)
#'
#' @param series_id Character. FRED series ID. Default "FEDFUNDS".
#' @param start_date Date or "YYYY-MM-DD". Observation start (default "2000-01-01").
#' @param end_date Date or "YYYY-MM-DD". Observation end (default = today).
#' @param frequency Character. FRED frequency code. Default "q" (quarterly).
#'   Common options: "d" (daily), "w" (weekly), "m" (monthly), "q" (quarterly), "sa" (semiannual), "a" (annual).
#' @param aggregation_method Character. How to aggregate within period. Default "eop" (end-of-period).
#'   Options: "avg", "sum", "eop".
#' @param as_data_table Logical. If TRUE, returns data.table (else tibble). Default FALSE.
#'
#' @return A tibble/data.table with at least columns `date` and `value`.
#' @examples
#' # One-time per session: set your API key
#' # fredr_set_key("658b7a752cbfdf5b06ee684f29f8d7e5 ")
#'
#' # Default: FEDFUNDS, quarterly, end-of-quarter
#' x <- get_fred("FEDFUNDS", start_date = "2015-01-01")
#'
#' # Monthly average instead
#' x_m <- get_fred("FEDFUNDS", start_date = "2015-01-01",
#'                 frequency = "m", aggregation_method = "avg")
get_fred <- function(series_id = "FEDFUNDS",
                     start_date = "2000-01-01",
                     end_date   = Sys.Date(),
                     frequency = "q",
                     aggregation_method = "eop",
                     as_data_table = FALSE) {
  
  # basic validation
  freq_ok <- c("d","w","bw","m","q","sa","a")
  agg_ok  <- c("avg","sum","eop")
  if (!frequency %in% freq_ok)
    stop(sprintf("`frequency` must be one of %s.", paste(freq_ok, collapse = ", ")))
  if (!aggregation_method %in% agg_ok)
    stop(sprintf("`aggregation_method` must be one of %s.", paste(agg_ok, collapse = ", ")))
  
  # coerce dates
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  # call FRED
  out <- fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end   = end_date,
    frequency = frequency,
    aggregation_method = aggregation_method
  )
  
  if (as_data_table) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      warning("data.table not installed; returning tibble.")
      return(out)
    }
    return(data.table::as.data.table(out))
  }
  
  out
}
