library(quantmod)
library(data.table)

download_price_data <- function(tickers, from = "2020-01-01") {
  all_data <- list()
  
  for (ticker in tickers) {
    message("Downloading: ", ticker)
    tryCatch({
      stock_xts <- getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE)
      stock_dt <- data.table(
        date = index(stock_xts),
        price = as.numeric(Ad(stock_xts)),
        ticker = ticker
      )
      all_data[[ticker]] <- stock_dt
    }, error = function(e) {
      message("Failed: ", ticker, " - ", conditionMessage(e))
    })
  }
  
  result <- rbindlist(all_data, use.names = TRUE, fill = TRUE)
  return(result)
}


# Example usage:
# tickers <- c("AAPL", "MSFT", "GOOG")
# prices_dt <- download_price_data(tickers, from = "2023-01-01")
# head(prices_dt)