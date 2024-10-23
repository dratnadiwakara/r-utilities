library(data.table)

# Function to convert a data.table with list columns into a basic data.table
expand_list_columns <- function(dt) {
  # Identify list columns
  list_cols <- sapply(dt, is.list)
  
  # For each list column, expand it into separate rows
  result <- dt[, lapply(.SD, function(x) unlist(x, recursive = FALSE)), .SDcols = list_cols]
  
  return(result)
}