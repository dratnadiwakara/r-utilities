library(data.table)

# Function to convert a data.table with list columns into a basic data.table
expand_list_columns <- function(dt) {
  # Identify list columns
  list_cols <- sapply(dt, is.list)
  
  # For each list column, expand it into separate rows
  dt_expanded <- dt[, lapply(.SD, function(x) unlist(x, recursive = FALSE)), .SDcols = list_cols]
  
  # Identify non-list columns
  non_list_cols <- dt[, !list_cols, with = FALSE]
  
  # Repeat the non-list columns to match the expanded rows from the list columns
  repeated_non_list_cols <- non_list_cols[rep(1:.N, sapply(dt[[which(list_cols)[1]]], length))]
  
  # Combine the repeated non-list columns with the expanded list columns
  result <- cbind(repeated_non_list_cols, dt_expanded)
  
  return(result)
}