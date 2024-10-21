library(simplermarkdown)
library(tidyr)
library(data.table)
library(DescTools)  # For Winsorize function

summary_stats_function <- function(df, column_list, group_by = NULL) {
  # Select the specified columns from call_chg
  summary_stat_df <- df %>%
    select(all_of(column_list)) %>%
    data.table
  
  # If group_by is provided, group by the specified column, else no grouping
  if (!is.null(group_by)) {
    summary_stats <- summary_stat_df %>%
      group_by(across(all_of(group_by))) %>%
      summarise(across(everything(), list(
        Obs = ~ length(.),
        Mean = ~ round(mean(., na.rm = TRUE), 2),
        SD = ~ round(sd(., na.rm = TRUE), 2),
        P10 = ~ round(quantile(., 0.1, na.rm = TRUE), 2),
        P25 = ~ round(quantile(., 0.25, na.rm = TRUE), 2),
        P50 = ~ round(quantile(., 0.5, na.rm = TRUE), 2),
        P75 = ~ round(quantile(., 0.75, na.rm = TRUE), 2),
        P90 = ~ round(quantile(., 0.9, na.rm = TRUE), 2)
      ), .names = "{col}__{fn}")) %>%
      pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "__")
  } else {
    summary_stats <- summary_stat_df %>%
      summarise(across(everything(), list(
        Obs = ~ length(.),
        Mean = ~ round(mean(., na.rm = TRUE), 2),
        SD = ~ round(sd(., na.rm = TRUE), 2),
        P10 = ~ round(quantile(., 0.1, na.rm = TRUE), 2),
        P25 = ~ round(quantile(., 0.25, na.rm = TRUE), 2),
        P50 = ~ round(quantile(., 0.5, na.rm = TRUE), 2),
        P75 = ~ round(quantile(., 0.75, na.rm = TRUE), 2),
        P90 = ~ round(quantile(., 0.9, na.rm = TRUE), 2)
      ), .names = "{col}__{fn}")) %>%
      pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "__")
  }
  
  # Return the summary statistics
  return(summary_stats)
}


mean_by_group <- function(df, columns_to_include, group_var, win_probs = c(0, 1)) {
  # Ensure the df is a data.table
  df <- as.data.table(df)
  setnames(df, group_var, "group_var")
  
  # Calculate the number of observations for each group
  count_table <- df[, .(N = .N), by = group_var]
  
  # Filter data and calculate the winsorized mean for the specified columns
  result_table <- df[, lapply(.SD, function(x) list(winsorized_Mean = round(mean(
    Winsorize(x, quantile(x, probs = win_probs, na.rm = FALSE)), na.rm = TRUE), 3))), 
    by = group_var, .SDcols = columns_to_include]
  
  # Merge the count table with the result table
  result_table <- merge(count_table, result_table, by = "group_var")
  
  # Sort by group_var
  setorder(result_table, group_var)
  
  # Transpose the table
  transposed_result_table <- t(result_table)
  
  # Set column names to be the first row, and remove the first row
  colnames(transposed_result_table) <- unlist(transposed_result_table[1, ])
  transposed_result_table <- transposed_result_table[-1, ]
  
  # Add a first column for 'N' and the column names from 'columns_to_include'
  first_column <- c("N", columns_to_include)
  transposed_result_table <- cbind(first_column, transposed_result_table)
  
  # Convert the transposed table to a data frame
  transposed_result_table <- as.data.table(transposed_result_table)
  
  # Convert the columns to numeric where applicable
  numeric_cols <- names(transposed_result_table)[sapply(transposed_result_table, is.numeric)]
  transposed_result_table[, (numeric_cols) := lapply(.SD, round, digits = 2), .SDcols = numeric_cols]
  
  return(transposed_result_table)
}