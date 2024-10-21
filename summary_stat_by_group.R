libray(simplermarkdown)

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

# Example usage
# summary_stats_function(call_chg, c("deposit_exp_deposits_chg", "domestic_deposits_chg"), "large_bank")
