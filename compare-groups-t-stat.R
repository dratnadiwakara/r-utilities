library(data.table)

compare_groups_t_stat <- function(dt, columns, group_var) {
  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    stop("The input data must be a data.table.")
  }
  
  # Check if required columns exist
  if (!all(columns %in% names(dt))) {
    stop("Some specified columns do not exist in the data.table.")
  }
  if (!(group_var %in% names(dt))) {
    stop("The grouping variable does not exist in the data.table.")
  }
  
  # Ensure the group_var contains only 0 and 1
  unique_vals <- unique(dt[[group_var]])
  if (!all(unique_vals %in% c(0, 1))) {
    stop("The grouping variable must contain only values 0 and 1.")
  }
  
  # Initialize results list
  results <- lapply(columns, function(col) {
    group_0 <- dt[get(group_var) == 0, ..col]
    group_1 <- dt[get(group_var) == 1, ..col]
    
    mean_0 <- mean(group_0[[1]], na.rm = TRUE)
    mean_1 <- mean(group_1[[1]], na.rm = TRUE)
    diff_means <- mean_1 - mean_0
    
    t_test_result <- t.test(group_1[[1]], group_0[[1]])
    
    list(
      variable = col,
      mean_0 = mean_0,
      mean_1 = mean_1,
      diff_means = diff_means,
      t_stat = t_test_result$statistic,
      p_value = t_test_result$p.value
    )
  })
  
  # Convert results to data.table
  result_dt <- rbindlist(results)
  
  return(result_dt)
}



# # Sample data
# dt <- data.table(
#   var1 = rnorm(100),
#   var2 = rnorm(100, mean = 5),
#   group = sample(c(0, 1), 100, replace = TRUE)
# )
# 
# # Run the function
# result <- compare_groups(dt, columns = c("var1", "var2"), group_var = "group")
# print(result)