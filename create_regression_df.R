create_regression_dataframe <- function(model) {
  # Extract coefficients and standard errors
  coef_table <- tidy(model)[, c("term", "estimate", "std.error")]
  colnames(coef_table) <- c("Variable", "Coefficient", "SE")
  coef_table <- data.table(coef_table)
  coef_table[, stars := fifelse(abs(Coefficient / SE) > 2.58, "***",
                                fifelse(abs(Coefficient / SE) > 1.96, "**",
                                        fifelse(abs(Coefficient / SE) > 1.65, "*", "")))]
  
  
  # Add summary statistics (Observations, R², Adjusted R²)
  model_summary <- summary(model)
  stats <- data.table(
    Variable = c("Observations", "R2", "Adjusted R2"),
    Coefficient = c(
      nobs(model), 
      model_summary$r.squared, 
      model_summary$adj.r.squared
    )
  )
  stats[,SE:=NA]
  stats[,stars:=NA]
  
  # Combine coefficients and statistics
  final_df <- rbind(coef_table, stats)
  return(final_df)
}

create_regressions_dataframe <- function(models) {
  df <- NULL
  for (i in 1:length(models)) {
    # Extract regression output
    reg_output <- data.table(create_regression_dataframe(models[[i]]))
    
    # Add "Model" row at the top
    model_row <- data.table(
      Variable = "Model",
      Coefficient = names(models)[[i]]
    )
    model_row[, SE := NA]
    model_row[, stars := NA]
    
    # Add "Dependent Variable" row below "Model"
    dep_var_row <- data.table(
      Variable = "Dependent Variable",
      Coefficient = all.vars(formula(models[[i]]))[1] # Extract dependent variable name
    )
    dep_var_row[, SE := NA]
    dep_var_row[, stars := NA]
    
    # Combine rows
    reg_output <- rbind(model_row, dep_var_row, reg_output, fill = TRUE)
    
    # Rename columns for the current model
    names(reg_output)[2:4] <- paste0(names(models)[[i]], "_", names(reg_output)[2:4])
    
    if (is.null(df)) {
      df <- reg_output
    } else {
      df <- merge(df, reg_output, by = "Variable", all.x = TRUE, all.y = TRUE)
    }
  }
  
  # Ensure "Model" is the first row, followed by "Dependent Variable," and stats at the end
  stats_order <- c("Observations", "R2", "Adjusted R2")
  stats_rows <- df[Variable %in% stats_order]
  middle_rows <- df[!Variable %in% c("Model", "Dependent Variable", stats_order)]
  final_df <- rbind(
    df[Variable == "Model"],
    df[Variable == "Dependent Variable"],
    middle_rows,
    stats_rows[match(stats_order, stats_rows$Variable)]
  )
  
  return(final_df)
}
# 
# # Define models
# models <- list()
# models[['model1']] <- lm(mpg ~ wt + hp, data = mtcars)
# models[['model2']] <- lm(mpg ~ wt + hp + cyl, data = mtcars)
# 
# # Generate regression table
# regression_table <- create_regressions_dataframe(models)
# 
# # Display the dataframe
# print(regression_table)
