library(data.table)
library(fixest)
library(pROC)

# Function
dynamic_logit_auc <- function(dt, outcome_var, predictors, fixed_effects, time_var, oos_year, num_prev_years) {
  stopifnot(is.data.table(dt))
  
  # Construct formula
  rhs <- paste0(paste(predictors, collapse = " + "),"|",paste(fixed_effects, collapse = " + "))
  fml <- as.formula(paste(outcome_var, "~", rhs))
  
  # Train and test sets
  train_years <- (oos_year - num_prev_years):(oos_year - 1)
  train <- dt[get(time_var) %in% train_years]
  test <- dt[get(time_var) == oos_year]
  
  # Fit model
  model <- feglm(fml, data = train, family = binomial())
  
  # Predict
  test[, pred_prob := predict(model, newdata = test, type = "response")]
  
  # AUC
  auc_val <- as.numeric(auc(test[[outcome_var]], test[["pred_prob"]]))
  
  return(list(model = model, auc = auc_val))
}


# # Example Use
# set.seed(123)
# n <- 2000
# dt_synth <- data.table(
#   year = sample(2015:2020, n, replace = TRUE),
#   income = rnorm(n, mean = 50, sd = 10),
#   credit_score = rnorm(n, mean = 700, sd = 50),
#   region_id = sample(LETTERS[1:5], n, replace = TRUE)
# )
# 
# # Binary outcome (logit based on predictors + fixed effect noise)
# dt_synth[, region_effect := as.numeric(factor(region_id))]
# dt_synth[, p := plogis(0.01 * income + 0.005 * credit_score - 2 + 0.2 * region_effect)]
# dt_synth[, default := rbinom(.N, 1, p)]
# dt_synth[, region_effect := NULL]
# dt_synth[, p := NULL]
# 
# # Example call
# result <- dynamic_logit_auc(
#   dt = dt_synth,
#   outcome_var = "default",
#   predictors = c("income", "credit_score"),
#   fixed_effects = c("region_id"),
#   time_var = "year",
#   oos_year = 2020,
#   num_prev_years = 3
# )
# 
# # Output model summary and AUC
# summary(result$model)
# print(result$auc)