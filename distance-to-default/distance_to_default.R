# Load necessary package
# install.packages("nleqslv")
library(nleqslv)

# Parameters known
VE <- 0.180155       # Observed equity value (in millions)
SE <- 0.267237       # Observed equity volatility (30%)
X <- 1        # Default threshold (debt value)
r <- 0.018625       # Risk-free interest rate (5%)
T <- 1          # Time to maturity (1 year)
d <- 0.0       # Dividend yield (2%)
m <- 0.06

# Function to calculate d1 and d2 based on current estimates of VA and SA
d1 <- function(VA, SA) {
  (log(VA/X) + (r - d + (SA^2)/2) * T) / (SA * sqrt(T))
}

d2 <- function(d1, SA) {
  d1 - SA * sqrt(T)
}

calculate_equity_value <- function(VA, SA) {
  d1_val <- d1(VA, SA)
  d2_val <- d2(d1_val, SA)
  
  # Merton model for equity value
  VA * exp(-d * T) * pnorm(d1_val) - X * exp(-r * T) * pnorm(d2_val) + (1-exp(-d * T))*VA
}

calculate_equity_volatility <- function(VA, SA) {
  d1_val <- d1(VA, SA)
  
  # Merton model for equity volatility
  (VA * exp(-d * T) * pnorm(d1_val) * SA) / VE
}



VA_values <- seq(0.5,3,by=0.01)  # Asset values to try (e.g., from 80 to 200)
SA_values <- seq(0.01, 1.5, by = 0.01)  # Asset volatilities to try (e.g., from 0.1 to 0.5)

# Initialize variables to store best results
best_VA <- NA
best_SA <- NA
min_error <- Inf

# Grid search trial and error
for (VA in VA_values) {
  for (SA in SA_values) {
    # Calculate the equity value and volatility using the current VA and SA
    estimated_VE <- calculate_equity_value(VA, SA)
    estimated_SE <- calculate_equity_volatility(VA, SA)
    
    # Calculate error as the sum of squared differences from the observed values
    error <- (estimated_VE - VE)^2 + (estimated_SE - SE)^2
    
    # Update the best solution if the current error is smaller
    if (error < min_error) {
      best_VA <- VA
      best_SA <- SA
      min_error <- error
    }
  }
}


DTD <- Mertond_d(VE+X, SE*VE/(VE+X), X, m, d, T)

pnorm(-DTD)

# System of non-linear equations to solve for VA and SA
equations <- function(vars) {
  VA <- vars[1]
  SA <- vars[2]
  
  d1_val <- d1(VA, SA)
  d2_val <- d2(d1_val, SA)
  
  # Equation 1: Equity value from Merton model
  eq1 <- VA * exp(-d * T) * pnorm(d1_val) - X * exp(-r * T) * pnorm(d2_val) + (1-exp(-d * T))*VA- VE
  
  # Equation 2: Equity volatility from Merton model
  eq2 <- (VA * exp(-d * T) * pnorm(d1_val) * SA / VE) - SE
  
  return(c(eq1, eq2))
}

# Initial guesses for VA and SA
initial_guess <- c(1.18, 0.2)

# Solve the system of equations
solution <- nleqslv(initial_guess, equations)

# Extract the solutions for VA and SA
VA_solution <- solution$x[1]
SA_solution <- solution$x[2]

# Output the results
cat("Estimated Asset Value (VA):", VA_solution, "\n")
cat("Estimated Asset Volatility (SA):", SA_solution, "\n")

Mertond_d <- function(VA, SA, X, m, d, T) {
  (log(VA/X) + (m - d - (SA^2)/2) * T) / (SA * sqrt(T))
}

# Calculate distance to default
DTD <- Mertond_d(best_VA, best_SA, X, m, d, T)

pnorm(-DTD)






























# Load required package for cumulative normal distribution
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}


# Load necessary package
library(stats)

# Function to calculate Merton's distance to default
merton_dd <- function(VE, X, r, d, se, T = 1, equity_premium = 0.06, tol = 1e-6, max_iter = 10000) {
  # Starting values
  VA <- VE + X
  sa <- se #* VE / (VE + X)
  
  # Newton method to solve for VA and sa
  # for (i in 1:max_iter) {
  #   d1 <- (log(VA / X) + (r - d + 0.5 * sa^2) * T) / (sa * sqrt(T))
  #   d2 <- d1 - sa * sqrt(T)
  #   
  #   # N(d1) and N(d2) using pnorm (CDF of normal distribution)
  #   Nd1 <- pnorm(d1)
  #   Nd2 <- pnorm(d2)
  #   
  #   # Calculate new VE and sa based on the current VA and sa
  #   VE_new <- VA * exp(-d * T) * Nd1 - X * exp(-r * T) * Nd2
  #   sa_new <- (VE_new / VE) * se
  #   
  #   # Check for convergence
  #   if (abs(VE_new - VE) < tol && abs(sa_new - sa) < tol) {
  #     break
  #   }
  #   
  #   # Update VA and sa for the next iteration
  #   VA <- VE_new + X
  #   sa <- sa_new
  # }
  
  # Final distance to default calculation (dd)
  m <- equity_premium # Equity premium as proxy for asset return
  dd <- (log(VA / X) + (m - d - 0.5 * sa^2) * T) / (sa * sqrt(T))
  
  return(dd)
}

# Example usage:
# VE = market value of equity, X = face value of debt, r = risk-free rate, d = dividend rate, se = equity volatility
merton_dd(VE = 0.180155, X = 1, r = 0.018625, d = 0.0, se = 0.267237)



# Function to calculate default probability
default_probability_merton <- function(E, D, r, se, T = 1) {
  # Calculate distance to default (DD)
  DD <- (log(E / D) + (r - 0.5 * se^2) * T) / (se * sqrt(T))
  
  # Calculate default probability using the cumulative distribution function of normal distribution
  default_prob <- pnorm(-DD)
  
  return(default_prob)
}

# Example usage:
# E = 0.180155, D = 1, r = 0.018625, se = 0.267237, T = 1
default_probability_merton(E = 0.180155, D = 1, r = 0.018625, se = 0.267237, T = 0.05)


# Define the Merton model function
merton_dd_rnpd <- function(E, D, sigma_E, r, T) {
  
  # Black-Scholes d1 and d2 functions
  d1 <- function(V, D, r, sigma_V, T) {
    (log(V / D) + (r + 0.5 * sigma_V^2) * T) / (sigma_V * sqrt(T))
  }
  
  d2 <- function(d1_val, sigma_V, T) {
    d1_val - sigma_V * sqrt(T)
  }
  
  # Black-Scholes call option (equity) function
  call_option <- function(V, D, r, sigma_V, T) {
    d1_val <- d1(V, D, r, sigma_V, T)
    d2_val <- d2(d1_val, sigma_V, T)
    C <- V * pnorm(d1_val) - D * exp(-r * T) * pnorm(d2_val)
    return(C)
  }
  
  # Solve for asset value (V) and asset volatility (sigma_V)
  solve_for_V_sigma <- function(E, D, r, sigma_E, T) {
    # Define initial guess for V and sigma_V
    V_guess <- E + D  # Initial guess for V
    sigma_V_guess <- sigma_E  # Initial guess for asset volatility (σ_V)
    
    # Use an optimizer to minimize the difference between observed equity and estimated equity
    opt_result <- optim(c(V_guess, sigma_V_guess), function(x) {
      V <- x[1]
      sigma_V <- x[2]
      equity_value_est <- call_option(V, D, r, sigma_V, T)
      diff <- (equity_value_est - E)^2
      return(diff)
    }, method = "BFGS")
    
    return(list(V = opt_result$par[1], sigma_V = opt_result$par[2]))
  }
  
  # Solve for V and sigma_V using the iterative method
  result <- solve_for_V_sigma(E, D, r, sigma_E, T)
  V <- result$V
  sigma_V <- result$sigma_V
  
  # Calculate distance to default (DD) using d1
  d1_val <- d1(V, D, r, sigma_V, T)
  d2_val <- d2(d1_val, sigma_V, T)
  
  DD <- d1_val  # Distance to default is d1
  
  # Calculate risk-neutral probability of default (RNPD) using -d2
  RNPD <- pnorm(-d2_val)
  
  # Return both DD and RNPD as a named list
  return(list(distance_to_default = DD, risk_neutral_probability_of_default = RNPD))
}

# Example usage:
# Define input values
E <- 50e6        # Equity value: $50 million
D <- 100e6       # Debt value: $100 million
sigma_E <- 0.30  # Equity volatility (30%)
r <- 0.03        # Risk-free rate (3%)
T <- 5           # Time to maturity (5 years)

# Calculate DD and RNPD using the Merton model
result <- merton_dd_rnpd(E, D, sigma_E, r, T)

# Display results
print(paste("Distance to Default (DD):", result$distance_to_default))
print(paste("Risk-Neutral Probability of Default (RNPD):", result$risk_neutral_probability_of_default))
