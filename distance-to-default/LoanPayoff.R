LoanPayoff <- function(F, f, ival, rho, sig, T) {
  # F = loan face value (either a scalar or a matrix the same size as f)
  # f = log asset value factor realizations
  # ival = initial log asset value
  # rho = correlation of asset values
  # sig = volatility of log asset values
  # T = loan maturity

  # Calculate expected asset value conditional on common factor f
  EA <- exp(f + ival + 0.5 * (1 - rho) * T * sig^2)

  # Calculate s
  s <- sig * sqrt(T) * sqrt(1 - rho)

  # Calculate a
  a <- (log(F) - f - ival) / s

  # Calculate loan portfolio payoff at maturity
  L <- EA * (1 - pnorm(s - a)) + pnorm(-a) * F

  return(L)
}

# Example usage with sample values
F <- 100    # Loan face value
f <- log(80)  # Log asset value factor realizations
ival <- log(90)  # Initial log asset value
rho <- 0.5   # Correlation of asset values
sig <- 0.2   # Volatility of log asset values
T <- 1       # Loan maturity

# Calculate the loan payoff
result <- LoanPayoff(F, f, ival, rho, sig, T)
print(result)
