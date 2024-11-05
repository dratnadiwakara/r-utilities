# Load necessary package
library(stats)

# Define the MertonSolution function in R
MertonSolution <- function(b, E, D, r, d, TT, sigE) {
  A <- b[1]  # Asset value
  sig <- b[2]  # Asset volatility
  
  # Penalize negative values of A and sig
  esig <- 0
  eA <- 0
  if (sig < 0) {
    sig <- 0
    esig <- 9999999
  }
  if (A < 0) {
    A <- 0
    eA <- 9999999
  }
  
  # Black-Scholes formula components
  d1 <- (log(A / D) + (r - d + sig^2 / 2) * TT) / (sig * sqrt(TT))
  C <- A * pnorm(d1) - D * exp(-r * TT) * pnorm(d1 - sig * sqrt(TT))
  
  # Add present value of dividends
  PVd <- A * (1 - exp(-d * TT))
  C <- C + PVd  # Adjusted equity value
  
  # Calculate model-implied equity volatility (v)
  v <- (exp(-d * TT) * pnorm(d1) + (1 - exp(-d * TT))) * (A / E) * sig
  
  # Calculate error between observed and model-implied values
  err <- c(E - C, sigE - v) + esig * b[2]^2 + eA * b[1]^2
  
  # Return sum of squared errors for optimization
  return(sum(err^2))
}

     # Observed equity volatility

MertonPD <- function(initial_guess,E, D, r, d, TT, sigE) {
  result <- optim(
    par = initial_guess,         # Initial guesses for A and s_A
    fn = MertonSolution,         # Function to minimize
    E = E, D = D, r = r, d = d, TT = TT, sigE = sigE,
    method = "BFGS"              # Optimization method suitable for smooth functions
  )
  
  # Extract optimal values for A and s_A
  A_optimal <- result$par[1]
  sA_optimal <- result$par[2]
  
  d1 <- (log(A_optimal / D) + (r - d + sA_optimal^2 / 2) * TT) / (sA_optimal * sqrt(TT))
  DD <- d1
  
  return(pnorm(-DD))
}


# # Initial guesses for asset value (A) and asset volatility (s_A)
# initial_guess <- c(1.17, 0.2)
# # Define parameters from your example
# E <- 0.17         # Market value of equity
# D <- 1            # Face value of debt
# r <- 0.0197       # Risk-free rate
# d <- 0.002            # Dividend yield (assuming zero if unknown)
# TT <- 10           # Time horizon in years
# sigE <- 0.25 
# 
# 
# MertonPD(initial_guess,E, D, r, d, TT, sigE)


