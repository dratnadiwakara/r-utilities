# Black-Scholes formula in R
black_scholes <- function(S, X, Time, r, sigma, type) {
  d1 <- (log(S / X) + (r + sigma^2 / 2) * Time) / (sigma * sqrt(Time))
  d2 <- d1 - sigma * sqrt(Time)
  
  if (type == "call") {
    price <- S * pnorm(d1) - X * exp(-r * Time) * pnorm(d2)
  } else if (type == "put") {
    price <- X * exp(-r * Time) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Option type must be either 'call' or 'put'")
  }
  
  return(price)
}

# Parameters
S <- 100      # Stock price
X <- 105      # Strike price
Time <- 1     # Time to expiration (in years)
r <- 0.05     # Risk-free rate
sigma <- 0.2  # Volatility
type <- "put"  # "call" or "put"

# Calculate the option price
option_price <- black_scholes(S, X, Time, r, sigma, type)
print(option_price)
