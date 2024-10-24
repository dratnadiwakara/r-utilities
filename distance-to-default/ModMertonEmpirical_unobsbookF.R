# Load necessary packages
library(fields)  # For interpolation

# Set working directory
direc <- 'C:/Users/dimut/Downloads/NagelPurnanandam_BankRiskDynamis_Code/'

compute <- 1  # Set to 1 to run the computation, otherwise it uses saved file
N <- 10       # Number of loan cohorts
Nsim2 <- 5000 # Number of simulated factor realization paths

# Model parameters
d <- 0.005     # Depreciation rate of borrower assets
y <- 0.002     # Bank payout rate
T <- 10        # Loan maturity
H <- 5         # Bank debt maturity
bookD <- 1     # Current book value of bank debt (normalized)
rho <- 0.5     # Share of factor variance in borrower asset variance
ltv <- 0.66    # Initial LTV

# Load empirical data
filename <- paste0(direc, 'test_inputdata.csv')
qtr_bankdd <- read.csv(filename)

# Extract necessary columns
E <- qtr_bankdd[, 1]
permco <- qtr_bankdd[, 2]
year <- qtr_bankdd[, 3]
month <- qtr_bankdd[, 4]
r <- qtr_bankdd[, 5]
sE <- qtr_bankdd[, 6]

# Define grid for asset vol, interest rates, etc.
xfs <- expand.grid(seq(-8, 8, by = 0.05), seq(0.15, 0.25, by = 0.05),
                   seq(0, 0.1, by = 0.005), seq(0.5, 2.5, by = 0.05))
xr <- array(xfs[, 1], dim = c(1, length(seq(-8, 8, by = 0.05)), 21, 41))
xsig <- array(xfs[, 2], dim = c(1, length(seq(-8, 8, by = 0.05)), 21, 41))
xF <- array(xfs[, 4], dim = c(1, length(seq(-8, 8, by = 0.05)), 21, 41))

# If compute is set to 1, run ModMertonCreateLookup and save the results
if (compute == 1) {
  # Run ModMertonCreateLookup function (converted previously)
  results <- ModMertonCreateLookup(d, y, T, H, bookD, rho, ltv, xfs, xr, xF, xsig, N, Nsim2)
  
  # Save the result as an RData file
  save(results, file = paste0(direc, 'ValueSurface.RData'))
}

# Load the saved file
load(paste0(direc, 'ValueSurface.RData'))

# Initialize necessary values
dataN <- length(E)
rs <- dim(xr)[3]  # Number of interest rate observations
minr <- xr[1, 1, 1, 1]
maxr <- xr[1, 1, rs, 1]
vol <- rep(0.2, dataN)  # Set asset volatility

# Prepare arrays to store results
Lr <- matrix(0, dataN, rs)
Br <- matrix(0, dataN, rs)
mdefr <- matrix(0, dataN, rs)
sigr <- matrix(0, dataN, rs)
bookFr <- matrix(0, dataN, rs)
fsr <- matrix(0, dataN, rs)

# Loop through each interest rate
for (a in 1:rs) {
  sigEt <- results$xsigEt[, , a, ]
  Et <- results$xEt[, , a, ]
  Bt <- results$xBt[, , a, ]
  mdef <- results$xmdef[, , a, ]
  Lt <- results$xLt[, , a, ]
  sig <- results$xsig[, , a, ]
  fs <- results$xfs[, , a, ]
  bookF <- results$xF[, , a, ]
  
  # Handle NaN and Inf in sigEt (replace them with a large value)
  sigEt[is.nan(sigEt)] <- 99
  sigEt[is.infinite(sigEt)] <- 99
  
  # Create interpolation functions for each variable
  yfs <- Tps(cbind(Et, sigEt, sig), fs)
  yLt <- Tps(cbind(Et, sigEt, sig), Lt)
  yBt <- Tps(cbind(Et, sigEt, sig), Bt)
  ybookF <- Tps(cbind(Et, sigEt, sig), bookF)
  ymdef <- Tps(cbind(Et, sigEt, sig), mdef)
  
  # Extract interpolated values corresponding to empirical data points
  Lr[, a] <- predict(yLt, cbind(E, sE, vol))
  Br[, a] <- predict(yBt, cbind(E, sE, vol))
  mdefr[, a] <- predict(ymdef, cbind(E, sE, vol))
  bookFr[, a] <- predict(ybookF, cbind(E, sE, vol))
  fsr[, a] <- predict(yfs, cbind(E, sE, vol))
}

# Interpolate results to find closest interest rates on grid
rstep <- (maxr - minr) / (rs - 1)
rmat <- matrix(rep(r, rs), nrow = dataN, ncol = rs)
rgrid <- matrix(seq(minr, maxr, by = rstep), nrow = dataN, ncol = rs, byrow = TRUE)
dr <- (rgrid - rmat) / rstep
Wl <- 1 + dr
Wu <- 1 - dr
Wl[dr <= -1] <- 0
Wu[dr >= 1] <- 0
W <- ifelse(dr < 0, Wl, Wu)

# Calculate weighted sums
L <- rowSums(Lr * W)
B <- rowSums(Br * W)
mdef <- rowSums(mdefr * W)
bookF <- rowSums(bookFr * W)
fs <- rowSums(fsr * W)

# Save relevant output
save(L, B, mdef, fs, E, bookF, r, permco, year, month, sE, vol, 
     file = paste0(direc, 'mdef_output.RData'))
