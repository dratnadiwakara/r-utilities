ModMertonComputation <- function(fs, param, N, Nsim2, w = NULL) {
  
  r <- param[1]     # log risk free rate
  T <- param[2]     # original maturity of bank loans
  bookF <- param[3] # cash amount of loan issued = book value for a coupon-bearing loan issued at par
  H <- param[4]     # bank debt maturity
  D <- param[5]     # face value of bank debt
  rho <- param[6]   # borrower asset value correlation
  ltv <- param[7]   # initial LTV
  sig <- param[8]   # borrower asset value volatility
  d <- param[9]     # depreciation rate of borrower assets
  y <- param[10]    # bank payout rate
  
  # Optional: government guarantee
  if (length(param) > 10) {
    g <- param[11]
  } else {
    g <- 0
  }
  
  # Optional: provide simulated factor shocks
  if (is.null(w)) {
    set.seed(1)
    w <- matrix(rnorm(Nsim2 * 3 * N), nrow = Nsim2, ncol = 3 * N)
  }
  
  ival <- log(bookF) - log(ltv)  # initial log asset value of borrower at origination
  sigf <- sqrt(rho) * sig
  HN <- H * (N / T)              # maturity in N time
  szfs <- nrow(fs)
  fs <- rbind(fs, fs, fs)        # use second and third block for numerical derivative
  Nsim1 <- nrow(fs)
  
  # Remaining maturity of first loans at t
  rmat <- array(rep(0:(N - 1), each = Nsim2 * Nsim1), c(Nsim2, N, Nsim1))
  ind1 <- (rmat >= HN)
  ind2 <- (rmat < HN)
  
  # Fractional accumulated loan life time at t+H and t
  aHmat <- array(rep(c(HN:0, (N - 1):(HN + 1)) / N, each = Nsim2 * Nsim1), c(Nsim2, N, Nsim1))
  atmat <- array(rep((N - 0:(N - 1)) / N, each = Nsim2 * Nsim1), c(Nsim2, N, Nsim1))
  
  # Euler discretization for log value with Jensen's term
  f <- cbind(matrix(0, Nsim2, 1), apply((r - d - 0.5 * sig^2) * (T / N) + sigf * sqrt(T / N) * w, 1, cumsum))
  fw <- cbind(matrix(0, Nsim2, 1), apply(-0.5 * rho * sig^2 * (T / N) + sigf * sqrt(T / N) * w, 1, cumsum))
  
  xf1 <- f[, N + 1] - f[, 1]
  f0w <- t(matrix(rep(fw[, N + 1], N), Nsim2, N)) - fw[, 1:N]
  f1 <- f[, (N + 1):(2 * N)] - f0w - f[, 1:N]
  f2 <- f[, (2 * N + 1):(3 * N)] - f[, (N + 1):(2 * N)]
  
  # Add fs shocks after loan origination
  fsa <- array(0, c(Nsim2, N, Nsim1))
  dstep <- 10
  df <- sigf / dstep
  fsa <- atmat * sigf * sqrt(T) + df * abind::abind(array(0, c(Nsim2, N, szfs)), array(1, c(Nsim2, N, szfs)), array(-1, c(Nsim2, N, szfs)), along = 3)
  f1j <- array(rep(f1, each = Nsim1), dim = c(Nsim2, N, Nsim1)) + fsa
  f2j <- array(rep(f2, each = Nsim1), dim = c(Nsim2, N, Nsim1))
  
  # Solve for promised yield on loan (a fixed point)
  initmu <- r + 0.01
  mu <- uniroot(function(mu) FindFaceValueIndiv(mu, bookF * exp(mu * T), ival, sig, T, r, d), c(initmu - 0.1, initmu + 0.1))$root
  F <- bookF * exp(mu * T)
  
  # Payoffs at loan maturities
  L1 <- LoanPayoff(F, f1j, ival, rho, sig, T)
  face1 <- matrix(F, nrow = Nsim2, ncol = N)
  
  ival2 <- log(L1 / ltv)
  F2 <- L1 * exp(mu * T)
  L2 <- LoanPayoff(F2, f2j, ival2, rho, sig, T)
  face2 <- F2
  
  face2[ind1] <- 0
  face1[ind2] <- 0
  
  # Factor realizations at t and t+H
  ft <- array(rep(f[, N + 1], each = Nsim1) - f0w - f[, 1:N], dim = c(Nsim2, N, Nsim1)) + fsa + ival
  fH1 <- repmat(f[, N + 1 + HN], each = Nsim1) - f0w - f[, 1:N]
  fH2 <- repmat(f[, N + 1 + HN], each = Nsim1) - f[, (N + 1):(2 * N)]
  fH1j <- array(rep(fH1, each = Nsim1), dim = c(Nsim2, N, Nsim1)) + fsa + ival
  fH2j <- array(rep(fH2, each = Nsim1), dim = c(Nsim2, N, Nsim1)) + ival2
  
  FH1j <- exp(fH1j) * exp(0.5 * (1 - rho) * (T * aHmat) * sig^2)
  FH2j <- exp(fH2j) * exp(0.5 * (1 - rho) * (T * aHmat) * sig^2)
  
  Ft <- apply(exp(ft) * exp(0.5 * (1 - rho) * (T * atmat) * sig^2), 2, mean)
  
  # Get conditional payoff distribution based on t+H information
  sc <- L1 / bookF
  sc[ind1] <- 1
  FHr1 <- FH1j
  FHr1[ind2] <- 0
  FHr2 <- FH2j / sc
  FHr2[ind1] <- 0
  Lr1 <- L1
  Lr1[ind2] <- 0
  Lr2 <- L2 / sc
  Lr2[ind1] <- 0
  
  LHj <- array(0, dim = c(Nsim2, N, Nsim1))
  for (j in 1:N) {
    FHr <- FHr1[, j, ] + FHr2[, j, ]
    Lr <- Lr1[, j, ] + Lr2[, j, ]
    sortF <- sort(FHr)
    sortL <- Lr[order(FHr)]
    win <- (Nsim2 * Nsim1) / 20
    LHs <- stats::fft(filter(sortL, rep(1 / win, win), sides = 2))
    LHj[, j, ] <- LHs
  }
  
  LH <- apply(LHj, 2, mean)
  FH <- apply(FHr1 + FHr2, 2, mean)
  
  face <- apply(face1 * exp(-r * (rmat - HN) * (T / N)) + face2 * exp(-r * (rmat - HN + N) * (T / N)), 2, mean)
  
  BH <- pmin(D, LH * exp(-y * H))
  EH <- LH - BH
  GH <- g * pmax(D - LH * exp(-y * H), 0)
  
  Lt <- mean(LH) * exp(-r * H)
  Bt <- mean(BH) * exp(-r * H)
  Et <- mean(EH) * exp(-r * H)
  Gt <- mean(GH)
  mFt <- mean(Ft)
  
  def <- ifelse(EH > 0, 0, 1)
  mdef <- mean(def)
  
  sigEt <- (dstep / 2) * (log(Et[szfs + 1:2 * szfs]) - log(Et[2 * szfs + 1:3 * szfs]))
  sigLt <- (dstep / 2) * (log(Lt[szfs + 1:2 * szfs]) - log(Lt[2 * szfs + 1:3 * szfs]))
  
  list(Lt = Lt[1:szfs], Bt = Bt[1:szfs], Et = Et[1:szfs], LH = LH[, 1:szfs], BH = BH[, 1:szfs], 
       EH = EH[, 1:szfs], FH = FH[, 1:szfs], mFt = mFt[1:szfs], def = def[, 1:szfs], mdef = mdef[1:szfs], 
       face = face[, 1:szfs], Gt = Gt[1:szfs], sigEt = sigEt, sigLt = sigLt)
}
