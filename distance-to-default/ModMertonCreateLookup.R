ModMertonCreateLookup <- function(d, y, T, H, bookD, rho, ltv, xfs, xr, xF, xsig, N, Nsim2) {
  
  set.seed(1)
  w <- matrix(rnorm(Nsim2 * 3 * N), nrow = Nsim2, ncol = 3 * N)
  
  J <- dim(xsig)[2]
  K <- dim(xr)[3]
  Q <- dim(xF)[4]
  G <- dim(xfs)[1]
  
  fs <- xfs[, 1, 1, 1]
  
  xLt <- array(0, dim = c(G, J, K, Q))
  xBt <- array(0, dim = c(G, J, K, Q))
  xEt <- array(0, dim = c(G, J, K, Q))
  xFt <- array(0, dim = c(G, J, K, Q))
  xmdef <- array(0, dim = c(G, J, K, Q))
  xsigEt <- array(0, dim = c(G, J, K, Q))
  
  for (j in 1:J) {
    
    for (k in 1:K) {
      
      for (q in 1:Q) {
        print(paste("j:", j, "k:", k, "q:", q))
        
        param <- c(xr[1, j, k, q], T, xF[1, j, k, q], H, bookD * exp(xr[1, j, k, q] * H), rho, ltv, xsig[1, j, k, q], d, y)
        
        # Call the ModMertonComputation function
        result <- ModMertonComputation(fs, param, N, Nsim2, w)
        
        xLt[, j, k, q] <- result$Lt
        xBt[, j, k, q] <- result$Bt
        xEt[, j, k, q] <- result$Et
        xFt[, j, k, q] <- result$mFt
        xmdef[, j, k, q] <- result$mdef
        xsigEt[, j, k, q] <- result$sigEt
      }
      
    }
    
  }
  
  list(xLt = xLt, xBt = xBt, xEt = xEt, xFt = xFt, xmdef = xmdef, xsigEt = xsigEt)
}
