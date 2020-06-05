sim.multiFGN <- function(N, Tj, H){
  1:N %>%
    map(~ as.vector(longmemo::simFGN0(n = Tj, H = H))) %>%
    map_dfc(~ .)
}


autocorrelation_function <- function(d, X){
  TJ <- length(t(X))
  X_mean <- mean(X, na.rm = T)
  Xdiff_den <- sum((X[1:TJ] - X_mean)^2)
  Pk <- c()
  for (k in 1:d) {
    Xdiff_num <- (X[(1 + k) : TJ] - X_mean)*(X[1:(TJ - k)] - X_mean)
    Rk <- (sum(Xdiff_num))/((Xdiff_den))
    Pk[k] <- (Rk + TJ^(-0.1))/(1 + TJ^(-0.1))
  }
  return(Pk)
}


Theoretical_autocorrelation_function <- function(d){
  gammak <- c()
  for (k in 1:d) {
    gammak[k] <- 0.5*((k+1)^(1.9)-2*(k^1.9)+(k-1)^(1.9))}
  return(gammak)
}

unbiased_autocorrelation_function <- function(d, X) {
  Rk <- true_autocorrelation_function(d, X)
  Pk_new <- c()
  for (k in 1:d) {
    Pk_new[k] <- (Rk[k] + 0.9)/1.9}
  return(Pk_new)
}


true_autocorrelation_function <- function(d, X){
  TJ <- length(t(X))
  X_mean <- mean(X, na.rm = T)
  Xdiff_den <- sum((X[1:TJ] - X_mean)^2)
  Rk <- c()
  for (k in 1:d) {
    Xdiff_num <- (X[(1 + k) : TJ] - X_mean)*(X[1:(TJ - k)] - X_mean)
    Rk[k] <- (sum(Xdiff_num))/((Xdiff_den))
  }
  return(Rk)
}
