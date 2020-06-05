#' Compute cos(Yj)
#'
#' \code{cosYd} is an helping function for the characteristic function estimators
#' @param Yj vector - FBM process
#' @param d numeric
#' @param lambda numeric parameter, default=0.5
#' @param na.remove logical
#' @return vector
cosYd <- function(Yj, d=d, lambda=0.5, na.remove=F){
  if(na.remove == T) {Yj <- Yj[which(Yj!="NA")]}
  Tj <- length(Yj)
  Ydiff <- Yj[(d+1):Tj]-Yj[1:(Tj-d)]
  kYdiff <- rep(Ydiff,each=(Tj-d))
  rYdiff <- rep(Ydiff,(Tj-d))
  f <- sqrt(sum(cos(lambda*(kYdiff-rYdiff)/sqrt(d))))/(Tj-d)
  return(f)}


#' Mu estimation by characteristic function
#'
#' @param Zj vector, Fractional Gaussian Noise process
#' @param lambda parameter, default = 0.5
#' @return numeric, mu estimator
#' @export
estim.cf.mu <- function(Zj, lambda=0.5){
  S <- (1/T)*sum(sin(lambda*Zj))
  C <- (1/T)*sum(cos(lambda*Zj))
  mu <- (1/lambda)*atan(S/C)
  names(mu) <- "Mean"
  return(mu)}


#' Sigma and H estim. by characteristic function
#'
#' @param Yj vector, Fractional Brownian Motion process
#' @param maxd numeric, default = 10
#' @param lambda parameter, default = 0.5
#' @param FBM logical, whether the Yj vector is a FBM. Dafault is T
#' @param na.remove logical
#' @return vector, sigma and H estimators
#' @export
estim.cf.reg <- function(Yj, maxd=10, FBM=T, lambda=0.5, na.remove=F){
  if(FBM==F){Yj <- cumsum(Yj)}
  dgraph <- numeric(maxd)
  for (d in 1:maxd){
    dgraph[d] <- cosYd(Yj, d=d, lambda=lambda, na.remove=na.remove)
  }
  yy <- log(-log(abs(dgraph)))
  xx <- log(1:maxd)
  coef <- lm(yy~xx)$coefficients
  sigma <- 2*exp(coef[1]-2*log(lambda))
  Hreg <- (coef[2]+1)/2
  res <- c(sigma,Hreg)
  names(res) <- c("Sigma","H")
  return(res)}



#' Sigma and H estim. by characteristic function
#'
#' @param Yj vector, Fractional Brownian Motion process
#' @param maxd numeric, default = 10
#' @param lambda parameter, default = 0.5
#' @param FBM logical, whether the Yj vector is a FBM. Dafault is T
#' @param na.remove logical
#' @return numeric, sigma estimator
#' @export
estim.cf.sigma <- function(Yj, maxd=10, FBM=T, lambda=0.5, na.remove=F){
  sigma <- estim.cf.reg(Yj, maxd=maxd, FBM=FBM, lambda=lambda, na.remove=na.remove)[1]
  return(sigma)}



#' H estim. by characteristic function
#'
#' @param Yj vector, Fractional Brownian Motion process
#' @param maxd numeric, default = 10
#' @param lambda parameter, default = 0.5
#' @param FBM logical, whether the Yj vector is a FBM. Dafault is T
#' @param na.remove logical
#' @return numeric, H estimator
#' @export
estim.cf.H <- function(Yj, maxd=10, FBM=T, lambda=0.5, na.remove=F){
  H <- estim.cf.reg(Yj, maxd=maxd, FBM=FBM, lambda=lambda, na.remove=na.remove)[2]
  return(H)}



#' H estim. by Whittle method
#'
#' @param Zj vector, FGN process
#' @return numeric, H estimator
#' @importFrom longmemo WhittleEst
#' @export
estim.w.H <- function(Zj) {
  Hw <- WhittleEst(Zj)$coefficients[1]
  names(Hw) <- "Hw"
  return(Hw)}



#' Alfa estim. by characteristic function
#'
#' @param Yj vector, Fractional Brownian Motion process
#' @param lambda parameter, default = 0.5
#' @param FBM logical, whether the Yj vector is a FBM. Dafault is T
#' @param na.remove logical
#' @return numeric, alpha estimator
#' @export
estim.cf.alpha <- function(Yj, FBM=T, lambda=lambda, na.remove=F){
  if(FBM==F){Yj <- cumsum(Yj)}
  l <- seq(0.1,1,by=0.1)
  lgraph <- numeric(10)
  for (i in 1:10){
    lgraph[i] <- cosYd(Yj=Yj,d=2,lambda=l[i])
  }
  yy <- log(-log(abs(lgraph)))
  alpha <- lm(yy~log(l))$coefficients[2]
  names(alpha) = "Alpha"
  return(alpha)}



#' Coefficients estim. by characteristic function
#'
#' @param Zj vector, Fractional Gaussian Noise process
#' @param maxd numeric, default = 10
#' @param lambda parameter, default = 0.5
#' @param FBM logical, whether the Yj vector is a FBM. Dafault is T
#' @param na.remove logical
#' @return vector, Mu, Sigma, H and Alpha estimators
#' @export
estim.cf.coef <- function(Zj, maxd=10, FBM=T, lambda=0.5, na.remove=F){
  Mean <- estim.cf.mu(Zj, lambda=lambda)
  reg <- estim.cf.reg(Zj, maxd=maxd, FBM=F, lambda=lambda, na.remove=na.remove)
  Alpha <- estim.cf.alpha(Zj, FBM=F, lambda=lambda, na.remove=na.remove)
  out <- c(Mean, reg, Alpha)
  return(out)
}



# Omega() function in the test-functions.R file

#' Maximum likelihood estimator for the mean
#'
#' @param Xj vector - FGN process
#' @param H numeric - H parameter
#' @param sigma numeric - Sigma value, default=1
#' @export
FgnMean <- function(Xj,H=H,sigma=1){
  Xjna <- Xj[!is.na(Xj)]
  n <- length(Xjna)
  OmegaInv <- solve(Omega(H=H,n=n))
  mu <- sum(OmegaInv%*%Xjna)/sum(OmegaInv)}



# Omega() function in the test-functions.R file

#' Maximum likelihood estimator for the standard deviation
#'
#' @param Xj vector - FGN process
#' @param mean numeric - Estimated mean of the FGN process
#' @export
FgnVar <- function(Xj,mean=mean){
  Xjna <- Xj[!is.na(Xj)]
  n <- length(Xjna)
  var <- sum((Xjna-rep(mean,n))^2)/n}


