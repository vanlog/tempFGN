#' Self-similarity graphical test
#'
#' @param Yj vector - FBM process
#' @param maxd numeric - Parameter, default=10
#' @param lambda numeric - Parameter, default=0.5
#' @param main string, title
#' @param subtitle logical. Defines if a subtitle should be printed
#' @param cex.axis numeric
#' @param cex.lab numeric
#' @param cex.main numeric
#' @param cex.dots numeric
#' @param lwd numeric
#' @export
fgtSelfSim <- function(Yj=Yj, maxd=10, lambda=0.5, main=NULL, subtitle = T, cex.axis=0.8,
                       cex.lab=0.8, cex.main=1, cex.dots=1, lwd=1){
  dgraph <- NULL
  for (d in 1:maxd){
    fun <- cosYd(Yj=Yj,d=d,lambda=lambda)
    dgraph <- c(dgraph,fun)}
  yy <- log(-log(abs(dgraph)))
  xx <- log(1:maxd)
  aa <- lm(yy~xx)$coefficients[1] # Intercept of the regression line
  bb <- lm(yy~xx)$coefficients[2] # Slope of the regression line
  Hreg <- (bb+1)/2    # Estimated (by regression) H
  par(bty="l", cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main,
      font.lab=2, mar=.1+c(5,4,4,2), las=1)
  p <- plot(xx,yy,pch=16,xlab="",ylab="",
            main=paste("Self-similarity test",main),cex=cex.dots,
            panel.first=abline(v=axTicks(1),h=axTicks(2),col="grey90"))
  abline(a=aa,b=bb,lty=2,col=2,lwd=lwd)
  if(subtitle ==T) {mtext(paste("Estimated H by regression =", round(Hreg,2)),
                          side=3,line=0.2,cex=0.7)}
  return(list(x=xx, y=yy, intercept=aa, slope=bb, Hregression=Hreg, plot=p))}



#' Normality graphical test
#'
#' @param Yj vector - FBM process
#' @param xmax numeric - Parameter, default=1
#' @param lambda numeric - Parameter, default=0.5
#' @param main string, title
#' @param cex.axis numeric
#' @param cex.lab numeric
#' @param cex.main numeric
#' @param cex.dots numeric
#' @param lwd numeric
#' @export
fgtNormality <- function(Yj=Yj, xmax=1, main=NULL,cex.axis=0.8,
                         cex.lab=0.8,cex.main=1,cex.dots=1,lwd=1){
  xx <- seq(0.1, xmax, by=0.1)
  dd <- 2
  lgraph <- NULL
  for (l in xx){
    fun <- cosYd(Yj=Yj,d=dd,lambda=l)
    lgraph <- c(lgraph,fun)}
  yy <- log(-log(abs(lgraph)))
  aa <- lm(yy~log(xx))$coefficients[1]
  bb <- lm(yy~log(xx))$coefficients[2]
  par(bty="l",cex.axis=cex.axis,cex.lab=cex.lab,cex.main=cex.main,
      font.lab=2,mar=.1+c(5,4,4,2),las=1)
  plot(log(xx),yy,pch=16,xlab="",ylab="",cex=cex.dots,
       main=paste("Normality test",main),
       panel.first=abline(v=axTicks(1),h=axTicks(2),col="grey90"))
  abline(a=aa,b=bb,lty=2,col=2,lwd=lwd)
  mtext(paste("Estimated alpha =",round(bb,2)),
        side=3,line=0.2,cex=0.7)
  return(list(x=xx,y=yy,intercept=aa,slope=bb))}



#' Omega matrix - Helping function for ML estimators and Chi-Square test
#'
#' @param H numeric - H parameter
#' @param n numeric
#' @param sigma numeric - Sigma value, default=1
# Omega <- function(H=H,n=n,sigma=1){
#   d <- abs(matrix(rep(1:n,n),nrow=n)-matrix(rep(1:n,n),nrow=n,byrow=T))
#   autocorr <- (sigma^2/2)*(abs(d+1)^(2*H)-2*(abs(d)^(2*H))+abs(d-1)^(2*H))
#   return(autocorr)}

Omega <- function(H=H,n=n,sigma=1){
  d <- abs(matrix(rep(1:n,n),nrow=n)-matrix(rep(1:n,n),nrow=n,byrow=T))
  autocorr <- (sigma^2/2)*((d+1)^(2*H)-2*(d^(2*H))+abs(d-1)^(2*H))
  return(autocorr)}


#' Chi-Square test
#'
#' @param Zj vector - FGN process
#' @param  H numeric - H parameter
#' @param TT numeric - Length of the time series
#' @export
Qstat <- function(Zj, H=H, TT=TT){
  OmegaInv <- solve(Omega(H=H,n=TT))
  Q <- ((t(Zj)%*%OmegaInv%*%Zj)-TT)/sqrt(2*TT)
  return(Q)
}


#' Significance - Stars
#'
#' @param x vector
#' @export
signZ <- function(x) {
  ifelse(x<qnorm(0.005)|x>qnorm(0.995),"***",
         ifelse(x<qnorm(0.025)|x>qnorm(0.975),"*"," "))}

