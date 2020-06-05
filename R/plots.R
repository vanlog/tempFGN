#' Temperature plot
#'
#' @param Xj vector - Temperature process
#' @param Year vector - Years of recorded data
#' @param main string - Title, default=NULL
#' @param cex.axis numeric, default=0.8
#' @param cex.lab numeric, default=0.8
#' @param cex.main numeric, default=1
#' @param break.val logical - Defines if there is a breaking value, after which the data
#' should be represented with a different linetype. Defaul=F
#' @export
temperaturePlot <- function(Xj=Xj, Year=Year, main=NULL, cex.axis=0.8,
                            cex.lab=0.8, cex.main=1, break.val=F){
  par(bty="l",cex.axis=cex.axis,cex.lab=cex.lab,cex.main=cex.main,
      font.lab=2,las=1)
  plot(Year,Xj,type="n",main=main,ylab="",xlab="",
       panel.first=abline(v=axTicks(1),h=axTicks(2),col="grey90"))
  if(break.val==F){
    lines(Year,Xj,col=4,lwd=2)}
  else {
    nb <- which(Year==break.val)
    ny <- length(Year)
    lines(Year[1:nb],Xj[1:nb],col=4,lwd=2)
    lines(Year[nb:ny],Xj[nb:ny],lwd=1,col=4)}
  abline(h=mean(Xj,na.rm=T),col=2,lty=2)}



#' Compute theoretical autocorrelation
#'
#' @param d numeric
#' @param H numeric, H parameter value
th.autocorr <- function(d,H) {
  0.5*((d+1)^(2*H)-2*(d)^(2*H)+abs(d-1)^(2*H))}



#' Autocorrelation plot
#'
#' @param Xj vector - FGN process
#' @param maxd numeric - Numbers of lags to be plotted
#' @param lambda numeric, default=0.5
#' @param H logical - Define if a theoretical autocorrelation is plotted and how .
#' The possible options for H are:
#' H=F           No theoretical autocorrelation is plotted
#' H=x           with x=number. The autocorrelation is computed with H=x
#' H="cfH"       H is estimated via ch. function estimator, and then used to plot the th. autocorr.
#' @param main string - Title, default=NULL
#' @param cex.axis numeric, default=0.8
#' @param cex.lab numeric, default=0.8
#' @param cex.main numeric, default=1
#' @param cex.dots numeric, default=1
#' @param ylim vector
#' @param labels logical. If the numerical values of the empirical autocorrelation have to be plotted
#' @param correct logical. If the computation of the autocorrelation has to be corrected
#' @export
autocorrPlot <- function(Xj, maxd=10, H=F, cex.axis=0.8, lambda=0.5,
                         main=NULL, cex.lab=0.8, cex.main=1, cex.dots=1,
                         ylim=c(min(min(rho),-0.5),1), labels=T,
                         correct=F){
  Xj <- Xj[which(Xj!="NA")]  # Remove missing data
  Tj <- length(Xj)
  if (H!=F) {
    if (H=="cfH") H <- estim.cf.H(Yj=Xj, maxd=maxd, lambda=lambda, FBM=F)}
  rho <- NULL    # Future vector of autocorrelation
  for (i in 1:maxd){
    rho <- c(rho,cor(Xj[(i+1):Tj],Xj[1:(Tj-i)]))}
  if (correct==T) {
    rho <- (rho+Tj^(2*H-2))/(1+Tj^(2*H-2))}
  par(bty="l",cex.axis=cex.axis,cex.lab=cex.lab,cex.main=cex.main,
      font.lab=2,las=1)
  p <- plot(1:maxd, rho, pch=16, type="o", lwd=2, cex=cex.dots,
            col=4, xlab="Lag", ylab=expression(rho),
            ylim=ylim,
            main=paste(main,"Autocorrelation"),
            panel.first=abline(v=axTicks(1), h=axTicks(2), col="grey90"))
  abline(h=0,lty=2,col=4)
  if(labels==T) {text(1:maxd,(rho+0.15),round(rho,digits=2),col=4,cex=cex.lab)}
  if(H!=F){
    thCorr <- th.autocorr(1:maxd,H)
    lines(thCorr,col=2,lwd=2)
    legend("bottomright",c("Empirical autocorrelation",
                           paste("FGN theorical autocorrelation, H=",
                                 round(H,2))),
           lwd=c(2,2),col=c(4,2),cex=cex.lab,bty="n")}
  return(list(plot=p, rho=rho))
}



#' Blue-red plot
#'
#' @param Zj vector - Temperature process, usually normalized
#' @param Time vector - Time of recorded data
#' @param mean numeric - Mean value, default=0
#' @param ic vector - Confidence inteval, default=c(-3,3)
#' @param main string - Title, default=NULL
#' @param cex.axis numeric, default=0.8
#' @param cex.lab numeric, default=0.8
#' @param cex.main numeric, default=1
#' @param break.val logical - Defines if there is a breaking value, after which the data
#' should be represented with a different color. Defaul=F
#' @export
blueRedPlot <- function(Zj=Zj, Time=Time, mean=0, ic=c(-3,3),
                        main=NULL, cex.axis=0.8,
                        cex.lab=0.8, cex.main=1, break.val=F){
  blueRed <- ifelse(Zj>mean,"red","blue")
  if (break.val!=F) {
    blueRed <- replace(blueRed,which(Time>break.val&Zj>mean),
                       "coral")
    blueRed <- replace(blueRed,which(Time>break.val&Zj<mean),
                       "steelblue2")}
  par(bty="l",cex.axis=cex.axis,cex.lab=cex.lab,cex.main=cex.main,
      font.lab=2,las=1)
  plot(Time,Zj,type="h",ylab="",xlab="",col=blueRed,lwd=3,
       main=paste(main,"Deviation from the mean"),
       ylim=c(min(min(Zj,na.rm=T),ic[1]),max(max(Zj,na.rm=T),ic[2])),
       panel.first=abline(v=axTicks(1),h=axTicks(2),col="grey90"))
  abline(h=c(ic[1],ic[2]),lty=2,col=c(4,2))
  abline(h=0,lwd=3)}


