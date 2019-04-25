install.packages("tseries")
library(tseries)
#Overlapping block bootstrap for ACF, lag1, m=2, for rainfall dataset (rain)
acflag1 <- function(x)
{
  xo <- c(x[,1], x[1,2])
  xm <- mean(xo)
  return(mean((x[,1]-xm)*(x[,2]-xm))/mean((xo-xm)^2))
}
acf1m2<-tsbootstrap(rain, nb=500, statistic=acflag1, m=2, b=5, type="block")
hist(acf1m2$statistic,breaks=50,main="",xlab="ACF (lag1)")
abline(v=acf1m2$orig.statistic,lty=2)

#Overlapping block bootstrap for ACF, lag1, m=1 (naive), for rainfall dataset (rain)
acf1naive<-function(tsb){
  acf<-acf(tsb,plot=FALSE)
  acf$acf[2]
}
library(boot)
acfbootnaive<-tsboot(rain,acf1naive, R=500,l=5,sim="fixed")
hist(acfbootnaive$t,breaks=50,main="",xlab="ACF (lag1)")
abline(v=acfbootnaive$t0,lty=2)

