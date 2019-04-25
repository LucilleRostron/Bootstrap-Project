#Creating set S of subseries of length m from time series x
S<-function(x,m){
  s<-x[1:m]
  for (i in 2:(length(x)-m+1)){
    s<-rbind(s,x[i:(i+(m-1))])
  }
  return(s)
}
#Calculating bootstrap estimate of bias from original sample, l0=5
library(boot)
samplemean<-function(x){
  return(mean(x))
}
OBBrain<- tsboot(rain,samplemean, R = 100, l = 5, sim = "fixed")
biasrain<-as.numeric(mean(OBBrain$t)-OBBrain$t0)
#Using rainfall time series and m=30 to create mean, bias, SD and MSE table
rainS<-S(rain,30)
table<-function(blocklen,x,s,m){
  biass<-replicate((length(x)-m+1),0)
  for (i in 1:(length(x)-m+1)){
    OBBsamp<- tsboot(s[i,],samplemean, R = 100, l = blocklen, sim = "fixed")
    biass[i]<-as.numeric(mean(OBBsamp$t)-OBBsamp$t0)
  }
  mean1<-mean(biass)
  b<-mean1-biasrain
  stan<-sd(biass)
  mse<-(b^2)+stan^2
  return(c(mean1,b,stan,mse))
}
tableout<-function(l){
  lvalues<-matrix(0,nrow=l,ncol=5)
  for (i in 1:l){
    lvalues[i,1]<-i
    lvalues[i,2:5]<-table(l,rain,rainS,30)
  }
  return(lvalues)
}
tableout(10)