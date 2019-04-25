
#Visual Comparison plots for various bootstrap methods for A&E sample 2(ae2)
plot(ae2,xlab="Week Number", ylab="Number of A&E Admissions")
install.packages("boot")
library(boot)
ae<-ae2-mean(ae2)

ae.fun<-function(tsb){
  ar.fit<-ar(tsb,order.max=25)
  c(ar.fit$order,mean(tsb),tsb)
}

#model based bootstrap
ae.ar<-ar(ae)
ae.model<-list(order=c(ae.ar$order,0,0),ar=ae.ar$ar)
ae.res<-ae.ar$res[!is.na(ae.ar$resid)]
ae.res<-ae.res-mean(ae.res)

ae.sim <- function(res,n.sim, ran.args) {
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig)+ts(arima.sim(model = ts.mod, n = n.sim,
                             rand.gen = rg1, res = as.vector(res)))
}

ae.1 <- tsboot(ae.res, ae.fun, R = 100, sim = "model", n.sim = 105,
               orig.t = FALSE, ran.gen = ae.sim, 
               ran.args = list(ts = ae, model = ae.model))

#model based bootstrap plot
modelae<-ar(ae)
residae<-modelae$resid
residae<-residae[-1]
residsamp<-sample(residae,replace=TRUE,105)
residsamp
bootsamp<-matrix(0,nrow=105,ncol=100)
for (i in 1:100){
  residsamp<-sample(residae,replace=TRUE,105)
  samp<-replicate(105,0)
  for (j in 2:105){
    samp[1]<-ae[1]
    samp[j]<-ae[j-1]+residsamp[j-1]
  }
  bootsamp[,i]<-samp
}
plot(bootsamp[,1],type="l", xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")

#fixed block bootstrap with length 5
ae.2<-tsboot(ae,ae.fun,R=100,l=5,sim="fixed")

#stationary block bootstrap with mean block length 5
ae.3<-tsboot(ae,ae.fun,R=100,l=5,sim="geom")



#post blackening
ae.black <- function(res, n.sim, ran.args) {
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig) + ts(arima.sim(model = ts.mod,n = n.sim,innov = res))
}

#post blackening from fixed length 5
ae.5a<- tsboot(ae.res, ae.fun, R = 100, l = 5, sim = "fixed", n.sim = 105, 
               orig.t = FALSE, ran.gen = ae.black, 
               ran.args = list(ts = ae, model = ae.model))

ae.5b<-tsboot(ae.res, ae.fun, R = 100, l = 5, sim = "geom", n.sim = 105, 
              orig.t = FALSE, ran.gen = ae.black, 
              ran.args = list(ts = ae, model = ae.model))


plot(ae[boot.array(ae.2)[1,]],type="l",xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")
plot(ae[boot.array(ae.3)[1,]],type="l",xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")
plot(ae[boot.array(ae.5a)[1,]],type="l",xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")
plot(ae[boot.array(ae.5b)[1,]],type="l",xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")

#AR-Sieve Bootstrap
install.packages("tseriesEntropy")
library(tseriesEntropy)
ae.4<-surrogate.AR(ae2,order.max=10,fit.method = "yule-walker",100)

plot(ae.4$surr[,1],type="l",xlab="Week Number", ylab="Bootstrap sample of Admission Numbers", main="")

