#Bootstrap replicates of maximum value for A&E sample 2 (ae2)
samplemax<-function(x){
  return(max(x))
}
maxae<-max(ae2)

ae.ar <- ar(ae2, aic=FALSE,order.max=1)
ae.model <- list(order = c(ae.ar$order, 0, 0), ar = ae.ar$ar)
ae.res <- ae.ar$resid[!is.na(ae.ar$resid)]
ae.res <- ae.res - mean(ae.res)

ae.sim <- function(res,n.sim, ran.args) {
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig)+ts(arima.sim(model = ts.mod, n = n.sim,
                             rand.gen = rg1, res = as.vector(res)))
}

library(boot)
ae.1 <- tsboot(ae.res, samplemax, R = 200, sim = "model", n.sim = 105,
               orig.t = FALSE, ran.gen = ae.sim, 
               ran.args = list(ts = ae2, model = ae.model))
ae.1max<-as.vector(ae.1$t)

ae.2 <- tsboot(ae2, samplemax, R = 200, l = 5, sim = "fixed")
ae.2max<-as.vector(ae.2$t)

#stationary
ae.3 <- tsboot(ae2, samplemax, R = 200, l = 5, sim = "geom")
ae.3max<-as.vector(ae.3$t)

library(tseriesEntropy)
ae.4<-surrogate.AR(ae2,order.max=25,fit.method = "yule-walker",200)
surr<-ae.4$surr
maxAR<-replicate(200,0)
for (i in 1:200){
  maxAR[i]<-max(surr[,i])
}
ae.4max<-maxAR+mean(ae2)

ae.black <- function(res, n.sim, ran.args) {
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig) + ts(arima.sim(model = ts.mod,n = n.sim,innov = res))
}

ae.5a <- tsboot(ae.res, samplemax, R = 200, l = 5, sim = "fixed",
                n.sim = 105, orig.t = FALSE, ran.gen = ae.black, 
                ran.args = list(ts = ae2, model = ae.model))

ae.5amax<-as.vector(ae.5a$t)

ae.5b <- tsboot(ae.res, samplemax, R = 200, l = 5, sim = "geom",
                n.sim = 105, orig.t = FALSE, ran.gen = ae.black, 
                ran.args = list(ts = ae2, model = ae.model))

ae.5bmax<-as.vector(ae.5b$t)

boxplot(ae.1max,ae.2max,ae.3max,ae.4max,ae.5amax,ae.5bmax, at=c(1,2,3,4,5,6),
        names= c("Model Based", "Overlapping Block", "Stationary Block", "AR-Sieve",
                 "PB (Fixed)", "PB (Stationary)"))
abline(h=maxae,lty=2)
