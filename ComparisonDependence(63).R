#Comparison of preservation of dependence for traffic fatality (mtf) data set
mtf<-mtf-mean(mtf)
acf(mtf,plot=FALSE)

ac1<-function(tsb){
  ac<-acf(tsb,plot=FALSE)
  ac$acf[2]
}
ac2<-function(tsb){
  ac<-acf(tsb,plot=FALSE)
  ac$acf[3]
}
ac3<-function(tsb){
  ac<-acf(tsb,plot=FALSE)
  ac$acf[4]
}

#Example of comparison of ACF lag 1 (same for lag and 2 just change ACF lag)

#model based with order 12
library(tseries)
mtf.ar <- ar(mtf, aic=FALSE,order.max=12)
mtf.model <- list(order = c(mtf.ar$order, 0, 0), ar = mtf.ar$ar)
mtf.res <- mtf.ar$resid[!is.na(mtf.ar$resid)]
mtf.res <- mtf.res - mean(mtf.res)

mtf.sim <- function(res,n.sim, ran.args) {
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig)+ts(arima.sim(model = ts.mod, n = n.sim,
                             rand.gen = rg1, res = as.vector(res)))
}

mtf.1 <- tsboot(mtf.res, ac1, R = 200, sim = "model", n.sim = 180,
                orig.t = FALSE, ran.gen = mtf.sim, 
                ran.args = list(ts = mtf, model = mtf.model))
mean(mtf.1$t)

# the stationary bootstrap with mean block length 6
mtf.3 <- tsboot(mtf, ac1, R = 200, l = 6, sim = "geom")
mean(mtf.3$t)

# the fixed block bootstrap with length 6
mtf.2 <- tsboot(mtf, ac1, R = 200, l = 6, sim = "fixed")
mean(mtf.2$t)

#post-blackening methods
mtf.black <- function(res, n.sim, ran.args) {
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  mean(ts.orig) + ts(arima.sim(model = ts.mod,n = n.sim,innov = res))
}

mtf.5a <- tsboot(mtf.res, ac1, R = 200, l = 6, sim = "geom",
                 n.sim = 180, orig.t = FALSE, ran.gen = mtf.black, 
                 ran.args = list(ts = mtf, model = mtf.model))
mean(mtf.5a$t)

#AR-sieve bootstrap
library(tseriesEntropy)
mtf.4<-surrogate.AR(mtf,order.max=25,fit.method = "yule-walker",200)
surr<-mtf.4$surr
ac1AR<-replicate(200,0)
for (i in 1:200){
  ac<-acf(surr[,i],plot=FALSE)
  ac1AR[i]<-ac$acf[2]
}
mean(ac1AR)
