#accuracy of parameter estimates for traffic fatality data set (mtf)
mtfc<-mtf-mean(mtf)
mtf.fit1<-ar(mtfc,order.max = 1,aic = FALSE,method="yule-walker")
resid<-mtf.fit1$resid
coefrep.fit1<-replicate(1000,0)
for (j in 1:1000){
  ressample<-replicate(180,0)
  ressample[1]<-resid[1]
  ressample[2:180]<-sample(resid[2:180],179,replace=TRUE)
  bootsamp<-replicate(180,0)
  bootsamp[1]<-mtfc[1]
  for (i in 2:180){
    bootsamp[i]<-(0.731*bootsamp[i-1])+ressample[i]
  }
  bootsamp<-bootsamp-mean(bootsamp)
  arboot<-ar(bootsamp, order.max= 1, aic= FALSE)
  coefrep.fit1[j]<-arboot$ar[1]
}
#Percentile CI
sortrep<-sort(coefrep.fit1)
bootpercentint<-c(sortrep[25],sortrep[975])
bootpercentint
#Histogram
hist(coefrep.fit1, main = "", xlab= "Bootstrap Replicate of Coefficient in AR(1)",breaks=50)
sd(coefrep.fit1)
abline(v=sort(coefrep.fit1)[25],lty=2)
abline(v=sort(coefrep.fit1)[975],lty=2)
abline(v=0.731,lty=3)

#Accuracy of coefficients with AR 2 model fitted
mtf.fit2<-ar(mtfc,order.max = 2,aic = FALSE,method="yule-walker")
mtf.fit2
resid2<-mtf.fit2$resid
coefrep1.fit2<-replicate(1000,0)
coefrep2.fit2<-replicate(1000,0)
for (j in 1:1000){
  ressample<-replicate(180,0)
  ressample[1]<-resid2[1]
  ressample[2]<-resid2[2]
  ressample[3:180]<-sample(resid2[3:180],178,replace=TRUE)
  bootsamp<-replicate(180,0)
  bootsamp[1]<-mtfc[1]
  bootsamp[2]<-mtfc[2]
  for (i in 3:180){
    bootsamp[i]<-(0.765*bootsamp[i-1])-(0.047*bootsamp[i-2])+ressample[i]
  }
  bootsamp<-bootsamp-mean(bootsamp)
  arboot<-ar(bootsamp, order.max= 2, aic= FALSE)
  coefrep1.fit2[j]<-arboot$ar[1]
  coefrep2.fit2[j]<-arboot$ar[2]
}
#Histogram plots
hist(coefrep1.fit2,breaks=50,main="", xlab= "Bootstrap Replicates of First Coefficient in AR(2)")
abline(v=sort(coefrep1.fit2)[25],lty=2)
abline(v=sort(coefrep1.fit2)[975],lty=2)
abline(v=0.765,lty=3)
hist(coefrep2.fit2,breaks=50,main="", xlab= "Bootstrap Replicates of Second Coefficient in AR(2)")
abline(v=sort(coefrep2.fit2)[25],lty=2)
abline(v=sort(coefrep2.fit2)[975],lty=2)
abline(v=-0.047,lty=3)
#QQ plots
sd(coefrep2.fit2)
qqnorm(coefrep1.fit2, main="")
qqline(coefrep1.fit2)
qqnorm(coefrep2.fit2, main="")
qqline(coefrep2.fit2)
