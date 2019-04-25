#Hypothesis test for Dow-Jones data set (dj)
djar1<-ar(dj,order.max=1,aic=FALSE)
res<-djar1$resid
res<-res[-1]

arbootsim<-matrix(0,nrow=160,ncol=500)
for (j in 1:500){
  ressamp<-sample(res,replace=TRUE,160)
  boot<-replicate(160,0)
  boot[1]<-dj[1]
  for (i in 2:160){
    boot[i]<-boot[i-1]+ressamp[i]
  }
  arbootsim[,j]<-boot
}

tstatbootrep<-replicate(500,0)
for (i in 1:500){
  arboot<-ar(arbootsim[,i],order.max=1,aic=FALSE)
  T<-(1-arboot$ar)/((arboot$var)**0.5)
  tstatbootrep[i]<-T
}

tstatbootrepsort<-sort(tstatbootrep)
hist(tstatbootrep,breaks=50, main= "", xlab="t-statistic Bootstrap Replicates")
abline(v=tstatbootrepsort[475],lty=2)
abline(v=tstatbootrepsort[25],lty=2)
#abline of t-statistic under dj sample
abline(v=0.00322,lty=3)
