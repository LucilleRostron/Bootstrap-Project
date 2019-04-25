#Computing bootstrap t-interval
bootsamples<-matrix(0,nrow=50,ncol=1000)
for (i in 1:1000){
  bootsamples[,i]<-sample(weights,replace=TRUE,50)
}
meanbootrep<-replicate(1000,0)
for (i in 1:1000){
  meanbootrep[i]<-mean(bootsamples[,i])
}
se<-sd(meanbootrep)

z<-replicate(1000,0)
for (i in 1:1000){
  z[i]<-(meanbootrep[i]-128.84)/se
}
sortz<-sort(z)
ztable<-rbind(c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),c(sortz[50],sortz[100],sortz[250],sortz[500],sortz[750],sortz[900],sortz[950]))
ztable

#constructing bootstrap percentile interval
bootsamples<-matrix(0,nrow=50,ncol=1000)
for (i in 1:1000){
  bootsamples[,i]<-sample(weights,replace=TRUE,50)
}
meanbootrep<-replicate(1000,0)
for (i in 1:1000){
  meanbootrep[i]<-mean(bootsamples[,i])
}
bootrepsort<-sort(meanbootrep)
bootpercentint<-c(bootrepsort[50],bootrepsort[950])
bootpercentint
