#prediction value and interval at lag 1for ae time series
ae.fitfor<-lm(ae[4:120]~ae[3:119]+ae[2:118]+ae[1:117])
ae.fitback<-lm(ae[1:117]~ae[2:118]+ae[3:119]+ae[4:120])
backres<-ae.fitback$residuals
forwardres<-ae.fitfor$residuals
futureval1<-replicate(1000,0)
for (j in 1:1000){
  residsampfor<-as.numeric(sample(forwardres,1,replace=TRUE))
  residsampback<-sample(backres,120,replace=TRUE)
  backbootsamp<-replicate(120,0)
  backbootsamp[118:120]<-ae[118:120]
  for (i in 117:1){
    backbootsamp[i]<-80200+(0.624*backbootsamp[i+1])-(0.105*backbootsamp[i+2])+(0.185*backbootsamp[i+3])+residsampback[i]
  }
  lmbacksamp<-lm(backbootsamp[4:120]~backbootsamp[3:119]+backbootsamp[2:118]+backbootsamp[1:117])
  a<-as.numeric(lmbacksamp$coefficients[1])
  b<-as.numeric(lmbacksamp$coefficients[2])
  c<-as.numeric(lmbacksamp$coefficients[3])
  d<-as.numeric(lmbacksamp$coefficients[4])
  xt1<-a+(b*backbootsamp[120])+(c*backbootsamp[119])+(d*backbootsamp[118])+residsampfor
  futureval1[j]<-xt1
}
prediction<-mean(futureval1)
predint<-c(sort(futureval1)[25],sort(futureval1)[975])
hist(futureval1,breaks=50,main="",xlab="Bootstrap Prediction for k=1")
abline(v=sort(futureval1)[25],lty=2)
abline(v=sort(futureval1)[975],lty=2)