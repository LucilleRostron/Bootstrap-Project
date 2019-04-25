#SE for mean using parametric bootstrap on heights dataset
meanparamboot<-replicate(200,0)
for (i in 1:200){
  samp<-rnorm(50,mean=68.05,sd=1.82)
  meanparamboot[i]<-mean(samp)
}
hist(meanparamboot,breaks=15,xlab = "Bootstrap Replicates of Mean Height", main= "")
abline(v=68.05,lty=2)
sd(meanparamboot)