#SE of mean estimator for heights dataset
meanbootrep<-replicate(200,0)
for (i in 1:200){
  bootsamp<-sample(heights,replace=TRUE,50)
  meanbootrep[i]<-mean(bootsamp)
}
hist(meanbootrep,breaks=15,xlab = "Bootstrap Replicates of Mean Height", main= "")
abline(v=mean(heights),lty=2)
sd(meanbootrep)

#SE of variance estimator for heights dataset
varbootrep<-replicate(200,0)
for (i in 1:200){
  bootsamp<-sample(heights,replace=TRUE,50)
  varbootrep[i]<-var(bootsamp)
}
hist(varbootrep,breaks=15,xlab = "Bootstrap Replicates of Height Variance", main= "")
abline(v=var(heights),lty=2)
sd(varbootrep)
