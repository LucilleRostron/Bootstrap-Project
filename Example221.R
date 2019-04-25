
#Function to find bootstrap SE of sample mean for varying B values with x=time series, min=min rep number, max= max rep number
bootseB<-function(x,min,max){
  bootSE<-replicate((max-min+1),0)
  for (i in min:max){
    bootrep<-replicate(i,0)
    for (j in 1:i){
      bootsamp<-sample(x,length(x),replace=TRUE)
      bootrep[j]<-mean(bootsamp)
    }
    bootSE[i]<-sd(bootrep)
  }
  return(bootSE[-1])
}
#Plotting bootstrap SE for various B value and adding plug-in SE reference line
plot(bootseB(heights,1,100),type="l",xlab="Number of Bootstrap Replicates", ylab="Bootstrap Estimate of Standard Error")
abline(h=0.255,lty=2)