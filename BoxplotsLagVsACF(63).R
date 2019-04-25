#Boxplots of various block lengths and corresponding ACF for bootstrap replicates (Figure 6.3)

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


library(boot)
mtfac1<-ac1(mtf)
mtf2.2 <- tsboot(mtf, ac1, R = 200, l = 2, sim = "fixed")
mtf2.2acf<-mtf2.2$t
mtf2.2acf<-as.vector(round(mtf2.2acf,digits = 3))
mtf2.4 <- tsboot(mtf, ac1, R = 200, l = 4, sim = "fixed")
mtf2.4acf<-mtf2.4$t
mtf2.4acf<-as.vector(round(mtf2.4acf,digits = 3))
mtf2.6 <- tsboot(mtf, ac1, R = 200, l = 6, sim = "fixed")
mtf2.6acf<-mtf2.6$t
mtf2.6acf<-as.vector(round(mtf2.6acf,digits = 3))
mtf2.10 <- tsboot(mtf, ac1, R = 200, l = 10, sim = "fixed")
mtf2.10acf<-mtf2.10$t
mtf2.10acf<-as.vector(round(mtf2.10acf,digits = 3))
mtf2.30 <- tsboot(mtf, ac1, R = 200, l = 30, sim = "fixed")
mtf2.30acf<-mtf2.30$t
mtf2.30acf<-as.vector(round(mtf2.30acf,digits = 3))
boxplot(mtf2.2acf,mtf2.4acf,mtf2.6acf,mtf2.10acf,mtf2.30acf, at=c(1,2,3,4,5), names=c("l=2","l=4","l=6","l=10","l=30"))
abline(h=mtfac1,lty=2)
        