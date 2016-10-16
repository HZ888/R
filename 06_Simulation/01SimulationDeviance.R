nreps<-1000 #number of replications
deviance<-matrix(0,nrow = nreps,ncol=1)
set.seed(1234)
n<-100
for (irep in 1:nreps){
  i<-seq(1,100)
  xi<-i/100
  b0<-0.2
  b1<-1
  eta<-b0+b1*xi
  pi_i<-exp(eta)/(1+exp(eta))
  yi<-rbinom(n,1,pi_i)
  fit<-glm(cbind(yi,1-yi)~1+xi,family = binomial)
  deviance[irep]<-fit$deviance
}
hist(deviance)
xs<-seq(80,160,length=1000)
ys<-dchisq(xs,df=98)
lines(xs,ys*1000*4,col="blue")