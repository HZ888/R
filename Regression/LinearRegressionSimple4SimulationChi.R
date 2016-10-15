
rm(list=ls())
(pdf<-integrate(function(a){ #pdf check=1
  1/sqrt(2*pi)*exp(1/2*(a-exp(a)))
},lower=-Inf,upper= Inf)$value)
s.mean<-(lambda<-integrate(function(a){ #mean
  a*1/sqrt(2*pi)*exp(1/2*(a-exp(a)))
},lower=-Inf,upper=Inf)$value); s.mean
(e.square<-integrate(function(a){ #variance
  a^2*1/sqrt(2*pi)*exp(1/2*(a-exp(a)))
},lower = -Inf,upper = Inf)$value)
(s.var<-e.square-s.mean^2)

nreps<-5000 # of replicates
ests.store<-matrix(0,nrow=nreps,ncol = 3)
tstat.store<-matrix(0,nrow=nreps,ncol=2)
n<-20 # Fix the sample size 20
set.seed(3323) #Reset the random number generator
x<-sort(runif(n,0,10)) #generate design points
be0<-2; be1<-0.5; sig<-0.5 #set the parameters
for(irep in 1:nreps) { #Generate the data and store the results
  y<-be0+be1*x  #compute the vector of means
  z<-log(rchisq(n,1,ncp=0))-s.mean
  y<-y+z*sig
  fit3<-lm(y~x)
  be.hat<-coef(fit3)
  sig.hat<-summary(fit3)$sigma #get the residual standard error. n-2 df. SSres
  ttab<-summary(fit3)$coef #beta0 and beta1
  ests.store[irep,]<-c(be.hat, sig.hat^2)
  tstat.store[irep,]<-(ttab[,1]-c(be0,be1))/ttab[,2]
}
#check the mean and variance of estimators
apply(ests.store,2,mean)
apply(ests.store,2,var)