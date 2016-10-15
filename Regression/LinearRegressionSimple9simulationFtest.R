set.seed(2332)
nreps<-5000 #number of replicates
par(mfrow=c(1,2))
ests.store<-numeric(nreps)
for(irep in c (1:nreps)){
  A<-as.factor(rep(c('m','n'),24))
  B<-as.factor(rep(c('p','q','r'),16))
  error<-rcauchy(48,0,3) #sample size is 60
  y<-rep(c(1))+error
  fit.3<-lm(y~A+B)
  summary(fit.3)
  ests.store[irep]<-summary(fit.3)$f[1]
}
hist(ests.store,freq = F,breaks=20,main=' with Cauchy error', xlab = 'F',ylab = 'density of F',ylim = c(0,1))
x=seq(0,10,length=1000)
lines(x,df(x,3,44),col='red')



ests.store2<-numeric(nreps)
for(irep in c (1:nreps)){
  A<-as.factor(rep(c('m','n'),24))
  B<-as.factor(rep(c('p','q','r'),16))
  x<-runif(48)
  error2<-rnorm(48,0,3) #sample size is 60
  y2<-rep(c(1))+error2
  fit.4<-lm(y2~A+B)
  summary(fit.4)
  ests.store2[irep]<-summary(fit.4)$f[1]
}
hist(ests.store2,freq = F,breaks=20,main=' with Normal error', xlab = 'F',ylab = 'density of F',ylim = c(0,1))
x=seq(0,10,length=1000)
lines(x,df(x,3,44),col='red')