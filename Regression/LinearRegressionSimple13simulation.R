#2015sept-30

#######################################
nreps<-10000

ss.store<-rep(0,nreps)
for(irep in 1:nreps){

n<-20
x<-sort(runif(n,0,10))
y<-2+0.5*x
#plot(x,y,type='1')

y<-y+rnorm(n)*0.1
#points(x,y,pch=19)

fit.misspec<-lm(y~x)

ss.res<-sum(residuals(fit.misspec)^2)
ss.store[irep]<-ss.res
}

hist(ss.store)
mean(ss.store)
#######################################


abline(coef(fit.misspec),col='red')
summary(fit.misspec)

# the real dependence on x is .5, the dependence overall on x is 3.06

plot(x,residuals(fit.misspec),pch=19)
abline(h=0,lty=2,col='red')
#pattern is indicative of misspecification. not the true data generated. the number is centered at zero.

mean(residuals(fit.misspec))
mean(x*residuals(fit.misspec))
mean(y*residuals(fit.misspec))
mean(fitted(fit.misspec)*residuals(fit.misspec))

plot