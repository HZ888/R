library(MASS)
fit5<-polr(as.factor(health)~insure)
summary(fit5)

#compare with the cumulative fit
cbind(coef(fit0),coef(fit2),coef(fit3),coef(fit4))

