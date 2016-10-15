
data1<-"http://www.math.mcgill.ca/dstephens/Regression/Data/AirPollution.txt"
mortality.rate<-read.table(data1,header=TRUE)

x<-mortality.rate$X12
y<-mortality.rate$Y
fit.rp12<-lm(y~x)
summary(fit.rp12)

par(mfrow=c(1,2))
plot(mortality.rate$X12, mortality.rate$Y, xlab = "X12: Relative pollution potential of hydrocarbons",pch=20)
abline(coef(fit.rp12),col='red')
title('Line of best fit for X12')
RP.residsX12<-residuals(fit.rp12)
plot(x,RP.residsX12,xlab = 'X12',ylab = 'Residuals',pch=19)
abline(h=0,lty=2)
title('Residuals vs X12')

x13<-mortality.rate$X13
y<-mortality.rate$Y
fit.rp13<-lm(y~x13)
summary(fit.rp13)

par(mfrow=c(1,2))
plot(x13,y,xlab="X13: Relative pollution potential of oxides of nitrogen",pch=18)
abline(coef(fit.rp13),col="red")
title('Line of best fit for X13')
RP.residsX13<-residuals(fit.rp13)
plot(x13,RP.residsX13,xlab = 'X13',ylab = 'Residuals',pch=19)
abline(h=0,lty=2)
title('Residuals vs X13')

x14<-mortality.rate$X14
y<-mortality.rate$Y
fit.rp14<-lm(y~x14)
summary(fit.rp14)

par(mfrow=c(1,2))
plot(x14,y,xlab = "X14:  Relative pollution potential of sulphur dioxide",pch=20)
abline(coef(fit.rp14),col="red")
title('Line of best fit for X14')
RP.residsX14<-residuals(fit.rp14)
plot(x14,RP.residsX14, xlab = 'X14', ylab = 'Residuals', pch=19)
abline(h=0,lty=2)
title('Residuals vs X14')

#ANOVA

x4<-mortality.rate$X4
fit.rp4<-lm(y~x4)
summary(fit.rp4)
anova(fit.rp4)
sum(anova(fit.rp4))

x6<-mortality.rate$X6
fit.rp6<-lm(y~x6)
summary(fit.rp6)
anova(fit.rp6)
sum(anova(fit.rp6))