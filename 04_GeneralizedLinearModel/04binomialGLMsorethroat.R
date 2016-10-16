data<-read.table("/Users/Hao/Downloads/Winter2016/math523/assignment2/SoreThroat.dat",header = TRUE)
#plotting the data
attach(data)
fit.binom<-glm(cbind(Y,1-Y)~1+D,family =binomial,x=TRUE)
summary(fit.binom)
beta<-fit.binom$coefficient[2]
sd.error<-summary(fit.binom)$coef[2,2]
c(beta-1.96*sd.error,beta+1.96*sd.error) #The confidence Interval

#likelihood ratio test
pchisq(deviance(fit.binom),df=df.residual(fit.binom),lower.tail = FALSE)
fit.1<-glm(cbind(Y,1-Y)~1,family = binomial)
anova(fit.1,fit.binom,test="Chi")

#deviance comparison
deviance(fit.binom)
deviance(fit.1)
fit.2<-glm(cbind(Y,1-Y)~1+D+as.factor(T),family = binomial);deviance(fit.2)
fit.3<-glm(cbind(Y,1-Y)~1+D+as.factor(T)+D:as.factor(T),family = binomial);deviance(fit.3)
fit.4<-glm(cbind(Y,1-Y)~1+D,family = binomial(link=probit));deviance(fit.4)
fit.5<-glm(cbind(Y,1-Y)~1+as.factor(T),family = binomial);deviance(fit.5)
anova(fit.2,fit.3,test="Chi")
anova(fit.binom,fit.2,test = "Chi")
anova(fit.binom,fit.4,test = "Chi")

#check for deviance
par(mfrow=c(1,2))
plot(residuals(fit.2,"deviance"),fitted(fit.2))