
ds.log<-data.frame(expand.grid(
  Gender=factor(c("M","F"),levels = c("M","F")),
  Age=factor(c("13 or less","14-18","19 and above"),levels=c("13 or less","14-18","19 and above"))),
  StatusF=c(3238,2448,7193,8769,4908,3361),
  StatusM=c(33,38,63,108,157,159))
head(ds.log)
G.log<-as.factor(ds.log$Gender)
A.log<-as.factor(ds.log$Age)
m1<-glm(cbind(StatusM,StatusF)~1,family = binomial,ds.log)
m2<-glm(cbind(StatusM,StatusF)~1+G.log,family = binomial,ds.log)
m3<-glm(cbind(StatusM,StatusF)~1+A.log,family = binomial,ds.log)
m4<-glm(cbind(StatusM,StatusF)~1+G.log+A.log,family = binomial,ds.log)
m5<-glm(cbind(StatusM,StatusF)~1+G.log*A.log,family = binomial,ds.log)
c(m1$aic,m2$aic,m3$aic,m4$aic,m5$aic)

summary(m4)

coef.m4<-summary(m4)$coefficients
#Odds ratio and 95% Confidence interval of Gender and above vs Status (at any level of Age)
exp(coef.m4[2,1])
c(exp(coef.m4[2,1]-1.96*coef.m4[2,2]),exp(coef.m4[2,1]+1.96*coef.m4[2,2]))

#Odds ratio and 95% Confidence interval of Age0-13 and 14-18 vs Status (at any level of Gender)
exp(coef.m4[3,1])
c(exp(coef.m4[3,1]-1.96*coef.m4[3,2]),exp(coef.m4[3,1]+1.96*coef.m4[3,2]))

#Odds ratio and 95% Confidence interval of Age0-13 and 19 above vs Status (at any level of Gender)
exp(coef.m4[4,1])
c(exp(coef.m4[4,1]-1.96*coef.m4[4,2]),exp(coef.m4[4,1]+1.96*coef.m4[4,2]))

#Fisher information
m4<-glm(cbind(StatusM,StatusF)~1+G.log+A.log,family = binomial,ds.log,x=T)
X4<-m4$x
W4<-diag(fitted(m4))
var.cov4<-solve(t(X4)%*%W4%*%X4)

#Odds ratio and 95% Confidence interval of Age0-13 and 19 above vs Status (at any level of Gender)
exp(coef.m4[4,1]-coef.m4[3,1])
sd.e.m4<-sqrt(var.cov4[3,3]+var.cov4[4,4]-var.cov4[3,4]-var.cov4[4,3])
c(exp(coef.m4[4,1]-coef.m4[3,1]-1.96*sd.e.m4),exp(coef.m4[4,1]-coef.m4[3,1]+1.96*sd.e.m4))