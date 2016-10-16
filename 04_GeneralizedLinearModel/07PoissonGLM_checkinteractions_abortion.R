#entering the data
Table<-data.frame(expand.grid(
  A=factor(c("Yes","No"),levels=c("No","Yes")), #abortion should be legal for any reason
  E=factor(c("Yes","No"),levels=c("No","Yes")), #willingness to pay higher tax to help the environment
  P=factor(c("Yes","No"),levels=c("No","Yes"))), #political party identification
  count=c(50,32,28,50,5,27,30,61))
attach(Table)
#check for main effects
mod1<-glm(count~A+E+P,family = poisson) # mutual independence
mod2<-glm(count~A+E+P+A:E,family = poisson) #joint independence, P is independent of (A, E)
mod3<-glm(count~A+E+P+A:P,family = poisson)
summary(mod3) 

#check for interactions
#two ways interactions
Table<-data.frame(expand.grid(
  A=factor(c("Yes","No"),levels=c("No","Yes")), #abortion should be legal for any reason
  E=factor(c("Yes","No"),levels=c("No","Yes")), #willingness to pay higher tax to help the environment
  P=factor(c("Yes","No"),levels=c("No","Yes"))), #political party identification
  count=c(50,32,28,50,5,27,30,61))
attach(Table)
mod1<-glm(count~A+E+P,family = poisson) # mutual independence
mod2<-glm(count~A+E+P+A:E,family = poisson) #joint independence, P is independent of (A, E)
mod3<-glm(count~A+E+P+A:P,family = poisson)
summary(mod3) 

mod4<-glm(count~A+E+P+P:E,family = poisson)
mod5<-glm(count~A+E+P+A:P+P:E,family = poisson)
# conditional independence, condition on P, A and E are independent
mod6<-glm(count~A+E+P+A:E+P:E,family = poisson)
mod7<-glm(count~A+E+P+A:P+A:E,family = poisson)
mod8<-glm(count~A+E+P+A:P+A:E+P:E,family = poisson) #homogeneous association
mod9<-glm(count~A*E*P,family = poisson,Table)

c(mod1$aic,mod2$aic,mod3$aic,mod4$aic,mod5$aic,mod6$aic,mod7$aic,mod8$aic,mod9$aic)
c(mod1$deviance,mod2$deviance,mod3$deviance,mod4$deviance,mod5$deviance,mod6$deviance,mod7$deviance,mod8$deviance,mod9$deviance)

anova(mod1,mod2,test = "Chi")

#Meanwhile, we may also check the models by applying Chi-square test to check if one model is significantly different from the other model.\\
anova(mod1,mod2,test = "Chi")
#mod2 is significantly different from mod1. mod2 has a lower deviance.
anova(mod2,mod3,mod4,test = "Chi")
#mod2,or 3 or 4 are not significantly different. choose the lowerest aic and deviance, so choose mod2.
anova(mod2,mod5,test = "Chi")
#mod5 is significantly different from mod2. Thus choose mod5 from this comparison.
anova(mod5,mod6,test = "Chi")
anova(mod6,mod7,test = "Chi")
#mod5 and mod6 are not significantly different. And mod6 is not significantly different from mod7. choose mod6, which has a lower aic and deviance.\\
anova(mod6,mod8,test = "Chi")
summary(mod6);summary(mod8)
#mod6 and mod8 are significantly different. Choose mod8 with one more interaction included.
anova(mod8,mod9,test = "Chi");summary(mod9)
#mod8 and mod9 are significantly different. Thus we arrive at the saturated mod9 as the final model.

#Compare the fitted counts.
round(cbind(A,E,P,fitted(mod1),fitted(mod2),fitted(mod3),fitted(mod4),fitted(mod5),fitted(mod6),fitted(mod7),fitted(mod8),fitted(mod9)),1)
#The saturated model yields the fitted value that is closest to the observed value. 

#Estimated Odds Ratio for mod9.
#Computing the confidence intervals for the odds ratios
mod9<-glm(count~A*E*P,family = poisson,x=TRUE)
#Compute the inverse of the fisher information
X<-mod9$x
W<-diag(fitted(mod9))
A<-t(X)%*%W%*%X
var.cov<-solve(A) # fisher inverse, variance-covariance matrix
Std.E<-as.numeric(sqrt(diag(solve(t(X)%*%W%*%X))))
Ests<-as.numeric(coef(mod9))
#CI for the OR of E vs P (at the level of A)
Odds.Ratio.EP<-exp(Ests[7]); Odds.Ratio.EP  #Odds Ratio for E vs P at A=0
lb.EP<-Ests[7]-1.96*Std.E[7]
up.EP<-Ests[7]+1.96*Std.E[7]
(CI.EP<-exp(c(lb.EP,up.EP)))# CI for the OR of E vs P (at the level of A as 0)
#CI for the OR of E vs P (at the level of A=1)
Odds.Ratio.EP.A<-exp(Ests[7]+Ests[8]); Odds.Ratio.EP.A #Odds Ratio of E vs P at A=1
Var.EP.A<-var.cov[7,7]+var.cov[7,8]+var.cov[8,7]+var.cov[8,8]
Std.E.EP.A<-sqrt(Var.EP.A)
lb.EP.A<-Ests[7]+Ests[8]-1.96*Std.E.EP.A
up.EP.A<-Ests[7]+Ests[8]+1.96*Std.E.EP.A
(CI.EP.A<-exp(c(lb.EP.A,up.EP.A)))# CI for the OR of E vs P (at the level of A as 1)

#Computing the confidence interval for the odds ratio of A vs E (at the level of P)
Odds.Ratio.AE<-exp(Ests[5]); Odds.Ratio.AE  #Odds Ratio for A vs E at P=0
lb.AE<-Ests[5]-1.96*Std.E[5]
up.AE<-Ests[5]+1.96*Std.E[5]
(CI.AE<-exp(c(lb.AE,up.AE)))# CI for the OR of A vs E (at the level of P=0)
#CI for the OR of A vs E (at any level of P = 1)
Odds.Ratio.AE.P<-exp(Ests[5]+Ests[8]);Odds.Ratio.AE.P #OR for A vs E at P=1
Var.AE.P<-var.cov[5,5]+var.cov[5,8]+var.cov[8,5]+var.cov[8,8]
Std.E.AE.P<-sqrt(Var.AE.P)
lb.AE.P<-Ests[5]+Ests[8]-1.96*Std.E.AE.P
up.AE.P<-Ests[5]+Ests[8]+1.96*Std.E.AE.P
(CI.AE<-exp(c(lb.AE.P,up.AE.P)))# CI for the OR of A vs E (at any level of P = 1)

    
#Computing the confidence interval for the odds ratio of A vs P (at the level of E)
Odds.Ratio.AP<-exp(Ests[6]); Odds.Ratio.AP #Odds Ratio for A vs P at E=0
lb.AP<-Ests[6]-1.96*Std.E[6]
up.AP<-Ests[6]+1.96*Std.E[6]
(CI.AP<-exp(c(lb.AP,up.AP)))# CI for the OR of A vs P (at the level of E as 0)
## CI for the OR of A vs P (at the level of E =1)
Odds.Ratio.AP.E<-exp(Ests[6]+Ests[8]);Odds.Ratio.AP.E #OR for A vs P at E=1
Var.AP.E<-var.cov[6,6]+var.cov[6,8]+var.cov[8,6]+var.cov[8,8]
Std.E.AP.E<-sqrt(Var.AP.E)
lb.AP.E<-Ests[6]+Ests[8]-1.96*Std.E.AP.E
up.AP.E<-Ests[6]+Ests[8]+1.96*Std.E.AP.E
(CI.AP.E<-exp(c(lb.AP.E,up.AP.E)))# CI for the OR of A vs P (at the level of E =1)
