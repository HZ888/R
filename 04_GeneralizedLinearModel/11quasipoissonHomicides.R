f2.qp<-glm(Freq~race+count,family = quasipoisson)
summary(f2.qp)
#comparing AIC
c(f2$aic,f2.nb$aic,f2.qp$aic)
c(f2$deviance,f2.nb$deviance,f2.qp$deviance)
#Chi-square comparison
anova(f2,f2.nb,f2.qp,test="Chi")
#For the grouped data
round(cbind(race,count,fitted(f2),fitted(f2.nb),fitted(f2.qp)))
#Among all three models, Negative Binomial model yields the lowest AIC, the lowest deviance, and the largest standarded error to account for over dispersion. \\