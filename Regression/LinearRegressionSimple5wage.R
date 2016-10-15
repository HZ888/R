
#ANOVA
data1<-"http://www.math.mcgill.ca/dstephens/Regression/Data/wages.txt"
Wages<-read.delim(file=data1,header=TRUE)
pairs(Wages,pch=19)
fit.Complete<-lm(wage~educ+exper+I(exper^2)+tenure+female,data = Wages)
summary(fit.Complete)
anova(fit.Complete)

anova(fit.Complete)[4,4] 

fit.Complete.c<-lm(wage~educ+exper+I(exper^2)+female+tenure,data = Wages)
anova(fit.Complete.c)
#Comment: the following F statistic is significant.
anova(fit.Complete.c)[4,4]

c((summary(fit.Complete)$coef[6,1]-1.96*(summary(fit.Complete)$coef)[6,2]),(summary(fit.Complete)$coef[6,1]+1.96*(summary(fit.Complete)$coef)[6,2]))

summary(fit.Complete)$r.squared
plot(Wages$wage,residuals(fit.Complete),xlab = "wage", ylab = 'Residuals', pch=19)
abline(h=0,lty=2)
title('Residuals vs wages')