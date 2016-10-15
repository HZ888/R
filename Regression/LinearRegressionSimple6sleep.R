

data2<-"http://www.math.mcgill.ca/dstephens/Regression/Data/sleepdata.txt"
Sleep<-read.table(data2,header = T)
fit.awecm<-lm(sleep~age+work+educ+child+male, data = Sleep);fit.awecm
summary(fit.awecm)
anova(fit.awecm)

fit.m<-lm(sleep~age+work+educ+child, data = subset(Sleep, male==1))
summary(fit.m)
anova(fit.m)
fit.f<-lm(sleep~age+work+educ+child, data = subset(Sleep, male==0))
summary(fit.f)
anova(fit.f)

anova(fit.awecm)
drop1(fit.awecm, test = 'F')
fit.awem<-lm(sleep~age+work+educ+male, data = Sleep)
anova(fit.awem)

fit.wmea<-lm(sleep~work+male+educ+age, data = Sleep)
summary(fit.wmea)
anova(fit.wmea)
drop1(fit.wmea, test = 'F')
fit.wme<-lm(sleep~work+male+educ, data = Sleep)
anova(fit.wme)