

data1<-"http://www.math.mcgill.ca/dstephens/Regression/Data/adapt.txt"
adaptation<-read.table(data1,header=TRUE)

fit.a<-lm(Y~group+educ, data = adaptation)
summary(fit.a)
drop1(fit.a,test = 'F')

table(adaptation$group,adaptation$educ)
fit.b1<-lm(Y~group+educ, data = adaptation)
fit.b2<-lm(Y~educ+group, data = adaptation)
anova(fit.b1)
anova(fit.b2)

fit.c<-lm(Y~group+educ+group:educ, data = adaptation)
summary(fit.c)
drop1(fit.c,test = 'F')

#two tables of fitted values by (cross-category)
tapply(fitted(fit.a), INDEX = list(group=adaptation$group, educ=adaptation$educ) , mean)
tapply(fitted(fit.c), INDEX = list(group=adaptation$group, educ=adaptation$educ), mean)

