B<-read.table("C:/Users/Hao/Downloads/Winter2016/math523/assignment3/BritishDoctors.txt",header = T)
fit1<-glm(deaths~offset(log(person))+as.factor(age)+as.factor(smoking),family=poisson,B)
fit1$deviance/fit1$df.residual #overdispersion
matrix1<-xtabs(fitted(fit1)/B$person~as.factor(B$age)+as.factor(B$smoking))
matrix1[,c(1)]/matrix1[,c(2)]

matrix2<-xtabs(B$deaths/B$person~as.factor(B$age)+as.factor(B$smoking))
matrix2[,c(1)]/matrix2[,c(2)]

B$Age[B$age=="35-44"]=39.5
B$Age[B$age=="45-54"]=49.5
B$Age[B$age=="55-64"]=59.5
B$Age[B$age=="65-74"]=69.5
B$Age[B$age=="75-84"]=79.5
Inter<-as.numeric(B$smoking)*B$Age
fit2<-glm(deaths~offset(log(person))+as.factor(age)+as.factor(smoking)+Inter,family = poisson,B); summary(fit2)
xtabs(fitted(fit2)/B$person~B$Age)