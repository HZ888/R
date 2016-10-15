


url<-"http://www.math.mcgill.ca/dstephens/Regression/Data/HeparinDataSet.csv"
heparin<-read.csv(url,header = T)
par(mfrow=c(2,2))
plot(Heparin~Sex+Weight+Age+Dose,data = heparin)
fit.2.add<-lm(Heparin~Sex+Weight+Age+Dose,data = heparin)
summary(fit.2.add)
anova(fit.2.add)

drop1(fit.2.add,test = 'F') #drop age

fit2s<-lm(Heparin~Sex+Age+Weight+Dose, data = heparin)
fit.by.step.aic<-step(fit2s,k=2)

fit2s.inter<-lm(Heparin~Sex*Age*Weight*Dose, data = heparin)
fit.by.step.aic2<-step(fit2s.inter)
summary(fit.by.step.aic2)

fit.2.twoway<-lm(Heparin~Age*(Sex+Weight+Dose),data = heparin)
summary(fit.2.twoway)
anova(fit.2.twoway)

fit.2.threeway<-lm(Heparin~Sex+Weight+Dose+Age+Age*Sex+Age*Weight+Age*Dose+Age*Sex*Weight+Age*Weight*Dose+Age*Sex*Dose,data = heparin)
summary(fit.2.threeway)
anova(fit.2.threeway)

fit.2.fourway<-lm(Heparin~Sex*Weight*Dose*Age,data = heparin)
summary(fit.2.fourway)
anova(fit.2.fourway)

fit.2.add.swd<-lm(Heparin~Sex+Weight+Dose,data = heparin)
summary(fit.2.add.swd)
anova(fit.2.add.swd)

#####check with AIC & BIC
dim(heparin)
set.seed(798)
n<-90
p<-4
library(MASS)
Sig<-rWishart(1, p+2, diag(1,p)/(p+2))[,,1]
x<-mvrnorm(n,mu=rep(0,p),Sigma = Sig)
be<-c(1,1,1,1,1,1)
xm<-cbind(rep(1,n),x,x[,1]*x[,2])
Y<-xm %*% be+rnorm(n)
x1<-x[,1]
x2<-x[,2]
x3<-x[,3]
fit0<-lm(Heparin~1, data = heparin)
fit.s<-lm(Heparin~Sex, data = heparin)
fit.a<-lm(Heparin~Age, data = heparin)
fit.w<-lm(Heparin~Weight, data = heparin)
fit.d<-lm(Heparin~Dose, data = heparin)
fit.sa<-lm(Heparin~Sex+Age, data = heparin)
fit.sw<-lm(Heparin~Sex+Weight, data = heparin)
fit.sd<-lm(Heparin~Sex+Dose, data = heparin)
fit.aw<-lm(Heparin~Age+Weight, data = heparin)
fit.ad<-lm(Heparin~Age+Dose, data = heparin)
fit.wd<-lm(Heparin~Weight+Dose, data = heparin)
fit.awd<-lm(Heparin~Age+Weight+Dose, data = heparin)
fit.swd<-lm(Heparin~Sex+Weight+Dose, data = heparin)
fit.sad<-lm(Heparin~Sex+Age+Dose, data = heparin)
fit.saw<-lm(Heparin~Sex+Age+Weight, data = heparin)
fit.sawd<-lm(Heparin~Sex+Age+Weight+Dose, data = heparin)
fit.sai<-lm(Heparin~Sex*Age, data = heparin)
fit.swi<-lm(Heparin~Sex*Weight, data = heparin)
fit.sdi<-lm(Heparin~Sex*Dose, data = heparin)
fit.awi<-lm(Heparin~Age*Weight, data = heparin)
fit.adi<-lm(Heparin~Age*Dose, data = heparin)
fit.wdi<-lm(Heparin~Weight*Dose, data = heparin)
fit.awdi<-lm(Heparin~Age*Weight*Dose, data = heparin)
fit.swdi<-lm(Heparin~Sex*Weight*Dose, data = heparin)
fit.sadi<-lm(Heparin~Sex*Age*Dose, data = heparin)
fit.sawi<-lm(Heparin~Sex*Age*Weight, data = heparin)
fit.sawdi<-lm(Heparin~Sex*Age*Weight*Dose, data = heparin)
criteria.eval<-function(fit.obj,nv,bigsig.hat){
  cvec<-rep(0,5)
  SSRes<-sum(residuals(fit.obj)^2)
  p<-length(coef(fit.obj))
  cvec[1]<-summary(fit.obj)$r.squared
  cvec[2]<-summary(fit.obj)$adj.r.squared
  cvec[3]<-SSRes/bigsig.hat^2-n+2*p
  cvec[4]<-AIC(fit.obj)
  cvec[5]<-BIC(fit.obj)
  return(cvec)
}
bigs.hat<-summary(fit.sawdi)$sigma
cvals<-matrix(0,nrow=27,ncol=5)
cvals[1,]<-criteria.eval(fit0,n,bigs.hat)
cvals[2,]<-criteria.eval(fit.s,n,bigs.hat)
cvals[3,]<-criteria.eval(fit.a,n,bigs.hat)
cvals[4,]<-criteria.eval(fit.w,n,bigs.hat)
cvals[5,]<-criteria.eval(fit.d,n,bigs.hat)
cvals[6,]<-criteria.eval(fit.sa,n,bigs.hat)
cvals[7,]<-criteria.eval(fit.sw,n,bigs.hat)
cvals[8,]<-criteria.eval(fit.sd,n,bigs.hat)
cvals[9,]<-criteria.eval(fit.aw,n,bigs.hat)
cvals[10,]<-criteria.eval(fit.ad,n,bigs.hat)
cvals[11,]<-criteria.eval(fit.wd,n,bigs.hat)
cvals[12,]<-criteria.eval(fit.awd,n,bigs.hat)
cvals[13,]<-criteria.eval(fit.swd,n,bigs.hat)
cvals[14,]<-criteria.eval(fit.sad,n,bigs.hat)
cvals[15,]<-criteria.eval(fit.saw,n,bigs.hat)
cvals[16,]<-criteria.eval(fit.sawd,n,bigs.hat)
cvals[17,]<-criteria.eval(fit.sai,n,bigs.hat)
cvals[18,]<-criteria.eval(fit.swi,n,bigs.hat)
cvals[19,]<-criteria.eval(fit.sdi,n,bigs.hat)
cvals[20,]<-criteria.eval(fit.awi,n,bigs.hat)
cvals[21,]<-criteria.eval(fit.adi,n,bigs.hat)
cvals[22,]<-criteria.eval(fit.wdi,n,bigs.hat)
cvals[23,]<-criteria.eval(fit.awdi,n,bigs.hat)
cvals[24,]<-criteria.eval(fit.swdi,n,bigs.hat)
cvals[25,]<-criteria.eval(fit.sadi,n,bigs.hat)
cvals[26,]<-criteria.eval(fit.sawi,n,bigs.hat)
cvals[27,]<-criteria.eval(fit.sawdi,n,bigs.hat)
Criteria<-data.frame(cvals)
names(Criteria)<-c('Rsq','Adj.Rsq','Cp','AIC','BIC')
rownames(Criteria)<-c('1','s','a','w','d','s+a','s+w','s+d','a+w','a+d','w+d','a+w+d','s+w+d','s+a+d','s+a+w','s+a+w+d','s*a','s*w','s*d','a*w','a*d','w*d','a*w*d','s*w*d','s*a*d','s*a*w','s*a*w*d')
round(Criteria,4)


fit.swd.inter<-lm(Heparin~Sex*Weight*Dose, data = heparin)
summary(fit.swd.inter)
anova(fit.swd.inter)

fit.2.final<-lm(Heparin~Sex+Weight+Dose+Sex:Weight, data = heparin)
summary(fit.2.final)
anova(fit.2.final)
#final model
newdata<-subset(heparin, Dose=='High')
newdata.f<-subset(newdata, Sex=='Female')
newdata.m<-subset(newdata, Sex=='Male')
f<-summary(lm(Heparin~Age+Weight, data = newdata.f))
m<-summary(lm(Heparin~Age+Weight, data = newdata.m))
B<-matrix(
  c(m$coefficients[1,1],m$coefficients[2,1],m$coefficients[3,1],f$coefficients[1,1],f$coefficients[2,1],f$coefficients[3,1]),
  nrow = 3,
  ncol=2
)
t(B)

#prediction
new.data<-data.frame(Sex="Male", Age=40, Weight=100, Dose="Low")
predict(fit.2.final,new.data)