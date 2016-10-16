#2015-11
library(multcomp)
library(MASS)
n<-1000
p<-3
set.seed(4543349)
Sig<-rWishart(1,p+5,diag(1,p)/(p+5))[,,1]
X<-mvrnorm(n,mu=rep(0,p),Sigma=Sig)
pairs(X,pch=19,cex=0.6)

be<-c(3.0,2.0,-0.5,-2.0)
A<-matrix(c(0,1,0,1,0,1,-1,1/2),byrow=T,nrow=2,ncol=4)
A

Y<-cbind(rep(1,n),X) %*% be + rnorm(n)

x1<-X[,1]
x2<-X[,2]
x3<-X[,3]

fit.tmp<-lm(Y~x1+x2+x3)
summary(glht(fit.tmp,linfct=A),Ftest())
summary(glht(fit.tmp,linfct=A))
summary(glht(fit.tmp,linfct=A),test=adjusted("none"))
summary(glht(fit.tmp,linfct=A),test=univariate())
summary(glht(fit.tmp,linfct=A),test=adjusted("bonf"))


#########################################################
library(ellipse)
par(pty='s')
plot(ellipse(fit.tmp,level=0.99),
     type='l',lty=2,xlim=range(2.70,3.10),ylim=range(1.80,2.20))
lines(ellipse(fit.tmp,level=0.95),col='blue',lwd=2)
polygon(ellipse(fit.tmp,level=0.95,which=c(1,2)),col=rgb(0,0,1,alpha=.1))
lines(ellipse(fit.tmp,level=0.90),lty=2)
points(fit.tmp$coefficients[1], fit.tmp$coefficients[2],pch=19)
abline(v=confint(fit.tmp)[1,],col='red')
abline(h=confint(fit.tmp)[2,],col='red')
dev.print(device=pdf,file='Ellipse1.pdf',height=6,width=6)

plot(ellipse(fit.tmp,level=0.99,which=c(1,3)),
     type='l',lty=2,xlim=range(2.70,3.10),ylim=range(-0.7,-0.3))
lines(ellipse(fit.tmp,level=0.95,which=c(1,3)),col='blue',lwd=2)
polygon(ellipse(fit.tmp,level=0.95,which=c(1,3)),col=rgb(0,0,1,alpha=.1))
lines(ellipse(fit.tmp,level=0.90,which=c(1,3)),lty=2)
points(fit.tmp$coefficients[1], fit.tmp$coefficients[3],pch=19)
abline(v=confint(fit.tmp)[1,],col='red')
abline(h=confint(fit.tmp)[3,],col='red')
dev.print(device=pdf,file='Ellipse2.pdf',height=6,width=6)

plot(ellipse(fit.tmp,level=0.99,which=c(2,3)),
     type='l',lty=2,xlim=range(1.80,2.20),ylim=range(-0.7,-0.3))
lines(ellipse(fit.tmp,level=0.95,which=c(2,3)),col='blue',lwd=2)
polygon(ellipse(fit.tmp,level=0.95,which=c(2,3)),col=rgb(0,0,1,alpha=.1))
lines(ellipse(fit.tmp,level=0.90,which=c(2,3)),lty=2)
points(fit.tmp$coefficients[2], fit.tmp$coefficients[3],pch=19)
abline(v=confint(fit.tmp)[2,],col='red')
abline(h=confint(fit.tmp)[3,],col='red')
dev.print(device=pdf,file='Ellipse3.pdf',height=6,width=6)
