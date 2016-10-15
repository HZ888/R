
rm(list=ls())
my.url<-"http://www.math.mcgill.ca/dstephens/Regression/Data/NetDomesticImmig.txt"
USIR<-read.delim(file=my.url,sep="\t",head=T)
r<-rep(0,10)
for (i in 3:12){
  fit<-lm(USIR[,2]~USIR[,i])
  r[i-2]<-summary(fit)$r.squared
}
R.squared<-data.frame(R.squared=r,Predictors=colnames(USIR[,3:12]))
Ordered.R2<-R.squared[order(R.squared[,1],decreasing = T),];Ordered.R2

#plot
y<-USIR$NDIR
x7<-USIR$Taxes
fit.x7<-lm(y~x7);summary(fit.x7)
par(mfrow=c(1,2))
plot(USIR$Taxes,USIR$NDIR,xlab="Taxes", ylab = "NDIR",pch=20)
abline(coef(lm(USIR$NDIR~USIR$Taxes)),col='blue')
title('Line of best fit for NDIR and Taxes')
RP.residsx7<-residuals(lm(USIR$NDIR~USIR$Taxes))
plot(USIR$Taxes,RP.residsx7,xlab ='Taxes',ylab = 'Residuals',pch=19)
abline(h=0,lty=2)
title('Residuals vs Taxes')