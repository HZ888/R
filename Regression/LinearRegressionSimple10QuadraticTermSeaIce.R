#2015-09
#linear regression with quadratic term

rm(list=ls())
data1<-"http://www.math.mcgill.ca/dstephens/Regression/Data/sea-ice-data.txt"
SeaIce.data<-read.table(data1,header=TRUE)
yl<-"September Arctic Sea Ice Extent (1,000,000 sq km)"
x1<-"Year"
plot(SeaIce.data,ylab=yl,pch=18,ylim=range(0,8))
x<-SeaIce.data$Year
y<-SeaIce.data$SeaIce
abline(coef(lm(y~x)),col='red')
#Create a new variable
xstar<-(SeaIce.data$Year-1979)^2
newfit<-lm(y~xstar,data = SeaIce.data)
summary(newfit)
coef(newfit)
#Quadratic term
lines(SeaIce.data$Year,fitted(lm(SeaIce.data$SeaIce~xstar)),col='blue')
title('Line of best fit for SeaIce data vs (x-1979)^2')
#Simple Linear Regression: Residuals against x
(x.RP.resids<-residuals(lm(y~x)))
#Produce plots
plot(x,x.RP.resids,xlab = 'X(Year)',ylab = 'Residuals',pch=19,ylim=range(-2.5,2.5))
abline(h=0,lty=2)
title('Residuals vs (x-1979)^2')
#Simple Linear Regression: Residuals against quadratic term
newfit.RP.resids<-residuals(newfit)
#Produce plots
plot(x,newfit.RP.resids,xlab = '(x-1979)^2',ylab = 'Residuals',pch=19,ylim=range(-2.5,2.5))
abline(h=0,lty=2)
title('Residuals vs (x-1979)^2')
