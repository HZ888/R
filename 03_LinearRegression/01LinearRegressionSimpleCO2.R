#2015-09
#get data
rm(list=ls())
data2<-"http://www.math.mcgill.ca/dstephens/Regression/Data/temp-co2-data.txt"
CO2.data<-read.table(data2,header=TRUE)
y2<-"CO2 in parts per million"
plot(CO2.data$Year,CO2.data$CO2,xlab="Year",ylab=y2,pch=18)

##########year~ CO2
#fit linear regression
x2<-CO2.data$Year
y2.C<-CO2.data$CO2
fit.rp.CY<-lm(y2.C~x2)
summary(fit.rp.CY)

plot(CO2.data$Year,CO2.data$CO2,xlab="Year",ylab=y2,pch=18)
abline(coef(fit.rp.CY),col='blue')
title('Line of best fit for Year and CO2 Data')

#checking residuals
(RP.resids.CY<-residuals(fit.rp.CY))
plot(x2,RP.resids.CY,xlab ='Year',ylab = 'Residuals',
     pch=19,ylim = range(-2.5,2.5))
abline(h=0,lty=2)
title('Residuals vs X(Year)')

#########year ~temperature
# fit for temperature
y3<-"Temperature Anomaly (degrees C x 100)"
plot(CO2.data$Year,CO2.data$Temp,xlab="Year",ylab=y3,pch=18)

#linear regression
x3<-CO2.data$Year
y3.T<-CO2.data$Temp
fit.rp.TY<-lm(y3.T~x3)
summary(fit.rp.TY)

coef(fit.rp.TY)
plot(CO2.data$Year,CO2.data$Temp,xlab="Year",ylab=y3,pch=18)
abline(coef(fit.rp.TY),col='red')
title('Line of best fit for Temp and Year Data')

#residuals checking
(res.b<-residuals(fit.rp.TY))
plot(CO2.data$Year,res.b,xlab="Year",ylab="Residuals of Temp",pch=19)
abline(h=0,lty=2)
title('residuals of Temp vs X(Year)')

##########temperature~CO2
plot(CO2.data$CO2,CO2.data$Temp,xlab=y2,ylab=y3,pch=18)

x4<-CO2.data$CO2
y4.T<-CO2.data$Temp
fit.rp.TC<-lm(y4.T~x4)
summary(fit.rp.TC)
coef(fit.rp.TC)

plot(CO2.data$CO2,CO2.data$Temp,xlab=y2,ylab=y3,pch=18)
abline(coef(fit.rp.TC),col='red')
title('Line of best fit for Temp and CO2 Data')

(res.c<-residuals(fit.rp.TY))
plot(res.c,xlab="CO2",ylab="Residuals of Temp",pch=19)
abline(h=0,lty=2)
title('residuals of Temp against CO2')

