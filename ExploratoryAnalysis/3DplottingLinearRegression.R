#2015 -oct-28
library(MPV) #load the textbook data sets library

Delivery<-p8.3

x1<-Delivery$x1
x2<-Delivery$x2
y<-Delivery$y
par(mfrow=c(1,2))#create a matrix of m row and n column
plot(x1,y,pch=19)
plot(x2,y,pch=19)
par(mfrow=c(1,1))
pairs(cbind(y,x1,x2),pch=19)
dev.print(device=postscript,file='DeliveryData.ps')

cor(cbind(y,x1,x2))#correlation

library(car)
scatter3d(y~x1+x2+x1*x2,data=Delivery)

(fit.Del2l<-lm(y~x2+x1,data = Delivery))
summary(fit.Del2l)

#3 D plotting
library(scatterplot3d) 
s3d <-scatterplot3d(x1,x2,y, pch=16, grid=FALSE,
                    main="Plane of best fit",angle=20)
s3d$plane3d(fit.Del2l,col='black')
dev.print(device=postscript,file='DeliveryData-Plane.ps')

s3d <- scatterplot3d(cbind(x1,x2,residuals(fit.Del2l)),lab.z=2, 
                     pch=16,scale.y=0.7,highlight.3d=TRUE, main="Residuals")
s3d$plane3d(0,0,0, "solid", col="grey")

anova(fit.Del2l)
par(mfrow=c(2,2),mar=c(2, 4, 4, 2) + 0.1)
plot(x1,residuals(fit.Del2l),pch=19,ylim=range(-10,10),ylab='Residuals')
abline(h=0,lty=2);title('Residuals vs x1')
plot(x2,residuals(fit.Del2l),pch=19,ylim=range(-10,10),ylab='Residuals')
abline(h=0,lty=2);title('Residuals vs x2')

plot(fitted(fit.Del2l),residuals(fit.Del2l),pch=19,ylim=range(-10,10),ylab='Residuals',xlab=expression(hat(y)[i]))
abline(h=0,lty=2);title('Residuals vs fitted values')
dev.print(device=postscript,file='DeliveryData-Residuals.ps',height=9,width=11)
#create a variable with only x1+x2
fit.Del21<-lm(y~x2+x1,data=Delivery)
summary(fit.Del21) #gives same regression results
anova(fit.Del21)  #gives different ANOVA table !

fit.Del1<-lm(y~x1,data=Delivery)
summary(fit.Del1)
anova(fit.Del1)
fit.Del2<-lm(y~x2,data=Delivery)
summary(fit.Del2)
anova(fit.Del2)

anova(fit.Del21)
# beta one for one variable is different from beta one for another variable
# adding modification for the slope parameter
fit.Del12i<-lm(y~x1+x2+x1:x2,data=Delivery)
summary(fit.Del12i)

#* is for interaction notation
fit.Del12i2<-lm(y~x1*x2,data=Delivery)
summary(fit.Del12i2)

################
Delivery<-p8.3

x1<-Delivery$x1
x2<-Delivery$x2
y<-Delivery$y

#Model 3 X1+X2
fit.Del12<-lm(y~x1+x2,data=Delivery)
summary(fit.Del12)
anova(fit.Del12)

#Model 2 X1
fit.Del1<-lm(y~x1,data=Delivery)
summary(fit.Del1)
anova(fit.Del1)

#Model 1 X2
fit.Del2<-lm(y~x2,data=Delivery)
summary(fit.Del2)
anova(fit.Del2)

#Model 0 1
fit.Del<-lm(y~1,data=Delivery)
summary(fit.Del)
anova(fit.Del)


drop1(fit.Del12,test='F')

#Model 3 X1+X2
fit.Del12<-lm(y~x2+x1,data=Delivery)
summary(fit.Del12)
anova(fit.Del12)