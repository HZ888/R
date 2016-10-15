


#2015-9-23
data.source<-"http://www.math.mcgill.ca/dstephens/Regression/Data/2-1-RocketProp.csv"
RocketProp<-read.csv(file=data.source)
names(RocketProp)<-c('i','Strength','Age')
x<-RocketProp$Age
y<-RocketProp$Strength

plot(x,y,pch=19,xlab='Age',ylab='Shear Strength')

(xmean<-mean(x))
(ymean<-mean(y))
abline(v=xmean,h=ymean,lty=2)

########statistics
fit.rp<-lm(y~x)
summary(fit.rp)

lm(y~x)
coef(fit.rp)

abline(coef(fit.rp),col='red')
n<-20
#first principal, by hand
x<-cbind(rep(1,n),x)
xtx<-t(x)%*%x

x<-cbind(1,x-mean(x))
#orthogonalized x vector
t(x) %*% x 
lm(y~I(x-mean(x)))
##################################

data.source<-"http://www.math.mcgill.ca/dstephens/Regression/Data/2-1-RocketProp.csv"

RocketProp<-read.csv(file=data.source)

names(RocketProp)<-c('i','Strength','Age')

x<-RocketProp$Age
y<-RocketProp$Strength
plot(x,y)

plot(x,y,pch=19)

plot(x,y,pch=19,xlab='Age',ylab='Shear Strength')

(xmean<-mean(x))
(ymean<-mean(y))

abline(v=xmean,h=ymean,lty=2)
dev.copy2pdf(file='RocketPropPlot.pdf',paper='USr',width=11,height=9)

############################################
#Fit the simple linear regression using lm

fit.RP<-lm(y~x)
summary(fit.RP)

coef(fit.RP)
abline(coef(fit.RP),col='red')
title('Line of best fit for Rocket Propulsion Data')

############################################
#Using the matrix formulae

X<-cbind(rep(1,n),x)

(XtX<-t(X)%*%X)

Xty<-t(X) %*% y

(beta.hat<-solve(xtx,Xty))

#hat matrix
H<-X %*% solve(xtx) %*% t(X)
max(t(H)-H)
max(t(H)%*%H-H)
diag(H)
sum(diag(H))
sum(residuals(fit.RP)^2)
t(y)%*%(diag(1,n)-H)%*%y
diag(1,n)
X1<-rep(1,n)
X1tX1<-t(X1)%*%(X1)
H1<-X1%*% solve(X1tX1)%*%t(X1)


###############
x<-RocketProp$Age
y<-RocketProp$Strength
n<-length(x)
plot(x,y,pch=19,xlab='Age',ylab='Shear Strength')
summary(fit.RP)
abline(coef(fit.RP),col='red')
title('Prediction Rocket Propulsion Data')
#prediction
xstar<-seq(0,25,by=0.1)
#CI
yostar.interval<-predict(fit.RP,newdata=data.frame(x=xstar),interval='prediction')
lines(xstar,yostar.interval[,2],lty=3,col='blue')
legend(15,2600,c('Prediction','Conf. Interv.','Pred. Interv.'),col=c('red','red','blue'),lty=c(1,2,3))
x
y
t(X) %*% (H-H1) %*% X
round(t(X) %*% (H-H1) %*% X,5)

#####ANOVA
fit.RP<-lm(y~x)
anova(fit.RP)
anova(fit.RP)[,'Df']
anova(fit.RP)[,'Sum Sq']
