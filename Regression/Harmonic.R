
#Read in data set 1

data1<-"http://www.math.mcgill.ca/dstephens/Regression/Data/sea-ice-data.txt"
SeaIce.data<-read.table(data1,header=TRUE)
yl<-"September Arctic Sea Ice Extent (1,000,000 sq km)"
plot(SeaIce.data,ylab=yl,pch=18,ylim=range(0,8))
x<-SeaIce.data$Year
y<-SeaIce.data$SeaIce

#Harmonic regression
#K=2
  
omega1<-1/2
omega2<-1/3
x1<-cos(2*pi*omega1*x/max(x))
x2<-sin(2*pi*omega1*x/max(x))
x3<-cos(2*pi*omega2*x/max(x))
x4<-sin(2*pi*omega2*x/max(x))

X<-cbind(rep(1,length(x)),x1,x2,x3,x4)
fit.har2<-lm(y~X-1)
summary(fit.har2)
plot(SeaIce.data,pch=19,main='Sea Ice data',cex=0.75)
lines(x,fitted(fit.har2),col='green')

omega3<-1/4
x5<-cos(2*pi*omega3*x/max(x))
x6<-sin(2*pi*omega3*x/max(x))
X<-cbind(rep(1,length(x)),x1,x2,x3,x4,x5,x6)
fit.har3<-lm(y~X-1)
summary(fit.har3)
lines(x,fitted(fit.har3),col='blue')


omega4<-1/5
x7<-cos(2*pi*omega4*x/max(x))
x8<-sin(2*pi*omega4*x/max(x))
X<-cbind(rep(1,length(x)),x1,x2,x3,x4,x5,x6,x7,x8)
fit.har4<-lm(y~X-1)
summary(fit.har4)
lines(x,fitted(fit.har4),col='purple')


omega5<-1/6
x9<-cos(2*pi*omega5*x/max(x))
x10<-sin(2*pi*omega5*x/max(x))
X<-cbind(rep(1,length(x)),x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
fit.har5<-lm(y~X-1)
plot(SeaIce.data,pch=19,main='Sea Ice data')
lines(x,fitted(fit.har4),col='green')

lines(x,fitted(fit.har5),col='red')

