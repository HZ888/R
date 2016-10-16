data<-read.csv("/Users/Hao/Downloads/Winter2016/math523/assignment1/awards.csv",header = TRUE)
#plotting the data
#log-link
probitmod<-glm(numawards~1+math,family =poisson(link = log),data = data )
summary(probitmod)
coef(probitmod)

#identify link
glm2<-glm(numawards~1+math,family =poisson(link = identity),data = data )
modelb_2<-glm(formula=numawards~1+math,family =poisson(link = identity),start=c(-1.5,0.05),data = data )
summary(modelb_2)
coef(modelb_2)
#check the fit
plot(data$math,data$numawards,xlab = "Math", ylab = "Number of awards received")
s<-seq(30,75,by=0.01)
lines(s,exp(coef(probitmod)[1]+coef(probitmod)[2]*s),col="red")
lines(data$math,coef(modelb_2)[1]+coef(modelb_2)[2]*data$math,col="blue")
#Binning math into nine intervals and compute the mean for each interval
x<-data$math
with(data, aggregate(x,by = list(Bins = cut(data$math,c(30,35,40,45,50,55,60,65,70,75),labels=1:9)), FUN = function(x) c(mean(x))))


#log-link
data$prog<-factor(data$prog,levels=1:3,labels = c("General","Academic","Vocational"))
glm4<-glm(numawards~1+math+prog,family =poisson(link = log),data = data )
summary(glm4)
coef(glm4)

glm5<-update(glm4,~.+math:prog)
summary(glm5)
coef(glm5)

plot(data$math,data$numawards,xlab = "Math", ylab = "Number of awards received")
s<-seq(30,75,by=0.01)
lines(s,exp(coef(glm4)[1]+coef(glm4)[2]*s),col="yellow")
lines(s,exp(coef(glm5)[1]+coef(glm5)[2]*s),col="red")
lines(s,exp(coef(glm5)[1]+coef(glm5)[3]+(coef(glm5)[5]+coef(glm5)[2])*s),col="blue")
lines(s,exp(coef(glm5)[1]+coef(glm5)[4]+(coef(glm5)[6]+coef(glm5)[2])*s),col="green")
