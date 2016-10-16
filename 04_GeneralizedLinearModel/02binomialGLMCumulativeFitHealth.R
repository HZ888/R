rm(list=ls())
setwd("C:/Users/Hao/Desktop/Fall2016/BIOS612/A2")
data<-read.csv("NHANES-new.csv")
attach(data)
library(MASS)

#check for histograms
hist(data[which (data$insure==0),]$health, main = "Histogram for the Not Insured")
hist(data[which (data$insure==1),]$health, main = "Histogram for the Insured" )

#without missing values
insure.cum1<-matrix(0,ncol=5,nrow=2)
for (i in 1:5){
  insure.cum1[1,i]<-sum(insure[health==i],na.rm = TRUE)/
    length(insure[health==i][!is.na(insure[health==i])])*100
  insure.cum1[2,i]<-100-insure.cum1[1,i]
}
barplot(insure.cum1, names=c(1,2,3,4,5),xlab = "Health", 
        ylab = "%",col = c(0,4),main = "Barplot for Percent of Insured and Not Insured")
legend(x=4.5,30,c("Insured", "Not insured"),fill=c(0,4))

#If we are looking at the insured and non insured differences for different and health conditions
insure.cum2<-matrix(0,nrow=5,ncol=2)
for (i in 0:1){
  insure.cum2[1,i+1]<-sum(health[health==1 & insure==i],na.rm = TRUE)/
    length(health[insure==i][!is.na(health[insure==i])])*100
  insure.cum2[2,i+1]<-(1/2)*sum(health[health==2 & insure==i],na.rm = TRUE)/
    length(health[insure==i][!is.na(health[insure==i])])*100
  insure.cum2[3,i+1]<-(1/3)*sum(health[health==3 & insure==i],na.rm = TRUE)/
    length(health[insure==i][!is.na(health[insure==i])])*100
  insure.cum2[4,i+1]<-(1/4)*sum(health[health==4 & insure==i],na.rm = TRUE)/
    length(health[insure==i][!is.na(health[insure==i])])*100
  insure.cum2[5,i+1]<-100-sum(insure.cum2[1:4,i+1])
}
barplot(insure.cum2, names=c(0,1),xlab = "Insured", 
        ylab = "%",col = c(0,2,3,4,5),main = "Barplot for the Percent of Health Conditions")
legend(x=1.9,39,c("Excellent", "Very Good", "Good","Fair","Poor"),
       fill=c(0,2,3,4,5))

#with missing value
insure.cum<-matrix(0,ncol=5,nrow=3)
for (i in 1:5){
  insure.cum[1,i]<-sum(insure[health==i],na.rm = TRUE)/length(insure[health==i])*100
  insure.cum[3,i]<-length(insure[health==i][is.na(insure[health==i])])/
    length(insure[health==i])*100
  insure.cum[2,i]<-100-insure.cum[1,i]-insure.cum[3,i]
}
barplot(insure.cum, names=c(1,2,3,4,5),xlab = "Health", 
        ylab = "%",col = c(0,4,3),main = "", sub ="Barplot for the Percent of Health Conditions" )
legend(x=4.5,30,c("Insured", "Not insured","Missing"),fill=c(0,4,3))

#cross-tabulation 
a<-xtabs(~health+insure);a

#general fit
summary(xtabs(~health+insure))

#cumulative fitting
h0<-as.numeric(health)<2
fit0<-glm(h0~insure,family = binomial)
fit0$coefficients

#cumulative fit
xtabs(~(as.numeric(health)<2)+insure)
fit0$coefficients[1]
sum(a[2:5,1])
log(477/1377)
exp(coefficients(fit0)[1])
fit0$coefficients[2]
#show that the coefficients are the same Odds Ratio for the cumlative and for the general fit
1867+1964+853+222
log(1377*2238/(477*4906))
exp(coefficients(fit0)[2])
fit0$deviance; fit0$df.residual
fit0$deviance/fit0$df.residual

#cumulative fit
h2<-as.numeric(health)<3
fit2<-glm(h2~insure,family = binomial)
xtabs(~h2+insure)
coefficients(fit2)[1]
log(xtabs(~h2+insure)[2,1]/xtabs(~h2+insure)[1,1])
exp(coefficients(fit2)[1])
coefficients(fit2)[2]
xtabs(~h2+insure)[1,1]*xtabs(~h2+insure)[2,2]/
  (xtabs(~h2+insure)[1,2]*xtabs(~h2+insure)[2,1])
log(xtabs(~h2+insure)[1,1]*xtabs(~h2+insure)[2,2]/
      (xtabs(~h2+insure)[1,2]*xtabs(~h2+insure)[2,1]))
exp(coefficients(fit2)[2])
xtabs(~h2+insure)[1,1]*xtabs(~h2+insure)[2,2]/
  (xtabs(~h2+insure)[1,2]*xtabs(~h2+insure)[2,1])

#cumulative fit
h3<-as.numeric(health)<4
fit3<-glm(h3~insure,family = binomial)
x3<-xtabs(~h3+insure);x3
coefficients(fit3)[1]
1480/374
log(x3[2,1]/x3[1,1])
coefficients(fit3)[2]
log(x3[1,1]*x3[2,2]/(x3[1,2]*x3[2,1]))
exp(coefficients(fit3)[2])
x3[1,1]*x3[2,2]/(x3[1,2]*x3[2,1])

#cumulative fit
h4<-as.numeric(health)<5
fit4<-glm(h4~insure,family = binomial)
x4<-xtabs(~h4+insure);x4
coefficients(fit4)[1]
log(x4[2,1]/x4[1,1])
exp(coefficients(fit4)[1])
1790/64
coefficients(fit4)[2]
log(x4[1,1]*x4[2,2]/(x4[1,2]*x4[2,1]))
64*6922/(1790*222)
exp(coefficients(fit4)[2])

