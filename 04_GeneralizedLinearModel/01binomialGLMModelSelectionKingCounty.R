mydata<-read.table(file="C:/Users/Hao/Desktop/Fall2016/BIOS612/A1/KingCounty2001_data.txt",header = T,sep = ",")
mydata$y<-ifelse(mydata$bwt<2500,1,0) #redefine the dependent variable a a binary indicator
sexf<-as.factor(mydata$sex) 
smokerf<-as.factor(mydata$smoker)
drinkerf<-as.factor(mydata$drinker)
racef<-as.factor(mydata$race)
marriedf<-as.factor(mydata$married)
firstepf<-as.factor(mydata$firstep)
welfaref<-as.factor(mydata$welfare)
parityf<-as.factor(mydata$parity)
y<-as.factor(mydata$y)
#A general check on the full model, no significance is yielded.
fit1<-glm(y~firstep+sexf+age+racef+parityf+marriedf,family = binomial,data = mydata)
summary(fit1)
# married and parify yield significance. We will keep them in future models.
backwards<-step(fit1)
#all AIC seems to be very high. 
fit3<-glm(y~firstep+bwt+welfare+smoker+drinker,family = binomial,data = mydata)
summary(fit3)
fit4<-glm(y~firstep+wpre+wgain+edu+gest,family = binomial,data = mydata)
summary(fit4)
#education p value is at .4, way above .05. Therefore, we remove this variable from our model.
#we now focusing on all the variable that yields significance in previous models
fit5<-glm(y~firstep+wpre+wgain+gest+marriedf+parityf,family = binomial,data = mydata)
summary(fit5)
# wpre, wgain and gest are highly significant. parity is significant at level 1, level2 and level5. 
backwards<-step(fit5)
#Married is not a significant predictor in the model6. 
#However, according to the backwards selection, parityf was firstly removed. 
#This might be due to most of the levels under parityf fail to yield significance. 
#By the backwards selection method, we do see that there is finally 4 variables that was kept. 
#Now we would like to check 
#if parity would have an impact on our model if we keep it as a continuous variable.
fit7<-glm(y~firstep+marriedf+wpre+wgain+gest+parity,family = binomial,data = mydata)
summary(fit7)
# we see that parity yields not significance.
#the AIC of the model is not the lowest among all the other models.
# we remove parity from our final model.
# Since married is not significant in some models, 
#we would like to investigate the impact of the variable married.
fit8<-glm(y~firstep+wpre+wgain+gest+marriedf,family = binomial,data = mydata)
summary(fit8)
backwards<-step(fit8)
#the variable married was not removed from the model
fit9<-glm(y~firstep+wpre+wgain+gest,family = binomial,data = mydata)
summary(fit9)
anova(fit8,fit9)
#compare fit8 and fit9. fit 8 yields a lower aic and lower residual deviance. 
#We thus decided to chose all the variables in this model as in our final model.
#Now we will keep all the variables in model 8 and test the interaction terms.
#Model 8 yields an aic=631.96.
#we are testing on the interactions of the factors that were remain significant in previous analysis.
fit10<-glm(y~firstep+marriedf*wpre*wgain*gest,family = binomial,data = mydata)
summary(fit10)
#No interaction shows significance in the full model. 
backwards<-step(fit10)
#Backwards selection leaves us the model with the lowest AIC=628.94. 
#That's so far the lowest we have seen.
fit11<-glm(y~firstep+marriedf + wpre + wgain + gest + marriedf:wpre + marriedf:gest + wpre:gest + marriedf:wpre:gest,family = binomial,data = mydata)
summary(fit11)