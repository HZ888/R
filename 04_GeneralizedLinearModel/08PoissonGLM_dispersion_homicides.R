H<-read.table("C:/Users/Hao/Downloads/Winter2016/math523/assignment3/Homicides.dat",header = TRUE)
f1<-glm(count~race,family = poisson,H)
summary(f1)
mu2<-exp(f1$coefficients[2]);mu2
#The model is $log(\hat\mu)= -2.38321+1.73314*(race)$. Estimated dispersion parameter$=0.6467897$ SE=0.14657. This model yields under-dispersion.\\
#beta_1=5.66. It indicates an increase in the value of race (from white$0$ to black $1$), corresponds to a $1.733$ increase in the log of the counts, i.e. an increase of $5.66$ counts of people who were the victims of homicide.\\
#From the counting table, we observed that the number of counts in different catergories vary drastically. For example, for number of counts being 0, the number of the white is 1070. Yet for the counts from 4-6, the number of the white is mostly 0 or 1. Simillary for the black as well.\\


H.table<-table(H$race,H$count);H.table
H.CITable<-as.data.frame(H.table);
names(H.CITable)<-c("race","count","Freq"); H.CITable
attach(H.CITable)

#Since the frequency of counts is skewed towards the high counts. We need to use the grouped data to perform analysis and compute dispersion parameter from the deviance.\\
  
#Dispersion
f2<-glm(Freq~as.factor(race)+as.factor(count),family = poisson)
f2$deviance/f2$df.residual
#The dispersion parameter $\phi=10.99178>>1$. Thus there is overdispersion in the poisson GLM.
  
