
#Wald
#For Poisson
summary(f1)$coefficient[2,2] #standard error Poisson
l.b.H<-exp(summary(f1)$coefficient[2,1]-(1.96*summary(f1)$coefficient[2,2]))
u.b.H<-exp(summary(f1)$coefficient[2,1]+(1.96*summary(f1)$coefficient[2,2]))
c(l.b.H,u.b.H)
#For negative binomial GLM
f1.nb<-glm.nb(count~race,link=log,H)
summary(f1.nb)$coefficient[2,2]#standard error NB GLM
l.b.H.nb<-exp(summary(f1.nb)$coefficient[2,1]-(1.96*summary(f1.nb)$coefficient[2,2]))
u.b.H.nb<-exp(summary(f1.nb)$coefficient[2,1]+(1.96*summary(f1.nb)$coefficient[2,2]))
c(l.b.H.nb,u.b.H.nb)
#Increased standard error for negative binomial compared to poisson. Thus more values are included in the wider confidence interval in the negative binomial. The negative binomial model is better since the poisson model does not take into account the overdispersion of the model and yields unrealistically small standard error and narrow confidence interval.\\

