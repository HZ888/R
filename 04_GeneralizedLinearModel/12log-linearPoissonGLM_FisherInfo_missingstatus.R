#We have log-linear models with counts as distributed in poisson. Homogeneous model is the best fitting model.
  ds<-data.frame(expand.grid(
    Gender=factor(c("M","F"),levels = c("M","F")),
    Age=factor(c("13 or less","14-18","19 and above"),levels=c("13 or less","14-18","19 and above")),
    Status=factor(c("Found","Missing"),levels=c("Found","Missing"))),
    C=c(3238,2448,7193,8769,4908,3361,33,38,63,108,157,159))
  head(ds)
  G<-as.factor(ds$Gender)
  A<-as.factor(ds$Age)
  S<-as.factor(ds$Status)
  mod1<-glm(C~1,family = poisson,ds) 
  mod2<-glm(C~1+G+A+S,family = poisson,ds)
  mod3<-glm(C~1+G+A+S+G:A,family = poisson,ds)
  mod4<-glm(C~1+G+A+S+G:S,family = poisson,ds)
  mod5<-glm(C~1+G+A+S+A:S,family = poisson,ds)
  mod6<-glm(C~1+G+A+S+G:A+G:S,family = poisson,ds)
  mod7<-glm(C~1+G+A+S+G:S+A:S,family = poisson,ds)
  mod8<-glm(C~1+G+A+S+A:S+G:A,family = poisson,ds)
  mod9<-glm(C~1+G+A+S+A:S+G:A+G:S,family = poisson,ds)
  mod10<-glm(C~1+G*A*S,family = poisson,ds)

# we are comparing models by the aic.
c(mod1$aic,mod2$aic,mod3$aic,mod4$aic,mod5$aic,mod6$aic,mod7$aic,mod8$aic,mod9$aic,mod10$aic)

#The model with the lowest aic yields the best fit. $mod9$ yields the lowest aic $118.7180$.\\
#Furthermore, we can also use Chi-square comparison to see that $mod10$, the saturated model, is not significantly different from the $mod9$, the homogeneous association.\\
anova(mod9,mod10,test="Chi")

#Now we are showing that compared to other models, $mod9$ is significantly different from the other models and cannot be simplified. We are using Chi-square comparison.
  anova(mod7,mod9,test="Chi")
  anova(mod6,mod9,test="Chi")
  anova(mod5,mod9,test="Chi")
  anova(mod4,mod9,test="Chi")
  anova(mod3,mod9,test="Chi")
  anova(mod2,mod9,test="Chi")
  anova(mod1,mod9,test="Chi")
#$mod9$ is significantly different from all other models and can not be simplified into other models. Thus we choose $mod9$, the homogeneous association model, to be the best fitted model. It yields the lowest aic.\\
    summary(mod9)

#For the log-linear Homogeneous Assocation mod9, we have the baseline level of Gender as Male, baseline level of Age group as 0-14, and baseline level of Status as Found. A2 indicates the age group 15-18. A3 is 19 and above. The model can be written as: 
#$$log\mu=\lambda+\lambda_1^G+\lambda_1^{A2}+\lambda_2^{A3}+\lambda_1^{SM}+\lambda_{11}^{A2,SM}+\lambda_{21}^{A3,SM}+\lambda_{11}^{G,A2}+\lambda_{12}^{G,A3}+\lambda_{11}^{G,SM}$$\\
#,where G indicates gender, A2 and A3 are age groups two and three and SM indicate status as missing.\\
  
#Now we are computing odds ratios and $95\%$ confidence interval for mod9.\\
  

#Fisher Information
  mod9<-glm(C~1+G+A+S+A:S+G:A+G:S,family = poisson,x=T,ds)
  X<-mod9$x
  W<-diag(fitted(mod9))
  var.cov<-solve(t(X)%*%W%*%X)
  Std.E<-as.numeric(sqrt(diag(solve(t(X)%*%W%*%X))))
  
  #Odds ratio and the 95% Confidence interval of Gender vs Status (at any level of age)
  coef<-summary(mod9)$coefficients
  OR.FS<-exp(coef[10,1])
  ub.FS<-exp(coef[10,1]+1.96*coef[10,2])
  lb.FS<-exp(coef[10,1]-1.96*coef[10,2])
  CI.FS<-c(lb.FS,ub.FS)
  #the Odds ratio of Gender vs Status (at any Age)
  OR.FS
  #the 95% Confidence interval of Gender vs Status (at any Age)
  CI.FS

  
  #Odds ratio and 95% Confidence interval of Gender vs Age1,3 levels (at any level of Status)
  coef<-summary(mod9)$coefficients
  OR.FA3<-exp(coef[9,1])
  ub.FA3<-exp(coef[9,1]+1.96*coef[9,2])
  lb.FA3<-exp(coef[9,1]-1.96*coef[9,2])
  CI.FA3<-c(lb.FA3,ub.FA3)
  #Odds ratio of Gender vs Age1,3 levels (at any level of Status)
  OR.FA3
  #95% Confidence interval of Gender vs Age1,3 levels (at any level of Status)
  CI.FA3
  
  #Odds ratio and 95% Confidence interval of Gender vs Age1,2 levels (at any level of Status)
  OR.FA2<-exp(coef[8,1])
  ub.FA2<-exp(coef[8,1]+1.96*coef[8,2])
  lb.FA2<-exp(coef[8,1]-1.96*coef[8,2])
  CI.FA2<-c(lb.FA2,ub.FA2)
  #Odds ratio of Gender vs Age1,2 levels (at any level of Status)
  OR.FA2
  #95% Confidence interval of Gender vs Age1,2 levels (at any level of Status)
  CI.FA2
  
  #Odds ratio and 95% Confidence interval of Gender vs Age2,3 (14-18 and 19 and above) (at any level of Status)
  OR.FA23<-exp(coef[9,1]-coef[8,1])
  sd.e.FA23<-sqrt(var.cov[9,9]+var.cov[8,8]-var.cov[8,9]-var.cov[9,8])
  ub.FA23<-exp(coef[9,1]-coef[8,1]+1.96*sd.e.FA23)
  lb.FA23<-exp(coef[9,1]-coef[8,1]-1.96*sd.e.FA23)
  CI.FA23<-c(lb.FA23,ub.FA23)
  #Odds ratio of Gender vs Age2,3 categories (at any level of Status)
  OR.FA23
  #95% Confidence interval of Gender vs Age2,3 (at any level of Status)
  CI.FA23
  
  #Odds ratio and 95% Confidence interval for Age1,3 categories (0-13 and19 and above) vs Status (at any level of Gender)
  OR.SA3<-exp(coef[7,1])
  ub.SA3<-exp(coef[7,1]+1.96*coef[7,2])
  lb.SA3<-exp(coef[7,1]-1.96*coef[7,2])
  CI.SA3<-c(lb.SA3,ub.SA3)
  #Odds ratio of Age1,3 vs Status (at any level of Gender)
  OR.SA3
  #95% Confidence interval of Age1,3 vs Status (at any level of Gender)
  CI.SA3
  
  #Odds ratio and 95% Confidence interval for Age1,2 categories (0-13 and 14-18) vs Status (at any level of Gender)
  OR.SA2<-exp(coef[6,1])
  ub.SA2<-exp(coef[6,1]+1.96*coef[6,2])
  lb.SA2<-exp(coef[6,1]-1.96*coef[6,2])
  CI.SA2<-c(lb.SA2,ub.SA2)
  #Odds ratio of Age1,2 vs Status (at any level of Gender)
  OR.SA2
  #95% Confidence interval of Age1,2 vs Status (at any level of Gender)
  CI.SA2
  
  #Odds ratio and 95% Confidence interval for Age2,3 vs Status (at any level of Gender)
  OR.SA23<-exp(coef[7,1]-coef[6,1])
  sd.e.23<-sqrt(var.cov[7,7]+var.cov[6,6]-var.cov[7,6]-var.cov[6,7])
  ub.SA23<-exp(coef[7,1]-coef[6,1]+1.96*sd.e.23)
  lb.SA23<-exp(coef[7,1]-coef[6,1]-1.96*sd.e.23)
  CI.SA23<-c(lb.SA23,ub.SA23)
  #Odds ratio of Age2,3 vs Status (at any level of Gender)
  OR.SA23
  #95% Confidence interval of Age2,3 vs Status (at any level of Gender)
  CI.SA23
  
  mod8<-glm(C~1+G+A+S+A:S+G:A,family = poisson,ds)
  mod8$deviance;mod8$deviance/mod8$df.residual
  
  summary(mod8)
  
  #Fisher Information for mod8
  mod8<-glm(C~1+G+A+S+A:S+G:A,family = poisson,x=T,ds)
  X8<-mod8$x
  W8<-diag(fitted(mod8))
  var.cov8<-solve(t(X8)%*%W8%*%X8)
  Std.E8<-as.numeric(sqrt(diag(solve(t(X8)%*%W8%*%X8))))
  #estimates
  coef.8<-summary(mod8)$coefficients
  #Odds ratio and 95% Confidence interval of Gender vs Age1,3 (at any level of Status)
  OR.mod8.GA3<-exp(coef.8[9,1])
  ub.mod8.GA3<-exp(coef.8[9,1]+1.96*coef.8[9,2])
  lb.mod8.GA3<-exp(coef.8[9,1]-1.96*coef.8[9,2])
  CI.mod8.GA3<-c(lb.mod8.GA3,ub.mod8.GA3)
  #Odds ratio of Gender vs Age1,3 (at any level of Status)
  OR.mod8.GA3
  #95% Confidence interval of Gender vs Age1,3 (at any level of Status)
  CI.mod8.GA3
  
  #Odds ratio and 95% Confidence interval of Gender vs Age1,2 (at any level of Status)
  OR.mod8.GA2<-exp(coef.8[8,1])
  ub.mod8.GA2<-exp(coef.8[8,1]+1.96*coef.8[8,2])
  lb.mod8.GA2<-exp(coef.8[8,1]-1.96*coef.8[8,2])
  CI.mod8.GA2<-c(lb.mod8.GA2,ub.mod8.GA2)
  #Odds ratio of Gender vs Age1,2 (at any level of Status)
  OR.mod8.GA2
  #95% Confidence interval of Gender vs Age1,2 (at any level of Status)
  CI.mod8.GA2
  
  #Odds ratio and 95% Confidence interval of Gender vs Age2,3 (at any level of Status)
  OR.mod8.GA23<-exp(coef.8[9,1]-coef.8[8,1])
  sd.e.mod8.GA23<-sqrt(var.cov8[9,9]+var.cov8[8,8]-var.cov8[8,9]-var.cov8[9,8])
  ub.mod8.GA23<-exp(coef.8[9,1]-coef.8[8,1]+1.96*sd.e.mod8.GA23)
  lb.mod8.GA23<-exp(coef.8[9,1]-coef.8[8,1]-1.96*sd.e.mod8.GA23)
  CI.mod8.GA23<-c(lb.mod8.GA23,ub.mod8.GA23)
  #Odds ratio of Gender vs Age2,3 (at any level of Status)
  OR.mod8.GA23
  #95% Confidence interval of Gender vs Age2,3 (at any level of Status)
  CI.mod8.GA23
  
  #Odds ratio and 95% Confidence interval of Age1,3 and above vs Status (at any level of Gender)
  OR.mod8.A3S<-exp(coef.8[7,1])
  ub.mod8.A3S<-exp(coef.8[7,1]+1.96*coef.8[7,2])
  lb.mod8.A3S<-exp(coef.8[7,1]-1.96*coef.8[7,2])
  CI.mod8.A3S<-c(lb.mod8.A3S,ub.mod8.A3S)
  #Odds ratio of Age1,3 and above vs Status (at any level of Gender)
  OR.mod8.A3S
  #95% Confidence interval of Age1,3 and above vs Status (at any level of Gender)
  CI.mod8.A3S
  
  #Odds ratio and 95% Confidence interval of Age1,2 and above vs Status (at any level of Gender)
  OR.mod8.A2S<-exp(coef.8[6,1])
  ub.mod8.A2S<-exp(coef.8[6,1]+1.96*coef.8[6,2])
  lb.mod8.A2S<-exp(coef.8[6,1]-1.96*coef.8[6,2])
  CI.mod8.A2S<-c(lb.mod8.A2S,ub.mod8.A2S)
  #Odds ratio of Age1,2 and above vs Status (at any level of Gender)
  OR.mod8.A2S
  #95% Confidence interval of Age1,2 and above vs Status (at any level of Gender)
  CI.mod8.A2S
  
  #Odds ratio and 95% Confidence interval of Age2,3 and above vs Status (at any level of Gender)
  OR.mod8.A23S<-exp(coef.8[7,1]-coef.8[6,1])
  sd.e.mod8.GA23<-sqrt(var.cov8[7,7]+var.cov8[6,6]-var.cov8[7,6]-var.cov8[6,7])
  ub.mod8.A23S<-exp(coef.8[7,1]-coef.8[6,1]+1.96*sd.e.mod8.GA23)
  lb.mod8.A23S<-exp(coef.8[7,1]-coef.8[6,1]-1.96*sd.e.mod8.GA23)
  CI.mod8.A23S<-c(lb.mod8.A23S,ub.mod8.A23S)
  #Odds ratio of Age1,2 and above vs Status (at any level of Gender)
  OR.mod8.A23S
  #95% Confidence interval of Age1,2 and above vs Status (at any level of Gender)
  CI.mod8.A23S
  
  # compare OR and CI
  matrixOR<-matrix(c(OR.FA3,OR.mod8.GA3,OR.FA2,OR.mod8.GA2,
                     OR.FA23,OR.mod8.GA23,OR.SA3,OR.mod8.A3S,
                     OR.SA2,OR.mod8.A2S,OR.SA23,OR.mod8.A23S),
                   nrow=2,
                   ncol=6)
  colnames(matrixOR)<-c("G vs A1,3","G vs A1,2","G vs A2,3","S vs A1,3","S vs A1,2","S vs A2,3")
  rownames(matrixOR)<-c("HA","CI")
  matrixOR
   # We see that the odds ratios are not significantly different. \\
  
 # Similarly we compare for the CIs. 

    matrixCI<-matrix(c(CI.FA3,CI.mod8.GA3,CI.FA2,CI.mod8.GA2,
                       CI.FA23,CI.mod8.GA23,CI.SA3,CI.mod8.A3S,
                       CI.SA2,CI.mod8.A2S,CI.SA23,CI.mod8.A23S),
                     nrow=2,
                     ncol=12)
  rownames(matrixCI)<-c("HA","CI")
  matrixCI
  
  round(cbind(G,A,S,ds$C,fitted(mod8),fitted(mod9)))
  
  #The fitted counts in the conditional independence model and the fitted counts in the homogeneous association model seems to be close to the observed counts.\\
  
  #The model without the interaction of gender and status perform equally well with the model with such interaction included. Thus the interaction of gender and status given age seems to be not significant. Taking the very large sample size into account, the association between missing status and gender seems to be practically not significant.
 