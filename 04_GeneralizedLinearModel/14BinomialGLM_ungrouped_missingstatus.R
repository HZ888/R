#Enter the data in an ungrouped form:\\
  mylist<-list()
  for (i in 1:dim(ds)[1]){
    ri<-as.data.frame(lapply(ds[i,],rep,ds[i,4]))
    mylist[[i]]<-ri
  }
  ds.U<-do.call("rbind",mylist) #the ungrouped dataset
  dim(ds.U);sum(ds[,4]) #check that this is the expanded ungrouped data
  head(ds.U)

    
   # Fit the winning model from part c), which is the homogeneous association model with two main effects gender and age.\\

  G.ds.U<-as.factor(ds.U$Gender)
  A.ds.U<-as.factor(ds.U$Age)
  S.ds.U<-as.factor(ds.U$Status)
  m4.ds.U<-glm(S.ds.U~1+G.ds.U+A.ds.U,family = binomial,ds.U)
  summary(m4.ds.U)
  summary(m4)$coefficients

  #  The estimates, the standard error, z values and p values does not change. All the significance levels in the model fit remains the same. \\
  

  c(m4$aic,m4.ds.U$aic)
  c(m4$deviance,m4.ds.U$deviance)
  c(m4$df.residual,m4.ds.U$df.residual)
  c(m4$deviance/m4$df.residual,m4.ds.U$deviance/m4.ds.U$df.residual)

   # The aic, residual deviance, degrees of freedom, and the dispersion parameter has changed. The residual deviance in the ungrouped data is $5348.4$ with $30471$ degrees of freedom. The residual deviance is $0.098749$ with $2$ degrees of freedom in the grouped data.\\
  
  #In the ungrouped data, all the variation of each variable are taken into account and each is treated as a binomial variable. The residual deviance thus is higher compared to the grouped data.\\

    dim(ds.U)[1]
    
   # The degree of freedom$=n-p=30475-4=30471$. $n=30475$ is the total number of combinations in the ungrouped data, i.e. the number of participants. $p=4$ indicates that in the model fit, the intercept, Gender as Female, Age group2 and Age group3 each takes one degree of freedom and in total takes four degree of freedoms. Thus the degree of freedom of the residual deviance is 30471.\\
  
    dim(ds.log)[1] #n
  length(m4$coefficients) #p

   # Since in the grouped data, the counts within the response bin follows a poisson distribution within each bin. And all the bins follow a binomial distribution. The residual deviance takes into account that of the binomial distribution. Thus compared to the residual deviance in the ungrouped data, the residual deviance in the grouped data is much smaller. The degrees of freedom $n-p=6-4=2$. $n=6$ indicates the total number of bins, which is the total number of the combination of all independent variable, Gender (F,M) and Age(1,2,3). The six combinations include, FA1,FA2,FA3, MA1,MA2 and MA3. The response is the status, which is represented by the counts. $p=4$ indicates that three parameters and an intercept in the model fit takes in total four degree of freedoms. Thus the degree of freedom of the residual deviance is 2.\\