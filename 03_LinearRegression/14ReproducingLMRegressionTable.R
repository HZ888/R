rm(list = ls())
library(lasso2)
data("Prostate")
t<-Prostate[,1:8]
k<-rep(1,97)
x<-cbind(k,t)
y<-Prostate[,9]
betaHat<-solve(t(x)%*%as.matrix(x))%*%t(x)%*%as.matrix(y) # coefficient estimates
r<-y-(betaHat[1]+betaHat[2]*t[,1]+betaHat[3]*t[,2]+betaHat[4]*t[,3]+betaHat[5]*t[,4]+betaHat[6]*t[,5]+betaHat[7]*t[,6]+betaHat[8]*t[,7]+betaHat[9]*t[,8])
summary(r) #reproducing the residuals
dSigmaSq <- sum((y - as.matrix(x)%*%betaHat)^2/(nrow(x)-ncol(x)))  # estimate of sigma-squared
mVarCovar <- dSigmaSq*chol2inv(chol(t(x)%*%as.matrix(x)))         # variance covariance matrix
vStdErr <- sqrt(diag(mVarCovar))                          # coeff. est. standard errors
DF<-nrow(x)-ncol(x)
t<-betaHat/vStdErr
print(cbind(betaHat, vStdErr,t ,2*pt(-abs(t),df=DF))) #reproducing the coefficients table
sqrt(sum(r^2)/DF) #reproducing the residual standard error
DF #reproducing the degrees of freedom

fit<-lm(Prostate[,9]~Prostate[,1]+Prostate[,2]+Prostate[,3]+Prostate[,4]+Prostate[,5]+Prostate[,6]+Prostate[,7]+Prostate[,8])
summary(fit)