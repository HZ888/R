library(MASS)
f2.nb<-glm.nb(Freq~as.factor(race)+as.factor(count),link = log)
summary(f2.nb)
mu2.nb<-mean(fitted(f2.nb));mu2.nb


f2.nb$theta
Var2.nb<-mu2.nb+mu2.nb^2/f2.nb$theta; Var2.nb

#the variance of negative binomial depends on the mean: $var(Y)=\mu+\mu^2/\theta_z=1851.849$. The negative binomial yields the main effect race as a non-significant predictor yet the interaction of race and counts are significant. The poisson model yield race as a significant main effect. \\ 

xtabs(Freq~race+count)
xtabs(fitted(f2)~race+count)
xtabs(fitted(f2.nb)~race+count)

#We observe that for the one with high frequencies, for example, for count 0 and race 0, the fitted value of poisson is close to the observed value. Yet for most of the other counts where the frequencies are low, the fitted value of negative binomial is close to the observed value. \\

