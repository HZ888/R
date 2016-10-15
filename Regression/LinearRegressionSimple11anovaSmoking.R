# 
data<-"http://www.math.mcgill.ca/dstephens/Regression/Data/cigs.csv"
smoking<-read.csv(data,header = T)
names(smoking)<-c("xi1","xi2","xi3","yi")
pairs(smoking,pch=19)


#SS_Res (beta0,1,2,3)
fit.123<-lm(yi~xi1+xi2+xi3, data=smoking)
anova(fit.123)
(a.val<-summary(aov(fit.123))[[1]][4,2])#(a)
#SS_Res(beta0,1,2)
fit.12<-lm(yi~xi1+xi2, data = smoking)
(b.val<-summary(aov(lm(yi~xi1+xi2,data = smoking))))[[1]][3,2]
b.val2<-anova(lm(yi~xi1+xi2,data = smoking))[3,2]
aov(lm(yi~xi1+xi2,data = smoking))
anova(lm(yi~xi1+xi2,data = smoking))
#F-stats for expection of beta0,1,2,3, comparing the F value
drop1(fit.123,test = 'F')
(b.val3<-drop1(fit.123,test = 'F')[4,5])
#term decomposition of beta3,2,1
fit.321<-lm(yi~xi3+xi2+xi1,data = smoking)
anova(fit.321)
d.val<-c(sum(anova(fit.321)[1:3,2]),(anova(fit.321))[1:3,2])
#decomposition R1,2=1|0,2|0,1
aov<-(anova(lm(yi~xi1+xi2, data = smoking)))
(e.val<-c(sum(aov[1:2,2]),aov[1:2,2]))             
#F stats
summary(fit.12)
anova(fit.12)
(g.val1<-summary(fit.12)$f[1])

