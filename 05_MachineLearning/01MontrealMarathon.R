#To predict Y2 for the finishing time
#import the data
rm(list=ls())
library(chron)
mydata<-read.csv("C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/Project1_data.csv",header = T,sep = ",")
store<-matrix(data=NA,nrow=8711*100,ncol=6)

# making all the informations in one dataset under 5 column names
m<-matrix(data=NA,nrow=1,ncol=6)
m0<-mydata[c(1,(2+0*5):(6+0*5))]

for (i in 0:99) {
  m<-mydata[c(1,(2+i*5):(6+i*5))]
  names(m)<-names(m0)
  m0<-rbind(m0,m)
}
total<-m0
total<-total[!duplicated(total),]
total$oldtime<-total$TIME
total$TIME[total$TIME %in% c("-1")]<-NA
total$TIME<-60*24*as.numeric(times(total$TIME))
total$Age<-substr(total$CATEGORY,2,10)
total$Gender<-substr(total$CATEGORY,1,1)

#this data has all the info
save(total,file="C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/total.RData")

#select only the full marathon event
Marathon<-total[total$EVENT.TYPE %in% c("Marathon"),]
save(Marathon,file="C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/Marathon.RData")

#now our goal is to make a file with: IDs, gender, age, previous finising time (which can be an average of all the times)
Marathon_five<-Marathon[Marathon$EVENT.DATE == "2015-09-20",]
Marathon_five$Five<-Marathon_five$TIME
Marathon_five<-Marathon_five[c(1,8,9,10)]

Marathon_four<-Marathon[Marathon$EVENT.DATE == "2014-09-28",]
Marathon_four$Four<-Marathon_four$TIME
Marathon_four<-Marathon_four[c(1,8,9,10)]

Marathon_three<-Marathon[Marathon$EVENT.DATE == "2013-09-22",]
Marathon_three$Three<-Marathon_three$TIME
Marathon_three<-Marathon_three[c(1,8,9,10)]

Marathon_two<-Marathon[Marathon$EVENT.DATE == "2012-09-23",]
Marathon_two$Two<-Marathon_two$TIME
Marathon_two<-Marathon_two[c(1,8,9,10)]

#we are making a dataset with 2012-2014 marathon finishing time: 
Marathon_before<-merge(Marathon_two,Marathon_three,by="PARTICIPANT.ID",all=T)
Marathon_before$a<-as.numeric(as.factor(Marathon_before$Gender.x))-1
Marathon_before$b<-as.numeric(as.factor(Marathon_before$Gender.y))-1
Marathon_before$Gender<-rowMeans(cbind(Marathon_before$a,Marathon_before$b),na.rm = T)
Marathon_before$Gender[Marathon_before$Gender.x %in% c("U")]<-NA
Marathon_before$Gender[Marathon_before$Gender.y %in% c("U")]<-NA
Marathon_before$Gender.x<-NULL
Marathon_before$Gender.y<-NULL
names(Marathon_before)[2]<-"Age2"
names(Marathon_before)[4]<-"Age3"

Marathon_before<-merge(Marathon_before,Marathon_four,by="PARTICIPANT.ID",all=T)
Marathon_before$c<-as.numeric(as.factor(Marathon_before$Gender.y))-1
Marathon_before$Gender<-rowMeans(cbind(Marathon_before$Gender.x,Marathon_before$c),na.rm = T)
Marathon_before$Gender[Marathon_before$Gender.y %in% c("N")]<-NA
Marathon_before$Gender.x<-NULL
Marathon_before$Gender.y<-NULL
names(Marathon_before)[8]<-"Age4"

Marathon_before<-merge(Marathon_before,Marathon_five,by="PARTICIPANT.ID",all=T)
Marathon_before$Gender.y[Marathon_before$Gender.y %in% c("N")]<-NA
Marathon_before$Gender.y[Marathon_before$Gender.y %in% c("")]<-NA
Marathon_before$d<-as.numeric(as.factor(Marathon_before$Gender.y))-1
Marathon_before$Gender<-rowMeans(cbind(Marathon_before$Gender.x,Marathon_before$d),na.rm = T)
Marathon_before$Gender.x<-NULL
Marathon_before$Gender.y<-NULL
names(Marathon_before)[11]<-"Age5"

#create an age variable that is the avarage of all other age data for the same id
S<-Marathon_before[c("PARTICIPANT.ID","Two","Three","Four","Five","Gender","a","b","c","d","Age2","Age3","Age4","Age5")]
M<-S
M$Age2[M$Age2 %in% c("0-0")]<-NA
M$Age3[M$Age3 %in% c("0-0")]<-NA
M$Age4[M$Age4 %in% c("O AGE")]<-NA
M$Age5[M$Age5 %in% c("O AGE")]<-NA
M$Age5[M$Age5 %in% c("")]<-NA
#due to the different levels, add a numeric value to match for each year
M$A2<-as.numeric(as.factor(M$Age2))+2
M$A3<-as.numeric(as.factor(M$Age3))+2
M$A4<-as.numeric(as.factor(M$Age4))
M$A5<-as.numeric(as.factor(M$Age5))+1
M$A<-rowMeans(cbind(M$A2,M$A3,M$A4),na.rm = T)


#detect errors in the average age category construction
#check for the misentered information
out<-M[which(abs(M$A-M$A2)>1|abs(M$A-M$A3)>1|abs(M$A-M$A4)>1|abs(M$A-M$A5)>1),]
#delete the values that is not sure
M$A[M$PARTICIPANT.ID %in% c("1713","6393")]<-NA

#####
#Define the finishing time variable
M$T<-rowMeans(cbind(M$Two,M$Three,M$Four),na.rm = T)
M$T2<-rowMeans(cbind(M$Two,M$Three,M$Four,M$Five),na.rm = T)

#check for missing values
MM<-M
MM<-MM[c("PARTICIPANT.ID","Gender","Five","A","T","T2")]
colMeans(is.na(MM))
MM$Gender<-round(MM$Gender,digits = 0)
MM$A<-ceiling(MM$A)

#10-fold leave one out cross validation
MM$indicator<-runif(nrow(MM))
MM$fold[MM$indicator<=0.1]<-1
MM$fold[MM$indicator<=0.2 & MM$indicator >0.1]<-2
MM$fold[MM$indicator<=0.3 & MM$indicator >0.2]<-3
MM$fold[MM$indicator<=0.4 & MM$indicator >0.3]<-4
MM$fold[MM$indicator<=0.5 & MM$indicator >0.4]<-5
MM$fold[MM$indicator<=0.6 & MM$indicator >0.5]<-6
MM$fold[MM$indicator<=0.7 & MM$indicator >0.6]<-7
MM$fold[MM$indicator<=0.8 & MM$indicator >0.7]<-8
MM$fold[MM$indicator<=0.9 & MM$indicator >0.8]<-9
MM$fold[MM$indicator >0.9]<-10

#checked the percentage of missing values 
colMeans(is.na(MM))

##########fit the model
fold<-10
error<-matrix(data=NA,nrow = fold,ncol = 10)
rownames(error)<-c("fold1","fold2","fold3","fold4","fold5","fold6","fold7","fold8","fold9","fold10")

er<-matrix(data=NA,nrow = fold,ncol = 10)
rownames(er)<-c("fold1","fold2","fold3","fold4","fold5","fold6","fold7","fold8","fold9","fold10")

i<-1
#model1: time~G
for (i in 1:fold){
  fit2<-lm(Five~as.factor(Gender),data=MM[which(!MM$fold ==i),])
  test2<-MM[which(MM$fold ==i),]
  c2<-fit2$coefficients
  test2$predict2<-c2[1]+c2[2]*test2$Gender
  test2$error<-(test2$predict2-test2$Five)^2
  error[i,1]<-mean(test2$error,na.rm = T)
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-c2[1]+c2[2]*train2$Gender
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,1]<-mean(train2$error,na.rm = T)
}

#model2: time~A
for (i in 1:fold){
  fit1<-lm(Five~as.factor(A),data=MM[which(!MM$fold ==i),])
  test1<-MM[which(MM$fold ==i),]
  c1<-fit1$coefficients
  test1$Abig3<-test1$A
  test1$Abig3[as.numeric(as.factor(test1$Abig3)) %in% c("NaN")]<-NA
  test1$Abig3[as.numeric(as.factor(test1$Abig3)) %in% c("3","2","1","0")]<-NA
  test1$predict1<-c1[1]+c1[as.numeric(as.factor(test1$Abig3))+2]
  test1$error<-(test1$predict1-test1$Five)^2
  error[i,2]<-mean(test1$error,na.rm = T)
  train1<-MM[which(!MM$fold ==i),]
  train1$predict1<-c1[1]+c2[2]*train1$Age
  train1$error<-(train1$predict1-train1$Five)^2
  er[i,2]<-mean(train1$error,na.rm = T)
}


#check for model3: time~T
for (i in 1:fold){
  fit3<-lm(Five~T,data=MM[which(!MM$fold ==i),])
  test3<-MM[which(MM$fold ==i),]
  test3$predict3<-predict(fit3,test3)
  test3$error<-(test3$predict3-test3$Five)^2
  error[i,3]<-mean(test3$error,na.rm = T)
  
  train1<-MM[which(!MM$fold ==i),]
  train1$predict1<-c2[1]+c2[2]*train1$Gender
  train1$error<-(train1$predict1-train1$Five)^2
  er[i,3]<-mean(train1$error,na.rm = T)
}



#model4: Y2~T+G
for (i in 1:fold){
  fit4<-lm(Five~T+as.factor(Gender),data=MM[which(!MM$fold ==i),])
  test4<-MM[which(MM$fold ==i),]
  c4<-fit4$coefficients
  test4$predict4<-c4[1]+c4[2]*test4$T+c4[3]*test4$Gender
  test4$error<-(test4$predict4-test4$Five)^2
  error[i,4]<-mean(test4$error,na.rm = T)
}


#model5:Y2~T+A
for (i in 1:fold){
  fit6<-lm(Five~T+as.factor(A),data=MM[which(!MM$fold ==i),])
  test6<-MM[which(MM$fold ==i),]
  c6<-fit6$coefficients
  test6$Abig3<-test6$A
  test6$Abig3[as.numeric(as.factor(test6$Abig3)) %in% c("2","1","0")]<-NA
  test6$predict6<-c6[1]+c6[2]*test6$T+c6[as.numeric(as.factor(test6$Abig3))+2]
  test6$error<-(test6$predict6-test6$Five)^2
  error[i,5]<-mean(test6$error,na.rm = T)
}



#model6:Y2~G+A
for (i in 1:fold){
  fit7<-lm(Five~as.factor(Gender)+as.factor(A),data=MM[which(!MM$fold ==i),])
  test7<-MM[which(MM$fold ==i),]
  c7<-fit7$coefficients
  test7$Abig3<-test7$A
  test7$Abig3[as.numeric(as.factor(test7$Abig3)) %in% c("2","1","0")]<-NA
  test7$predict7<-c7[1]+c7[2]*test7$Gender+c7[as.numeric(as.factor(test7$Abig3))+3]
  test7$error<-(test7$predict7-test7$Five)^2
  error[i,6]<-mean(test7$error,na.rm = T)
}

#model7: Y2~T+G+T*G
for (i in 1:fold){
  fit5<-lm(Five~T+as.factor(Gender)+T:as.factor(Gender),data=MM[which(!MM$fold ==i),])
  test5<-MM[which(MM$fold ==i),]
  c5<-fit5$coefficients
  test5$predict5<-c5[1]+c5[2]*test5$T+c5[3]*test5$Gender+c5[4]*test5$T*test5$Gender
  test5$error<-(test5$predict5-test5$Five)^2
  error[i,7]<-mean(test5$error,na.rm = T)
}


#model8:Y2~G+A+A:G
for (i in 1:fold){
  fit9<-lm(Five~as.factor(Gender)+as.factor(A)+as.factor(Gender):as.factor(A),data=MM[which(!MM$fold ==i),])
  test9<-MM[which(MM$fold ==i),]
  c9<-fit9$coefficients
  test9$Abig3<-test9$A
  test9$Abig3[as.numeric(as.factor(test9$Abig3)) %in% c("2","1","0")]<-NA
  test9$predict9<-c9[1]+c9[2]*test9$Gender+c9[as.numeric(as.factor(test9$Abig3))+1]+test9$Gender*c9[as.numeric(as.factor(test9$Abig3))*2+9]
  test9$error<-(test9$predict9-test9$Five)^2
  error[i,8]<-mean(test9$error,na.rm = T)
}

#model9:Y2~T+A+T:A
for (i in 1:fold){
  fit8<-lm(Five~T+as.factor(A)+T:as.factor(A),data=MM[which(!MM$fold ==i),])
  test8<-MM[which(MM$fold ==i),]
  c8<-fit8$coefficients
  test8$Abig3<-test8$A
  test8$predict8<-c8[1]+c8[2]*test8$T+c8[as.numeric(as.factor(test8$Abig3))]+c8[12+as.numeric(as.factor(test8$Abig3))]*test8$T
  test8$error<-(test8$predict8-test8$Five)^2
  error[i,9]<-mean(test8$error,na.rm = T)
}

#plot the MSE for all models 
par(mar = rep(2, 4))
x<-colMeans(error)
boxplot(error)
title(main="Model fit based on Age as a categorical variable",xlab="Model fit",ylab="Error")
points(colMeans(error))
lines(colMeans(error))


#leave the model with exploded error out the plot, but keep the information of MSE in the table with other MSEs 
#model10:Y2~T+G+A
for (i in 1:fold){
  fit10<-lm(Five~T+as.factor(Gender)+as.factor(A),data=MM[which(!MM$fold ==i),])
  test10<-MM[which(MM$fold ==i),]
  c10<-fit10$coefficients
  test10$Abig3<-test10$A
  test10$predict10<-c10[1]+c10[2]*T+c10[3]*test10$Gender+c10[as.numeric(as.factor(test10$Abig3))+1]
  test10$error<-(test10$predict10-test10$Five)^2
  error[i,10]<-mean(test10$error,na.rm = T)
}
error1<-rbind(error,colMeans(error))
error_ca<-data.frame(colMeans(error1))


#try the age as a continuous variable 
error2<-matrix(data=NA,nrow=fold,ncol = 16)
rownames(error2)<-c("fold1","fold2","fold3","fold4","fold5","fold6","fold7","fold8","fold9","fold10")

#f1:t~G
for (i in 1:fold){
  f2<-lm(Five~as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t2<-MM[which(MM$fold ==i),]
  e2<-f2$coefficients
  t2$p2<-e2[1]+e2[2]*t2$A
  t2$error<-(t2$p2-t2$Five)^2
  error2[i,1]<-mean(t2$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e2[1]+e2[2]*train2$Gender
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,1]<-mean(train2$error,na.rm = T)
} 

#f2:t~A
for (i in 1:fold){
  f1<-lm(Five~A,data=MM[which(!MM$fold ==i),])
  t1<-MM[which(MM$fold ==i),]
  e1<-f1$coefficients
  t1$p1<-e1[1]+e1[2]*t1$A
  t1$error<-(t1$p1-t1$Five)^2
  error2[i,2]<-mean(t1$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e1[1]+e1[2]*train2$A
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,2]<-mean(train2$error,na.rm = T)
} 

#f3:t~T
for (i in 1:fold){
  f3<-lm(Five~T,data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,3]<-mean(t3$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e3[1]+e3[2]*train2$T
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,3]<-mean(train2$error,na.rm = T)
} 


#f4:t~T+G
for (i in 1:fold){
  f4<-lm(Five~T+as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t4<-MM[which(MM$fold ==i),]
  e4<-f4$coefficients
  t4$p4<-e4[1]+e4[2]*t4$T+e4[3]*t4$Gender
  t4$error<-(t4$p4-t4$Five)^2
  error2[i,4]<-mean(t4$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e4[1]+e4[2]*train2$T+e4[3]*train2$Gender
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,4]<-mean(train2$error,na.rm = T)
}  

#f5:t~T+A
for (i in 1:fold){
  f5<-lm(Five~T+A,data=MM[which(!MM$fold ==i),])
  t5<-MM[which(MM$fold ==i),]
  e5<-f5$coefficients
  t5$p5<-e5[1]+e5[2]*t5$T+e5[3]*t5$A
  t5$error<-(t5$p5-t5$Five)^2
  error2[i,5]<-mean(t5$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e5[1]+e5[2]*train2$T+e5[3]*train2$A
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,5]<-mean(train2$error,na.rm = T)
} 


#f6:t~G+A
for (i in 1:fold){
  f6<-lm(Five~as.factor(Gender)+A,data=MM[which(!MM$fold ==i),])
  t6<-MM[which(MM$fold ==i),]
  e6<-f6$coefficients
  t6$p6<-e6[1]+e6[2]*t6$Gender+e6[3]*t6$A
  t6$error<-(t6$p6-t6$Five)^2
  error2[i,6]<-mean(t6$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e6[1]+e6[2]*train2$Gender+e6[3]*train2$A
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,6]<-mean(train2$error,na.rm = T)
} 

#f7:Y2~T+G+T:G
for (i in 1:fold){
  f4<-lm(Five~T*as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t4<-MM[which(MM$fold ==i),]
  e4<-f4$coefficients
  t4$p4<-e4[1]+e4[2]*t4$T+e4[3]*t4$Gender+e4[4]*t4$T*t4$Gender
  t4$error<-(t4$p4-t4$Five)^2
  error2[i,7]<-mean(t4$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e4[1]+e4[2]*train2$T+e4[3]*train2$Gender+e4[4]*train2$T*train2$Gender
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,7]<-mean(train2$error,na.rm = T)
}  

#f8:t~G+A+G:A
for (i in 1:fold){
  f6<-lm(Five~as.factor(Gender)+A+as.factor(Gender):A,data=MM[which(!MM$fold ==i),])
  t6<-MM[which(MM$fold ==i),]
  e6<-f6$coefficients
  t6$p6<-e6[1]+e6[2]*t6$Gender+e6[3]*t6$A+e6[4]*t6$A*t6$Gender
  t6$error<-(t6$p6-t6$Five)^2
  error2[i,8]<-mean(t6$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e6[1]+e6[2]*train2$Gender+e6[3]*train2$A+e6[4]*train2$A*train2$Gender
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,8]<-mean(train2$error,na.rm = T)
} 


#f10:t~T+G+A
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,10]<-mean(t3$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e3[1]+e3[2]*train2$T+e3[3]*train2$Gender+e3[4]*train2$A
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,10]<-mean(train2$error,na.rm = T)
} 


#f11:Y2~T+G+A+T:G
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+T:as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$Gender
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,11]<-mean(t3$error,na.rm = T)
} 

#f12:Y2~T+G+A+A:G
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+A:as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$Gender*t3$A
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,12]<-mean(t3$error,na.rm = T)
} 

#f13:Y2~T+G+A+T:A
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+T:A,data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$T*t3$A
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,13]<-mean(t3$error,na.rm = T)
} 


#f14:Y2~T+G+A+T:G+A:G
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+T:as.factor(Gender)+A:as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$Gender+e3[6]*t3$A*t3$Gender
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,14]<-mean(t3$error,na.rm = T)
} 

#f15:Y2~T+G+A+A:G+T:A
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+A:as.factor(Gender)+T:A,data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$Gender*t3$A+e3[6]*t3$T*t3$A
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,15]<-mean(t3$error,na.rm = T)
} 

#f16:Y2~T+G+A+T:A+T:G
for (i in 1:fold){
  f3<-lm(Five~T+A+as.factor(Gender)+T:A+T:as.factor(Gender),data=MM[which(!MM$fold ==i),])
  t3<-MM[which(MM$fold ==i),]
  e3<-f3$coefficients
  t3$p3<-e3[1]+e3[2]*t3$T+e3[3]*t3$Gender+e3[4]*t3$A+e3[5]*t3$T*t3$A+e3[6]*t3$T*t3$Gender
  t3$error<-(t3$p3-t3$Five)^2
  error2[i,16]<-mean(t3$error,na.rm = T)
} 

boxplot(error2)
title(main="Model fit based on Age as a continuous variable",xlab="Model",ylab="Error")
points(colMeans(error2))
lines(colMeans(error2))

#f9:t~t+a+a:t
for (i in 1:fold){
  f5<-lm(Five~T+A+T:A,data=MM[which(!MM$fold ==i),])
  t5<-MM[which(MM$fold ==i),]
  e5<-f5$coefficients
  t5$p5<-e5[1]+e5[2]*t5$T+e5[3]*t5$A+e5[3]*t5$A*t5$T
  t5$error<-(t5$p5-t5$Five)^2
  error2[i,9]<-mean(t5$error,na.rm = T)
  
  train2<-MM[which(!MM$fold ==i),]
  train2$predict2<-e5[1]+e5[2]*train2$T+e5[3]*train2$A+e5[3]*train2$A*train2$T
  train2$error<-(train2$predict2-train2$Five)^2
  er[i,9]<-mean(train2$error,na.rm = T)
} 

error2<-rbind(error2,colMeans(error2))
error_con<-data.frame(error2[11,1:10])

er<-rbind(er,colMeans(er))
error_con<-data.frame(error2[11,1:10])

error_final<-cbind(error_ca,error_con)
colnames(error_final)<-c("Cat","Con")
write.csv(error_final,file="C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/error.csv")

#select the model to fit: this is the final model we selected
#model4: time~finishingTime+gender
for (i in 1:fold){
  fit4<-lm(Five~T+as.factor(Gender),data=MM[which(!MM$fold ==i),])
  test4<-MM[which(MM$fold ==i),]
  c4<-fit4$coefficients
  test4$predict4<-c4[1]+c4[2]*test4$T+c4[3]*test4$Gender
  test4$error<-(test4$predict4-test4$Five)^2
  error[i,4]<-mean(test4$error,na.rm = T)
}
colMeans(is.na(MM))

#to predict for the 2016 data using 2015 data. we are constructing a small dataset only with information that we need
vars<-c("PARTICIPANT.ID","Gender","T2","A")
Final<-MM[vars]
id<-mydata[,c(1,6)]
id$A_mydata<-substr(id$CATEGORY,2,10)
id$G_mydata<-substr(id$CATEGORY,1,1)
#fill in the missing values with the mean for T and G variables.
id$CATEGORY<-NULL
id$A_mydata<-NULL
F<-merge(Final,id,by="PARTICIPANT.ID",all.y = T)
F$G_mydata[F$G_mydata %in% c("G","H","N","S","U","")]<-NA
F$Gender[F$Gender=="."]<-F$G_mydata
F$G_mydata<-as.numeric(as.factor(F$G_mydata))
F[is.na(F[,7]), 7] <- mean(F[,7], na.rm = TRUE)
F$G_mydata[F$G_mydata>1.5]<-2
F$G_mydata<-F$G_mydata-1
F[is.na(F[,3]), 3] <- mean(F[,3], na.rm = TRUE)

F$predict<-c4[1]+c4[2]*F$T2+c4[3]*as.numeric(as.factor(F$G_mydata))

#convert back to time format
F$Time<-sub(":\\d{3}", "", times((F$predict %/%60 + F$predict%% 60/3600)/24))
F$Gender<-NULL
F$T2<-NULL
F$A<-NULL
F$CATEGORY<-NULL
F$A_mydata<-NULL
F$G_mydata<-NULL
F$predict<-NULL

#save the file for the prediction for the finishing time for all the subjects
write.csv(F,file="C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/time.csv")

##merge the three csv files

csv1<-read.csv("C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/sunmee/logistic.csv",header = T,sep = ",")
csv1$X<-NULL
csv1$Y2015<-NULL
Prediction<-merge(csv1,F,by="PARTICIPANT.ID",all = T)
write.csv(Prediction,file="C:/Users/Hao/Desktop/Fall2016/COMP551/Project1/prediction.csv")



