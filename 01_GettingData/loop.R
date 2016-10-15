
data<-array(,dim=c(1,5));data

for (i in 1:40){
  a=runif(1,-90,90); o=runif(1,-180,180)
  data[i]= getDataforOneLocation(a,o) 
  print(data)
}
for (i in 1:40){
  cbind(data[i],data[i+1]
}
