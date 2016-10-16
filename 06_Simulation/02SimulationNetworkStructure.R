rm(list=ls())
library(MCMCpack)
library(RDS)
library(survey)
library(ICC)

replicate<-1000
store<-matrix(data=NA, nrow=replicate,ncol=3)
options(error=utils::recover)

for (irep in 1:replicate) {
  #Population of 1000, p is probability of knowing someone;
  n <- 1000
  p <- 1
  
  network <- xpnd(rbinom((n^2 + n)/2, 1, p), n)
  #Mean network sizes of np, but subject to variability,
  #Adjust to varying p later but fine for now;
  degree <- rowSums(network)
  #pOut is probability of outcome
  pOut <- 0.50
  y <- rbinom(n, size=1, prob=pOut)
  #x is random normal for testing type I error rates
  x <- rnorm(n)
  #Setup number of seeds, max number of coupons and sampleData matrix to store sample
  nSeeds <- 5
  sampleData <- matrix(0, nrow=1, ncol=7)
  maxCoupons <- 3
  
  #For each seed, pick a start value from the network 
  for (i in 1:nSeeds)
  {
    recruiter <- NA;
    
    #Pick initial seed
    startVal <- sample(n, size=1)
    
    for (mr in 1:nrow(sampleData))
    {
      if (startVal == as.numeric(sampleData[mr,1]))
      {
        startVal <- sample(places, size=1)
      }
    }
    
    wave <- 0;
    nCoupons <- sample(c(0:maxCoupons), size=1)
    
    Coupons <- rep(NA, maxCoupons)
    
    for (c in 1:nCoupons)
    {
      Coupons[c] <- paste(as.character(i), as.character(c), as.character(wave), sep="")
    }
    
    #Combine sample, id, wave, recruiter, x, y and degree into sample;
    sampleData <- rbind(sampleData, c(startVal, paste(as.character(i), as.character(c), as.character(wave), sep=""), recruiter, wave, x[i], y[i], degree[i]))
    
    #Do sampling without replacement if desired,
    #network <- network[-startVal,-startVal]
    
    #If you get a coupon, daisy-chain through to get your sample
    while (nCoupons !=0)
    {
      
      wave <- wave +1
      recruiter <- sampleData[nrow(sampleData),1]
      
      for (j in 1:nCoupons)
      {
        places <- NULL
        for (p.ind in 1:nrow(network))
        {
          if (network[startVal, p.ind] == 1)
          {
            places <- append(places, p.ind)
          }
        }
        
        temp <- sample(places, size=1)
        
        for (mr in 1:nrow(sampleData))
        {
          if (temp == as.numeric(sampleData[mr,1]))
          {
            temp <- sample(places, size=1)
          }
        }
        #Append everyone
        sampleData <- rbind(sampleData, c(temp, paste(as.character(i), as.character(j), as.character(wave), sep=""), recruiter, wave, x[temp], y[temp], degree[temp]))
      }
      
      nCoupons <- sample(c(0:maxCoupons), size=1)
    }
    
    
  }
  
  sampleData
  
  #Remove row 1 since its zeros?
  sampleData <- sampleData[-1,]
  
  #Getting close, data is unlikely to select rows more than once with first error checking, but might
  #still do it with a seed, process can be edited to re-seed until fixed sample size is reached.
  
  #Check for duplicates - pulled out some in the simulations with an if statement and another loop
  #if ( sum(duplicated(sampleData[,1])) !=1 )
  sampleData<-sampleData[!duplicated(sampleData[,1]),]
  
  #Trying to get the RDS data.frame setup so that I can run RDS analyses using the package...
  
  colnames(sampleData) <- c("id", "coupon", "recruiter.id", "wave", "x", "y", "network.size.variable")
  
  #Make NAs into -1 for seeds and RDS
  for (i in 1:nrow(sampleData))
  {
    if (is.na(as.numeric(sampleData[i,3])))
    {
      sampleData[i,3] <- -1
    }
  }
  
  sampleData <- as.rds.data.frame(sampleData)
  
  
  #########Use RDS-I weights
  weights <- compute.weights(sampleData, outcome.variable='y', weight.type="RDS-I")
  
  weights <- weights/(sampleData[,7])
  weights <- weights/min(weights)
  
  #############
  #Need cluster/recruiter indicators
  
  tree <- rep(NA, length(sampleData))
  recruiter <- rep(NA, length(sampleData))
  
  #Trees are done
  for (i in 1:nrow(sampleData))
  {
    tree[i] <- substr(as.matrix(sampleData[i,2]), start=1, stop=nchar(as.matrix(sampleData[i,2]))-2)
  }
  
  #Now recruiters...
  recruiter <- sampleData[,3]
  
  estimate<-RDS.I.estimates(sampleData, outcome.variable='y')$estimate
  
  real<-sum(y)/1000
  
  maxwave<-max(as.numeric(sampleData$wave))
  
  estimate
  real
  maxwave
  
  store[irep,2]<-estimate[2]
  store[irep,1]<-real
  store[irep,3]<-maxwave
}

colnames(store)<-c("observed","estimated","MaxWave")
head(store)
tail(store)

summary(store)[3,3]
summary(store)[4,3]


store<-data.frame(store)
mean(store$estimated,na.rm = TRUE)-0.5

#var(store$estimated,na.rm = T)

#store$MaxWave<-NULL
#fit<-lm(estimated~observed, data=store)
#plot(store,pch=10,ylim=range(0,1),xlim=range(0,1),cex=0.75)
#abline(fit,col='red')

#s<-NA
#a<-c(0.02565996,0.01527768,0.008945386,0.006979688,0.005026906)
#a<-c(0.02565996,0.01409987,0.01050716,0.008656616)
#a<-c(0.02565996,0.01851391,0.01232233)
#a<-c(0.02565996,0.01528668)
#for (i in 1:5){
# s[i]<-a[i+1]/a[i]
#}

#b<-c(0.02565996,0.01527768,0.008945386,0.006979688,0.005026906,0.01409987,0.01050716, 0.008656616, 0.01851391, 0.01232233,0.0249751,0.02506725, 0.02436425, 0.02580942,0.01528668)
#sqrt(b)