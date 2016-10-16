setwd("C:/Users/Hao/Desktop/Fall2016/COMP551/Project2/Q1") # set the directory

train_in<-read.csv("train_in.csv")
train_out<-read.csv("train_out.csv")
train<-merge(train_in,train_out,by="id")

train_cs<-train[train$category %in% c("cs"),]
train_math<-train[train$category %in% c("math"),]
train_physics<-train[train$category %in% c("physics"),]
train_stat<-train[train$category %in% c("stat"),]

library("RTextTools")
library("plyr")
library("dplyr")
require(SnowballC)
library(tm)
library(rJava)
library(RWeka)
library(ngram)
library(text2vec)

prob_cs<-dim(train_cs)[1]/dim(train)[1] #creating the probability for each category
prob_stat<-dim(train_stat)[1]/dim(train)[1]
prob_physics<-dim(train_physics)[1]/dim(train)[1]
prob_math<-dim(train_math)[1]/dim(train)[1]
prob<-data.frame(prob_cs,prob_stat,prob_physics,prob_math)
write.csv(prob,"prob.csv")
prob<-read.csv("prob.csv") 

#test1<-read.csv("test_in.csv") # use this line of code to import the test set after features selection are done

#for tri-gram features
#cs
data<-train_cs[1:1000,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_cs<-data.frame(termFreq)
trigram_cs<-subset(d_cs,termFreq>1)
trigram_cs$word<-row.names(trigram_cs)
trigram_cs$prob<-(trigram_cs$termFreq+1)/(sum(trigram_cs$termFreq)+dim(trigram_cs)[1])

#stat
data<-train_stat[1:1000,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_stat<-data.frame(termFreq)
trigram_stat<-subset(d_stat,termFreq>1)
trigram_stat$word<-row.names(trigram_stat)
trigram_stat$prob<-(trigram_stat$termFreq+1)/(sum(trigram_stat$termFreq)+dim(trigram_stat)[1])

#math
data<-train_math[1:1000,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_math<-data.frame(termFreq)
trigram_math<-subset(d_math,termFreq>1)
trigram_math$word<-row.names(trigram_math)
trigram_math$prob<-(trigram_math$termFreq+1)/(sum(trigram_math$termFreq)+dim(trigram_math)[1])

#physics
data<-train_physics[1:1000,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_physics<-data.frame(termFreq)
trigram_physics<-subset(d_physics,termFreq>1)
trigram_physics$word<-row.names(trigram_physics)
trigram_physics$prob<-(trigram_physics$termFreq+1)/(sum(trigram_physics$termFreq)+dim(trigram_physics)[1])
################ for bi-gram features

#cs
data<-train_cs[1:2500,]
data<-data$abstract 
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
BigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=BigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_cs<-data.frame(termFreq)
bigram_cs<-subset(d_cs,termFreq>1)
bigram_cs$word<-row.names(bigram_cs)
bigram_cs$prob<-(bigram_cs$termFreq+1)/(sum(bigram_cs$termFreq)+dim(bigram_cs)[1])

#stat
data<-train_stat[1:2500,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=BigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_stat<-data.frame(termFreq)
bigram_stat<-subset(d_stat,termFreq>1)
bigram_stat$word<-row.names(bigram_stat)
bigram_stat$prob<-(bigram_stat$termFreq+1)/(sum(bigram_stat$termFreq)+dim(bigram_stat)[1])

#math
data<-train_math[1:2500,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=BigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_math<-data.frame(termFreq)
bigram_math<-subset(d_math,termFreq>1)
bigram_math$word<-row.names(bigram_math)
bigram_math$prob<-(bigram_math$termFreq+1)/(sum(bigram_math$termFreq)+dim(bigram_math)[1])

#physics
data<-train_physics[1:2500,]
data<-data$abstract
Corp<-Corpus(VectorSource(data))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=BigramTokenizer))
termFreq<-rowSums(as.matrix(tdm))
d_physics<-data.frame(termFreq)
bigram_physics<-subset(d_physics,termFreq>1)
bigram_physics$word<-row.names(bigram_physics)
bigram_physics$prob<-(bigram_physics$termFreq+1)/(sum(bigram_physics$termFreq)+dim(bigram_physics)[1])

##unigram features
Corp<-Corpus(VectorSource(train_cs[1:10000,]$abstract))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
c<-DocumentTermMatrix(CorpPTD)
termFreq<-colSums(as.matrix(c))
d_cs<-data.frame(termFreq)
uni_cs<-subset(d_cs,termFreq!=1)
uni_cs$word<-rownames(uni_cs)
uni_cs$prob<-(uni_cs$termFreq+1)/(sum(uni_cs$termFreq)+dim(uni_cs)[1])


Corp<-Corpus(VectorSource(train_stat[1:10000,]$abstract))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
c<-DocumentTermMatrix(CorpPTD)
termFreq<-colSums(as.matrix(c))
d_stat<-data.frame(termFreq)
uni_stat<-subset(d_stat,termFreq!=1)
uni_stat$word<-rownames(uni_stat)
uni_stat$prob<-(uni_stat$termFreq+1)/(sum(uni_stat$termFreq)+dim(uni_stat)[1])


Corp<-Corpus(VectorSource(train_math[1:10000,]$abstract))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
c<-DocumentTermMatrix(CorpPTD)
termFreq<-colSums(as.matrix(c))
d_math<-data.frame(termFreq)
uni_math<-subset(d_math, termFreq!=1)
uni_math$word<-rownames(uni_math)
uni_math$prob<-(uni_math$termFreq+1)/(sum(uni_math$termFreq)+dim(uni_math)[1])


Corp<-Corpus(VectorSource(train_physics[1:10000,]$abstract))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
c<-DocumentTermMatrix(CorpPTD)
termFreq<-colSums(as.matrix(c))
d_physics<-data.frame(termFreq)
uni_physics<-subset(d_physics,termFreq!=1)
uni_physics$word<-rownames(uni_physics)
uni_physics$prob<-(uni_physics$termFreq+1)/(sum(uni_physics$termFreq)+dim(uni_physics)[1])



#test1<-train[1:10000,]

test1<-test1[9000:10000,]

test1$cat<-NA
##  

for (r in 1:dim(test1)[1]){
  s<-test1[r,]
  Corp<-Corpus(VectorSource(s$abstract))
  b<-tm_map(Corp, removePunctuation)
  b<-tm_map(b,removeNumbers)
  b<-tm_map(b,removeWords,stopwords("english"))
  b<-tm_map(b,stemDocument)
  b<-tm_map(b,stripWhitespace)
  CorpPTD<-tm_map(b,PlainTextDocument)
  TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
#build up feature matrix
  tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
  termFreq<-rowSums(as.matrix(tdm))
  d<-data.frame(termFreq)
  d$word<-rownames(d)
#count the number of occurence of each word
  table_cs<-merge(d,trigram_cs,by="word", all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_stat<-merge(d,trigram_stat,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_math<-merge(d,trigram_math,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_physics<-merge(d,trigram_physics,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  
  test1$tri_cs[r]<-sum(log(table_cs$prob^table_cs$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_cs$prob))
  test1$tri_stat[r]<-sum(log(table_stat$prob^table_stat$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_stat$prob))
  test1$tri_math[r]<-sum(log(table_math$prob^table_math$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_math$prob))
  test1$tri_physics[r]<-sum(log(table_physics$prob^table_physics$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_physics$prob))
}
##########Similarly we do for the bi-gram

for (r in 1:dim(test1)[1]){
  s<-test1[r,]
  Corp<-Corpus(VectorSource(s$abstract))
  b<-tm_map(Corp, removePunctuation)
  b<-tm_map(b,removeNumbers)
  b<-tm_map(b,removeWords,stopwords("english"))
  b<-tm_map(b,stemDocument)
  b<-tm_map(b,stripWhitespace)
  CorpPTD<-tm_map(b,PlainTextDocument)
  TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
  
  tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))
  termFreq<-rowSums(as.matrix(tdm))
  d<-data.frame(termFreq)
  d$word<-rownames(d)
  
  table_cs<-merge(d,bigram_cs,by="word", all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_stat<-merge(d,bigram_stat,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_math<-merge(d,bigram_math,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_physics<-merge(d,bigram_physics,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)

  test1$bi_cs[r]<-sum(log(table_cs$prob^table_cs$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_cs$prob))
  test1$bi_stat[r]<-sum(log(table_stat$prob^table_stat$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_stat$prob))
  test1$bi_math[r]<-sum(log(table_math$prob^table_math$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_math$prob))
  test1$bi_physics[r]<-sum(log(table_physics$prob^table_physics$termFreq.x),na.rm = TRUE)-log(10^8)*sum(is.na(table_physics$prob))
}

###############for the uni-gram

for (r in 1:dim(test1)[1]){
  s<-test1[r,]
  Corp<-Corpus(VectorSource(s$abstract))
  b<-tm_map(Corp, removePunctuation)
  b<-tm_map(b,removeNumbers)
  b<-tm_map(b,removeWords,stopwords("english"))
  b<-tm_map(b,stemDocument)
  b<-tm_map(b,stripWhitespace)
  CorpPTD<-tm_map(b,PlainTextDocument)
  c<-DocumentTermMatrix(CorpPTD)
  termFreq<-colSums(as.matrix(c))
  d<-data.frame(termFreq)
  d$word<-rownames(d)
  table_cs<-merge(d,uni_cs,by="word", all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_stat<-merge(d,uni_stat,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_math<-merge(d,uni_math,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)
  table_physics<-merge(d,uni_physics,by="word",all.x=TRUE, all.y=FALSE,na.rm=FALSE)

  test1$uni_cs[r]<-log(prob$prob_cs)+sum(table_cs$termFreq.x*log(table_cs$prob),na.rm = TRUE)-log(sum(uni_cs$termFreq)+dim(uni_cs)[1])*sum(is.na(table_cs$prob))
  test1$uni_stat[r]<-log(prob$prob_stat)+sum(log(table_stat$prob)*table_stat$termFreq.x,na.rm = TRUE)-log(sum(uni_stat$termFreq)+dim(uni_stat)[1])*sum(is.na(table_stat$prob))
  test1$uni_math[r]<-log(prob$prob_math)+sum(log(table_math$prob)*table_math$termFreq.x,na.rm = TRUE)-log((sum(uni_math$termFreq)+dim(uni_math)[1]))*sum(is.na(table_math$prob))
  test1$uni_physics[r]<-log(prob$prob_physics)+sum(log(table_physics$prob)*table_physics$termFreq.x,na.rm = TRUE)-log(sum(uni_physics$termFreq)+dim(uni_physics)[1])*sum(is.na(table_physics$prob))
}

############## We did trial and error to determine the weight of using uni- bi- & tri- gram
test1$weighted_cs<-NA
test1$weighted_stat<-NA
test1$weighted_math<-NA
test1$weighted_physics<-NA

a<-1; b<-1; c<-1

a<-0.2470862  ;b<-0.4097902  ;c<-0.3431235


test1$weighted_cs<-test1$tri_cs*a+test1$bi_cs*b+test1$uni_cs*c
test1$weighted_stat<-test1$tri_stat*a+test1$bi_stat*b+test1$uni_stat*c
test1$weighted_math<-test1$tri_math*a+test1$bi_math*b+test1$uni_math*c
test1$weighted_physics<-test1$tri_physics*a+test1$bi_physics*b+test1$uni_physics*c

#all the probabilities of the four category, select the highest probability
store<-NA #tri, bi, uni, weighted
for (c in 1:4){
test1$predict<-(max.col(test1[,(5+4*(c-1)):(8+4*(c-1))],ties.method = c("random")))
test1$cat[which(test1$predict == 1)]<-"cs"
test1$cat[which(test1$predict == 2)]<-"stat"
test1$cat[which(test1$predict == 3)]<-"math"
test1$cat[which(test1$predict == 4)]<-"physics"

#p<-data.frame(test1$id,test1$cat)
#colnames(p)<-c("id","category")
#write.table(p,file = "weighted3.csv",sep = ",",row.names = FALSE,qmethod = "double")

# calculate the error rate in the validation sets
test1$i<-0
test1$i[which(test1$cat == test1$category)]<-1
store[c]<-sum(test1$i)/dim(test1)[1]
}
store
store/sum(store[1:3])