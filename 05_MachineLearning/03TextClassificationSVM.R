rm(list=ls())
setwd("C:/Users/Hao/Desktop/Fall2016/COMP551/Project2/Q1")

library(tm)
library(rJava)
library(RWeka)
library(ngram)
library(text2vec)
library(RTextTools)
library(e1071)


train_in<-read.csv("train_in.csv")
train_out<-read.csv("train_out.csv")
train<-merge(train_in,train_out, by="id")

train_cs<-train[train$category=="cs",]
#write.table(train_cs,file = "train_cs.csv",sep = ",",col.names = NA,qmethod = "double")
train_stat<-train[train$category=="stat",]
train_math<-train[train$category=="math",]
train_physics<-train[train$category=="physics",]

data<-train[1:100,]
abstract<-data$abstract
Corp<-Corpus(VectorSource(abstract))
b<-tm_map(Corp, removePunctuation)
b<-tm_map(b,removeNumbers)
b<-tm_map(b,removeWords,stopwords("english"))
b<-tm_map(b,stemDocument)
b<-tm_map(b,stripWhitespace)
CorpPTD<-tm_map(b,PlainTextDocument)
TrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
tdm<-TermDocumentMatrix(CorpPTD,control = list(tokenize=TrigramTokenizer))

#termFreq<-rowSums(as.matrix(tdm))
#d<-data.frame(termFreq)


#dtMatrix <- create_matrix(tdm["abstract"])
container <- create_container(tdm, data$category, trainSize=1:11, virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

str(data)
doc_matrix<-create_matrix(data,language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=.8)
container <- create_container(doc_matrix, data$category, trainSize=1:10, virgin=FALSE)
SVM<-train_model(container,"SVM")
t<-train[3000:3010,]
predMatrix <- create_matrix(t, originalMatrix=doc_matrix)
