# Author: Handong Ma
# Basic R functions

compareList.findSig.MI<-function(primaryDF,secondaryDF){
  #inputs are two combined dataframe which have two columns:first for document name, second for word name
  #function finds the significant terms for primary list by excluding those which are also frequently occur in secondary list using Mutual Information
  #n11:num of doc that contains vocab, belongs to p; n10: num of doc that contains vocab, belongs to s; n00: num of doc that contains no vocab, belongs to s; n01: num of doc that contains no vocab, belongs to p
  p<-primaryDF
  s<-secondaryDF
  #smoothing  
  n <- length(unique(p[,1]))+length(unique(s[,1]))+2#total doc num, smoothing 2
  mylist <- list()
  for (vocab in unique(p[,2])){
    pcandi <- p[p[,2]==vocab,]
    scandi <- s[s[,2]==vocab,]
    n11 <- length(unique(pcandi[,1]))+0.5#smoothing0.5
    n10 <- length(unique(scandi[,1]))+0.5#smoothing0.5
    n00 <- length(unique(s[,1])) - length(unique(scandi[,1]))+0.5#smoothing0.5
    n01 <- length(unique(p[,1])) - length(unique(pcandi[,1]))+0.5#smoothing0.5
    n0. <- n00 + n01
    n1. <- n10 + n11
    n.0 <- n00 + n10
    n.1 <- n11 + n01
    I <- (n11/n)*log2(n*n11/n.1/n1.) + (n01/n)*log2(n*n01/n0./n.1) + (n10/n)*log2(n*n10/n1./n.0) + (n00/n)*log2(n*n00/n.0/n0.)
    mylist[vocab]<-I
  }
  midf<-data.frame(vocab=names(mylist),mi=unlist(mylist,use.names=F))
  occurp <- data.frame(table(p[,2]))
  occurs <- data.frame(table(s[,2]))
  for (check in occurp[,1]){
    if(length(occurs[occurs[,1]==check,][,1])!=0){
      if(occurp[occurp[,1]==check,][,2] < occurs[occurs[,1]==check,][,2]){
        occurp <- occurp[!(occurp[,1]==check),]
      }
    }
  }
  names(occurp)<-c('vocab','count')
  mydf<- merge(midf,occurp,by='vocab')
  mydf.sort<-mydf[order(mydf[,2],mydf[,3],decreasing=T),]
  return (mydf.sort)
}


mkdir <- function(dir){
  if (!file.exists(dir)){
    dir.create(dir)
  }
}


numberize.list<-function(targetList){
  t<-targetList
  t.unique<-unique(t)
  t.length<-length(t.unique)
  transform<-data.frame(t.unique,1:length(t.unique))
  names(t.out)<-c('name','id')
  for (j in 1:length(t)){
    new<-c(t[j],transform[transform[,1]==t[j],][2])
    t.out<-rbind(t.out,new)
  }
}

rmf<-function(file){
  if(file.exists(file)){
    file.remove(file)
    print(paste("original file removed",file))
  }
}

rme<-function(){
  rm(list=ls())
}