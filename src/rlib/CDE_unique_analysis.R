library(ggplot2)
library(methods)
rm(list=ls())
groups = 'inclusion'#c("exclusion","inclusion")
for (group in groups){
  source("./util.R")
  source.file <-c(paste("../../result/",group,"/allData_table",sep = ""))
  mkdir(paste("../../result/",group,"/CDE_unique/",sep=""))
  out.pdf <- c(paste('../../result/',group,'/CDE_unique/CDE_unique_analysis.pdf',sep=''))
  out.list.unique.feature <- c(paste('../../result/',group,'/CDE_unique/CDE_unique_list',sep=''))
  out.csv.common.feature <- c(paste('../../result/',group,'/CDE_unique/CDE_common_table',sep=''))
  out.rank <- c(paste('../../result/',group,'/CDE_unique/CDE_ranking_overall',sep=''))
  out.per.disease <-c(paste('../../result/',group,'/CDE_unique/CDE_ranking_per_disease',sep=''))
  out.log <- c(paste('../../result/',group,'/CDE_unique/LOG',sep=''))
  
  allFreq <- read.table(source.file,sep='\t',header = T,row.names=NULL)#table with each col represents a variable
  id1<-allFreq[,1]
  id2<-allFreq[,2]
  features<-allFreq[,3]
  rate <- allFreq[,4]
  
  #output cde ranking list for all diseases
  ranking <- list()
  for (cde in features){
    if(is.null(ranking[[cde]])){
      ranking[[cde]] = 0
    }
    ranking[[cde]] = ranking[[cde]] + 1
  }
  trans <- as.data.frame(ranking)
  ranking.df<-t(sort(trans,decreasing=T))
  write.table(ranking.df,out.rank,quote=F,sep='\t',col.names=F)
  
  #output ranking list: top 5 cde for each disease
  if(file.exists(out.per.disease)){
    file.remove(out.per.disease)
    print(paste("original file removed",out.per.disease))
  }
  num.per.disease <- 5
  for (disease in unique(id2)){
    candi = allFreq[id2==disease,]
    write.table(candi[order(candi[,4],decreasing=T)[1:num.per.disease],],out.per.disease,quote = F,row.names=F,col.names=F,append = T,sep='\t')
  }
  
  
  #plot feature popularity distribution
  features.table<-table(features)
  features.occur.freq<-features.table/length(unique(id1))
  pdf(out.pdf)
  plot(sort(features.occur.freq,decreasing = T),col="azure4",type='b',xlab='CEF Index',ylab='percentage of diseases shared',main='CEFs distribution between different mental diseases')
  abline(h=mean(features.occur.freq),lty=3)
  (mean(features.occur.freq)*length(unique(id1)))
  
  num.featuresInDisease<-table(id2)
  plot(sort(num.featuresInDisease,decreasing = T),type='b',xlab='Disease Index',ylab='CEF number',main='Number of CEFs each disease included',col="blue")
  x.names <- names(sort(num.featuresInDisease,decreasing = T))
  abline(h=mean(num.featuresInDisease),lty=3)
  
  features.unique <- features.table[features.table==1]
  features.unique<-names(features.unique)
  write.table(features.unique,out.list.unique.feature,quote=F,row.names=F)
  features.unique.fullList<-features %in% features.unique
  allFreq.unique <- allFreq[features.unique.fullList,]
  length(unique(allFreq.unique[,1]))
  
  features.unique.rate<-table(allFreq.unique[,2])/num.featuresInDisease*100
  features.unique.df <-as.data.frame(cbind(num.featuresInDisease,features.unique.rate))
  features.unique.df<-features.unique.df[order(num.featuresInDisease,decreasing=T),]
  
  #output common CEF list
  features.common <- features.table[features.table > mean(features.occur.freq)*length(unique(id1))]
  features.common <- as.matrix(features.common)
  features.common <- as.vector(attributes(features.common)$dimnames[[1]])
  features.common.table <-allFreq[features %in% features.common,]
  #cde.common.table<- as.numeric(cde.common.table)
  write.csv(features.common.table,out.csv.common.feature,quote=F,row.names=F)
  
  par(mar=c(5,4,4,5)+.1)
  plot(features.unique.df[,1],type="l",ylim=c(0,max(features.unique.df[,1])),lwd=2,col="red",xlab='',ylab='Total CEF numbers in each disease',col.lab="red")
  par(new=TRUE)
  barplot(features.unique.df[,2],ylim=c(0,100),col="gray",xaxt="n",yaxt="n",xlab="Disease Index",ylab="",col.lab='black',main="Disease-specific CEFs shared among mental health diseases")
  axis(4)
  mtext("Percentage of Disease-specific CEFs (%)",side=4,line=3)
  #legend("topright",col=c("red","blue"),lty=1,legend=c("y1","y2"))
  write.table(paste("current group is:",group,'\n',"the correlation between the disease-specific CEF and total CEF number is: ",(cor(features.unique.df[,1],features.unique.df[,2]))),out.log,quote=F,sep='\t',col.names=F)
  dev.off()
  #rm(list=ls())
}