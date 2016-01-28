#semantic type analysis
#using tokens
pdf("../../result/ST_analysis/ST.pdf")
library(ggplot2)

allFreq <- read.table("../../data/preparation/allFreq_disease",sep = '\t',header = T)
allST <- read.table("../../data/preparation/allST_sep",sep='\t')

attach(allFreq)

#how many CDEs in each ST
cde.in.ST<-list()
for (st in allST$V1){
  cde.candi <- CDE[grepl(st, SemanticType)] 
  cde.unique <- length(unique(cde.candi))#can be changed if want actuall cdes in ST
  cde.in.ST[[st]] <- cde.unique
}
cde.in.ST <- sort(unlist(cde.in.ST,use.names= T),decreasing = T)
#how to plot barlot
#barplot(cde.in.ST,cex.name=0.5,las=2)
mp<-barplot(cde.in.ST,main="Distribution of CDEs in each Semantic Type",ylab='CDE number',axisnames =F,axes=F)
abline(h=mean(cde.in.ST),lty=2)
text(mp, par('usr')[3], labels = names(cde.in.ST), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.6)
axis(2)
write.table(cde.in.ST,"../../result/ST_analysis/CDE_in_ST_total",quote= F,sep='\t',col.names=F)
#


occurrence.and.freq <-function(did,st){
  freq.sum <- sum(Freq[DiseaseName==did & grepl(st,SemanticType)])
  occur.sum <- length(Freq[DiseaseName==did & grepl(st,SemanticType)])
  vec <- c(freq.sum,occur.sum)
  return(vec)
}
#get the 1~5 most frequent ST for disease using freq and occurrence
did.freq.rank<-list()
did.occur.rank<-list()
for (did in unique(DiseaseName)){#
  occur.compare<-vector()
  freq.compare<-vector()
  for (i in 1:length(allST$V1)){
    st <- allST$V1[i]
    vec <- occurrence.and.freq(did,st)
    freq.compare[i]<-vec[1]
    occur.compare[i]<-vec[2]
  }
  if(sd(freq.compare)==0){print ("something may go wrong here!")}
  names(occur.compare)<-allST$V1
  names(freq.compare)<-allST$V1
  did.occur.rank[[did]] <- names(head(sort(occur.compare,decreasing = T),5))
  did.freq.rank[[did]] <- names(head(sort(freq.compare,decreasing = T),5))
}

#
ST.table <- data.frame(cde.in.ST)
num <- rep(0,length(row.names(ST.table)))
ST.table.freq<-cbind(ST.table,num,num,num,num,num)
ST.table.occur<-cbind(ST.table,num,num,num,num,num)
ST.sum <-list()
for (sig.level in 1:5){
  name <- paste("top",sig.level,sep='')
  colnames(ST.table.freq)[sig.level+1]<-name
  colnames(ST.table.occur)[sig.level+1]<-name
  cal.freq <- vector()
  cal.occur <- vector()
  i=1
  for (disease in names(did.freq.rank)){
    cal.freq[i] <- did.freq.rank[[disease]][sig.level]
    cal.occur[i] <- did.occur.rank[[disease]][sig.level]
    i=i+1
  }
  table.freq <- table(cal.freq)
  table.occur <- table(cal.occur)
  for (rownum in 1:length(names(table.freq))){
    ST.table.freq[[sig.level+1]][which(row.names(ST.table)==names(table.freq)[rownum])]=table.freq[[rownum]]
  }
  for (rownum in 1:length(names(table.occur))){
    ST.table.occur[[sig.level+1]][which(row.names(ST.table)==names(table.occur)[rownum])]=table.occur[[rownum]]
  }
}

ST.table.freq<- sweep(ST.table.freq,2,colSums(ST.table.freq),`/`)
ST.table.occur<-sweep(ST.table.occur,2,colSums(ST.table.occur),`/`)
#try to generate random sample to plot

x<-seq(1:30)
try1 <- cbind(x,ST.table.occur)*100#freq
arr1<-vector()
arr2<-vector()
for( i in 1:30){
  append1<-vector()
  for(j in 3:7){
    val2<-names(try1)[j]
    rep2<-try1[[j]][i]
    array2<-rep(val2,rep2)
    append1<-c(append1,array2)
  }
  
  val<-row.names(try1)[i]
  rep<-try1[[2]][i]#cde in st
  array1<-rep(val,rep)
  arr1<-c(arr1,array1)
  
  rep3<-rep#-length(append1)
  append2<-c(append1,rep("common",rep3))
  arr2<-c(arr2,append2)
}
sample<-as.matrix(arr1,arr2)
sample<-cbind(sample,arr2)
colnames(sample)<-c("all","rank")
sample<-data.frame(sample)
#qplot(sort(all,decreasing =F), data = sample, geom = "histogram", fill = rank)
k<-qplot(all, data = sample, geom = "histogram", fill = rank)
k + theme(axis.text.x = element_text(family = "",face="bold",colour="black",size=8,angle=45,vjust=1,hjust=1)) #+ scale_fill_brewer()


dev.off()