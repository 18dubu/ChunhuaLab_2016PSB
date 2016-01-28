library(igraph)
library(splines)
library(survival)
library(tnet)#network analysis
source("./util.R")
groups=c('pubmed','inclusion','exclusion')#c("inclusion","exclusion")
mode="CEF_center"#'Disease_center,CEF_center

for (group in groups){

  source.file <-c(paste("../../result/",group,"/allData_table",sep = ""))
  source("./util.R")
  mkdir(paste("../../result/",group,"/network_analysis/",sep=""))
  out.table <- c(paste('../../result/',group,'/network_analysis/node_degree_table_',mode,sep=''))
  
  allFreq <- read.table(source.file,sep='\t',header = T, row.names=NULL)
  if (mode =='CEF_center'){
    allFreq_clean = allFreq[,c(3,2,4)]
  }
  if (mode =='Disease_center'){
    allFreq_clean = allFreq[,c(2,3,4)]
  }
  #find duplicate
  test<-paste(allFreq_clean[,1],allFreq_clean[,2],sep='~')
  allFreq_clean<-allFreq_clean[!duplicated(test),]
  
  DiseaseName=allFreq_clean[,1]
  CDE=allFreq_clean[,2]
  Freq=allFreq_clean[,3]
  
  #disease and cde list
  disease<-unique(DiseaseName)
  diseaseList<-1:length(disease)
  names(diseaseList)<-disease
  
  index2disease<-disease
  names(index2disease)<-1:length(disease)
  
  cde<-unique(CDE)
  cdeList<-1:length(cde)
  names(cdeList)<-cde
  
  
  index.table<-data.frame()
  a<-NULL
  b<-NULL
  c<-NULL
  for (i in 1:nrow(allFreq_clean)){
    a<-append(a,diseaseList[[DiseaseName[i]]])
    b<-append(b,cdeList[[CDE[i]]])
    c<-Freq
  }
  index.table<-data.frame(a,b,c)
  net <- as.tnet(index.table, type="weighted two-mode tnet")
  
  # Calculate two-mode degree
  out <- degree_tm(net, measure="degree")
  
  # Create one-mode projection
  net1 <- projecting_tm(net, "Newman")
  
  # Calculate one-mode degree
  tmp <- degree_w(net1)[,"degree"]
  
  # Append to table
  out <- data.frame(out, onemodedegree=tmp)
  
  # Calculate closeness and append to table
  tmp <- closeness_w(net1 )[,"closeness"]
  out <- data.frame(out, closeness=tmp)
  
  # Calculate betweenness and append to table
  tmp <- betweenness_w(net1 )[,"betweenness"]
  out <- data.frame(out, betweenness=tmp)
  
  #set node name
  out[,"node"]<-disease
  
  write.table(out,out.table,sep='\t',quote=F,row.names=F)
}
# Pair-wise correlation table
#tmp <- matrix(nrow=4, ncol=4)
#tmp[lower.tri(tmp)] <- apply(which(lower.tri(tmp), arr.ind=TRUE)+1, 1, function(a) cor.test(out[,a[1]], out[,a[2]])$estimate)

###########
source.in <- c(paste('../../result/inclusion/network_analysis/node_degree_table_',mode,sep=''))
source.ex <- c(paste('../../result/exclusion/network_analysis/node_degree_table_',mode,sep=''))
source.pubmed <- c(paste('../../result/pubmed/network_analysis/node_degree_table_',mode,sep=''))
network.in<-read.table(source.in,header=T,sep='\t')
network.ex<-read.table(source.ex,header=T,sep='\t')
network.pubmed<-read.table(source.pubmed,header=T,sep='\t')
out.pdf=c(paste('../../result/network_In_Ex_Pubmed/network_compare_',mode,'.pdf',sep=''))
pdf(out.pdf)

out.compare=c(paste('../../result/network_In_Ex_Pubmed/network_compare_table_',mode,sep=''))
rm(out.compare)
write.table(paste('Data','Network Attributes',sep='\t'),out.compare,col.names=F,row.names=F,quote=F)
write.table(paste('','Disease in network','Degree',	'One mode degree',	'Closeness', 'Betweenness',sep='\t'),out.compare,col.names=F,row.names=F,quote=F,append=T)
write.table(paste('Inclusion',length(network.in[,1]),paste(format(colMeans(network.in[2:5]),digits=6),format(lapply(network.in[,2:5],sd),digits=6),sep='~',collapse='\t'),sep='\t'),out.compare,col.names=F,row.names=F,append=T,quote=F)
write.table(paste('Exclusion',length(network.ex[,1]),paste(format(colMeans(network.ex[2:5]),digits=6),format(lapply(network.ex[,2:5],sd),digits=6),sep='~',collapse='\t'),sep='\t'),out.compare,col.names=F,row.names=F,append=T,quote=F)
write.table(paste('Pubmed',length(network.pubmed[,1]),paste(format(colMeans(network.pubmed[2:5]),digits=6),format(lapply(network.pubmed[,2:5],sd),digits=6),sep='~',collapse='\t'),sep='\t'),out.compare,col.names=F,row.names=F,append=T,quote=F)
(colMeans(network.in[2:5]))
(colMeans(network.ex[2:5]))
(colMeans(network.pubmed[2:5]))

ex<- displ$new(network.ex[,3])
inc<- displ$new(network.in[,3])
pub<- displ$new(network.pubmed[,3])
##Estimate the cut-off
est1<-estimate_xmin(ex)
est2<-estimate_xmin(inc)
est3<-estimate_xmin(pub)
ex$setXmin(est1)
inc$setXmin(est2)
pub$setXmin(est3)
plot(ex,col='chocolate1',xlim=c(5,4000),main='Degree Distribution of three networks',xlab='Degree (log)',ylab='Frequency (log)')
lines(ex, lty=1,col='chocolate1',lwd=3)
d = plot(inc, draw=FALSE)
points(d$x,d$y,col='palegreen3')
lines(inc,lty=2,col='palegreen3',lwd=3)
d = plot(pub, draw=FALSE)
points(d$x,d$y,col='purple1')
lines(pub,lty=3,col='purple1',lwd=3)
legend(800, 0.95, c("Exclusion",'Inclusion','Pubmed'), col = c('chocolate1','palegreen3','purple1'),text.col = "black",lty = c(1, 1,1),cex=0.9,bg = "gray90")

dev.off()

