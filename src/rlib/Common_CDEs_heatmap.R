library(gplots)
#use disease table
table <-read.table("../../data/preparation/Did_CDE_table",sep = '\t',header = F)
table<- table[,-1555]
table.name <- table[1,2:dim(table)[2]]
table.obs<- table[2:dim(table)[1],1]
table.data <-table[-1,]
table.data <-table.data[,-1]

pdf("../../result/Common_CDE/Common_CDE.pdf")

table.allone <- as.matrix(table.data)
table.allone[table.allone>0]<-1
class(table.allone)<-"numeric"
col.keep2 <- colSums(table.allone)>6


col.keep1 <- apply(table.data,2,sd)!=0 & col.keep2=="TRUE"
#which(col.keep1=="FALSE") %in% which(col.keep2=="FALSE")
table.data <- table.data[,col.keep1]
table.data<- as.matrix(table.data)
class(table.data) <- "numeric"
table.data <- scale(table.data)

table.name <-table.name[,col.keep1==TRUE]
table.name <-as.vector(as.matrix(table.name))
colnames(table.data)<-table.name

Dname <- read.table("../../data/preparation/diseaseNumber",sep='\t',header=F)
new.labels <- Dname[,2][Dname[,1] %in% table.obs]
rownames(table.data)<- new.labels

heatmap.2(table.data,main="Pattern in common CEFs and Mental Health Diseases",col=rainbow(ncol(table.data), start=0, end=1), scale="row", key=F,trace="none",cexCol=0.3,cexRow=0.3,dendrogram = "none",lhei = c(0.1,0.9),lwid=c(0.1,0.9))
#heatmap.2(table.data,col=redgreen(75), scale="row", key=T, keysize=1.5, trace="none",cexCol=0.7)
#heatmap.2(table.data,col=rainbow(ncol(table.data), start=0, end=1), scale="row", key=T,trace="none",cexCol=0.3,cexRow=0.3)
#heatmap.2(table.data,col=redgreen(75), scale="row", key=T, keysize=1.5, trace="none",cexCol=0.3,cexRow=0.3)
dev.off()
