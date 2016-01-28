#PCA, hclust 
#using disease table
#library(ggplot2)
library(ape)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))]) 
}

pdf("../../result/PCA_hclust/hclust.pdf")
table <-read.table("../../data/preparation/Did_CDE_table",sep = '\t',header = F)
table<- table[,-1555]
table.name <- table[1,2:dim(table)[2]]
table.obs<- table[2:dim(table)[1],1]
table.data <-table[-1,]
table.data <-table.data[,-1]
table.data <- as.matrix(table.data)
class(table.data) <-"numeric"

#table.data<-data.matrix(table.data, rownames.force = NA)

col.keep <- apply(table.data,2,sd)!=0
table.data <- table.data[,col.keep]
table.name <-table.name[,col.keep]
table.data <- scale(table.data)

Dname <- read.table("../../data/preparation/diseaseNumber",sep='\t',header=F)
new.labels <- Dname[,2][Dname[,1] %in% table.obs]

pca.out <- prcomp(table.data,scale = T)

plot(pca.out$x[,1:2], col=Cols(table.obs), pch=19,
     xlab="Principle Component 1",ylab="Principle Component 2",main="Projection of data onto the first and second Principle Components")
plot(pca.out$x[,c(1,3)], col=Cols(table.obs), pch=19,
     xlab="Principle Component 1",ylab="Principle Component 3",main="Projection of data onto the first and third Principle Components")

pve=100*pca.out$sdev^2/sum(pca.out$sdev^2)
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
       col =" blue ",main="Proportion of Variance Explained (PVE) of the principal components")
abline( v = 22,lty=3, col = "gray60")
plot(cumsum(pve), type="o", ylab="Cumulative PVE of the principal components", xlab="
Principal Component ", col =" brown3 ")
abline( v = 22,lty=3, col = "gray60")
sd.data=scale(table.data)
#par(mfrow=c(1,3))
data.dist=dist(sd.data)
par(cex=0.7)
hc <-hclust(data.dist)
hc$labels<-as.character(new.labels)
#plot(hc, labels=new.labels, main="Complete Linkage", xlab="", sub="",ylab="",hang=-1)
plot(as.phylo(hc), type="fan",main='Hierarchical Clustering using Complete Linkage')

#plot(hclust(data.dist, method="average"), labels=table.obs,main="Average Linkage", xlab="", sub="",ylab="")
#plot(hclust(data.dist, method="single"), labels=table.obs,main="Single Linkage", xlab="", sub="",ylab="")


par(cex=0.7)
hc.out=hclust(dist(pca.out$x[,1:22]))
hc.out$labels<-as.character(new.labels)
#plot(hc.out, labels=new.labels,hang=-1, main="Hier. Clust. on First Five Score Vectors ")
plot(as.phylo(hc.out), type="fan",main="Hierarchical Clustering using first 22 Principal Components")

write.table(sd.data,'../../result/PCA_hclust/scaled_data/standard matrix')
write.table(new.labels,"../../result/PCA_hclust/scaled_data/Disease labels")
write.table(table.name,"../../result/PCA_hclust/scaled_data/CECs")

dev.off()