rm(list=ls())
source("./util.R")

target = "Comparison_in_ex"#'pubmed_ex'
#mkdir(paste("../../result/Comparison_in_ex/overlap/",sep=""))###
mkdir(paste("../../result/",target,"/overlap/",sep=""))###


in.file <- "inclusion"#"inclusion",'pubmed'
ex.file <- "exclusion"
source.file.in <-c(paste("../../result/",in.file,"/allData_table",sep = ""))
source.file.ex <-c(paste("../../result/",ex.file,"/allData_table",sep = ""))
source.file.st.in <-c(paste("../../result/",in.file,"/allST_sep",sep = ""))
source.file.st.ex <-c(paste("../../result/",ex.file,"/allST_sep",sep = ""))

all.in <- read.table(source.file.in,sep='\t',header = T,row.names=NULL)
all.ex <- read.table(source.file.ex,sep='\t',header = T,row.names=NULL)
all.st.in <- read.table(source.file.st.in,sep='\t',row.names=NULL)
all.st.ex <- read.table(source.file.st.ex,sep='\t',row.names=NULL)

in.id1<-all.in[,1]
in.DiseaseName<-all.in[,2]
in.CDE<-all.in[,3]
in.Freq<-all.in[,4]
in.st <-all.in[,5]
ex.id1<-all.ex[,1]
ex.DiseaseName<-all.ex[,2]
ex.CDE<-all.ex[,3]
ex.Freq<-all.ex[,4]
ex.st<-all.ex[,5]

#output combined ST list
#out.list.st<-c(paste('../../result/allST_combine',sep=''))
#st <- union(all.st.ex$V1,all.st.in$V1)
#write.table(st,out.list.st,row.names=F,col.names=F,quote=F)

total.id<- union(in.DiseaseName,ex.DiseaseName)

compare.plot.overlap<-function(st=x.axis.group.overall,in.st=x.axis.group.in,ex.st=x.axis.group.ex){
  cde.num.ST.in<-list()
  for (st1 in st){
    cde.candi.in <- in.CDE[grepl(st1, in.st)] 
    cde.num.ST.in[[st1]] <- unique(cde.candi.in)
  }
  cde.num.ST.ex<-list()
  for (st1 in st){
    cde.candi.ex <- ex.CDE[grepl(st1, ex.st)] 
    cde.num.ST.ex[[st1]] <- unique(cde.candi.ex)
  }
  #generate dataset for plotting
  for.plot <- data.frame(st="",section="",stringsAsFactors=F)
  j=0
  for (st1 in st){
    inter <- length(intersect(cde.num.ST.ex[[st1]],cde.num.ST.in[[st1]]))
    for (i in 0:(inter-1)){
      j = j+1
      add<-c(st1,"Intersect")
      for.plot[j,]<-add
    }
    unique.in <-length(cde.num.ST.in[[st1]])-inter
    for (i in 0:(unique.in-1)){
      j = j+ 1
      add<-c(st1,in.file)#"Inclusion"
      for.plot[j,]<-add
    }
    unique.ex <-length(cde.num.ST.ex[[st1]])-inter
    for (i in 0:(unique.ex-1)){
      j=j+1
      add<-c(st1,"Exclusion")
      for.plot[j,]<-add
    } 
  }
  return (for.plot)
}

library(ggplot2)
#disease
out.pdf.discription <- c(paste('../../result/',target,'/overlap/In_Ex_compare_disease.pdf',sep=''))
pdf(out.pdf.discription)
out.table1<-c(paste('../../result/',target,'/overlap/In_Ex_compare_disease_table',sep=''))

x.axis.group.overall = total.id
x.axis.group.in = in.DiseaseName
x.axis.group.ex = ex.DiseaseName
for.plot<-compare.plot.overlap(x.axis.group.overall,x.axis.group.in,x.axis.group.ex)
write.table(for.plot,out.table1,sep="\t",row.names=F,col.names=F,quote=F)

myPalette<-c('chocolate1','palegreen3','skyblue2')
k<-qplot(reorder(for.plot$st,for.plot$section,length), data = for.plot,geom = "histogram", fill = section,xlab='Mental Disorders',ylab='CEF counts',main='Overlapping between Exclusion/Inclusion CEFs')
k + theme(axis.text.x = element_text(family = "",face="bold",colour="black",size=4,angle=45,vjust=1,hjust=1))+scale_fill_manual(values=myPalette)# + scale_fill_brewer()
dev.off()

#ST
library(ggplot2)
out.table2 <- c(paste("../../result/",target,"/overlap/In_Ex_compare_st_table",sep=''))
out.pdf.discription2 <- c(paste('../../result/',target,'/overlap/In_Ex_compare_st.pdf',sep=''))
pdf(out.pdf.discription2)

x.axis.group.overall = st
x.axis.group.in = in.st
x.axis.group.ex = ex.st
for.plot<-compare.plot.overlap(x.axis.group.overall,x.axis.group.in,x.axis.group.ex)
write.table(for.plot,out.table2,sep="\t",row.names=F,col.names=F,quote=F)

k<-qplot(st, data = for.plot,geom = "histogram", fill = section)
k + theme(axis.text.x = element_text(family = "",face="bold",colour="black",size=8,angle=45,vjust=1,hjust=1)) #+ scale_fill_brewer()
dev.off()