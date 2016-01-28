rm(list=ls())
source("./util.R")
target = 'Comparison_pubmed_ex'
mkdir(paste("../../result/",target,"/topCEF/all/",sep=""))

target1 = 'pubmed'
target2 = 'exclusion'
ref1 = 'inclusion'
ref2 = 'exclusion'
mi.cutoff=0.00000
numlimitperdisease=40
benchmark = 1

count_occur<-function(features,restrict=0){
  ranking <- list()
  for (cde in features){
#    if(!(cde %in% restrict)){##
#      next##
#    }##
    if(is.null(ranking[[cde]])){
      ranking[[cde]] = 0
    }
    ranking[[cde]] = ranking[[cde]] + 1
  }
  trans <- data.frame(unlist(ranking))
  trans<-data.frame(CEF=attributes(trans)$row.names,COUNT=trans$unlist.ranking.)
  return (trans[order(trans[,2],decreasing=T),])#
}


mapST<-function(listCDE,mappingTable){
  mapping <- read.table(mappingTable,sep='\t',header=T,row.names=NULL)
  mapping.cde <- mapping[,1]
  mapping.st <- mapping[,2]
  listST = array()
  j=1
  index = array()
  for (i in listCDE){
    index[j] = which(i == mapping.cde)
    j =j+1
  }
  listST <- mapping.st[index] 
  
  if(length(listST) == length(listCDE)){
    return(listST)
  }
  else{
    print ("input/output length differ\n")
    return(0)
  }
}

compareList.findSig.MI<-function(primaryDF,secondaryDF){
  #inputs are two combined dataframe which have two columns:first for document name, second for word name
  #function finds the significant terms for primary list by excluding those which are also frequently occur in secondary list using Mutual Information
  #n11:num of doc that contains vocab, belongs to p; n10: num of doc that contains vocab, belongs to s; n00: num of doc that contains no vocab, belongs to s; n01: num of doc that contains no vocab, belongs to p
  p<-primaryDF #ex: adjustment-disorders                    allergy severity - severe
  s<-secondaryDF#in: adjustment-disorders                          malignant neoplasms
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

###MI
all.ref1<-read.table(paste("../../result/",ref1,"/allData_table",sep=''),sep='\t',header=T)
all.ref2<-read.table(paste("../../result/",ref2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
out.mi<- paste("../../result/",target,"/topCEF/all/MI_table_candi",sep='')
out.loss<- paste("../../result/",target,"/topCEF/all/MI_table_toploss",sep='')
EX<-all.ref2[,c(2,3)]
IN<-all.ref1[,c(2,3)]
mi<-compareList.findSig.MI(EX,IN)
###mi
##benchmark
if (benchmark==1){
  pdf(paste("../../result/",target,"/topCEF/all/MI_choose_cutoff.pdf",sep=''))
  mi.bench<-mi
  step<-(max(mi.bench[,2])-min(mi.bench[,2]))/100
  countlist1<-list()
  countlist2<-list()
  for (cut in seq(min(mi.bench[,2])-0.001,max(mi.bench[,2]),by=step)){
    countlist1[[as.character(cut)]]<-length(mi.bench[mi.bench[,2]>=cut,][,2])
    countlist2[[as.character(cut)]]<-mean(mi.bench[mi.bench[,2]>=cut,][,3])
  }
  tmp<-data.frame(unlist(countlist1))
  dat1<-data.frame(as.numeric(row.names(tmp)),tmp)
  tmp<-data.frame(unlist(countlist2))
  dat2<-data.frame(as.numeric(row.names(tmp)),tmp)
  plot(dat1,log='y',ylim=c(1,3000),xlab='MI cutoff',ylab='CEF count (log)',main='Distribution of candicate CEFs with different MI cutoff',type='l',col='chocolate1',lwd=2)
  #lines(dat2)
  dev.off()
}
#mi cutoff
mi<-mi[mi[,2]>mi.cutoff,]
print(paste(length(mi[,2]),'CEF left',sep=' '))
write.table(mi,out.mi,quote = F,row.names=F,col.names=F,sep='\t')
###TABLE: Most frequently used CEF (In/Ex/Joint) for all diseases
all.ex<-read.table(paste("../../result/",target2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
all.ex.ori<-all.ex
all.in<-read.table(paste("../../result/",target1,"/allData_table",sep=''),sep='\t',header=T)
ex.all<-readLines(paste("../../result/",target2,"/allCDE",sep=''))
in.all<-readLines(paste("../../result/",target1,"/allCDE",sep=''))

###PRUNE CEF
loss.ex<-data.frame()
for (current in all.ex[,3]){
  if(length(mi[mi[,1]==current,][,1])==0){
    loss.ex<-rbind(loss.ex,all.ex[all.ex[,3]==current,])
    all.ex<-all.ex[!(all.ex[,3]==current),]
  }
}
top.loss<-data.frame(sort(table(loss.ex[,3]),decreasing=T)[1:20])
count.ex<-lapply(row.names(top.loss), function(x) length(unique(all.ex.ori[all.ex.ori[,3]==x,][,2])))
count.in<-lapply(row.names(top.loss), function(x) length(unique(all.ref1[all.ref1[,3]==x,][,2])))
t<-data.frame(row.names(top.loss),top.loss,unlist(count.in))
colnames(t)<-c('Top lost CEF','Count in Ex','Count in In')
write.table(t,out.loss,quote = F,row.names=F,col.names=T,sep='\t')

in.DiseaseName<-all.in[,2]
ex.DiseaseName<-all.ex[,2]
share.disease = intersect(in.DiseaseName,ex.DiseaseName)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]
all.in <- all.in[all.in[,2] %in% share.disease,]
###prune cef
in.CDE<-all.in[,3]
in.Freq<-all.in[,4]
in.st <-all.in[,5]
ex.CDE<-all.ex[,3]
ex.Freq<-all.ex[,4]
ex.st<-all.ex[,5]

inter <- intersect(ex.all,in.all)
ex.unique<-setdiff(ex.all,inter)
in.unique<-setdiff(in.all,inter)
mappingTable <- paste('../../result/cde_st_table_',target1,'_',target2,sep='') #in
out.rank<- paste("../../result/",target,"/topCEF/all/intersection_sig_MI",sep='') #out

ranking.df1<-count_occur(ex.CDE,inter)
ranking.df2<-count_occur(in.CDE,inter)
union.all<-union(ranking.df1[,1],ranking.df2[,1])
ranking.df<-list()

for (i in union.all){
  if(!(i %in% ranking.df1[,1])){
    a<-data.frame(CEF=i,COUNT=0)
    ranking.df1=rbind(ranking.df1,a)
  }
}

for (i in union.all){
  if(!(i %in% ranking.df2[,1])){
    a<-data.frame(CEF=i,COUNT=0)
    ranking.df2=rbind(ranking.df2,a)
  }
}

ranking.df<-merge(ranking.df1,ranking.df2,by='CEF')

listCDE<-ranking.df[,1]
mapped.st<- mapST(listCDE,mappingTable)
ranking.df <- data.frame(ranking.df,mapped.st,stringsAsFactors=F)
names(ranking.df)<-c('CEF','#Exclusion occurrence','#Pubmed occurrence','Semantic Type')
write.table(ranking.df[order(ranking.df[,2],ranking.df[,3],decreasing=T),],out.rank,quote=F,sep='\t',col.names=T,row.names=F)


###########################
all.ex<-read.table(paste("../../result/",target2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
share.disease = intersect(in.DiseaseName,ex.DiseaseName)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]
for (current in all.ex[,3]){
  if(length(mi[mi[,1]==current,][,1])==0){
    all.ex<-all.ex[!(all.ex[,3]==current),]
  }
}
in.DiseaseName<-all.in[,2]
ex.DiseaseName<-all.ex[,2]
#table: Most frequently used CEF (In/Ex/Joint) for each disease 
mkdir(paste("../../result/",target,"/topCEF/each/",sep=""))
out.top5.sig <- paste("../../result/",target,"/topCEF/each/intersection_sig_MI_top",numlimitperdisease,sep='')
#output ranking list: top 5 cde for each disease
if(file.exists(out.top5.sig)){
  file.remove(out.top5.sig)
  print(paste("original file removed",out.top5.sig))
}
write.table(paste('id','DiseaseName','CEF','Freq Ex','Semantic Type','Pubmed',sep='\t'),out.top5.sig,quote = F,row.names=F,col.names=F)
for (disease in unique(share.disease)){#share.disease
  num.per.disease <- numlimitperdisease
  candi.in = all.in[in.DiseaseName==disease,]
  candi.ex = all.ex[ex.DiseaseName==disease,]
  Pubmed = rep(0,length(candi.ex[,1]))
  candi.ex = data.frame(candi.ex,Pubmed)
  union <- union(candi.in$CDEs,candi.ex$CDEs)
  inter <- intersect(candi.in$CDEs,candi.ex$CDEs)
  if (length(union)==0){next}
  if (length(union)<num.per.disease){num.per.disease=length(union)}
  ex.unique<-setdiff(candi.ex$CDEs,inter)
  in.unique<-setdiff(candi.in$CDEs,inter)
  for (i in inter){
    add = sum(candi.in[candi.in$CDEs==i,4]) #pubmed count,get rid of duplicate
    candi.ex[candi.ex$CDEs==i,length(candi.ex[1,])]<-candi.ex[candi.ex$CDEs==i,length(candi.ex[1,])]+add
  }
  write.table(candi.ex[order(candi.ex[,4],candi.ex[,6],decreasing=T)[1:num.per.disease],],out.top5.sig,quote = F,row.names=F,col.names=F,append = T,sep='\t')
}
