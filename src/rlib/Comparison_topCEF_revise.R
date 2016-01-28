rm(list=ls())
source("./util.R")
target = 'Comparison_in_ex'
mkdir(paste("../../result/",target,"/topCEF/all/",sep=""))

target1 = 'inclusion'
target2 = 'exclusion'

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


#table: Most frequently used CEF (In/Ex/Joint) for all diseases

all.ex<-read.table(paste("../../result/",target2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
all.in<-read.table(paste("../../result/",target1,"/allData_table",sep=''),sep='\t',header=T)
ex.all<-readLines(paste("../../result/",target2,"/allCDE",sep=''))
in.all<-readLines(paste("../../result/",target1,"/allCDE",sep=''))

in.DiseaseName<-all.in[,2]
ex.DiseaseName<-all.ex[,2]
share.disease = intersect(in.DiseaseName,ex.DiseaseName)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]
all.in <- all.in[all.in[,2] %in% share.disease,]

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
write.table(ranking.df[order(as.numeric(ranking.df[,2]),decreasing=T),],out.rank,quote=F,sep='\t',col.names=F,row.names=F)


###########################
all.ex<-read.table(paste("../../result/",target2,"/allData_table",sep=''),header=T,sep='\t', row.names=NULL)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]
in.DiseaseName<-all.in[,2]
ex.DiseaseName<-all.ex[,2]
share.disease = intersect(in.DiseaseName,ex.DiseaseName)
all.ex <- all.ex[all.ex[,2] %in% share.disease,]


numlimitperdisease=20
#table: Most frequently used CEF (In/Ex/Joint) for each disease 
mkdir(paste("../../result/",target,"/topCEF/each/",sep=""))
out.top5.sig <- paste("../../result/",target,"/topCEF/each/intersection_sig_MI",sep='')
#output ranking list: top 5 cde for each disease
if(file.exists(out.top5.sig)){
  file.remove(out.top5.sig)
  print(paste("original file removed",out.top5.sig))
}
for (disease in share.disease){#share.disease
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
    add = sum(candi.in[candi.in$CDEs==i,4])
    candi.ex[candi.ex$CDEs==i,length(candi.ex[1,])]=candi.ex[candi.ex$CDEs==i,length(candi.ex[1,])]+add
  }
  write.table(candi.ex[order(candi.ex[,4],decreasing=T)[1:num.per.disease],],out.top5.sig,quote = F,row.names=F,col.names=F,append = T,sep='\t')
}
