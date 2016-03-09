used <- fread("/media/NAS/Uni/Projects/P057_helena_Vertprofile/work/France/fr025_subset.csv", stringsAsFactors = FALSE)
used<-as.data.table(used)

cal = readRDS("/media/NAS/Uni/Projects/P057_helena_Vertprofile/work/France/France.calipso.rds")
cal<-as.data.table(cal)

setnames(cal,"day","DAY")
setnames(cal,"date","day")

setwd("/media/NAS/Uni/Projects/P057_helena_Vertprofile/work/France/A")
filenames <- list.files( pattern="*.rds", full.names=TRUE)
tmp.2<-NULL

for (I in 1:length(filenames)) {
  
  y<-substr(filenames[I],11,14)
  
  tmp<-readRDS(filenames[I])
  tmp<-as.data.table(tmp)
  tmp1<-na.omit(tmp)
  tmp.1<-tmp1[,c("aodid","day","PBL"),with=FALSE]
  tmp.1 <- tmp.1[tmp.1$aodid %in% used$aodid, ]
  tmp.1 <- tmp.1[tmp.1$day %in% cal$day, ]
  tmp.2<-rbind(tmp.2,tmp.1)
  
  print(paste("Finished year", I))
  name=paste("PBL.",y,sep="")
  assign(name,tmp.2)
  
}

pbl<-tmp.2
saveRDS(pbl,"P:/work/France/A/PBL_France.rds")

new_var<-merge(cal,pbl,by=c("aodid","day"))