###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)

#import NAS
nascases<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_guid_2014.csv")

######## import pollution sets
pm2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2003.rds")
pm2003<-pm2003[,c(1,2,9),with=FALSE]
#subset to nas 
pm2003.nasse <- pm2003 [pm2003 $guid %in% nascases[,unique(guid)], ] 
rm(pm2003)

######## import pollution sets
pm2004<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2004.rds")
pm2004<-pm2004[,c(1,2,9),with=FALSE]
#subset to nas 
pm2004.nasse <- pm2004 [pm2004 $guid %in% nascases[,unique(guid)], ] 
rm(pm2004)

######## import pollution sets
pm2005<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2005.rds")
pm2005<-pm2005[,c(1,2,9),with=FALSE]
#subset to nas 
pm2005.nasse <- pm2005 [pm2005 $guid %in% nascases[,unique(guid)], ] 
rm(pm2005)

######## import pollution sets
pm2006<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2006.rds")
pm2006<-pm2006[,c(1,2,9),with=FALSE]
#subset to nas 
pm2006.nasse <- pm2006 [pm2006 $guid %in% nascases[,unique(guid)], ] 

rm(pm2006)

######## import pollution sets
pm2007<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2007.rds")
pm2007<-pm2007[,c(1,2,9),with=FALSE]
#subset to nas 
pm2007.nasse <- pm2007 [pm2007 $guid %in% nascases[,unique(guid)], ] 

rm(pm2007)

######## import pollution sets
pm2008<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2008.rds")
pm2008<-pm2008[,c(1,2,9),with=FALSE]
#subset to nas 
pm2008.nasse <- pm2008 [pm2008 $guid %in% nascases[,unique(guid)], ] 
rm(pm2008)

######## import pollution sets
pm2009<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2009.rds")
pm2009<-pm2009[,c(1,2,9),with=FALSE]
#subset to nas 
pm2009.nasse <- pm2009 [pm2009 $guid %in% nascases[,unique(guid)], ] 
rm(pm2009)

######## import pollution sets
pm2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2010.rds")
pm2010<-pm2010[,c(1,2,9),with=FALSE]
#subset to nas 
pm2010.nasse <- pm2010 [pm2010 $guid %in% nascases[,unique(guid)], ] 

rm(pm2010)

######## import pollution sets
pm2011<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2011.rds")
pm2011<-pm2011[,c(1,2,9),with=FALSE]
#subset to nas 
pm2011.nasse <- pm2011 [pm2011 $guid %in% nascases[,unique(guid)], ] 
rm(pm2011)

nasPM<- rbindlist(list(pm2003.nasse,pm2003.nasse,pm2004.nasse,pm2005.nasse,pm2006.nasse,pm2007.nasse,pm2008.nasse,pm2009.nasse,pm2010.nasse,pm2011.nasse))

saveRDS(nasPM,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN002_Exposure/nasPM.rds")






