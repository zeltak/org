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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")
#the 'right' grid numbers
#282622*365=103157030
#for leap= 282622*366= 103439652




#y2000
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2000.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2000")
rm(list = ls(all = TRUE))
gc()

#y2001
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2001.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2001")
rm(list = ls(all = TRUE))
gc()

#y2002
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2002.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2002")
rm(list = ls(all = TRUE))
gc()

#y2003
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2003.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2003")
rm(list = ls(all = TRUE))
gc()

#y2004
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2004.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2004")
rm(list = ls(all = TRUE))
gc()

#y2005
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2005.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2005")
rm(list = ls(all = TRUE))
gc()

#y2006
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2006.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2006")
rm(list = ls(all = TRUE))
gc()

#y2007
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2007.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2007")
rm(list = ls(all = TRUE))
gc()

#y2008
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2008.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2008")
rm(list = ls(all = TRUE))
gc()

#y2009
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2009.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2009")
rm(list = ls(all = TRUE))
gc()

#y2010
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2010.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2010")
rm(list = ls(all = TRUE))
gc()

#y2011
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2011.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2011")
rm(list = ls(all = TRUE))
gc()

#y2012
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2012.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2012")
rm(list = ls(all = TRUE))
gc()

#y2013
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2013.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2013")
rm(list = ls(all = TRUE))
gc()

#y2014
mod3best<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2014.rds")
#create single aod point per aodid per day (this addresses cartesean error below)
m3best <-mod3best %>%
    group_by(GUID,day) %>%
    summarise_each(funs(mean),Long,Lat,bestpred )
print(dim(m3best))
setnames(m3best,"bestpred","pm25_final")
summary(m3best$pm25_final)
saveRDS(m3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2014")
rm(list = ls(all = TRUE))
gc()
