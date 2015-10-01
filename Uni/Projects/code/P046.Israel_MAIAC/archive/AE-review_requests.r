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


#create full database
data.m3.2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2003.pred3.rds")
data.m3.2004<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2004.pred3.rds")
data.m3.2005<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2005.pred3.rds")
data.m3.2006<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2006.pred3.rds")
data.m3.2007<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2007.pred3.rds")
data.m3.2008<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2008.pred3.rds")
data.m3.2009<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2009.pred3.rds")
data.m3.2010<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2010.pred3.rds")
data.m3.2011<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2011.pred3.rds")
data.m3.2012<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2012.pred3.rds")
data.m3.2013<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2013.pred3.rds")
mod3<-rbindlist(list(data.m3.2003,data.m3.2004,data.m3.2005,data.m3.2006,data.m3.2007,data.m3.2008,data.m3.2009,data.m3.2010,data.m3.2011,data.m3.2012,data.m3.2013))


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1C.AQ.PM25.pred.rds")
m1.all[,aodid:= paste(m1.all$long_aod.x,m1.all$lat_aod.x,sep="-")]
m1.all<-m1.all[,c("aodid","day","PM25","pred.m1","stn","c"),with=FALSE]

tlv<-m1.all[stn==c("ANT","IRD","YLB","REM","PTR","TMM","HOL")]
haf<-m1.all[stn==c("AHU","NSH","NES","AZB","BIA","BIN","ATA")]
jer<-m1.all[stn==c("AGR","BIL","EFR")]
