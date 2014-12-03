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
library(FNN)
#snipps
source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")




##################################
#2003
##################################
basegrid <-  fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")
# #to create a date range based on start and end points use
# days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
# #create date range
# mod3grid <- data.table(expand.grid(aodid = basegrid[, unique(aodid)], day = days_2003))
# setkey(mod3grid,aodid)
# setkey(basegrid,aodid)
# mod3grid <- merge(mod3grid,basegrid)
# 
# if(!exists("m2_agg")){
#   m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
# }
# 
# #subset to study area
# mod3grid.se <- mod3grid [mod3grid $aodid %in% m2_agg$aodid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/media/NAS/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
#saveRDS(mod3grid.se,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")
mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")


#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
#num. of obsv per year per stn
PM25[,length(na.omit(PM25)),by=list(stn,c)]
#PM25_m means avialble obs per year
PM25[, PM25_n := length(na.omit(PM25)),by=list(stn,c)]
#clear non PM25 days
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
PM25 <- PM25[PM25_n > 5  , ]
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2003<- PM25[c==2003]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2003[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2003 <- pmall2003[pmall2003$stn %in% table_temp$stn, ] 


#create PM matrix
pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")

### load grid
aodf.2003.tmp.s9<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2003.rds")
setkey(aodf.2003.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2003.tmp.s9[aodf.2003.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            aodf.2003.tmp.s9, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM", "PM25", knearest = 18, maxdistance = NA)

#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,meanPM :=NULL]
closestaodse[,meanPMknn:=NULL]
closestaodse[,meanPMnobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2003.tmp.s9,aodid,day)
mod3grid <- merge(aodf.2003.tmp.s9,closestaodse,all.x = T)

#check data completness
x1<-mod3grid[, .N, by=c("aodid")]
summary(x1)
saveRDS(midmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")

#clear workspace
rm(list = ls())
gc()
