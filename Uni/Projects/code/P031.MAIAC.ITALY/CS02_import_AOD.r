###############
#LIBS- load all necceseary libraries
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
library(dplyr)
library(readr)

# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#####create grid


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2003.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2003.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2003.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()

#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")
str(out)
#send clipping to Qgis (only have to do this once at first year!)
#create single aod point per aodid per day using dplyr syntax 
UClip <-out %>%
    group_by(aodid) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod))
tail(UClip)
write.csv(UClip,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/1.RAW/UClip.csv")
rm(UClip ,out)
gc()

#in Qgis we use the spatial query tool to select only points inside Italy in a sense clipping the data

#import back clipped grid
#import clipped grid
fullgrid<-read_csv("/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/keys.grids/italy.grid.csv")
head(fullgrid)



####################
## 2003
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2003.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2003.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2003.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2003.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2004
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2004.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2004.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2004.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2004.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2005
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2005.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2005.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2005.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2005.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2006
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2006.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2006.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2006.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2006.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2007
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2007.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2007.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2007.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2007.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2008
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2008.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2008.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2008.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2008.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2009
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2009.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2009.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2009.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2009.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2010
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2010.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2010.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2010.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2010.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2011
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2011.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2011.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2011.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2011.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2012
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2012.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2012.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2012.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2012.rds")
keep(fullgrid, sure=TRUE) 
gc()
####################
## 2013
###################


out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v02.2013.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h02v02.2013.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Italy_Aq.h01v03.2013.csv")

out<-rbind(out1,out2,out3)
rm(out1,out2,out3)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")

##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]

#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
#this command only leaves variables needed in future analysis (dplyr syntax)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)

#create single aod point per aodid per day (take our 2 obs per day occasionaly)
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2013.rds")
keep(fullgrid, sure=TRUE) 
gc()

