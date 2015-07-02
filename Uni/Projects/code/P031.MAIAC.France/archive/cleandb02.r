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

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.c.rds")

keep(mod1, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.rds")
   #take out stations in non contigious france
   mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
   #delete water flags
   mod1<-filter(mod1,wflag != 1)
#   mod1<-filter(mod1,UN >0  & UN  <0.04)
   #recreate aodid
   mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1[,dair.s:= scale(dair)]
mod1[,dport.s:= scale(dport)]
mod1[,dtrain.s:= scale(dist_train)]
mod1[,daroad.s:= scale(dist.aroad)]
mod1[,dcoast.s:= scale(dist.coast)]
mod1[,dwb.s:= scale(dist.wb)]
mod1[,NO2.s:= scale(NO2 )]
mod1[,SO2.s:= scale(SO2)]
mod1[,PM25ems.s:= scale(PM2.5)]
mod1[,PM10ems.s:= scale(PM10)]
mod1[,p.agric.s:= scale(p.agric)]
mod1[,p.open.s:= scale(p.open)]
mod1[,p.urban.s:= scale(p.urban)]
mod1[,p.forest.s:= scale(p.forest)]
mod1[,da1.s:= scale(distA1)]
mod1[,pbl.s:= scale(PBL)]
mod1[,temp.s:= scale(tempavg)]
mod1[,ws.s:= scale(wsavg)]

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")
gc()

#delete water flags
mod2<-filter(mod2,wflag != 1)
#mod2<-filter(mod2,UN >0  & UN  <0.04)
gc()

mod2[,dair.s:= scale(dair)]
mod2[,dport.s:= scale(dport)]
mod2[,dtrain.s:= scale(dist_train)]
mod2[,daroad.s:= scale(dist.aroad)]
mod2[,dcoast.s:= scale(dist.coast)]
mod2[,dwb.s:= scale(dist.wb)]
mod2[,NO2.s:= scale(NO2 )]
mod2[,SO2.s:= scale(SO2)]
mod2[,PM25ems.s:= scale(PM2.5)]
mod2[,PM10ems.s:= scale(PM10)]
mod2[,p.agric.s:= scale(p.agric)]
mod2[,p.open.s:= scale(p.open)]
mod2[,p.urban.s:= scale(p.urban)]
mod2[,p.forest.s:= scale(p.forest)]
mod2[,da1.s:= scale(distA1)]
mod2[,pbl.s:= scale(PBL)]
mod2[,temp.s:= scale(tempavg)]
mod2[,ws.s:= scale(wsavg)]

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.c.rds")

keep(mod1, sure=TRUE) 
gc()
