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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



#add emissions
key.ems<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ems.rds")
ems<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.emission.rds")



#Y2003
#mod1 PM25

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)

#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
head(mod1)


#add lu-key
setkey(mod1,aodid)
setkey(key.ems,aodid)
mod1 <- merge(mod1, key.ems, all.x = T)
#add lu
setkey(mod1,emsid)
setkey(ems,emsid)
mod1 <- merge(mod1, ems, all.x = T)

y<-unique(mod1$emsid)
dim(y)
x<-inner_join(mod1,ems)
head(x)



################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2004
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2005
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2006
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2007
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2008
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2009
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2010
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2011
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2012
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





#Y2013
#mod1 PM25
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.clean.rds")



#mod1 PM10
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM10.rds")
#create aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
setkey(mod1,aodid)
setkey(reg,aodid)
mod1 <- merge(mod1,reg,all.x = T)

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1, c("stn","c"), 
      function(x) {
        mod1 <- lm(pm10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
mod1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM10.clean.rds")





#mod2 
mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")
#create aodid
mod2$aodid<-paste(mod2$long_aod,mod2$lat_aod,sep="-")
setkey(mod2,aodid)
setkey(reg,aodid)
mod2 <- merge(mod2,reg,all.x = T)
saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")

#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]



keep(rmse,reg, sure=TRUE) 
gc()





