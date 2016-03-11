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

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.rds")
head(mod1)

library(ggmap)
map <- get_map(location = 'France',zoom=6)
x<-filter(mod1,is.na(ndvi))
mapPoints <- ggmap(map) +
   geom_point(data = x,aes(x = long_aod, y = lat_aod,size=5),  alpha = .5)
mapPoints

describe(mod1$wflag)
#delete water flags
#mod1<-filter(mod1,ndvi > 0)
mod1<-filter(mod1, wflag < 1)
#filter nasa
mod1<-filter(mod1,UN >0  & UN  <0.04)

#massimos thresholds
x<-select(mod1,aod,stn)
x$c<-1
x <- x %>%
    group_by (stn) %>%
        summarise(saod=sum(c))
#merge back count
setkey(x,stn)
setkey(mod1,stn)
mod1 <- merge(mod1,x, all.x = T)

mod1$exobs<-0
mod1<-mod1[aod < quantile(aod, c(.50)) & PM25new >  quantile(PM25new, c(.90)), exobs := 2]
mod1<-mod1[aod > quantile(aod, c(.90)) & PM25new <  quantile(PM25new, c(.50)), exobs := 3]
mod1<-mod1[aod > 1.2 , exobs := 4]
mod1<-mod1[saod < 30 , exobs := 5]

#take out bad exobs
mod1<-filter(mod1,exobs==0)


#pm10
mod1.pm10 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM10.rds")
head(mod1)


describe(mod1.pm10$wflag)
#delete water flags
#mod1.pm10<-filter(mod1.pm10,ndvi > 0)
mod1.pm10<-filter(mod1.pm10, wflag < 1)
#filter nasa
mod1.pm10<-filter(mod1.pm10,UN >0  & UN  <0.04)

#massimos thresholds
x<-select(mod1.pm10,aod,stn)
x$c<-1
x <- x %>%
    group_by (stn) %>%
        summarise(saod=sum(c))
#merge back count
setkey(x,stn)
setkey(mod1.pm10,stn)
mod1.pm10 <- merge(mod1.pm10,x, all.x = T)

mod1.pm10$exobs<-0
mod1.pm10<-mod1.pm10[aod < quantile(aod, c(.50)) & pm10 >  quantile(pm10, c(.90)), exobs := 2]
mod1.pm10<-mod1.pm10[aod > quantile(aod, c(.90)) & pm10 <  quantile(pm10, c(.50)), exobs := 3]
mod1.pm10<-mod1.pm10[aod > 1.2 , exobs := 4]
mod1.pm10<-mod1.pm10[saod < 30 , exobs := 5]

#take out bad exobs
mod1.pm10<-filter(mod1.pm10,exobs==0)



#take out station with wildly diff PM from surrounding stations
#neveruse <- c("REM","HEF","AGR") 
#mod1 <- mod1[!stn %in% neveruse]

# # take out stn with co located PM10/25 with very high ratios
# #calculate meanPM per grid per day to each station (excluding first station)
# PM25<-read.csv("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/PM/PMconcentrations.csv")
# PM25<-as.data.table(PM25)
# PM25[, day:=as.Date(strptime(Date, "%m/%d/%Y"))]
# PM25[, c := as.numeric(format(day, "%Y")) ]
# PM25<-filter(PM25,c==2013)
# str(PM25)
# PM25$stn<-as.character(PM25$stn)
# PM25<- PM25[PM10conc  == -999 , PM10conc  := 'NA']
# PM25<-PM25[PM25new > 0.000000000001 & PM25new < 900 ]
# #leave only stations with both PM2.5 and PM 10 measurements

# PM25$ratio=PM25[,PM25new]/PM25[,PM10conc]
# PM25[,badstn := paste(stn,day,sep="-")]
# #################BAD Site7Name
# mod1[,badstn := paste(stn,day,sep="-")]
# describe(PM25)
# PM25<- PM25[ratio < 0.95]
# ####Take out bad stations
# mod1 <- mod1[!(mod1$badstn %in% PM25$badstn), ] 
# 
# mod1[,elev.s:= scale(elev)]
# mod1[,tden.s:= scale(tden)]
# mod1[,pden.s:= scale(pden)]
# mod1[,dist2A1.s:= scale(dist2A1)]
# mod1[,dist2water.s:= scale(dist2water)]
# mod1[,dist2rail.s:= scale(dist2rail)]
# mod1[,Dist2road.s:= scale(Dist2road)]
# mod1[,ndvi.s:= scale(ndvi)]
# mod1[,MeanPbl.s:= scale(MeanPbl)]
# mod1[,p_ind.s:= scale(p_ind)]
# mod1[,p_for.s:= scale(p_for)]
# mod1[,p_farm.s:= scale(p_farm)]
# mod1[,p_dos.s:= scale(p_dos)]
# mod1[,p_dev.s:= scale(p_dev)]
# mod1[,p_os.s:= scale(p_os)]
# mod1[,tempa.s:= scale(Temp.im)]
# mod1[,WDa.s:= scale(WD.im)]
# mod1[,WSa.s:= scale(WS.im)]
# mod1[,RHa.s:= scale(RH.im)]
# mod1[,Raina.s:= scale(Rain.im)]
# mod1[,NOa.s:= scale(NO.im)]
# mod1[,O3a.s:= scale(O3.im)]
# mod1[,SO2a.s:= scale(SO2.im)]
# 
# 
# ################# month clean BAD stn PM25 and check if improved model?
# raWDaf <- ddply(mod1, c("stn"), 
#       function(x) {
#         mod1 <- lm(PM25new ~ aod, data=x)
#         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
#                    nsamps = length(summary(mod1)$resid))
# })
# raWDaf
# raWDaf<-as.data.table(raWDaf)
# setkey(raWDaf,stn)
# bad<- raWDaf[R2 <= 0.01]
# bad[,badid := paste(stn,sep="-")]
# #################BAD stn
# mod1[,badid := paste(stn,sep="-")]
# ####Take out bad stations
# mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 
# 
# 
# 
# 
# ################# month clean BAD stn PM25 and check if improved model?
# raWDaf <- ddply(mod1, c("stn","m"), 
#       function(x) {
#         mod1 <- lm(PM25new ~ aod, data=x)
#         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
#                    nsamps = length(summary(mod1)$resid))
# })
# raWDaf
# raWDaf<-as.data.table(raWDaf)
# bad<- raWDaf[R2 <= 0.001]
# bad[,badid := paste(stn,m,sep="-")]
# #################BAD stn
# mod1[,badid := paste(stn,m,sep="-")]
# ####Take out bad stations
# mod1 <- mod1[!(mod1$badid %in% bad$badid), ] 
# 
# 

#-------->>> loc stage
### add local LU data
#add met regions 
ems<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/lpm_lanuds/Emissions_per_stn.dbf"))
road<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/lpm_lanuds/RoadDensity_local.dbf"))
ndvi<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/lpm_lanuds/Localndvi.dbf"))
elev<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/lpm_lanuds/LocalAlt.dbf"))
setnames(ndvi,"RASTERVALU","ndviloc")
setnames(elev,"RASTERVALU","elevloc")

setkey(mod1,stn)
setkey(ems,stn)
mod1 <- merge(mod1, ems, all.x = T)
setkey(road,stn)
mod1 <- merge(mod1, road[,list(stn,MAJRDS_EU ,ROADS_EU)], all.x = T)
setkey(ndvi,stn)
mod1 <- merge(mod1, ndvi[,list(stn,ndviloc)], all.x = T)
setkey(elev,stn)
mod1 <- merge(mod1, elev[,list(stn,elevloc)], all.x = T)


saveRDS(mod1,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod1.AQ.2013.PMloc.rds")







