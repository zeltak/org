###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
library(zoo)
library(DataCombine)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

# "sticky" DF's
#load clipped italy grid 
fgrid <- fread("/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/keys.grids/italy.grid.csv")


#########Aqua.2010



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2010<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_Italy/2.work/WORKDIR/AOD.AQ.2010.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2010<-as.data.frame(aqua.2010)
aqua.2010<-as.data.table(aqua.2010)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
#create date range
days2010 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2010,aodid,day)
setkey(days2010 ,aodid,day)
aqm1.2010 <- merge(days2010,aqua.2010, all.x = T)  
tail(aqm1.2010)

#add lu-key
setkey(aqm1.2010,aodid)
setkey(key.lu,aodid)
aqm2.2010 <- merge(aqm1.2010, key.lu, all.x = T)
#add lu
setkey(aqm2.2010,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2010 <- merge(aqm2.2010, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2010$long_aod <- NULL
aqm2.2010$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2010 ,aodid)
aqm2.2010 <- merge(aqm2.2010 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2010,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2010)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2010, aodid)
lu.m <- makepointsmatrix(aqm2.2010[aqm2.2010[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2010, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2010,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2010 <- merge(aqm2.2010, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2010$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2010, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2010,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2010 <- merge(aqm2.2010, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2010$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2010, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2010,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2010 <- merge(aqm2.2010, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2010$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2010, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2010,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2010 <- merge(aqm2.2010, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2010$rainday)

#saveRDS(aqm2.2010,"/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2010,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2010$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/italy/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2010)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2010,aodid)
setkey(key.pbl,aodid)
aqm2.2010 <- merge(aqm2.2010, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2010,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2010 <- merge(aqm2.2010, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2010$pblid<-NULL
#add month
aqm2.2010[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2010$season<-recode(aqm2.2010$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2010$seasonSW<-recode(aqm2.2010$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/italy/ndvi_italy/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2010)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2010,aodid)
setkey(key.ndvi,aodid)
aqm2.2010 <- merge(aqm2.2010, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2010,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2010 <- merge(aqm2.2010, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2010,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()



#Fix LAT/LONG
setkey(aqm2.2010,aodid)
setkey(fgrid,aodid)
aqm2.2010 <- merge(aqm2.2010,fgrid[,list(aodid,long_aod,lat_aod)],all.x = T)


#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2010)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2010)


#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2010, aodid)
aod.m <- makepointsmatrix(aqm2.2010[aqm2.2010[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2010, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2010,aodid,day)
aqm2.2010 <- merge(aqm2.2010,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2010,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2010, aodid)
aod.m <- makepointsmatrix(aqm2.2010[aqm2.2010[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2010, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2010,aodid,day)
aqm2.2010 <- merge(aqm2.2010,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2010,"closestmean","meanPM10")
summary(aqm2.2010$meanPM10)
#cleanup
keep(fgrid,aqm2.2010,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#import regions
reg<-read.dbf("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/Qgis/italy_grid_region.dbf")
reg<-as.data.table(select(reg,aodid,reg,region))
#create aodid
#aqm2.2010$aodid<-paste(aqm2.2010$long_aod,aqm2.2010$lat_aod,sep="-")
setkey(aqm2.2010,aodid)
setkey(reg,aodid)
aqm2.2010 <- merge(aqm2.2010,reg,all.x = T)

#---------> save mod3
#clean
aqm2.2010[,c("ndviid"):=NULL]
saveRDS(aqm2.2010,"/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod3.AQ.2010.rds")

#########-------------------weights ############
aqm2.2010<-aqm2.2010[,obs:=1]
aqm2.2010[is.na(aod), obs:= 0]
ws.2010<-select(aqm2.2010,obs,elev_m,PBL,m,tempavg,aodid,day)
ws.2010<-filter(ws.2010,!(is.na(tempavg)))
rm(aqm2.2010)
gc()

#splits
ws.2010.s1<-ws.2010[1:50000000,]
w1.s1<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2010.s1)
ws.2010.s1$prob <- predict(w1.s1,type = c("response"))  
ws.2010.s1$wt <- 1/ws.2010.s1$prob
ws.2010.s1$normwt <- ws.2010.s1$wt/mean(ws.2010.s1$wt)
ws.2010.s1[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s1)
gc()


#splits
ws.2010.s2<-ws.2010[50000001:100000000,]
w1.s2<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2010.s2)
ws.2010.s2$prob <- predict(w1.s2,type = c("response"))  
ws.2010.s2$wt <- 1/ws.2010.s2$prob
ws.2010.s2$normwt <- ws.2010.s2$wt/mean(ws.2010.s2$wt)
ws.2010.s2[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s2)
gc()

#splits
ws.2010.s3<-ws.2010[100000001:150000000,]
w1.s3<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2010.s3)
ws.2010.s3$prob <- predict(w1.s3,type = c("response"))  
ws.2010.s3$wt <- 1/ws.2010.s3$prob
ws.2010.s3$normwt <- ws.2010.s3$wt/mean(ws.2010.s3$wt)
ws.2010.s3[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s3)
gc()


#splits
x<-dim(ws.2010)
ws.2010.s4<-ws.2010[150000001:x[1],]
w1.s4<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2010.s4)
ws.2010.s4$prob <- predict(w1.s4,type = c("response"))  
ws.2010.s4$wt <- 1/ws.2010.s4$prob
ws.2010.s4$normwt <- ws.2010.s4$wt/mean(ws.2010.s4$wt)
ws.2010.s4[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s4)
gc()

wf<-rbindlist(list(ws.2010.s1,ws.2010.s2,ws.2010.s3,ws.2010.s4))

#reread m3
aqm2.2010<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod3.AQ.2010.rds")
setkey(aqm2.2010,aodid,day)
setkey(wf,aodid,day)
aqm2.2010 <- merge(aqm2.2010,wf,all.x = T)

###################################mod2
aqm2.2010.m2 <- aqm2.2010[!is.na(aod)]
#rm m3
rm(aqm2.2010)
gc()


#calculate low retrival day
f<- aqm2.2010.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
#describe(f)
setkey(f,aodid)
setkey(aqm2.2010.m2,aodid)
aqm2.2010.m2<-merge(aqm2.2010.m2,f)
aqm2.2010.m2$flag1000<-0
aqm2.2010.m2<-aqm2.2010.m2[numadata < 1000, flag1000 :=1]
aqm2.2010.m2$flag500<-0
aqm2.2010.m2<-aqm2.2010.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2010.m2[,tden.s:= scale(tden)]
aqm2.2010.m2[,elev.s:= scale(elev_m)]
aqm2.2010.m2[,pden.s:= scale(pop06)]
aqm2.2010.m2[,dist2A1.s:= scale(distA1)]
aqm2.2010.m2[,ndvi.s:= scale(ndvi)]
aqm2.2010.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2010.m2[,p_urb.s:= scale(pcturb )]
aqm2.2010.m2[,tempa.s:= scale(tempavg)]
aqm2.2010.m2[,WSa.s:= scale(wsavg)]
aqm2.2010.m2[,RHa.s:= scale(rhavg)]
aqm2.2010.m2[,Raina.s:= scale(rainday)]
#save mod2
saveRDS(aqm2.2010.m2,"/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod2.AQ.2010.rds")
#aqm2.2010.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod2.AQ.2010.rds")
gc()



#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2010days <- sort(unique(aqm2.2010.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2010)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_italy/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2010)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
aqm2.2010.m2$aodid<-as.character(aqm2.2010.m2$aodid)
setkey(aqm2.2010.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2010.m2[aqm2.2010.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2010days,], aqm2.2010.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")



########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
aqm2.2010.m2$aodid<-as.character(aqm2.2010.m2$aodid)
setkey(aqm2.2010.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2010.m2[aqm2.2010.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2010days,], aqm2.2010.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_italy/2.work/WORKDIR/mod1.AQ.2010.PM10.rds")

