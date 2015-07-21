
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#load clipped france grid 
fgrid <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/france.grid.csv")

#load aod data
aqua.2003<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2003.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2003<-as.data.frame(aqua.2003)
aqua.2003<-as.data.table(aqua.2003)

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
days2003 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
#merge
setkey(aqua.2003,aodid,day)
setkey(days2003 ,aodid,day)
db2003 <- merge(days2003,aqua.2003, all.x = T)

lu <- readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/lu/france.grid.allLU.rds")
    #merge
    setkey(db2003,aodid)
    setkey(lu ,aodid)
    db2003 <- merge(db2003,lu, all.x = T)  
    #fix LAT/LONG
  db2003$long_aod<- NULL
  db2003$lat_aod<- NULL
  gc()

  #Fix LAT/LONG
  setkey(db2003,aodid)
  setkey(fgrid,aodid)
  db2003 <- merge(db2003,fgrid[,list(aodid,long_aod,lat_aod)],all.x = T)

    #clean
  db2003$QA<-NULL
  db2003$Year<-NULL
gc()

#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2003)

#begin "spatio-temporal joins" using GeoMerge

met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(db2003, aodid)
lu.m <- makepointsmatrix(db2003[db2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            db2003, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
setkey(db2003,aodid,day)
setkey(aqua.met,aodid,day)
db2003 <- merge(db2003, aqua.met[,list(day,tempavg,aodid)], all.x = T)

gc()

#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            db2003, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(db2003,aodid,day)
setkey(aqua.met,aodid,day)
db2003 <- merge(db2003, aqua.met[,list(day,wsavg,aodid)], all.x = T)


gc()

## #rhavg
## aqua.met <- nearestbyday(lu.m ,met.m , 
##                             db2003, Temp[, list(day,rhavg,stn)], 
##                             "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
## setkey(db2003,aodid,day)
## setkey(aqua.met,aodid,day)
## db2003 <- merge(db2003, aqua.met[,list(day,rhavg,aodid)], all.x = T)

## #rainday
## aqua.met <- nearestbyday(lu.m ,met.m , 
##                             db2003, Temp[, list(day,rainday,stn)], 
##                             "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
## setkey(db2003,aodid,day)
## setkey(aqua.met,aodid,day)
## db2003 <- merge(db2003, aqua.met[,list(day,rainday,aodid)], all.x = T)

#cleanup
keep(fgrid,db2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
db2003$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2003)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(db2003,aodid)
setkey(key.pbl,aodid)
db2003 <- merge(db2003, key.pbl, all.x = T)
#add pbl
setkey(db2003,pblid,day)
setkey(fin.pbl,pblid,day)
db2003 <- merge(db2003, fin.pbl[,list(pblid,PBL,day)], all.x = T)
db2003$pblid<-NULL
gc()

#add month
db2003[, m := as.numeric(format(day, "%m")) ]
## #add season
## #1-winter, 2-spring,3-summer,4-autum
## db2003$season<-recode(db2003$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
## #1-winter, 2-summer
## db2003$seasonSW<-recode(db2003$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

## #join NDVI to aod
## fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
## fin.ndvi<-filter(fin.ndvi,year==2003)
## gc() 
## key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
## #add ndvi-key
## setkey(db2003,aodid)
## setkey(key.ndvi,aodid)
## db2003 <- merge(db2003, key.ndvi, all.x = T)
## #add ndvi
## setkey(db2003,ndviid,m)
## setkey(fin.ndvi,ndviid,m)
## db2003 <- merge(db2003, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,db2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(db2003, aodid)
aod.m <- makepointsmatrix(db2003[db2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            db2003, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 10, maxdistance = 120000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(db2003,aodid,day)
db2003 <- merge(db2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(db2003,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(db2003, aodid)
aod.m <- makepointsmatrix(db2003[db2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            db2003, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 10, maxdistance = 120000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(db2003,aodid,day)
db2003 <- merge(db2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(db2003,"closestmean","meanPM10")
summary(db2003$meanPM10)
#cleanup
keep(fgrid,db2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#take out uneeded
#save
gc()
saveRDS(db2003,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
gc()

## db2003<-db2003[,obs:=1]
## db2003[is.na(aod), obs:= 0]
## ws.2003<-select(db2003,obs,elev_m,PBL,m,tempavg,aodid,day)
## #ws.2003<-filter(ws.2003,!(is.na(tempavg)))
## rm(db2003)
## gc()

## #splits
## ws.2003.s1<-ws.2003[1:50000000,]
## w1.s1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2003.s1)
## ws.2003.s1$prob <- predict(w1.s1,type = c("response"))  
## ws.2003.s1$wt <- 1/ws.2003.s1$prob
## ws.2003.s1$normwt <- ws.2003.s1$wt/mean(ws.2003.s1$wt)
## ws.2003.s1[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
## rm(w1.s1)
## gc()


## #splits
## ws.2003.s2<-ws.2003[50000001:100000000,]
## w1.s2<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2003.s2)
## ws.2003.s2$prob <- predict(w1.s2,type = c("response"))  
## ws.2003.s2$wt <- 1/ws.2003.s2$prob
## ws.2003.s2$normwt <- ws.2003.s2$wt/mean(ws.2003.s2$wt)
## ws.2003.s2[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
## rm(w1.s2)
## gc()

## #splits
## ws.2003.s3<-ws.2003[100000001:150000000,]
## w1.s3<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2003.s3)
## ws.2003.s3$prob <- predict(w1.s3,type = c("response"))  
## ws.2003.s3$wt <- 1/ws.2003.s3$prob
## ws.2003.s3$normwt <- ws.2003.s3$wt/mean(ws.2003.s3$wt)
## ws.2003.s3[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
## rm(w1.s3)
## gc()


## #splits
## x<-dim(ws.2003)
## ws.2003.s4<-ws.2003[150000001:x[1],]
## w1.s4<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2003.s4)
## ws.2003.s4$prob <- predict(w1.s4,type = c("response"))  
## ws.2003.s4$wt <- 1/ws.2003.s4$prob
## ws.2003.s4$normwt <- ws.2003.s4$wt/mean(ws.2003.s4$wt)
## ws.2003.s4[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
## rm(w1.s4)
## gc()

## wf<-rbindlist(list(ws.2003.s1,ws.2003.s2,ws.2003.s3,ws.2003.s4))

## #reread m3
## db2003<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
## setkey(db2003,aodid,day)
## setkey(wf,aodid,day)
## db2003 <- merge(db2003,wf,all.x = T)

db2003.m2 <- db2003[!is.na(aod)]
#rm m3
rm(db2003)
gc()
#save mod2
saveRDS(db2003.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
db2003days <- sort(unique(db2003.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
db2003.m2$aodid<-as.character(db2003.m2$aodid)
setkey(db2003.m2,aodid)
aod.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% db2003days,], db2003.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")



########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
db2003.m2$aodid<-as.character(db2003.m2$aodid)
setkey(db2003.m2,aodid)
aod.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% db2003days,], db2003.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM10.rds")

#cleanup
keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()
