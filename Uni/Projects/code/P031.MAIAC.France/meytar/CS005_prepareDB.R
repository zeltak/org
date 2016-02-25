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
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
#load lu
lu <- readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/lu/france.grid.allLU.rds")

#load aod data (Loop on years)
setwd("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR")
filenames <- list.files( pattern="AOD.AQ.*.rds", full.names=TRUE)

for (I in 1:length(filenames)) {
y<-substr(filenames[I],10,13)
aq<-readRDS(filenames[I])

#get rid of dplyr tbl_df until bug gets fixed
aq<-as.data.frame(aq)
aq<-as.data.table(aq)

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
days.y <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
#merge
setkey(aq,aodid,day)
setkey(days.y ,aodid,day)
db <- merge(days.y,aq, all.x = T)
#check no dup point/day aod 
    
#merge lu data
    setkey(db,aodid)
    setkey(lu ,aodid)
    db <- merge(db,lu, all.x = T)  
    #fix LAT/LONG
  db$long_aod<- NULL
  db$lat_aod<- NULL
  gc()

  #Fix LAT/LONG
  setkey(db,aodid)
  setkey(fgrid,aodid)
  db <- merge(db,fgrid[,list(aodid,long_aod,lat_aod)],all.x = T)

    #clean
  db$QA<-NULL
  db$Year<-NULL
gc()

#load met
Temp<-filter(Temp,c==y)

#begin "spatio-temporal joins" using GeoMerge

met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(db, aodid)
lu.m <- makepointsmatrix(db[db[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            db, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
setkey(db,aodid,day)
setkey(aqua.met,aodid,day)
db <- merge(db, aqua.met[,list(day,tempavg,aodid)], all.x = T)

gc()

# #wsavg - WE DON'T USE WS IN SECOND RUN
# aqua.met <- nearestbyday(lu.m ,met.m , 
#                             db, Temp[, list(day,wsavg,stn)], 
#                             "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
# #head(aqua.met)
# setkey(db,aodid,day)
# setkey(aqua.met,aodid,day)
# db <- merge(db, aqua.met[,list(day,wsavg,aodid)], all.x = T)
# 
# 
# gc()

## #rhavg
# aqua.met <- nearestbyday(lu.m ,met.m , 
#                             db, Temp[, list(day,rhavg,stn)], 
#                             "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
# setkey(db,aodid,day)
# setkey(aqua.met,aodid,day)
# db <- merge(db, aqua.met[,list(day,rhavg,aodid)], all.x = T)

## #rainday
# aqua.met <- nearestbyday(lu.m ,met.m , 
#                             db, Temp[, list(day,rainday,stn)], 
#                             "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
# setkey(db,aodid,day)
# setkey(aqua.met,aodid,day)
# db <- merge(db, aqua.met[,list(day,rainday,aodid)], all.x = T)

#cleanup
keep(fgrid,db,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
db$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2003)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(db,aodid)
setkey(key.pbl,aodid)
db <- merge(db, key.pbl, all.x = T)
#add pbl
setkey(db,pblid,day)
setkey(fin.pbl,pblid,day)
db <- merge(db, fin.pbl[,list(pblid,PBL,day)], all.x = T)
db$pblid<-NULL
gc()

#add month
db[, m := as.numeric(format(day, "%m")) ]
## #add season
## #1-winter, 2-spring,3-summer,4-autum
## db$season<-recode(db$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
## #1-winter, 2-summer
## db$seasonSW<-recode(db$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

## #join NDVI to aod
## fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
## fin.ndvi<-filter(fin.ndvi,year==2003)
## gc() 
## key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
## #add ndvi-key
## setkey(db,aodid)
## setkey(key.ndvi,aodid)
## db <- merge(db, key.ndvi, all.x = T)
## #add ndvi
## setkey(db,ndviid,m)
## setkey(fin.ndvi,ndviid,m)
## db <- merge(db, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,db,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(db, aodid)
aod.m <- makepointsmatrix(db[db[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            db, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 10, maxdistance = 120000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(db,aodid,day)
db <- merge(db,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(db,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(db, aodid)
aod.m <- makepointsmatrix(db[db[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            db, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 10, maxdistance = 120000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(db,aodid,day)
db <- merge(db,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(db,"closestmean","meanPM10")
summary(db$meanPM10)
#cleanup
keep(fgrid,db,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#take out uneeded
#save
gc()
saveRDS(db,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
gc()

## db<-db[,obs:=1]
## db[is.na(aod), obs:= 0]
## ws.2003<-select(db,obs,elev_m,PBL,m,tempavg,aodid,day)
## #ws.2003<-filter(ws.2003,!(is.na(tempavg)))
## rm(db)
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
## db<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
## setkey(db,aodid,day)
## setkey(wf,aodid,day)
## db <- merge(db,wf,all.x = T)

db.m2 <- db[!is.na(aod)]
#rm m3
rm(db)
gc()
#save mod2
saveRDS(db.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
dbdays <- sort(unique(db.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
db.m2$aodid<-as.character(db.m2$aodid)
setkey(db.m2,aodid)
aod.m <- makepointsmatrix(db.m2[db.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% dbdays,], db.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")
}


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
db.m2$aodid<-as.character(db.m2$aodid)
setkey(db.m2,aodid)
aod.m <- makepointsmatrix(db.m2[db.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% dbdays,], db.m2, 
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
