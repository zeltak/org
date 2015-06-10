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
#load clipped france grid 
fgrid <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/france.grid.csv")


#########Aqua.2003



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2003<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2003.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2003<-as.data.frame(aqua.2003)
aqua.2003<-as.data.table(aqua.2003)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
days2003 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2003,aodid,day)
setkey(days2003 ,aodid,day)
aqm1.2003 <- merge(days2003,aqua.2003, all.x = T)  


#add lu-key
setkey(aqm1.2003,aodid)
setkey(key.lu,aodid)
aqm2.2003 <- merge(aqm1.2003, key.lu, all.x = T)
#add lu
setkey(aqm2.2003,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2003 <- merge(aqm2.2003, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2003$long_aod <- NULL
aqm2.2003$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2003 ,aodid)
aqm2.2003 <- merge(aqm2.2003 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2003)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2003, aodid)
lu.m <- makepointsmatrix(aqm2.2003[aqm2.2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2003, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2003,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2003 <- merge(aqm2.2003, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2003$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2003, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2003,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2003 <- merge(aqm2.2003, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2003$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2003, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2003,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2003 <- merge(aqm2.2003, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2003$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2003, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2003,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2003 <- merge(aqm2.2003, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2003$rainday)

#saveRDS(aqm2.2003,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2003$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2003)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2003,aodid)
setkey(key.pbl,aodid)
aqm2.2003 <- merge(aqm2.2003, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2003,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2003 <- merge(aqm2.2003, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2003$pblid<-NULL
#add month
aqm2.2003[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2003$season<-recode(aqm2.2003$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2003$seasonSW<-recode(aqm2.2003$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2003)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2003,aodid)
setkey(key.ndvi,aodid)
aqm2.2003 <- merge(aqm2.2003, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2003,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2003 <- merge(aqm2.2003, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2003, aodid)
aod.m <- makepointsmatrix(aqm2.2003[aqm2.2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2003, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2003,aodid,day)
aqm2.2003 <- merge(aqm2.2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2003,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2003, aodid)
aod.m <- makepointsmatrix(aqm2.2003[aqm2.2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2003, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2003,aodid,day)
aqm2.2003 <- merge(aqm2.2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2003,"closestmean","meanPM10")
summary(aqm2.2003$meanPM10)
#cleanup
keep(fgrid,aqm2.2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2003<-aqm2.2003[,obs:=1]
aqm2.2003[is.na(aod), obs:= 0]
# ws.2003<-select(aqm2.2003,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2003)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2003$prob <- predict(w1,type = c("response"))  
# aqm2.2003$wt <- 1/aqm2.2003$prob
# aqm2.2003$normwt <- aqm2.2003$wt/mean(aqm2.2003$wt)
# #Cleanup
# aqm2.2003<-aqm2.2003[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2003[,c("ndviid"):=NULL]
saveRDS(aqm2.2003,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
#aqm2.2003<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2003.m2 <- aqm2.2003[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2003.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2003.m2,aodid)
aqm2.2003.m2<-merge(aqm2.2003.m2,f)
aqm2.2003.m2$flag1000<-0
aqm2.2003.m2<-aqm2.2003.m2[numadata < 1000, flag1000 :=1]
aqm2.2003.m2$flag500<-0
aqm2.2003.m2<-aqm2.2003.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2003.m2[,tden.s:= scale(tden)]
aqm2.2003.m2[,elev.s:= scale(elev_m)]
aqm2.2003.m2[,pden.s:= scale(pop06)]
aqm2.2003.m2[,dist2A1.s:= scale(distA1)]
aqm2.2003.m2[,ndvi.s:= scale(ndvi)]
aqm2.2003.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2003.m2[,p_urb.s:= scale(pcturb )]
aqm2.2003.m2[,tempa.s:= scale(tempavg)]
aqm2.2003.m2[,WSa.s:= scale(wsavg)]
aqm2.2003.m2[,RHa.s:= scale(rhavg)]
aqm2.2003.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2003.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2003days <- sort(unique(aqm2.2003.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2003days,], aqm2.2003.m2, 
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
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2003days,], aqm2.2003.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2004



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2004<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2004.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2004<-as.data.frame(aqua.2004)
aqua.2004<-as.data.table(aqua.2004)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
days2004 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2004,aodid,day)
setkey(days2004 ,aodid,day)
aqm1.2004 <- merge(days2004,aqua.2004, all.x = T)  


#add lu-key
setkey(aqm1.2004,aodid)
setkey(key.lu,aodid)
aqm2.2004 <- merge(aqm1.2004, key.lu, all.x = T)
#add lu
setkey(aqm2.2004,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2004 <- merge(aqm2.2004, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2004$long_aod <- NULL
aqm2.2004$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2004 ,aodid)
aqm2.2004 <- merge(aqm2.2004 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2004,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2004)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2004, aodid)
lu.m <- makepointsmatrix(aqm2.2004[aqm2.2004[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2004, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2004,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2004 <- merge(aqm2.2004, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2004$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2004, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2004,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2004 <- merge(aqm2.2004, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2004$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2004, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2004,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2004 <- merge(aqm2.2004, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2004$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2004, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2004,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2004 <- merge(aqm2.2004, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2004$rainday)

#saveRDS(aqm2.2004,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2004,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2004$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2004)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2004,aodid)
setkey(key.pbl,aodid)
aqm2.2004 <- merge(aqm2.2004, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2004,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2004 <- merge(aqm2.2004, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2004$pblid<-NULL
#add month
aqm2.2004[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2004$season<-recode(aqm2.2004$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2004$seasonSW<-recode(aqm2.2004$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2004)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2004,aodid)
setkey(key.ndvi,aodid)
aqm2.2004 <- merge(aqm2.2004, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2004,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2004 <- merge(aqm2.2004, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2004,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2004)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2004)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2004, aodid)
aod.m <- makepointsmatrix(aqm2.2004[aqm2.2004[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2004, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2004,aodid,day)
aqm2.2004 <- merge(aqm2.2004,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2004,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2004, aodid)
aod.m <- makepointsmatrix(aqm2.2004[aqm2.2004[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2004, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2004,aodid,day)
aqm2.2004 <- merge(aqm2.2004,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2004,"closestmean","meanPM10")
summary(aqm2.2004$meanPM10)
#cleanup
keep(fgrid,aqm2.2004,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2004<-aqm2.2004[,obs:=1]
aqm2.2004[is.na(aod), obs:= 0]
# ws.2004<-select(aqm2.2004,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2004)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2004$prob <- predict(w1,type = c("response"))  
# aqm2.2004$wt <- 1/aqm2.2004$prob
# aqm2.2004$normwt <- aqm2.2004$wt/mean(aqm2.2004$wt)
# #Cleanup
# aqm2.2004<-aqm2.2004[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2004[,c("ndviid"):=NULL]
saveRDS(aqm2.2004,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2004.rds")
#aqm2.2004<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2004.m2 <- aqm2.2004[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2004.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2004.m2,aodid)
aqm2.2004.m2<-merge(aqm2.2004.m2,f)
aqm2.2004.m2$flag1000<-0
aqm2.2004.m2<-aqm2.2004.m2[numadata < 1000, flag1000 :=1]
aqm2.2004.m2$flag500<-0
aqm2.2004.m2<-aqm2.2004.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2004.m2[,tden.s:= scale(tden)]
aqm2.2004.m2[,elev.s:= scale(elev_m)]
aqm2.2004.m2[,pden.s:= scale(pop06)]
aqm2.2004.m2[,dist2A1.s:= scale(distA1)]
aqm2.2004.m2[,ndvi.s:= scale(ndvi)]
aqm2.2004.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2004.m2[,p_urb.s:= scale(pcturb )]
aqm2.2004.m2[,tempa.s:= scale(tempavg)]
aqm2.2004.m2[,WSa.s:= scale(wsavg)]
aqm2.2004.m2[,RHa.s:= scale(rhavg)]
aqm2.2004.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2004.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2004days <- sort(unique(aqm2.2004.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2004)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2004)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2004.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2004.m2[aqm2.2004.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2004days,], aqm2.2004.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2004.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2004.m2[aqm2.2004.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2004days,], aqm2.2004.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2005



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2005<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2005.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2005<-as.data.frame(aqua.2005)
aqua.2005<-as.data.table(aqua.2005)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), 1)
#create date range
days2005 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2005,aodid,day)
setkey(days2005 ,aodid,day)
aqm1.2005 <- merge(days2005,aqua.2005, all.x = T)  


#add lu-key
setkey(aqm1.2005,aodid)
setkey(key.lu,aodid)
aqm2.2005 <- merge(aqm1.2005, key.lu, all.x = T)
#add lu
setkey(aqm2.2005,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2005 <- merge(aqm2.2005, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2005$long_aod <- NULL
aqm2.2005$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2005 ,aodid)
aqm2.2005 <- merge(aqm2.2005 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2005,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2005)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2005, aodid)
lu.m <- makepointsmatrix(aqm2.2005[aqm2.2005[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2005, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2005,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2005 <- merge(aqm2.2005, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2005$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2005, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2005,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2005 <- merge(aqm2.2005, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2005$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2005, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2005,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2005 <- merge(aqm2.2005, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2005$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2005, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2005,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2005 <- merge(aqm2.2005, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2005$rainday)

#saveRDS(aqm2.2005,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2005,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2005$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2005)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2005,aodid)
setkey(key.pbl,aodid)
aqm2.2005 <- merge(aqm2.2005, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2005,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2005 <- merge(aqm2.2005, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2005$pblid<-NULL
#add month
aqm2.2005[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2005$season<-recode(aqm2.2005$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2005$seasonSW<-recode(aqm2.2005$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2005)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2005,aodid)
setkey(key.ndvi,aodid)
aqm2.2005 <- merge(aqm2.2005, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2005,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2005 <- merge(aqm2.2005, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2005,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2005)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2005)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2005, aodid)
aod.m <- makepointsmatrix(aqm2.2005[aqm2.2005[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2005, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2005,aodid,day)
aqm2.2005 <- merge(aqm2.2005,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2005,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2005, aodid)
aod.m <- makepointsmatrix(aqm2.2005[aqm2.2005[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2005, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2005,aodid,day)
aqm2.2005 <- merge(aqm2.2005,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2005,"closestmean","meanPM10")
summary(aqm2.2005$meanPM10)
#cleanup
keep(fgrid,aqm2.2005,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2005<-aqm2.2005[,obs:=1]
aqm2.2005[is.na(aod), obs:= 0]
# ws.2005<-select(aqm2.2005,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2005)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2005$prob <- predict(w1,type = c("response"))  
# aqm2.2005$wt <- 1/aqm2.2005$prob
# aqm2.2005$normwt <- aqm2.2005$wt/mean(aqm2.2005$wt)
# #Cleanup
# aqm2.2005<-aqm2.2005[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2005[,c("ndviid"):=NULL]
saveRDS(aqm2.2005,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2005.rds")
#aqm2.2005<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2005.m2 <- aqm2.2005[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2005.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2005.m2,aodid)
aqm2.2005.m2<-merge(aqm2.2005.m2,f)
aqm2.2005.m2$flag1000<-0
aqm2.2005.m2<-aqm2.2005.m2[numadata < 1000, flag1000 :=1]
aqm2.2005.m2$flag500<-0
aqm2.2005.m2<-aqm2.2005.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2005.m2[,tden.s:= scale(tden)]
aqm2.2005.m2[,elev.s:= scale(elev_m)]
aqm2.2005.m2[,pden.s:= scale(pop06)]
aqm2.2005.m2[,dist2A1.s:= scale(distA1)]
aqm2.2005.m2[,ndvi.s:= scale(ndvi)]
aqm2.2005.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2005.m2[,p_urb.s:= scale(pcturb )]
aqm2.2005.m2[,tempa.s:= scale(tempavg)]
aqm2.2005.m2[,WSa.s:= scale(wsavg)]
aqm2.2005.m2[,RHa.s:= scale(rhavg)]
aqm2.2005.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2005.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2005days <- sort(unique(aqm2.2005.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2005)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2005)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2005.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2005.m2[aqm2.2005.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2005days,], aqm2.2005.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2005.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2005.m2[aqm2.2005.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2005days,], aqm2.2005.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2006



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2006<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2006.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2006<-as.data.frame(aqua.2006)
aqua.2006<-as.data.table(aqua.2006)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
days2006 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2006,aodid,day)
setkey(days2006 ,aodid,day)
aqm1.2006 <- merge(days2006,aqua.2006, all.x = T)  


#add lu-key
setkey(aqm1.2006,aodid)
setkey(key.lu,aodid)
aqm2.2006 <- merge(aqm1.2006, key.lu, all.x = T)
#add lu
setkey(aqm2.2006,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2006 <- merge(aqm2.2006, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2006$long_aod <- NULL
aqm2.2006$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2006 ,aodid)
aqm2.2006 <- merge(aqm2.2006 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2006,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2006)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2006, aodid)
lu.m <- makepointsmatrix(aqm2.2006[aqm2.2006[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2006, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2006,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2006 <- merge(aqm2.2006, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2006$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2006, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2006,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2006 <- merge(aqm2.2006, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2006$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2006, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2006,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2006 <- merge(aqm2.2006, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2006$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2006, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2006,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2006 <- merge(aqm2.2006, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2006$rainday)

#saveRDS(aqm2.2006,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2006,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2006$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2006)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2006,aodid)
setkey(key.pbl,aodid)
aqm2.2006 <- merge(aqm2.2006, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2006,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2006 <- merge(aqm2.2006, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2006$pblid<-NULL
#add month
aqm2.2006[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2006$season<-recode(aqm2.2006$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2006$seasonSW<-recode(aqm2.2006$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2006)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2006,aodid)
setkey(key.ndvi,aodid)
aqm2.2006 <- merge(aqm2.2006, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2006,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2006 <- merge(aqm2.2006, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2006,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2006)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2006)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2006, aodid)
aod.m <- makepointsmatrix(aqm2.2006[aqm2.2006[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2006, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2006,aodid,day)
aqm2.2006 <- merge(aqm2.2006,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2006,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2006, aodid)
aod.m <- makepointsmatrix(aqm2.2006[aqm2.2006[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2006, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2006,aodid,day)
aqm2.2006 <- merge(aqm2.2006,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2006,"closestmean","meanPM10")
summary(aqm2.2006$meanPM10)
#cleanup
keep(fgrid,aqm2.2006,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2006<-aqm2.2006[,obs:=1]
aqm2.2006[is.na(aod), obs:= 0]
# ws.2006<-select(aqm2.2006,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2006)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2006$prob <- predict(w1,type = c("response"))  
# aqm2.2006$wt <- 1/aqm2.2006$prob
# aqm2.2006$normwt <- aqm2.2006$wt/mean(aqm2.2006$wt)
# #Cleanup
# aqm2.2006<-aqm2.2006[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2006[,c("ndviid"):=NULL]
saveRDS(aqm2.2006,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2006.rds")
#aqm2.2006<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2006.m2 <- aqm2.2006[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2006.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2006.m2,aodid)
aqm2.2006.m2<-merge(aqm2.2006.m2,f)
aqm2.2006.m2$flag1000<-0
aqm2.2006.m2<-aqm2.2006.m2[numadata < 1000, flag1000 :=1]
aqm2.2006.m2$flag500<-0
aqm2.2006.m2<-aqm2.2006.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2006.m2[,tden.s:= scale(tden)]
aqm2.2006.m2[,elev.s:= scale(elev_m)]
aqm2.2006.m2[,pden.s:= scale(pop06)]
aqm2.2006.m2[,dist2A1.s:= scale(distA1)]
aqm2.2006.m2[,ndvi.s:= scale(ndvi)]
aqm2.2006.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2006.m2[,p_urb.s:= scale(pcturb )]
aqm2.2006.m2[,tempa.s:= scale(tempavg)]
aqm2.2006.m2[,WSa.s:= scale(wsavg)]
aqm2.2006.m2[,RHa.s:= scale(rhavg)]
aqm2.2006.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2006.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2006days <- sort(unique(aqm2.2006.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2006)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2006)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2006.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2006.m2[aqm2.2006.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2006days,], aqm2.2006.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2006.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2006.m2[aqm2.2006.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2006days,], aqm2.2006.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2007



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2007<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2007.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2007<-as.data.frame(aqua.2007)
aqua.2007<-as.data.table(aqua.2007)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
days2007 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2007,aodid,day)
setkey(days2007 ,aodid,day)
aqm1.2007 <- merge(days2007,aqua.2007, all.x = T)  


#add lu-key
setkey(aqm1.2007,aodid)
setkey(key.lu,aodid)
aqm2.2007 <- merge(aqm1.2007, key.lu, all.x = T)
#add lu
setkey(aqm2.2007,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2007 <- merge(aqm2.2007, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2007$long_aod <- NULL
aqm2.2007$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2007 ,aodid)
aqm2.2007 <- merge(aqm2.2007 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2007,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2007)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2007, aodid)
lu.m <- makepointsmatrix(aqm2.2007[aqm2.2007[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2007, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2007,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2007 <- merge(aqm2.2007, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2007$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2007, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2007,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2007 <- merge(aqm2.2007, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2007$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2007, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2007,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2007 <- merge(aqm2.2007, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2007$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2007, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2007,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2007 <- merge(aqm2.2007, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2007$rainday)

#saveRDS(aqm2.2007,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2007,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2007$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2007)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2007,aodid)
setkey(key.pbl,aodid)
aqm2.2007 <- merge(aqm2.2007, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2007,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2007 <- merge(aqm2.2007, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2007$pblid<-NULL
#add month
aqm2.2007[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2007$season<-recode(aqm2.2007$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2007$seasonSW<-recode(aqm2.2007$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2007)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2007,aodid)
setkey(key.ndvi,aodid)
aqm2.2007 <- merge(aqm2.2007, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2007,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2007 <- merge(aqm2.2007, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2007,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2007)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2007)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2007, aodid)
aod.m <- makepointsmatrix(aqm2.2007[aqm2.2007[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2007, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2007,aodid,day)
aqm2.2007 <- merge(aqm2.2007,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2007,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2007, aodid)
aod.m <- makepointsmatrix(aqm2.2007[aqm2.2007[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2007, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2007,aodid,day)
aqm2.2007 <- merge(aqm2.2007,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2007,"closestmean","meanPM10")
summary(aqm2.2007$meanPM10)
#cleanup
keep(fgrid,aqm2.2007,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2007<-aqm2.2007[,obs:=1]
aqm2.2007[is.na(aod), obs:= 0]
# ws.2007<-select(aqm2.2007,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2007)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2007$prob <- predict(w1,type = c("response"))  
# aqm2.2007$wt <- 1/aqm2.2007$prob
# aqm2.2007$normwt <- aqm2.2007$wt/mean(aqm2.2007$wt)
# #Cleanup
# aqm2.2007<-aqm2.2007[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2007[,c("ndviid"):=NULL]
saveRDS(aqm2.2007,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2007.rds")
#aqm2.2007<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2007.m2 <- aqm2.2007[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2007.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2007.m2,aodid)
aqm2.2007.m2<-merge(aqm2.2007.m2,f)
aqm2.2007.m2$flag1000<-0
aqm2.2007.m2<-aqm2.2007.m2[numadata < 1000, flag1000 :=1]
aqm2.2007.m2$flag500<-0
aqm2.2007.m2<-aqm2.2007.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2007.m2[,tden.s:= scale(tden)]
aqm2.2007.m2[,elev.s:= scale(elev_m)]
aqm2.2007.m2[,pden.s:= scale(pop06)]
aqm2.2007.m2[,dist2A1.s:= scale(distA1)]
aqm2.2007.m2[,ndvi.s:= scale(ndvi)]
aqm2.2007.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2007.m2[,p_urb.s:= scale(pcturb )]
aqm2.2007.m2[,tempa.s:= scale(tempavg)]
aqm2.2007.m2[,WSa.s:= scale(wsavg)]
aqm2.2007.m2[,RHa.s:= scale(rhavg)]
aqm2.2007.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2007.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2007days <- sort(unique(aqm2.2007.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2007)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2007)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2007.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2007.m2[aqm2.2007.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2007days,], aqm2.2007.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2007.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2007.m2[aqm2.2007.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2007days,], aqm2.2007.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2008



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2008<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2008.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2008<-as.data.frame(aqua.2008)
aqua.2008<-as.data.table(aqua.2008)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), 1)
#create date range
days2008 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2008,aodid,day)
setkey(days2008 ,aodid,day)
aqm1.2008 <- merge(days2008,aqua.2008, all.x = T)  


#add lu-key
setkey(aqm1.2008,aodid)
setkey(key.lu,aodid)
aqm2.2008 <- merge(aqm1.2008, key.lu, all.x = T)
#add lu
setkey(aqm2.2008,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2008 <- merge(aqm2.2008, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2008$long_aod <- NULL
aqm2.2008$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2008 ,aodid)
aqm2.2008 <- merge(aqm2.2008 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2008,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2008)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2008, aodid)
lu.m <- makepointsmatrix(aqm2.2008[aqm2.2008[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2008, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2008,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2008 <- merge(aqm2.2008, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2008$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2008, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2008,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2008 <- merge(aqm2.2008, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2008$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2008, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2008,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2008 <- merge(aqm2.2008, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2008$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2008, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2008,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2008 <- merge(aqm2.2008, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2008$rainday)

#saveRDS(aqm2.2008,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2008,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2008$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2008)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2008,aodid)
setkey(key.pbl,aodid)
aqm2.2008 <- merge(aqm2.2008, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2008,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2008 <- merge(aqm2.2008, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2008$pblid<-NULL
#add month
aqm2.2008[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2008$season<-recode(aqm2.2008$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2008$seasonSW<-recode(aqm2.2008$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2008)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2008,aodid)
setkey(key.ndvi,aodid)
aqm2.2008 <- merge(aqm2.2008, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2008,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2008 <- merge(aqm2.2008, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2008,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2008)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2008)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2008, aodid)
aod.m <- makepointsmatrix(aqm2.2008[aqm2.2008[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2008, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2008,aodid,day)
aqm2.2008 <- merge(aqm2.2008,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2008,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2008, aodid)
aod.m <- makepointsmatrix(aqm2.2008[aqm2.2008[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2008, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2008,aodid,day)
aqm2.2008 <- merge(aqm2.2008,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2008,"closestmean","meanPM10")
summary(aqm2.2008$meanPM10)
#cleanup
keep(fgrid,aqm2.2008,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2008<-aqm2.2008[,obs:=1]
aqm2.2008[is.na(aod), obs:= 0]
# ws.2008<-select(aqm2.2008,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2008)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2008$prob <- predict(w1,type = c("response"))  
# aqm2.2008$wt <- 1/aqm2.2008$prob
# aqm2.2008$normwt <- aqm2.2008$wt/mean(aqm2.2008$wt)
# #Cleanup
# aqm2.2008<-aqm2.2008[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2008[,c("ndviid"):=NULL]
saveRDS(aqm2.2008,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2008.rds")
#aqm2.2008<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2008.m2 <- aqm2.2008[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2008.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2008.m2,aodid)
aqm2.2008.m2<-merge(aqm2.2008.m2,f)
aqm2.2008.m2$flag1000<-0
aqm2.2008.m2<-aqm2.2008.m2[numadata < 1000, flag1000 :=1]
aqm2.2008.m2$flag500<-0
aqm2.2008.m2<-aqm2.2008.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2008.m2[,tden.s:= scale(tden)]
aqm2.2008.m2[,elev.s:= scale(elev_m)]
aqm2.2008.m2[,pden.s:= scale(pop06)]
aqm2.2008.m2[,dist2A1.s:= scale(distA1)]
aqm2.2008.m2[,ndvi.s:= scale(ndvi)]
aqm2.2008.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2008.m2[,p_urb.s:= scale(pcturb )]
aqm2.2008.m2[,tempa.s:= scale(tempavg)]
aqm2.2008.m2[,WSa.s:= scale(wsavg)]
aqm2.2008.m2[,RHa.s:= scale(rhavg)]
aqm2.2008.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2008.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2008days <- sort(unique(aqm2.2008.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2008)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2008)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2008.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2008.m2[aqm2.2008.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2008days,], aqm2.2008.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2008.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2008.m2[aqm2.2008.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2008days,], aqm2.2008.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2009



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2009<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2009.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2009<-as.data.frame(aqua.2009)
aqua.2009<-as.data.table(aqua.2009)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
days2009 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2009,aodid,day)
setkey(days2009 ,aodid,day)
aqm1.2009 <- merge(days2009,aqua.2009, all.x = T)  


#add lu-key
setkey(aqm1.2009,aodid)
setkey(key.lu,aodid)
aqm2.2009 <- merge(aqm1.2009, key.lu, all.x = T)
#add lu
setkey(aqm2.2009,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2009 <- merge(aqm2.2009, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2009$long_aod <- NULL
aqm2.2009$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2009 ,aodid)
aqm2.2009 <- merge(aqm2.2009 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2009,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2009)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2009, aodid)
lu.m <- makepointsmatrix(aqm2.2009[aqm2.2009[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2009, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2009,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2009 <- merge(aqm2.2009, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2009$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2009, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2009,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2009 <- merge(aqm2.2009, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2009$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2009, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2009,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2009 <- merge(aqm2.2009, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2009$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2009, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2009,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2009 <- merge(aqm2.2009, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2009$rainday)

#saveRDS(aqm2.2009,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2009,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2009$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2009)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2009,aodid)
setkey(key.pbl,aodid)
aqm2.2009 <- merge(aqm2.2009, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2009,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2009 <- merge(aqm2.2009, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2009$pblid<-NULL
#add month
aqm2.2009[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2009$season<-recode(aqm2.2009$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2009$seasonSW<-recode(aqm2.2009$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2009)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2009,aodid)
setkey(key.ndvi,aodid)
aqm2.2009 <- merge(aqm2.2009, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2009,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2009 <- merge(aqm2.2009, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2009,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2009)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2009)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2009, aodid)
aod.m <- makepointsmatrix(aqm2.2009[aqm2.2009[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2009, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2009,aodid,day)
aqm2.2009 <- merge(aqm2.2009,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2009,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2009, aodid)
aod.m <- makepointsmatrix(aqm2.2009[aqm2.2009[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2009, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2009,aodid,day)
aqm2.2009 <- merge(aqm2.2009,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2009,"closestmean","meanPM10")
summary(aqm2.2009$meanPM10)
#cleanup
keep(fgrid,aqm2.2009,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2009<-aqm2.2009[,obs:=1]
aqm2.2009[is.na(aod), obs:= 0]
# ws.2009<-select(aqm2.2009,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2009)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2009$prob <- predict(w1,type = c("response"))  
# aqm2.2009$wt <- 1/aqm2.2009$prob
# aqm2.2009$normwt <- aqm2.2009$wt/mean(aqm2.2009$wt)
# #Cleanup
# aqm2.2009<-aqm2.2009[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2009[,c("ndviid"):=NULL]
saveRDS(aqm2.2009,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2009.rds")
#aqm2.2009<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2009.m2 <- aqm2.2009[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2009.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2009.m2,aodid)
aqm2.2009.m2<-merge(aqm2.2009.m2,f)
aqm2.2009.m2$flag1000<-0
aqm2.2009.m2<-aqm2.2009.m2[numadata < 1000, flag1000 :=1]
aqm2.2009.m2$flag500<-0
aqm2.2009.m2<-aqm2.2009.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2009.m2[,tden.s:= scale(tden)]
aqm2.2009.m2[,elev.s:= scale(elev_m)]
aqm2.2009.m2[,pden.s:= scale(pop06)]
aqm2.2009.m2[,dist2A1.s:= scale(distA1)]
aqm2.2009.m2[,ndvi.s:= scale(ndvi)]
aqm2.2009.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2009.m2[,p_urb.s:= scale(pcturb )]
aqm2.2009.m2[,tempa.s:= scale(tempavg)]
aqm2.2009.m2[,WSa.s:= scale(wsavg)]
aqm2.2009.m2[,RHa.s:= scale(rhavg)]
aqm2.2009.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2009.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2009days <- sort(unique(aqm2.2009.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2009)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2009)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2009.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2009.m2[aqm2.2009.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2009days,], aqm2.2009.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2009.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2009.m2[aqm2.2009.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2009days,], aqm2.2009.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2010



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2010<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2010.rds")
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
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
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

#saveRDS(aqm2.2010,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2010,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2010$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2010)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

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
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2010)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
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

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2010)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2010)

gc()

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

#########-------------------weights ############
aqm2.2010<-aqm2.2010[,obs:=1]
aqm2.2010[is.na(aod), obs:= 0]
# ws.2010<-select(aqm2.2010,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2010)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2010$prob <- predict(w1,type = c("response"))  
# aqm2.2010$wt <- 1/aqm2.2010$prob
# aqm2.2010$normwt <- aqm2.2010$wt/mean(aqm2.2010$wt)
# #Cleanup
# aqm2.2010<-aqm2.2010[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2010[,c("ndviid"):=NULL]
saveRDS(aqm2.2010,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2010.rds")
#aqm2.2010<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2010.m2 <- aqm2.2010[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2010.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
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
saveRDS(aqm2.2010.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2010days <- sort(unique(aqm2.2010.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2010)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2010)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2010.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2010.m2[aqm2.2010.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
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
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2010.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2010.m2[aqm2.2010.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
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
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2011



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2011<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2011.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2011<-as.data.frame(aqua.2011)
aqua.2011<-as.data.table(aqua.2011)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
days2011 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2011,aodid,day)
setkey(days2011 ,aodid,day)
aqm1.2011 <- merge(days2011,aqua.2011, all.x = T)  


#add lu-key
setkey(aqm1.2011,aodid)
setkey(key.lu,aodid)
aqm2.2011 <- merge(aqm1.2011, key.lu, all.x = T)
#add lu
setkey(aqm2.2011,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2011 <- merge(aqm2.2011, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2011$long_aod <- NULL
aqm2.2011$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2011 ,aodid)
aqm2.2011 <- merge(aqm2.2011 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2011,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2011)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2011, aodid)
lu.m <- makepointsmatrix(aqm2.2011[aqm2.2011[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2011, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2011,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2011 <- merge(aqm2.2011, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2011$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2011, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2011,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2011 <- merge(aqm2.2011, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2011$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2011, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2011,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2011 <- merge(aqm2.2011, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2011$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2011, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2011,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2011 <- merge(aqm2.2011, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2011$rainday)

#saveRDS(aqm2.2011,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2011,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2011$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2011)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2011,aodid)
setkey(key.pbl,aodid)
aqm2.2011 <- merge(aqm2.2011, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2011,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2011 <- merge(aqm2.2011, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2011$pblid<-NULL
#add month
aqm2.2011[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2011$season<-recode(aqm2.2011$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2011$seasonSW<-recode(aqm2.2011$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2011)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2011,aodid)
setkey(key.ndvi,aodid)
aqm2.2011 <- merge(aqm2.2011, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2011,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2011 <- merge(aqm2.2011, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2011,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2011)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2011)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2011, aodid)
aod.m <- makepointsmatrix(aqm2.2011[aqm2.2011[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2011, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2011,aodid,day)
aqm2.2011 <- merge(aqm2.2011,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2011,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2011, aodid)
aod.m <- makepointsmatrix(aqm2.2011[aqm2.2011[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2011, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2011,aodid,day)
aqm2.2011 <- merge(aqm2.2011,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2011,"closestmean","meanPM10")
summary(aqm2.2011$meanPM10)
#cleanup
keep(fgrid,aqm2.2011,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2011<-aqm2.2011[,obs:=1]
aqm2.2011[is.na(aod), obs:= 0]
# ws.2011<-select(aqm2.2011,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2011)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2011$prob <- predict(w1,type = c("response"))  
# aqm2.2011$wt <- 1/aqm2.2011$prob
# aqm2.2011$normwt <- aqm2.2011$wt/mean(aqm2.2011$wt)
# #Cleanup
# aqm2.2011<-aqm2.2011[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2011[,c("ndviid"):=NULL]
saveRDS(aqm2.2011,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2011.rds")
#aqm2.2011<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2011.m2 <- aqm2.2011[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2011.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2011.m2,aodid)
aqm2.2011.m2<-merge(aqm2.2011.m2,f)
aqm2.2011.m2$flag1000<-0
aqm2.2011.m2<-aqm2.2011.m2[numadata < 1000, flag1000 :=1]
aqm2.2011.m2$flag500<-0
aqm2.2011.m2<-aqm2.2011.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2011.m2[,tden.s:= scale(tden)]
aqm2.2011.m2[,elev.s:= scale(elev_m)]
aqm2.2011.m2[,pden.s:= scale(pop06)]
aqm2.2011.m2[,dist2A1.s:= scale(distA1)]
aqm2.2011.m2[,ndvi.s:= scale(ndvi)]
aqm2.2011.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2011.m2[,p_urb.s:= scale(pcturb )]
aqm2.2011.m2[,tempa.s:= scale(tempavg)]
aqm2.2011.m2[,WSa.s:= scale(wsavg)]
aqm2.2011.m2[,RHa.s:= scale(rhavg)]
aqm2.2011.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2011.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2011days <- sort(unique(aqm2.2011.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2011)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2011)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2011.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2011.m2[aqm2.2011.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2011days,], aqm2.2011.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2011.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2011.m2[aqm2.2011.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2011days,], aqm2.2011.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2012



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2012<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2012.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2012<-as.data.frame(aqua.2012)
aqua.2012<-as.data.table(aqua.2012)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
days2012 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2012,aodid,day)
setkey(days2012 ,aodid,day)
aqm1.2012 <- merge(days2012,aqua.2012, all.x = T)  


#add lu-key
setkey(aqm1.2012,aodid)
setkey(key.lu,aodid)
aqm2.2012 <- merge(aqm1.2012, key.lu, all.x = T)
#add lu
setkey(aqm2.2012,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2012 <- merge(aqm2.2012, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2012$long_aod <- NULL
aqm2.2012$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2012 ,aodid)
aqm2.2012 <- merge(aqm2.2012 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2012,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2012)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2012, aodid)
lu.m <- makepointsmatrix(aqm2.2012[aqm2.2012[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2012, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2012,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2012 <- merge(aqm2.2012, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2012$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2012, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2012,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2012 <- merge(aqm2.2012, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2012$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2012, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2012,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2012 <- merge(aqm2.2012, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2012$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2012, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2012,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2012 <- merge(aqm2.2012, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2012$rainday)

#saveRDS(aqm2.2012,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2012,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2012$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2012)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2012,aodid)
setkey(key.pbl,aodid)
aqm2.2012 <- merge(aqm2.2012, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2012,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2012 <- merge(aqm2.2012, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2012$pblid<-NULL
#add month
aqm2.2012[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2012$season<-recode(aqm2.2012$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2012$seasonSW<-recode(aqm2.2012$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2012)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2012,aodid)
setkey(key.ndvi,aodid)
aqm2.2012 <- merge(aqm2.2012, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2012,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2012 <- merge(aqm2.2012, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2012,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2012)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2012)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2012, aodid)
aod.m <- makepointsmatrix(aqm2.2012[aqm2.2012[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2012, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2012,aodid,day)
aqm2.2012 <- merge(aqm2.2012,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2012,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2012, aodid)
aod.m <- makepointsmatrix(aqm2.2012[aqm2.2012[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2012, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2012,aodid,day)
aqm2.2012 <- merge(aqm2.2012,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2012,"closestmean","meanPM10")
summary(aqm2.2012$meanPM10)
#cleanup
keep(fgrid,aqm2.2012,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2012<-aqm2.2012[,obs:=1]
aqm2.2012[is.na(aod), obs:= 0]
# ws.2012<-select(aqm2.2012,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2012)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2012$prob <- predict(w1,type = c("response"))  
# aqm2.2012$wt <- 1/aqm2.2012$prob
# aqm2.2012$normwt <- aqm2.2012$wt/mean(aqm2.2012$wt)
# #Cleanup
# aqm2.2012<-aqm2.2012[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2012[,c("ndviid"):=NULL]
saveRDS(aqm2.2012,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2012.rds")
#aqm2.2012<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2012.m2 <- aqm2.2012[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2012.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2012.m2,aodid)
aqm2.2012.m2<-merge(aqm2.2012.m2,f)
aqm2.2012.m2$flag1000<-0
aqm2.2012.m2<-aqm2.2012.m2[numadata < 1000, flag1000 :=1]
aqm2.2012.m2$flag500<-0
aqm2.2012.m2<-aqm2.2012.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2012.m2[,tden.s:= scale(tden)]
aqm2.2012.m2[,elev.s:= scale(elev_m)]
aqm2.2012.m2[,pden.s:= scale(pop06)]
aqm2.2012.m2[,dist2A1.s:= scale(distA1)]
aqm2.2012.m2[,ndvi.s:= scale(ndvi)]
aqm2.2012.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2012.m2[,p_urb.s:= scale(pcturb )]
aqm2.2012.m2[,tempa.s:= scale(tempavg)]
aqm2.2012.m2[,WSa.s:= scale(wsavg)]
aqm2.2012.m2[,RHa.s:= scale(rhavg)]
aqm2.2012.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2012.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2012days <- sort(unique(aqm2.2012.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2012)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2012)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2012.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2012.m2[aqm2.2012.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2012days,], aqm2.2012.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2012.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2012.m2[aqm2.2012.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2012days,], aqm2.2012.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########Aqua.2013



####  #import LU
key.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")
fin.lu<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

###load Aqua
#load aod data
aqua.2013<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2013.rds")
#get rid of dplyr tbl_df until bug gets fixed
aqua.2013<-as.data.frame(aqua.2013)
aqua.2013<-as.data.table(aqua.2013)

#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#create full LU TS
days<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
days2013 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
setkey(aqua.2013,aodid,day)
setkey(days2013 ,aodid,day)
aqm1.2013 <- merge(days2013,aqua.2013, all.x = T)  


#add lu-key
setkey(aqm1.2013,aodid)
setkey(key.lu,aodid)
aqm2.2013 <- merge(aqm1.2013, key.lu, all.x = T)
#add lu
setkey(aqm2.2013,LUaodid)
setkey(fin.lu,LUaodid)
aqm2.2013 <- merge(aqm2.2013, fin.lu[,list(LUaodid,pop06,pcturb,elev_m,distA1,wflag,tden)], all.x = T)

#fix missing lat/long
aqm2.2013$long_aod <- NULL
aqm2.2013$lat_aod <- NULL
#add lu-key
setkey(fgrid,aodid)
setkey(aqm2.2013 ,aodid)
aqm2.2013 <- merge(aqm2.2013 , fgrid[,list(long_aod,lat_aod,aodid)], all.x = T)
#cleanup
keep(fgrid,aqm2.2013,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#join met 
#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2013)
#Temp
met.m <- makepointsmatrix(Temp, "long_met", "lat_met", "stn")
setkey(aqm2.2013, aodid)
lu.m <- makepointsmatrix(aqm2.2013[aqm2.2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2013, Temp[, list(day,tempavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "tempavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2013,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2013 <- merge(aqm2.2013, aqua.met[,list(day,tempavg,aodid)], all.x = T)
#summary(aqm2.2013$tempavg)


#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2013, Temp[, list(day,wsavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2013,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2013 <- merge(aqm2.2013, aqua.met[,list(day,wsavg,aodid)], all.x = T)
#summary(aqm2.2013$wsavg)


#rhavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2013, Temp[, list(day,rhavg,stn)], 
                            "aodid", "stn", "cloesetSTN", "rhavg", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2013,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2013 <- merge(aqm2.2013, aqua.met[,list(day,rhavg,aodid)], all.x = T)
#summary(aqm2.2013$rhavg)


#rainday
aqua.met <- nearestbyday(lu.m ,met.m , 
                            aqm2.2013, Temp[, list(day,rainday,stn)], 
                            "aodid", "stn", "cloesetSTN", "rainday", knearest = 10, maxdistance = NA)
#head(aqua.met)
setkey(aqm2.2013,aodid,day)
setkey(aqua.met,aodid,day)
aqm2.2013 <- merge(aqm2.2013, aqua.met[,list(day,rainday,aodid)], all.x = T)
#summary(aqm2.2013$rainday)

#saveRDS(aqm2.2013,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/tmpfile.rds")

#cleanup
keep(fgrid,aqm2.2013,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
aqm2.2013$LUaodid<-NULL
gc()

#Join PBL
fin.pbl<-readRDS("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
fin.pbl<-filter(fin.pbl,c==2013)
gc() 
key.pbl<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")

#add pbl-key
setkey(aqm2.2013,aodid)
setkey(key.pbl,aodid)
aqm2.2013 <- merge(aqm2.2013, key.pbl, all.x = T)
#add pbl
setkey(aqm2.2013,pblid,day)
setkey(fin.pbl,pblid,day)
aqm2.2013 <- merge(aqm2.2013, fin.pbl[,list(pblid,PBL,day)], all.x = T)
aqm2.2013$pblid<-NULL
#add month
aqm2.2013[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
aqm2.2013$season<-recode(aqm2.2013$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aqm2.2013$seasonSW<-recode(aqm2.2013$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")

#join NDVI to aod
fin.ndvi<-readRDS("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2013)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(aqm2.2013,aodid)
setkey(key.ndvi,aodid)
aqm2.2013 <- merge(aqm2.2013, key.ndvi, all.x = T)
#add ndvi
setkey(aqm2.2013,ndviid,m)
setkey(fin.ndvi,ndviid,m)
aqm2.2013 <- merge(aqm2.2013, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,aqm2.2013,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2013)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2013)

gc()

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2013, aodid)
aod.m <- makepointsmatrix(aqm2.2013[aqm2.2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2013, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2013,aodid,day)
aqm2.2013 <- merge(aqm2.2013,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2013,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2013, aodid)
aod.m <- makepointsmatrix(aqm2.2013[aqm2.2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2013, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2013,aodid,day)
aqm2.2013 <- merge(aqm2.2013,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2013,"closestmean","meanPM10")
summary(aqm2.2013$meanPM10)
#cleanup
keep(fgrid,aqm2.2013,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()

#########-------------------weights ############
aqm2.2013<-aqm2.2013[,obs:=1]
aqm2.2013[is.na(aod), obs:= 0]
# ws.2013<-select(aqm2.2013,obs,elev_m,PBL,m)
# #model
# w1<- glm(obs ~ elev_m+PBL+as.factor(m),family=binomial,data=ws.2013)
# #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
# aqm2.2013$prob <- predict(w1,type = c("response"))  
# aqm2.2013$wt <- 1/aqm2.2013$prob
# aqm2.2013$normwt <- aqm2.2013$wt/mean(aqm2.2013$wt)
# #Cleanup
# aqm2.2013<-aqm2.2013[, c("prob","wt") := NULL]
# gc()




#---------> save mods 2+3
#clean
aqm2.2013[,c("ndviid"):=NULL]
saveRDS(aqm2.2013,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2013.rds")
#aqm2.2013<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
#mod2
aqm2.2013.m2 <- aqm2.2013[!is.na(aod)]
#calculate low retrival day
f<- aqm2.2013.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
describe(f)
setkey(f,aodid)
setkey(aqm2.2013.m2,aodid)
aqm2.2013.m2<-merge(aqm2.2013.m2,f)
aqm2.2013.m2$flag1000<-0
aqm2.2013.m2<-aqm2.2013.m2[numadata < 1000, flag1000 :=1]
aqm2.2013.m2$flag500<-0
aqm2.2013.m2<-aqm2.2013.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2013.m2[,tden.s:= scale(tden)]
aqm2.2013.m2[,elev.s:= scale(elev_m)]
aqm2.2013.m2[,pden.s:= scale(pop06)]
aqm2.2013.m2[,dist2A1.s:= scale(distA1)]
aqm2.2013.m2[,ndvi.s:= scale(ndvi)]
aqm2.2013.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2013.m2[,p_urb.s:= scale(pcturb )]
aqm2.2013.m2[,tempa.s:= scale(tempavg)]
aqm2.2013.m2[,WSa.s:= scale(wsavg)]
aqm2.2013.m2[,RHa.s:= scale(rhavg)]
aqm2.2013.m2[,Raina.s:= scale(rainday)]
saveRDS(aqm2.2013.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")
gc()

#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2013days <- sort(unique(aqm2.2013.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2013)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2013)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2013.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2013.m2[aqm2.2013.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2013days,], aqm2.2013.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2013.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2013.m2[aqm2.2013.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2013days,], aqm2.2013.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()
