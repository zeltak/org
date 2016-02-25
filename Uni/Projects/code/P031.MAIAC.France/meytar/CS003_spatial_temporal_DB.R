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
library(lubridate)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
# "sticky" DF's
#load clipped france grid 
fgrid <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/france.grid.csv")




#land use data
m1<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/merge_data.csv")
m2<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/merge_data2.csv")
setkey(m1,aodid)
setkey(m2,aodid)
m3<-merge(m1,m2)
#summary(m3)
fin.lu<-select(m3,aodid, Longitude,Latitude , pop06,pcturb,elev_m,distA1,wflag,tden )
setnames(fin.lu,"aodid","LUaodid")
saveRDS(fin.lu,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.lu.rds")

#use geomerge to find closest point per day
met.m <- makepointsmatrix(fin.lu, "Longitude", "Latitude", "LUaodid")
setkey(fgrid, aodid)
lu.m <- makepointsmatrix(fgrid[fgrid[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#create fake day
fin.lu$day<-as.Date("2000-01-01")
fgrid$day<-as.Date("2000-01-01")
#we need to leave at least one numeric variable to make the join work... here we use pcturb
key.lu <- nearestbyday(lu.m ,met.m ,fgrid, fin.lu[, list(day,LUaodid,pcturb)], 
                            "aodid", "LUaodid","closestID", "pcturb",knearest = 1, maxdistance = NA)

key.lu<-select(key1,aodid,closestID)
setnames(key.lu,"closestID","LUaodid")
saveRDS(key.lu,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.lu.rds")

#emission
out<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/emissions_france.csv")
out<-select(out,c(1:15))
setnames(out,"Longitude","long_ems")
setnames(out,"Latitude","lat_ems")
out$emsid<-paste(out$long_ems,out$lat_ems,sep="-")
saveRDS(out,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.emission.rds")
write.csv(out,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.emission.csv")

#use geomerge to find closest point per day
met.m <- makepointsmatrix(out, "long_ems", "lat_ems", "emsid")
setkey(fgrid, aodid)
lu.m <- makepointsmatrix(fgrid[fgrid[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#create fake day
out$day<-as.Date("2000-01-01")
fgrid$day<-as.Date("2000-01-01")
#we need to leave at least one numeric variable to make the join work... here we use pcturb
key.lu <- nearestbyday(lu.m ,met.m ,fgrid, out[, list(day,emsid,PM2.5)], 
                            "aodid", "emsid","closestID", "PM2.5",knearest = 1, maxdistance = NA)

key.lu<-select(key.lu,aodid,closestID)
setnames(key.lu,"closestID","emsid")
saveRDS(key.lu,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ems.rds")




########### import NDVI
ndvidh17v04<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h17v04.csv")
ndvidh18v03<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h18v03.csv")
ndvidh18v04<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h18v04.csv")
ndvi<-rbindlist(list(ndvidh17v04,ndvidh18v04,ndvidh18v03))
#tail(ndvi)
setnames(ndvi,"V4","lat_ndvi")
setnames(ndvi,"V5","long_ndvi")
setnames(ndvi,"V1","year")
setnames(ndvi,"V2","m")
setnames(ndvi,"V3","ndvi")
ndvi$V6<-NULL
#summary(ndvi)
#create ndviid
ndvi$ndviid<-paste(ndvi$long_ndvi,ndvi$lat_ndvi,sep="-")
#-0.3 is missing in MODIS NDVI. these are points over the ocean
ndvi<- ndvi[ndvi == -0.3 , ndvi  := NA]
ndvi<-na.omit(ndvi)
saveRDS(ndvi,"/media/NAS/Uni/Data/Europe/france/ndvi_france/out/fin.ndvi.rds")
#create grid
ngrid <- unique(ndvi, by="ndviid")
#use geomerge to find closest point per day

met.m <- makepointsmatrix(ngrid, "long_ndvi", "lat_ndvi", "ndviid")
setkey(fgrid, aodid)
lu.m <- makepointsmatrix(fgrid[fgrid[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#create fake day
ngrid$day<-as.Date("2000-01-01")
fgrid$day<-as.Date("2000-01-01")
#we need to leave at least one numeric variable to make the join work... here we use ndvi
key.ndvi <- nearestbyday(lu.m ,met.m ,fgrid, ngrid[, list(day,ndviid,ndvi)], 
                            "aodid", "ndviid","closestID", "ndvi",knearest = 1, maxdistance = NA)

key.ndvi<-select(key.ndvi,aodid,closestID)
setnames(key.ndvi,"closestID","ndviid")
saveRDS(key.ndvi,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")


########### import pbl
pbl1<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._0_250.csv")
pbl2<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._0_250.csv")
pbl3<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._251_500.csv")
pbl4<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._501_750.csv")
pbl5<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1001_1250.csv")
pbl6<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1251_1500.csv")
pbl7<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1751_2000.csv")
pbl8<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._2001_2250.csv")
pbl9<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/pbl_france._2501_2750.csv")
pbl11<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._2751_3000.csv")
pbl12<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3251_3500.csv")
pbl13<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3501_3750.csv")
pbl14<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3751_4000.csv")
pbl15<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4001_4250.csv")
pbl16<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4251_4500.csv")
pbl17<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4501_4750.csv")
pbl18<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4751_5000.csv")
pbl19<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._5001_5250.csv")
pbl20<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._5251_5479.csv")

pbl<-rbindlist(list(pbl1,pbl2,pbl3,pbl4,pbl5,pbl6,pbl7,pbl8,pbl9,pbl11,pbl12,pbl13,pbl14,pbl15,pbl16,pbl17,pbl18,pbl19,pbl20))

#createPBL ID 
pbl$pblid<-paste(pbl$X,pbl$Y,sep="-")
#dates
pbl$day<-as.Date(strptime(pbl$Date, "%Y-%m-%d"))
pbl$Date<-NULL
pbl[, c := as.numeric(format(day, "%Y")) ]
saveRDS(pbl,"/media/NAS/Uni/Data/Europe/france/pbl/final_csv/fin.pbl.rds")
#create grid
pgrid <- unique(pbl, by="pblid")
setnames(pgrid,"X","long_pbl")
setnames(pgrid,"Y","lat_pbl")

#use geomerge to find closest point per day

met.m <- makepointsmatrix(pgrid, "long_pbl", "lat_pbl", "pblid")
setkey(fgrid, aodid)
lu.m <- makepointsmatrix(fgrid[fgrid[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#create fake day
pgrid$day<-as.Date("2000-01-01")
fgrid$day<-as.Date("2000-01-01")
#we need to leave at least one numeric variable to make the join work... here we use PBL
key.pbl <- nearestbyday(lu.m ,met.m ,fgrid, pgrid[, list(day,pblid,PBL)], 
                            "aodid", "pblid","closestID", "PBL",knearest = 1, maxdistance = NA)

key.pbl<-select(key.pbl,aodid,closestID)
setnames(key.pbl,"closestID","pblid")
saveRDS(key.pbl,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.pbl.rds")




#Met (temporal variables)
Temp <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/Met_france.csv")
Temp2 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/Met2013.csv")
Temp<- rbindlist(list(Temp,Temp2))
#head(Temp)
Temp[, day:=as.Date(strptime(Date, "%d-%m-%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Date","V1"):=NULL]
setnames(Temp,"long","long_met")
setnames(Temp,"lat","lat_met")
saveRDS(Temp,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")



#----------------------------------> PM Data
#PM25
#calculate meanPM per grid per day to each station (excluding first station)


#read file with all PM25 observations for 2000-2010 (including daily mean calculated from hourly data in days with at least #18 hourly obs.)
PM25 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM25.all.csv")
str(PM25)                                                                       
PM25$day<-as.Date(PM25$day,format="%Y-%m-%d")
PM25$year<-year(PM25$day)
summary(PM25)

#create single aod point per aodid per day (this addresses cartesean error below)
PM25.gr <-PM25 %>%
    group_by(stn,day,year) %>%
    summarise_each(funs(mean),Long,Lat,PM25)
setnames(PM25.gr,"Lat","lat_pm25")
setnames(PM25.gr,"Long","long_pm25")
setnames(PM25.gr,"PM25","pm25")
setnames(PM25.gr,"year","c")

#PM25.2011
PM25.2011 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM25_ 2011 .csv")
str(PM25.2011)                                                                       
PM25.2011[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM25.2011[, c := as.numeric(format(day, "%Y")) ]
setnames(PM25.2011,"CODESTATION","stn")
setnames(PM25.2011,"lat","lat_pm25")
setnames(PM25.2011,"long","long_pm25")
setnames(PM25.2011,"ALTITUDE","stn.elev")
setnames(PM25.2011,"PMconc","pm25")
PM25.2011<-select(PM25.2011,stn,day,c,lat_pm25,long_pm25,stn.elev,pm25)
PM25.2011 <- PM25.2011[pm25 != 'NaN']
PM25.2011 <- PM25.2011[lat_pm25 != 'NaN']
PM25.2011 <- PM25.2011[long_pm25 != 'NaN']
summary(PM25.2011$pm25)
PM25.2011 <- PM25.2011[PM25.2011$pm25 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM25.2011 <-PM25.2011 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm25,lat_pm25,stn.elev,pm25)
PM25.2011 <- PM25.2011[PM25.2011$pm25 >= 0]
PM25.2011 <- PM25.2011[PM25.2011$pm25 <= 300]

#PM25.2012
PM25.2012 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM25_ 2012 .csv")
str(PM25.2012)                                                                       
PM25.2012[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM25.2012[, c := as.numeric(format(day, "%Y")) ]
setnames(PM25.2012,"CODESTATION","stn")
setnames(PM25.2012,"lat","lat_pm25")
setnames(PM25.2012,"long","long_pm25")
setnames(PM25.2012,"ALTITUDE","stn.elev")
setnames(PM25.2012,"PMconc","pm25")
PM25.2012<-select(PM25.2012,stn,day,c,lat_pm25,long_pm25,stn.elev,pm25)
PM25.2012 <- PM25.2012[pm25 != 'NaN']
PM25.2012 <- PM25.2012[lat_pm25 != 'NaN']
PM25.2012 <- PM25.2012[long_pm25 != 'NaN']
summary(PM25.2012$pm25)
PM25.2012 <- PM25.2012[PM25.2012$pm25 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM25.2012 <-PM25.2012 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm25,lat_pm25,stn.elev,pm25)
PM25.2012 <- PM25.2012[PM25.2012$pm25 >= 0]
PM25.2012 <- PM25.2012[PM25.2012$pm25 <= 300]

#PM25.2013
PM25.2013 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM25_2013.csv")
str(PM25.2013)                                                                       
PM25.2013$lat<-as.numeric(PM25.2013$lat)
PM25.2013$long<-as.numeric(PM25.2013$long)
PM25.2013$ALTITUDE<-as.numeric(PM25.2013$ALTITUDE)
PM25.2013[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM25.2013[, c := as.numeric(format(day, "%Y")) ]
setnames(PM25.2013,"CODESTATION","stn")
setnames(PM25.2013,"lat","lat_pm25")
setnames(PM25.2013,"long","long_pm25")
setnames(PM25.2013,"ALTITUDE","stn.elev")
setnames(PM25.2013,"PMconc","pm25")
PM25.2013<-select(PM25.2013,stn,day,c,lat_pm25,long_pm25,stn.elev,pm25)
PM25.2013 <- PM25.2013[pm25 != 'NaN']
PM25.2013 <- PM25.2013[lat_pm25 != 'NaN']
PM25.2013 <- PM25.2013[long_pm25 != 'NaN']
summary(PM25.2013$pm25)
PM25.2013 <- PM25.2013[PM25.2013$pm25 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM25.2013 <-PM25.2013 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm25,lat_pm25,stn.elev,pm25)
PM25.2013 <- PM25.2013[PM25.2013$pm25 >= 0]
PM25.2013 <- PM25.2013[PM25.2013$pm25 <= 300]


PM25.n<-rbindlist(list(PM25.gr,PM25.2011[,!6,with=FALSE],PM25.2012[,!6,with=FALSE],PM25.2013[,!6,with=FALSE]))
saveRDS(PM25.n,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
#describe(PM25$pm25)



#----------------------------------> PM Data
#PM10 
#calculate meanPM per grid per day to each station (excluding first station)

#read file with all PM10 observations for 2000-2010 (including daily mean calculated from hourly data in days with at least #18 hourly obs.)
PM10 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM10.all.csv")
str(PM10)                                                                       
PM10$day<-as.Date(PM10$day,format="%Y-%m-%d")
PM10$year<-year(PM10$day)
summary(PM10)

#create single aod point per aodid per day (this addresses cartesean error below)
PM10.gr <-PM10 %>%
    group_by(stn,day,year) %>%
    summarise_each(funs(mean),Long,Lat,PM10)
setnames(PM10.gr,"Lat","lat_pm10")
setnames(PM10.gr,"Long","long_pm10")
setnames(PM10.gr,"PM10","pm10")
setnames(PM10.gr,"year","c")

#PM10.2011
PM10.2011 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM10_ 2011 .csv")
str(PM10.2011)                                                                       
PM10.2011[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM10.2011[, c := as.numeric(format(day, "%Y")) ]
setnames(PM10.2011,"CODESTATION","stn")
setnames(PM10.2011,"lat","lat_pm10")
setnames(PM10.2011,"long","long_pm10")
setnames(PM10.2011,"ALTITUDE","stn.elev")
setnames(PM10.2011,"PMconc","pm10")
PM10.2011<-select(PM10.2011,stn,day,c,lat_pm10,long_pm10,stn.elev,pm10)
PM10.2011 <- PM10.2011[pm10 != 'NaN']
PM10.2011 <- PM10.2011[lat_pm10 != 'NaN']
PM10.2011 <- PM10.2011[long_pm10 != 'NaN']
summary(PM10.2011$pm10)
PM10.2011 <- PM10.2011[PM10.2011$pm10 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM10.2011 <-PM10.2011 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm10,lat_pm10,stn.elev,pm10)
PM10.2011 <- PM10.2011[PM10.2011$pm10 >= 0]
PM10.2011 <- PM10.2011[PM10.2011$pm10 <= 300]

#PM10.2012
PM10.2012 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM10_ 2012 .csv")
head(PM10.2012)                                                                       
PM10.2012[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM10.2012[, c := as.numeric(format(day, "%Y")) ]
setnames(PM10.2012,"CODESTATION","stn")
setnames(PM10.2012,"lat","lat_pm10")
setnames(PM10.2012,"lon","long_pm10")
setnames(PM10.2012,"alt","stn.elev")
setnames(PM10.2012,"PMconc","pm10")
PM10.2012<-select(PM10.2012,stn,day,c,lat_pm10,long_pm10,stn.elev,pm10)
PM10.2012 <- PM10.2012[pm10 != 'NaN']
PM10.2012 <- PM10.2012[lat_pm10 != 'NaN']
PM10.2012 <- PM10.2012[long_pm10 != 'NaN']
summary(PM10.2012$pm10)
PM10.2012 <- PM10.2012[PM10.2012$pm10 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM10.2012 <-PM10.2012 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm10,lat_pm10,stn.elev,pm10)
PM10.2012 <- PM10.2012[PM10.2012$pm10 >= 0]
PM10.2012 <- PM10.2012[PM10.2012$pm10 <= 300]

#PM10.2013

PM10.2013 <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM10_2013.csv")
str(PM10.2013)                                                                       
PM10.2013$lat<-as.numeric(PM10.2013$lat)
PM10.2013$long<-as.numeric(PM10.2013$long)
PM10.2013$ALTITUDE<-as.numeric(PM10.2013$ALTITUDE)
PM10.2013[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
PM10.2013[, c := as.numeric(format(day, "%Y")) ]
setnames(PM10.2013,"CODESTATION","stn")
setnames(PM10.2013,"lat","lat_pm10")
setnames(PM10.2013,"long","long_pm10")
setnames(PM10.2013,"ALTITUDE","stn.elev")
setnames(PM10.2013,"PMconc","pm10")
PM10.2013<-select(PM10.2013,stn,day,c,lat_pm10,long_pm10,stn.elev,pm10)
PM10.2013 <- PM10.2013[pm10 != 'NaN']
PM10.2013 <- PM10.2013[lat_pm10 != 'NaN']
PM10.2013 <- PM10.2013[long_pm10 != 'NaN']
summary(PM10.2013$pm10)
PM10.2013 <- PM10.2013[PM10.2013$pm10 >= 0]
#create single aod point per aodid per day (this addresses cartesean error below)
PM10.2013 <-PM10.2013 %>%
    group_by(c,stn,day) %>%
    summarise_each(funs(mean),long_pm10,lat_pm10,stn.elev,pm10)
PM10.2013 <- PM10.2013[PM10.2013$pm10 >= 0]
PM10.2013 <- PM10.2013[PM10.2013$pm10 <= 300]



PM10<-rbindlist(list(PM10,PM10.2011,PM10.2012,PM10.2013))
saveRDS(PM10,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
#describe(PM10$pm10)
