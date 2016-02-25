#load libraries 
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
library(readr)
library(bit64)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")

#load clipped grid 
fgrid <- read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/NewEUgrid/NewGridCH_ETRS.dbf")
head(fgrid)
fgrid<-as.data.table(fgrid)


#load aod data
aqua.2013<-readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/satellite/WORK/AOD.AQ.2013.rds")
#fix a bug, convert first from dplyr format to data.frame
aqua.2013<-as.data.frame(aqua.2013)
#convert to data.table
aqua.2013<-as.data.table(aqua.2013)

#create full LU TS
days<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
days2013 <- data.table(expand.grid(aodid = fgrid[, unique(aodid)], day = days))
days2013$aodid<-as.character(days2013$aodid)

#merge
setkey(aqua.2013,aodid,day)
setkey(days2013 ,aodid,day)
db2013 <- merge(days2013,aqua.2013, all.x = T)

#create single aod point per aodid per day 
db2013 <-db2013 %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )


# import land use component (need package bit64)
lup <- read.csv("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/CorVarAreaPerc.csv")
lup<-as.data.table(lup)
#add lu-key
setkey(db2013,aodid)
setkey(lup,aodid)
db2013 <- merge(db2013, lup, all.x = T)
head(db2013)


#add indus emis

inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/CalcVariables/Em_ind_2010_pt.dbf")
inds<-as.data.table(inds)
summary(inds$RASTERVALU)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
db2013<- db2013[RASTERVALU  == -9999 , RASTERVALU  := 0]
setnames(db2013,"RASTERVALU","inds2010pm")
head(db2013)


#add traff emis

inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/CalcVariables/Em_traff_2010_pt.dbf")
inds<-as.data.table(inds)
summary(inds$RASTERVALU)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
db2013<- db2013[RASTERVALU  == -9999 , RASTERVALU  := 0]
setnames(db2013,"RASTERVALU","traficpm")
head(db2013)



#add near major roads

inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/CalcVariables/NearMajorRd5000.dbf")
inds<-as.data.table(inds)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
setnames(db2013,"NEAR_DIST","nearmajorrd")
#take out non swiss regions
db2013<-filter(db2013,!is.na(nearmajorrd))

#add all road sum
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/CalcVariables/Sum_AllRds.dbf")
inds<-as.data.table(inds)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
setnames(db2013,"SUM","roadsum")
head(db2013)

#get rid of duplicate names to cut down on DB size
db2013<-dplyr::select(db2013,aodid,day,long_aod,lat_aod,aod,UN,QA,PercRes,PercInd,PercUrbgr,PercTotbld,PercNat,PercAgr,inds2010pm,traficpm,nearmajorrd,roadsum)
             
               



#add elev
inds<-read.csv("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/altitude.csv")
inds<-as.data.table(inds)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
head(db2013)

#get rid of duplicate names to cut down on DB size
db2013<-dplyr::select(db2013,aodid,day,long_aod,lat_aod,aod,UN,QA,PercRes,PercInd,PercUrbgr,PercTotbld,PercNat,PercAgr,inds2010pm,traficpm,nearmajorrd,roadsum,aspect_200,range_1km, slope_200m ,std_1km)

#additional elev
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/alt.dbf")
inds<-as.data.table(inds)
setkey(db2013,aodid)
setkey(inds,aodid)
db2013 <- merge(db2013, inds, all.x = T)
setnames(db2013,"RASTERVALU","elev")
#get rid of duplicate names to cut down on DB size
db2013<-dplyr::select(db2013,aodid,day,long_aod,lat_aod,aod,UN,QA,PercRes,PercInd,PercUrbgr,PercTotbld,PercNat,PercAgr,inds2010pm,traficpm,nearmajorrd,roadsum,aspect_200,range_1km, slope_200m ,std_1km,elev)

gc()

#load met
Temp<-read.csv("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/ecmwf/Switzerland/BLH_T2m_ALL.csv")
head(Temp)
Temp<-as.data.table(Temp)
Temp[, day:=as.Date(strptime(DATE, "%m/%d/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp<-filter(Temp,c==2013)


#add temp and pbl
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/temporal/LUT_MET_AOD.dbf")
inds$SHAPEID<-inds$NEAR_FID
inds<-as.data.table(inds)

#create full LU TS
days<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
days2013 <- data.table(expand.grid(aodid = inds[, unique(aodid)], day = days))
days2013$aodid<-as.character(days2013$aodid)

#merge
setkey(inds,aodid)
setkey(days2013 ,aodid)
x <- merge(days2013,inds, all.x = T)
head(x)


setkey(Temp,SHAPEID,day)
setkey(x,SHAPEID,day)
y <- merge(Temp,x, all.x = T)


#merge
setkey(db2013,aodid,day)
setkey(y ,aodid,day)
xdb2013 <- merge(db2013,y, all.x = T)
head(xdb2013)
xdb2013$temp<-xdb2013$T2m - 273.15


#get rid of duplicate names to cut down on DB size
xdb2013<-dplyr::select(xdb2013,aodid,day,aod,UN,QA,PercRes,PercInd,PercUrbgr,PercTotbld,PercNat,PercAgr,inds2010pm,traficpm,nearmajorrd,roadsum,aspect_200,range_1km, slope_200m ,std_1km,elev,BLH,temp)
gc()

#merge
setkey(xdb2013,aodid)
setkey(fgrid ,aodid)
xdb2013 <- merge(xdb2013,fgrid[,list(aodid,Long, Lat)], all.x = T)
head(xdb2013)
setnames(xdb2013,"Long","long_aod")
setnames(xdb2013,"Lat","lat_aod")


#mean PM calculations
PM25<-read.csv("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/PM/PMconcentrations.csv")
str(PM25)
PM25<-as.data.table(PM25)
PM25[, day:=as.Date(strptime(Date, "%m/%d/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25<-filter(PM25,c==2013)
str(PM25)
PM25$SiteName<-as.character(PM25$SiteName)
###NOTE this gives a warning
PM25<- PM25[PM10conc  == -999 , PM10conc  := 'NA']
str(PM25)

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "lon_wgs", "lat_wgs","SiteName")
xdb2013$aodid<-as.character(xdb2013$aodid)
setkey(xdb2013, aodid)
aod.m <- makepointsmatrix(xdb2013[xdb2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            xdb2013, PM25 [, list(day,PM25new,SiteName)], 
                            "aodid", "SiteName", "closest","PM25new",knearest = 10, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(xdb2013,aodid,day)
x1db2013 <- merge(xdb2013,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(x1db2013,"closestmean","meanPM25")
gc()
head(x1db2013)
summary(x1db2013$meanPM25)

#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM25, "lon_wgs", "lat_wgs","SiteName")
xdb2013$aodid<-as.character(xdb2013$aodid)
setkey(xdb2013, aodid)
aod.m <- makepointsmatrix(xdb2013[xdb2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            xdb2013, PM25 [, list(day,PM10conc,SiteName)], 
                            "aodid", "SiteName", "closest","PM10conc",knearest = 10, maxdistance = 60000, nearestmean = T)


gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(x1db2013,aodid,day)
x1db2013 <- merge(x1db2013,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(x1db2013,"closestmean","meanPM10")
summary(x1db2013$meanPM10)

#cleanup
#keep(fgrid,db2013,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()



#add regions 
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/region_LUT2.dbf")
inds<-as.data.table(inds)
setnames(inds,"GRID_CODE","regid")
setkey(x1db2013,aodid)
setkey(inds,aodid)
x1db2013 <- merge(x1db2013, inds[,list(aodid,regid)], all.x = T)

#add met regions 
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/region_lut_climate2.dbf")
inds<-as.data.table(inds)
setnames(inds,"Id","climid")
setkey(x1db2013,aodid)
setkey(inds,aodid)
x1db2013 <- merge(x1db2013, inds[,list(aodid,climid)], all.x = T)


#add major road sum
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/CalcVariables/Sum_MajRds.dbf")
inds<-as.data.table(inds)
setkey(x1db2013,aodid)
setkey(inds,aodid)
x1db2013 <- merge(x1db2013, inds[,list(aodid,SUM)], all.x = T)
setnames(x1db2013,"SUM","mjroadsum")
head(x1db2013)




#add ndvi 
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/NDVI_variable.dbf")
inds<-as.data.table(inds)
setnames(inds,"RASTERVALU","ndvi")
setkey(x1db2013,aodid)
setkey(inds,aodid)
x1db2013 <- merge(x1db2013, inds[,list(aodid,ndvi)], all.x = T)




#TCC
i1<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/temporal/ecmwf_other_var/tcc2013xynew.dbf"))
i1$idx<-as.character(i1$ID)
i1<-as.data.table(i1)
setnames(i1,"DATE","day")
setkey(i1, idx)
met.m <- makepointsmatrix(i1, "LAT", "LON", "idx")
x1db2013$aodid<-as.character(x1db2013$aodid)
setkey(x1db2013, aodid)
lu.m <- makepointsmatrix(x1db2013[x1db2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                           x1db2013, i1[, list(day,TCC,idx)], 
                            "aodid", "idx", "meanT", "TCC", knearest = 1, maxdistance = NA)


setkey(x1db2013,aodid,day)
setkey(closestaodse,aodid,day)
x1db2013 <- merge(x1db2013, closestaodse[,list(day,TCC,aodid)], all.x = T)


#Tp
i1<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/temporal/ecmwf_other_var/tp2013xynew.dbf"))
i1$idx<-as.character(i1$ID)
i1<-as.data.table(i1)
setnames(i1,"DATE","day")
setkey(i1, idx)
met.m <- makepointsmatrix(i1, "LAT", "LON", "idx")
x1db2013$aodid<-as.character(x1db2013$aodid)
setkey(x1db2013, aodid)
lu.m <- makepointsmatrix(x1db2013[x1db2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                           x1db2013, i1[, list(day,TP,idx)], 
                            "aodid", "idx", "meanT", "TP", knearest = 1, maxdistance = NA)


setkey(x1db2013,aodid,day)
setkey(closestaodse,aodid,day)
x1db2013 <- merge(x1db2013, closestaodse[,list(day,TP,aodid)], all.x = T)

#WS
i3<-fread("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/temporal/ecmwf_other_var/u2013xyfinal.csv")
i4<-fread("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/temporal/ecmwf_other_var/v2013xyfinal.csv")

i1<-full_join(i3,i4,by = c("DATE","lat","lon"))
i1<-as.data.table(i1)
i1[, day:=as.Date(strptime(DATE, "%m/%d/%Y"))]
i1$ws<-sqrt(i1$U^2+i1$V^2)
i1$idx<-as.character(i1$id.y)
setkey(i1, idx)
met.m <- makepointsmatrix(i1, "lat", "lon", "idx")
x1db2013$aodid<-as.character(x1db2013$aodid)
setkey(x1db2013, aodid)
lu.m <- makepointsmatrix(x1db2013[x1db2013[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                           x1db2013, i1[, list(day,ws,idx)], 
                            "aodid", "idx", "meanT", "ws", knearest = 1, maxdistance = NA)


setkey(x1db2013,aodid,day)
setkey(closestaodse,aodid,day)
x1db2013 <- merge(x1db2013, closestaodse[,list(day,ws,aodid)], all.x = T)
head(x1db2013)


#add additional emis
inds<-read.dbf("/media/NAS/Uni/Projects/P059_SWISS_AOD/Satellite air pollution/data/landuse/Emissions.dbf")
inds<-as.data.table(inds)
setkey(x1db2013,aodid)
setkey(inds,aodid)
x1db2013 <- merge(x1db2013, inds[,list(aodid,emtr2005 ,emwo2005 ,emho2005 ,emag2005 ,emwo2010 ,emho2010, emag2010)], all.x = T)


#take out uneeded
#save
gc()
saveRDS(x1db2013,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.AQ.2013.rds")
#x1db2013<- readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.AQ.2013.rds")

#calculate weights
x1db2013[, m := as.numeric(format(day, "%m")) ]

x1db2013<-x1db2013[,obs:=1]
x1db2013[is.na(aod), obs:= 0]
ws.2013<-select(x1db2013,obs,elev,BLH,m,temp,aodid,day)
#to save memory
gc()

w1 <- glm(obs ~ elev+temp+BLH+as.factor(m),family=binomial,data=ws.2013)
ws.2013$prob <- predict(w1 ,type = c("response"))  
ws.2013$wt <- 1/ws.2013$prob
ws.2013$normwt <- ws.2013$wt/mean(ws.2013$wt)
ws.2013[, c("prob", "wt","obs","elev", "BLH" , "m","temp"  ) := NULL]
gc()

setkey(x1db2013,aodid,day)
setkey(ws.2013,aodid,day)
x1db2013 <- merge(x1db2013,ws.2013,all.x = T)


#create mod 2 file
x1db2013.m2 <- x1db2013[!is.na(aod)]
#rm m3
rm(x1db2013)
gc()
#save mod2
saveRDS(x1db2013.m2,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod2.AQ.2013.rds")
gc()


#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
x1db2013days <- sort(unique(x1db2013.m2$day))


#-------> meanPM25  for mod 2+3


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "lon_wgs", "lat_wgs","SiteName")
#create aod terra matrix
x1db2013.m2$aodid<-as.character(x1db2013.m2$aodid)
setkey(x1db2013.m2, aodid)
aod.m <- makepointsmatrix(x1db2013.m2[x1db2013.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")



#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% x1db2013days,], x1db2013.m2, 
                           "SiteName", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


closestaod[,closestknn :=NULL]
setkey(PM25,SiteName,day)
setkey(closestaod,SiteName,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod1.AQ.2013.PM.rds")

