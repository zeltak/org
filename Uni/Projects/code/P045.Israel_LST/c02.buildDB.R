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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")

#import clipped grid
fullgrid<-fread("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/gridXY_IL.csv")

#load LST data
aqua.2003<-readRDS("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/lst.AQ.2003.rds")
head(aqua.2003)
# #get rid of dplyr tbl_df until bug gets fixed
# aqua.2003<-as.data.frame(aqua.2003)
# aqua.2003<-as.data.table(aqua.2003)

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
days2003 <- data.table(expand.grid(lstid = fullgrid[, unique(lstid)], day = days))
#merge
setkey(aqua.2003,lstid,day)
setkey(days2003 ,lstid,day)
db2003 <- merge(days2003,aqua.2003, all.x = T)

fullgrid<-select(fullgrid,lstid ,  stn,   itm_e ,     itm_n ,   ELEVATION,  ASPECT  ,   roadden ,   DENS_POP,   ndviid  ,   long_ndvi , lat_ndvi  )

#######spatial 
#bring in all spatial components
    #merge
    setkey(db2003,lstid)
    setkey(fullgrid ,lstid)
    db2003 <- merge(db2003,fullgrid, all.x = T)  
gc()
head(db2003)

#add month
db2003[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
db2003$season<-recode(db2003$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
db2003$seasonSW<-recode(db2003$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


#join NDVI to lst
fin.ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
fin.ndvi<-filter(fin.ndvi,year==2003)
gc() 
key.ndvi<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/key.ndvi.rds")
#add ndvi-key
setkey(db2003,lstid)
setkey(key.ndvi,lstid)
db2003 <- merge(db2003, key.ndvi, all.x = T)
#add ndvi
setkey(db2003,ndviid,m)
setkey(fin.ndvi,ndviid,m)
db2003 <- merge(db2003, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
#cleanup
keep(fgrid,db2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()





#load met
Temp<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/met/fin.met.rds")
Temp<-filter(Temp,c==2003)

#wsavg
aqua.met <- nearestbyday(lu.m ,met.m , 
                            db2003, Temp[, list(day,wsavg,stn)], 
                            "lstid", "stn", "cloesetSTN", "wsavg", knearest = 10, maxdistance = 20000)
setkey(db2003,lstid,day)
setkey(aqua.met,lstid,day)
db2003 <- merge(db2003, aqua.met[,list(day,wsavg,lstid)], all.x = T)


#WS cleanup
keep(fgrid,db2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
db2003$LUlstid<-NULL
gc()
#################STOP 

#mean Ta calculations
Ta<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/Ta.rds")
Ta<-filter(Ta,c==2003)


#-------> mean Ta  for mod 2+3
Ta.m <- makepointsmatrix(Ta, "long_Ta", "lat_Ta", "stn")
setkey(db2003, lstid)
lst.m <- makepointsmatrix(db2003[db2003[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")

Taj1<- nearestbyday(lst.m  ,Ta.m , 
                            db2003, Ta [, list(day,Ta,stn)], 
                            "lstid", "stn", "closest","Ta",knearest = 10, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(Taj1,lstid,day)
setkey(db2003,lstid,day)
db2003 <- merge(db2003,Taj1[,list(day,lstid,closestmean)],all.x = T)
setnames(db2003,"closestmean","meanTa")
gc()

#take out uneeded
#save
gc()
saveRDS(db2003,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
gc()


# take out missing night LST >>> mod3 night
# take out missing day LST >>> mod3 night

#create mod 2 file
db2003.m2 <- db2003[!is.na(lst)]
#rm m3
rm(db2003)
gc()
#save mod2
saveRDS(db2003.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()

#--------->mod1
#Ta
#to fix missing days issues resulting in cartesean error
db2003days <- sort(unique(db2003.m2$day))

#Ta import again
Ta<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/Ta.rds")
Ta<-filter(Ta,c==2003)
Ta10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/Ta10.rds")
Ta10<-filter(Ta10,c==2003)

########### join lst to Ta
#create Ta matrix
Ta.m <- makepointsmatrix(Ta, "long_Ta", "lat_Ta", "stn")
#create lst terra matrix
db2003.m2$lstid<-as.character(db2003.m2$lstid)
setkey(db2003.m2,lstid)
lst.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")



#run function
closestlst <- nearestbyday(Ta.m, lst.m, 
                           Ta[day %in% db2003days,], db2003.m2, 
                           "stn", "lstid", "closest", "lst", knearest = 9, maxdistance = 1500)


#closestlst[,i.stn :=NULL]
closestlst[,closestknn :=NULL]

setkey(Ta,stn,day)
setkey(closestlst,stn,day)
Ta.m1 <- merge(Ta, closestlst, all.x = T)
Ta.m1<-Ta.m1[!is.na(lst)]

#save mod 1
saveRDS(Ta.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.Ta.rds")

########### join lst to Ta10
#create Ta matrix
Ta.m <- makepointsmatrix(Ta10, "long_Ta10", "lat_Ta10", "stn")
#create lst terra matrix
db2003.m2$lstid<-as.character(db2003.m2$lstid)
setkey(db2003.m2,lstid)
lst.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")

#run function
closestlst <- nearestbyday(Ta.m, lst.m, 
                           Ta10[day %in% db2003days,], db2003.m2, 
                           "stn", "lstid", "closest", "lst", knearest = 9, maxdistance = 1500)


#closestlst[,i.stn :=NULL]
closestlst[,closestknn :=NULL]

setkey(Ta10,stn,day)
setkey(closestlst,stn,day)
Ta10.m1 <- merge(Ta10, closestlst, all.x = T)
Ta10.m1<-Ta10.m1[!is.na(lst)]

#save mod 1
saveRDS(Ta10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.Ta10.rds")

#cleanup
keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()
