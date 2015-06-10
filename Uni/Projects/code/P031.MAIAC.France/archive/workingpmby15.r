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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")




#2003
aqm2.2003.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")


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
aqm2.2003.m2$aodid<-as.character(aqm2.2003.m2$aodid)
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2003.m2$aodid<-as.character(aqm2.2003.m2$aodid)
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2004
aqm2.2004.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")


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
aqm2.2004.m2$aodid<-as.character(aqm2.2004.m2$aodid)
setkey(aqm2.2004.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2004.m2[aqm2.2004.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2004.m2$aodid<-as.character(aqm2.2004.m2$aodid)
setkey(aqm2.2004.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2004.m2[aqm2.2004.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2005
aqm2.2005.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")


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
aqm2.2005.m2$aodid<-as.character(aqm2.2005.m2$aodid)
setkey(aqm2.2005.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2005.m2[aqm2.2005.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2005.m2$aodid<-as.character(aqm2.2005.m2$aodid)
setkey(aqm2.2005.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2005.m2[aqm2.2005.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2006
aqm2.2006.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")


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
aqm2.2006.m2$aodid<-as.character(aqm2.2006.m2$aodid)
setkey(aqm2.2006.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2006.m2[aqm2.2006.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2006.m2$aodid<-as.character(aqm2.2006.m2$aodid)
setkey(aqm2.2006.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2006.m2[aqm2.2006.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2007
aqm2.2007.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")


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
aqm2.2007.m2$aodid<-as.character(aqm2.2007.m2$aodid)
setkey(aqm2.2007.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2007.m2[aqm2.2007.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2007.m2$aodid<-as.character(aqm2.2007.m2$aodid)
setkey(aqm2.2007.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2007.m2[aqm2.2007.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2008
aqm2.2008.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")


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
aqm2.2008.m2$aodid<-as.character(aqm2.2008.m2$aodid)
setkey(aqm2.2008.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2008.m2[aqm2.2008.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2008.m2$aodid<-as.character(aqm2.2008.m2$aodid)
setkey(aqm2.2008.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2008.m2[aqm2.2008.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2009
aqm2.2009.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")


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
aqm2.2009.m2$aodid<-as.character(aqm2.2009.m2$aodid)
setkey(aqm2.2009.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2009.m2[aqm2.2009.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2009.m2$aodid<-as.character(aqm2.2009.m2$aodid)
setkey(aqm2.2009.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2009.m2[aqm2.2009.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2010
aqm2.2010.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")


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
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")



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
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM10.rds")



#2011
aqm2.2011.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")


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
aqm2.2011.m2$aodid<-as.character(aqm2.2011.m2$aodid)
setkey(aqm2.2011.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2011.m2[aqm2.2011.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2011.m2$aodid<-as.character(aqm2.2011.m2$aodid)
setkey(aqm2.2011.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2011.m2[aqm2.2011.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2012
aqm2.2012.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")


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
aqm2.2012.m2$aodid<-as.character(aqm2.2012.m2$aodid)
setkey(aqm2.2012.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2012.m2[aqm2.2012.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2012.m2$aodid<-as.character(aqm2.2012.m2$aodid)
setkey(aqm2.2012.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2012.m2[aqm2.2012.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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



#2013
aqm2.2013.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")


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
aqm2.2013.m2$aodid<-as.character(aqm2.2013.m2$aodid)
setkey(aqm2.2013.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2013.m2[aqm2.2013.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
aqm2.2013.m2$aodid<-as.character(aqm2.2013.m2$aodid)
setkey(aqm2.2013.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2013.m2[aqm2.2013.m2[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
head(aod.m)



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
