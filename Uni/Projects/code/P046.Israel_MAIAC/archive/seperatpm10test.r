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

########### import datasets
#import full XY

fullxy<-fread("/media/NAS/Uni/Data/Israel/IPA_stations/pmstn_miss.csv")
setnames(fullxy,"V2","stn")
setnames(fullxy,"V3","x_stn_ITM.n")
setnames(fullxy,"V4", "y_stn_ITM.n")
fullxy<-fullxy[,c(2:4),with=FALSE]

#Temp
temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp.csv")
temp$date<-paste(temp$Day,temp$Month,temp$Year,sep="/")
temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
temp[, c := as.numeric(format(day, "%Y")) ]
temp[,c("Year","Month","Day","date"):=NULL]
setnames(temp,"StationID/AOD","stn")
setnames(temp,"X","x_stn_ITM")
setnames(temp,"Y", "y_stn_ITM")

setkey(fullxy , stn)
setkey(temp, stn)
temp.1 <- merge(temp, fullxy, all.x = T)


temp[,length(na.omit(Temp)),by=list(stn,c)]
temp[, Temp_miss := length(na.omit(Temp)),by=list(stn,c)]
temp<-temp[Temp_miss > 300]




#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
setnames(WD,"StationID/AOD","stn")
setnames(WD,"X","x_stn_ITM")
setnames(WD,"Y", "y_stn_ITM")
WD[,length(na.omit(WD)),by=list(stn,c)]
WD[, WD_miss := length(na.omit(WD)),by=list(stn,c)]
WD<-WD[WD_miss > 300]



#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","V10","Day","date"):=NULL]
setnames(RH,"StationID/AOD","stn")
setnames(RH,"X","x_stn_ITM")
setnames(RH,"Y", "y_stn_ITM")
RH[,length(na.omit(RH)),by=list(stn,c)]
RH[, RH_miss := length(na.omit(RH)),by=list(stn,c)]
RH<-RH[RH_miss > 300]



#no2
no2 <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/NO2.csv")
no2$date<-paste(no2$Day,no2$Month,no2$Year,sep="/")
no2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
no2[, c := as.numeric(format(day, "%Y")) ]
no2[,c("Year","Month","Day","date"):=NULL]
no2[,length(na.omit(NO2)),by=list(stn,c)]
no2[, no2_miss := length(na.omit(NO2)),by=list(stn,c)]
no2<-no2[no2_miss > 300]


#so2
so2 <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/SO2.csv")
so2$date<-paste(so2$Day,so2$Month,so2$Year,sep="/")
so2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
so2[, c := as.numeric(format(day, "%Y")) ]
so2[,c("Year","Month","Day","date"):=NULL]
so2[,length(na.omit(SO2)),by=list(stn,c)]
so2[, so2_miss := length(na.omit(SO2)),by=list(stn,c)]
so2<-so2[so2_miss > 300]


#ws
ws <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/WS.csv")
ws$date<-paste(ws$Day,ws$Month,ws$Year,sep="/")
ws[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
ws[, c := as.numeric(format(day, "%Y")) ]
ws[,c("Year","Month","Day","date"):=NULL]
ws[,length(na.omit(WS)),by=list(stn,c)]
ws[, WS_miss := length(na.omit(WS)),by=list(stn,c)]
ws<-ws[WS_miss > 300]



###load Terra

#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyears.RDS")
terra<- terra[yr >= "2002"]
terra<- terra[ y_aod_ITM >= 500000]
# terra<- terra[ aod >= 0.1 &  aod <= 2]


summary(terra$aod)

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyearsAQ.RDS")
aqua<- aqua[yr >= "2002"]
aqua<- aqua[ y_aod_ITM >= 500000]




#PM10
pm10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData10.csv")
pm10$date<-paste(pm10$Day,pm10$Month,pm10$Year,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm10[, c := as.numeric(format(day, "%Y")) ]
pm10[,c("Year","Month","Day","V10","date"):=NULL]
setnames(pm10,"StationID","x_stn_ITM")
setnames(pm10,"X","y_stn_ITM")
setnames(pm10,"Y", "stn")
summary(pm10)
pm10[,length(na.omit(PM10)),by=list(stn,c)]
pm10[, pm10_miss := length(na.omit(PM10)),by=list(stn,c)]
pm10<-pm10[!is.na(PM10)]
pm10<-na.omit(pm10)

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm10, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod terra matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#create aod aqua matrix
aod.m.aq <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

########### join Terra to pm10
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm10, terra [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

########### join aqua to pm10
closestaod.aq <- nearestbyday(pm.m, aod.m.aq, 
                              pm10.m1, aqua [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge

setnames(closestaod.aq,"aod","aod.aq")



setkey(pm10,stn,day)
setkey(closestaod,stn,day)
pm10.m1 <- merge(pm10, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# pm10.m1 <- pm10.m1[aod != "NA"]

setkey(pm10.m1 ,stn,day)
setkey(closestaod.aq ,stn,day)
pm10.m1 <- merge(pm10.m1 , closestaod.aq[,list(stn,day,aod.aq)] , all.x = T)

 
# 
# #map location
# 
# RHx<-RH[,c(3,4,5),with=FALSE]
# WDx<-WD[,c(3,4,5),with=FALSE]
# tempx<-temp[,c(3,4,5),with=FALSE]
# PM10x<-pm10[,c(3,4,5),with=FALSE]
# PM25x<-pm25[,c(3,4,5),with=FALSE]
# so2x<-so2[,c(3,4,5),with=FALSE]
# no2x<-no2[,c(3,4,5),with=FALSE]
# wsx<-ws[,c(3,4,5),with=FALSE]
# 
# 
# alls<-rbind(RHx,WDx,tempx,so2x,no2x,wsx,PM10x,PM25x)
# describe(alls)
# 
# #export unique stn location to join to guid
# alls_agg <- (alls[, list(
#   x_stn_ITM =  x_stn_ITM[1], 
#   y_stn_ITM =  y_stn_ITM[1]),
#   by = stn])
# write.csv(alls_agg,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY.csv")






#####################
#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = pm10.m1[,unique(stn)], day = days))



setkey(pm10.m1, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,pm10.m1, all.x = T)

setkey(temp, day,stn)
setkey(J1, day,stn)
J3 <- merge(J1,temp[,list(day,stn,Temp)], all.x = T)

setkey(RH, day,stn)
setkey(J3, day,stn)
J4 <- merge(J3,RH[,list(day,stn,RH)], all.x = T)

setkey(WD, day,stn)
setkey(J4, day,stn)
J5 <- merge(J4,WD[,list(day,stn,WD)], all.x = T)

setkey(ws, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,ws[,list(day,stn,WS)], all.x = T)

setkey(so2, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,so2[,list(day,stn,SO2)], all.x = T)

setkey(no2, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,no2[,list(day,stn,NO2)], all.x = T)

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]

lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]


lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]

lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]


lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]


lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]


#import stn keytable
stnkey<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY_aodid.csv")

setkey(stnkey,stn)
setkey(J5,stn)
J7 <- merge(J5,stnkey[,list(stn, aodid)], all.x = T)



setkey(lu1,aodid)
setkey(J7,aodid)
J8 <- merge(J7,lu1, all.x = T)
summary(J8)
#delete with missing aodid
J9 <- J8[!is.na(long_aod)]
#add season
J9$month <- as.numeric(format(J9$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
J9$season<-recode(J9$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
J9$seasonSW<-recode(J9$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


# #create daily temperature and RH values since we have alot of missing data
# daymeans <- (J9[, list(daytemp =mean(Temp, na.rm = TRUE),dayRH =mean(RH, na.rm = TRUE),
#                            x_stn_ITM = x_stn_ITM[1],
#                            x_stn_ITM = x_stn_ITM[1]),by = day])  


# ###join daily means
# setkey(daymeans , day)
# setkey(J9, day)
# J10 <- merge(J9,daymeans[,list(day,daytemp,dayRH)], all.x = T)
# describe(J10$daytemp)



# #Create annual mean and sum of SO2 and NO2
# 
# annmean <- (J10[, list(
#                       ymeanNO2 =mean(NO2, na.rm = TRUE),
#                       ysumNO2 =sum(NO2, na.rm = TRUE),
#                       ymeanSO2 =mean(SO2, na.rm = TRUE),
#                       ysumSO2 =sum(SO2, na.rm = TRUE),
#                       x_stn_ITM = x_stn_ITM[1],
#                       x_stn_ITM = x_stn_ITM[1]),
#                       by = c])  
# 
# ###join daily means
# setkey(annmean , c)
# setkey(J10, c)
# J10 <- merge(J10,annmean[,list(c,ymeanNO2,ysumNO2,ymeanSO2,ysumSO2)], all.x = T)


J10<-J9

#import PBL by year and join

###PBL
pbl <-  fread("/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/fianlpblXY_2002.csv")

allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2002:2013){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]
#Join PBL
setkey(pbl , day, pblid)
setkey(J10, day, pblid)
J11 <- merge(J10, pbl, all.x = T)
#head(am2.lu.nd.pb)
J11[, c("date", "longitude.x", "latitude.x","latitude.y", "longitude.y") := NULL]
#add month
J11[, m := as.numeric(format(day, "%m")) ]


ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")


#join NDVI to aod
setkey(ndvid, aodid)
setkey(J11,  aodid)
J12 <- merge(J11, ndvid, all.x = T)


#join NDVI to aod
setkey(ndvi, ndviid, m )
setkey(J12,  ndviid, m)
J13 <- merge(J12, ndvi, all.x = T)


#add dust days
dust<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/dust_days/DD_allIsr_20022012.csv")
dust$date<-paste(dust$Day,dust$Month,dust$Year,sep="/")
dust[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust[,c("Year","Month","Day","V9","V8","date","X","Y"):=NULL]
setnames(dust,"StationID","stn")


setkey(J13 , day, stn)
setkey(dust, day, stn)
J14 <- merge(J13, dust, all.x = T)
J14<-J14[is.na(Dust), Dust:= 0]

summary(J14)

setnames(pm10all,"c.y","c")
setnames(pm25all,"c.y","c")

pm25all[,c("c.x"):=NULL]
pm10all[,c("c.x"):=NULL]


