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

# fullxy<-fread("/media/NAS/Uni/Data/Israel/IPA_stations/pmstn_miss.csv")
# setnames(fullxy,"V2","stn")
# setnames(fullxy,"V3","x_stn_ITM")
# setnames(fullxy,"V4", "y_stn_ITM")
# fullxy<-fullxy[,c(2:4),with=FALSE]

#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")


#import PBL
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



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
#remove missing
Temp[,length(na.omit(Temp)),by=list(stn,c)]
Temp[, Temp_miss := length(na.omit(Temp)),by=list(stn,c)]
Temp<-Temp[Temp_miss > 300]

#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WS<- WD[X != 'NaN']
#remove missing
WD[,length(na.omit(WD)),by=list(stn,c)]
WD[, WD_miss := length(na.omit(WD)),by=list(stn,c)]
WD<-WD[WD_miss > 300]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NA']
#remove missing
WS[,length(na.omit(WS)),by=list(stn,c)]
WS[, WS_miss := length(na.omit(WS)),by=list(stn,c)]
WS<-WS[WS_miss > 300]

#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
#remove missing
RH[,length(na.omit(RH)),by=list(stn,c)]
RH[, RH_miss := length(na.omit(RH)),by=list(stn,c)]
RH<-RH[RH_miss > 300]


#NO2
NO2 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/NO2_D.csv")
NO2$date<-paste(NO2$Day,NO2$Month,NO2$Year,sep="/")
NO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO2[, c := as.numeric(format(day, "%Y")) ]
NO2[,c("Year","Month","Day","date"):=NULL]
NO2 <- NO2[X != 'NaN']
#remove missing
NO2[,length(na.omit(NO2)),by=list(stn,c)]
NO2[, NO2_miss := length(na.omit(NO2)),by=list(stn,c)]
NO2<-NO2[NO2_miss > 300]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
#remove missing
Rain[,length(na.omit(Rain)),by=list(stn,c)]
Rain[, Rain_miss := length(na.omit(Rain)),by=list(stn,c)]
Rain<-Rain[Rain_miss > 300]




####################
###load Terra/Aqua
####################

#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyears.RDS")
terra<- terra[yr >= "2002"]
terra<- terra[ y_aod_ITM >= 500000]
# terra<- terra[ aod >= 0.1 &  aod <= 2]

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyearsAQ.RDS")
aqua<- aqua[yr >= "2002"]
aqua<- aqua[ y_aod_ITM >= 500000]





##################################
#PM10
##################################



#PM10
pm10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
pm10$date<-paste(pm10$Day,pm10$Month,pm10$Year,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm10[, c := as.numeric(format(day, "%Y")) ]
pm10[,c("Year","Month","Day","date"):=NULL]
pm10 <- pm10[X != 'NaN']
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
                              pm10, aqua [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod","UN","WV","QA", knearest = 5, maxdistance = 1500)
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



 
#####################
#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = pm10.m1[,unique(stn)], day = days))


##### start merges


setkey(pm10.m1, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,pm10.m1, all.x = T)
#readd year
J1[, c := as.numeric(format(day, "%Y")) ]

setkey(Temp, day,stn)
setkey(J1, day,stn)
J3 <- merge(J1,Temp[,list(day,stn,Temp)], all.x = T)

setkey(RH, day,stn)
setkey(J3, day,stn)
J4 <- merge(J3,RH[,list(day,stn,RH)], all.x = T)

setkey(WD, day,stn)
setkey(J4, day,stn)
J5 <- merge(J4,WD[,list(day,stn,WD)], all.x = T)

setkey(WS, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,WS[,list(day,stn,WS)], all.x = T)

setkey(SO2, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,SO2[,list(day,stn,SO2)], all.x = T)

setkey(NO2, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,NO2[,list(day,stn,NO2)], all.x = T)



#import stn keytable
stnkey<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY_aodid.csv")

setkey(stnkey,stn)
setkey(J5,stn)
J7 <- merge(J5,stnkey[,list(stn, aodid)], all.x = T)



setkey(lu1,aodid)
setkey(J7,aodid)
J8 <- merge(J7,lu1, all.x = T)

#delete with missing aodid
J9 <- J8[!is.na(long_aod)]
#add season
J9$month <- as.numeric(format(J9$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
J9$season<-recode(J9$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
J9$seasonSW<-recode(J9$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


#Join PBL
setkey(pbl , day, pblid)
setkey(J9, day, pblid)
J11 <- merge(J9, pbl, all.x = T)
#head(am2.lu.nd.pb)
J11[, c("date", "longitude.x", "latitude.x","latitude.y", "longitude.y") := NULL]
#add month
J11[, m := as.numeric(format(day, "%m")) ]

#join NDVI to aod
setkey(ndvid, aodid)
setkey(J11,  aodid)
J12 <- merge(J11, ndvid, all.x = T)

#readd year
J12[, c := as.numeric(format(day, "%Y")) ]


#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(J12,  ndviid, c, m)
J13 <- merge(J12, ndvi, all.x = T)


#add dust days
#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")


setkey(J13 , day, stn)
setkey(dust2, day, stn)
J14 <- merge(J13, dust2, all.x = T)
J14<-J14[is.na(Dust), Dust:= 0]
saveRDS(J14,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all.RDS")











##################################
#PM25
##################################

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']

PM25[,length(na.omit(PM25)),by=list(stn,c)]
PM25[, PM25_miss := length(na.omit(PM25)),by=list(stn,c)]
PM25<-PM25[!is.na(PM25)]
PM25<-na.omit(PM25)
PM25 <- PM25[PM25 > 0  , ]
#PM25 <- PM25[PM25 < 1200  , ]
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")


#PM25 <- PM25[c == 2012  , ]


# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod terra matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#create aod aqua matrix
aod.m.aq <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

########### join Terra to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, terra [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

########### join aqua to PM25
closestaod.aq <- nearestbyday(pm.m, aod.m.aq, 
                              PM25, aqua [, list(day, aodid,aod,UN,WV,QA,QA1,QA2,QA3,QA4,QA5,QA6,QA7,QA8,QA9,QA10,QA11,QA12,QA13,QA14,QA15)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge

setnames(closestaod.aq,"aod","aod.aq")



setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# PM25.m1 <- PM25.m1[aod != "NA"]

setkey(PM25.m1 ,stn,day)
setkey(closestaod.aq ,stn,day)
PM25.m1 <- merge(PM25.m1 , closestaod.aq[,list(stn,day,aod.aq)] , all.x = T)


setkey(PM25 ,stn,day)
setkey(closestaod.aq ,stn,day)
PM25.m1 <- merge(PM25 , closestaod.aq[,list(stn,day,aod,UN,WV,QA,QA1,QA2,QA3,QA4,QA5,QA6,QA7,QA8,QA9,QA10,QA11,QA12,QA13,QA14,QA15)] , all.x = T)




#####################
#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = PM25.m1[,unique(stn)], day = days))


##### start merges
setkey(PM25.m1, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,PM25.m1, all.x = T)
#readd year
J1[, c := as.numeric(format(day, "%Y")) ]

setkey(Temp, day,stn)
setkey(J1, day,stn)
J3 <- merge(J1,Temp[,list(day,stn,Temp)], all.x = T)

setkey(RH, day,stn)
setkey(J3, day,stn)
J4 <- merge(J3,RH[,list(day,stn,RH)], all.x = T)

setkey(WD, day,stn)
setkey(J4, day,stn)
J5 <- merge(J4,WD[,list(day,stn,WD)], all.x = T)

setkey(WS, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,WS[,list(day,stn,WS)], all.x = T)

setkey(Rain, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,Rain[,list(day,stn,Rain)], all.x = T)

setkey(NO2, day,stn)
setkey(J5, day,stn)
J5 <- merge(J5,NO2[,list(day,stn,NO2)], all.x = T)



#import stn keytable
stnkey<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY_aodid.csv")

setkey(stnkey,stn)
setkey(J5,stn)
J7 <- merge(J5,stnkey[,list(stn, aodid)], all.x = T)



setkey(lu1,aodid)
setkey(J7,aodid)
J8 <- merge(J7,lu1, all.x = T)

#delete with missing aodid
J9 <- J8[!is.na(long_aod)]
#add season
J9$month <- as.numeric(format(J9$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
J9$season<-recode(J9$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
J9$seasonSW<-recode(J9$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


#Join PBL
setkey(pbl , day, pblid)
setkey(J9, day, pblid)
J11 <- merge(J9, pbl, all.x = T)
#head(am2.lu.nd.pb)
J11[, c("date", "longitude.x", "latitude.x","latitude.y", "longitude.y") := NULL]
#add month
J11[, m := as.numeric(format(day, "%m")) ]

#join NDVI to aod
setkey(ndvid, aodid)
setkey(J11,  aodid)
J12 <- merge(J11, ndvid, all.x = T)

#readd year
J12[, c := as.numeric(format(day, "%Y")) ]


#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(J12,  ndviid, c, m)
J13 <- merge(J12, ndvi, all.x = T)


#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")


setkey(J13 , day, stn)
setkey(dust2, day, stn)
J14 <- merge(J13, dust2, all.x = T)
J14<-J14[is.na(Dust), Dust:= 0]

saveRDS(J14,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm25_2012_TEST.RDS")




#add regions and flags

pm10.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all.RDS")
pm25.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm25all.RDS")
reg<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_reg_accurate.csv")

#pm10
setkey(pm10.m1 , aodid)
setkey(reg, aodid)
jreg10 <- merge(pm10.m1, reg, all.x = T)

y<-as.data.frame(jreg10)
y$aod.both<-rowMeans(y[,14:15], na.rm=T)
jreg10<-as.data.table(y)
saveRDS(jreg10,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all_reg.RDS")

#pm25
setkey(pm25.m1 , aodid)
setkey(reg, aodid)
jreg25 <- merge(pm25.m1, reg, all.x = T)
y<-as.data.frame(jreg25)
y$aod.both<-rowMeans(y[,14:15], na.rm=T)
jreg25<-as.data.table(y)
saveRDS(jreg25,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm25all_reg.RDS")



