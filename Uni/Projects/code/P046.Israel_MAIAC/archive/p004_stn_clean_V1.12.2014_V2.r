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
#delete "palestine"
wlu<-lu1[!is.na(p_for)]



#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
Temp <- Temp[Temp != 'NaN']


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WS<- WD[X != 'NaN']


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


#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

# ####################
# ###load Terra/Aqua
# ####################
# #load aod data
# terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
# terra<- terra[yr >= "2003"]
# #clear PA points
# terra <- terra[terra$aodid %in% ilgreen$aodid, ] 

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua<- aqua[yr >= "2003"]
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
fullaod <- data.table(expand.grid(aodid = aqua[, unique(aodid)], day = days))
setkey(fullaod,aodid)
setkey(aqua,aodid)
aodf<-left_join(fullaod,aqua)
aodf<-aodf[, c(3:4,6:11) := NULL]

#add land use
setkey(aodf,aodid)
setkey(wlu,aodid)
aodf.lu<-left_join(aodf,wlu)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2003[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)







#####################################
#####################################
#TEST
#####################################

# aodpm5k<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/aod_pm_5k.csv")
# aqua.se <- aqua[aqua$aodid %in% aodpm5k$aodid, ] 
# aqua.se<-aqua.se[, c(8:24) := NULL]
# 
# 
# setkey(aodpm5k,aodid)
# setkey(aqua.se,aodid)
# aqua.se <- merge(aqua.se,aodpm5k[,list(dist,aodid,stn)], all.x = T)
# 
# setkey(PM25,day,stn)
# setkey(aqua.se,day,stn)
# m1.s1 <- merge(PM25,aqua.se,all.x = T)
# 
# #to leave only THE 1 closest sat data point to station in each day
# setkey(m1.s1,stn,day,dist)
# #take first ocurance by day per STN (its sorted by dist so the shortest one)
# x<-m1.s1[unique(m1.s1[, list(stn, day)]), mult = "first"]










################################
##2003
################################

## subset AOD
aod2003<-aodf[c==2003]

#xtract year met
met2003<- Temp[c==2003]
#create full LU TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2003))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2003[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2003 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2003 ,aodid,day)
setkey(aod2003,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2003, ot2003)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)





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
#num. of obsv per year per stn
PM25[,length(na.omit(PM25)),by=list(stn,c)]
#PM25_m means avialble obs per year
PM25[, PM25_n := length(na.omit(PM25)),by=list(stn,c)]
#clear non PM25 days
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
PM25 <- PM25[PM25_n > 5  , ]
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")

#########
#terra
#########

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, terra [, list(day, aodid, aod,UN,WV,QA)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod[,list(stn,day,aod,UN,WV,QA)], all.x = T)

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
#add regions and flags
reg<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_reg_accurate.csv")

#PM25
setkey(J14 , aodid)
setkey(reg, aodid)
jreg10 <- merge(J14, reg, all.x = T)
PM25.terra<-jreg10
PM25.terra$A_T<-0

#########
#aqua
#########

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod aqua matrix
aod.m <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aqua [, list(day, aodid, aod,UN,WV,QA)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod[,list(stn,day,aod,UN,WV,QA)], all.x = T)

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
#add regions and flags
reg<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_reg_accurate.csv")

#PM25
setkey(J14 , aodid)
setkey(reg, aodid)
jreg10 <- merge(J14, reg, all.x = T)
PM25.aqua<-jreg10
PM25.aqua$A_T<-1

##SAVE
PM25.AT<-rbindlist(list(PM25.aqua,PM25.terra))
saveRDS(PM25.AT,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM25all_reg.RDS")





##################################
#PM10
##################################

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
#num. of obsv per year per stn
PM10[,length(na.omit(PM10)),by=list(stn,c)]
#pm10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")



#########
#terra
#########

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM10
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, terra [, list(day, aodid, aod,UN,WV,QA)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

########### 
# meanaod <- nearestbyday(pm.m, aod.m, 
#                            PM10, terra [, list(day, aodid, aod,UN,WV,QA)], 
#                            "stn", "aodid", "closestaod", "aod", knearest = 9, maxdistance = 2200, nearestmean = T)




setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod[,list(stn,day,aod,UN,WV,QA)], all.x = T)
  

#####################
#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = PM10.m1[,unique(stn)], day = days))
##### start merges
setkey(PM10.m1, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,PM10.m1, all.x = T)
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
#add regions and flags
reg<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_reg_accurate.csv")

#PM10
setkey(J14 , aodid)
setkey(reg, aodid)
jreg10 <- merge(J14, reg, all.x = T)
PM10.terra<-jreg10
PM10.terra$A_T<-1




#########
#aqua
#########

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod aqua matrix
aod.m <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM10
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aqua [, list(day, aodid, aod,UN,WV,QA)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod[,list(stn,day,aod,UN,WV,QA)], all.x = T)



#####################
#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = PM10.m1[,unique(stn)], day = days))
##### start merges
setkey(PM10.m1, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,PM10.m1, all.x = T)
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
#add regions and flags
reg<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_reg_accurate.csv")

#PM10
setkey(J14 , aodid)
setkey(reg, aodid)
jreg10 <- merge(J14, reg, all.x = T)
PM10.aqua<-jreg10
PM10.aqua$A_T<-0


##SAVE
PM10.AT<-rbindlist(list(PM10.aqua,PM10.terra))
saveRDS(PM10.AT,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM10all_reg.RDS")






