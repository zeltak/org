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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")




########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2003:2013){
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
l=seq(names(wlu));names(l)=names(wlu);l
wlu[,c("OBJECTID","Join_Count" ,"TARGET_FID","longitude", "latitude","lat_aod", "long_aod" ,"x_aod_ITM","y_aod_ITM","x_stn_ITM" ,"y_stn_ITM"):=NULL]



#Temp
Temp <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
#set the key by day 
setkey(Temp, day)
Temp[, Temp.im := na.locf(Temp, na.rm = F)]
#describe(Temp[, list(Temp, Temp.im)])
length(unique(Temp$day))

#WD
WD <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
#set the key by day 
setkey(WD, day)
WD[, WD.im := na.locf(WD, na.rm = F)]
#describe(WD[, list(WD, WD.im)])

#WS
WS <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
#set the key by day 
setkey(WS, day)
WS[, WS.im := na.locf(WS, na.rm = F)]
#describe(WS[, list(WS, WS.im)])

#SR
SR <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/SolarRad_D.csv")
SR$date<-paste(SR$Day,SR$Month,SR$Year,sep="/")
SR[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
SR[, c := as.numeric(format(day, "%Y")) ]
SR[,c("Year","Month","Day","date"):=NULL]
SR <- SR[X != 'NaN']
#set the key by day 
setkey(SR, day)
SR[, SR.im := na.locf(SR, na.rm = F)]
#describe(SR[, list(SR, SR.im)])

#SO2
SO2 <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/SO2_D.csv")
SO2$date<-paste(SO2$Day,SO2$Month,SO2$Year,sep="/")
SO2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
SO2[, c := as.numeric(format(day, "%Y")) ]
SO2[,c("Year","Month","Day","date"):=NULL]
SO2 <- SO2[X != 'NaN']
#set the key by day 
setkey(SO2, day)
SO2[, SO2.im := na.locf(SO2, na.rm = F)]
#describe(SO2[, list(SO2, SO2.im)])

#RH
RH <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
#set the key by day 
setkey(RH, day)
RH[, RH.im := na.locf(RH, na.rm = F)]
#describe(RH[, list(RH, RH.im)])

#Rain
Rain <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
#set the key by day 
setkey(Rain, day)
Rain[, Rain.im := na.locf(Rain, na.rm = F)]
#describe(Rain[, list(Rain, Rain.im)])

#O3
O3 <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/O3_D.csv")
O3$date<-paste(O3$Day,O3$Month,O3$Year,sep="/")
O3[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
O3[, c := as.numeric(format(day, "%Y")) ]
O3[,c("Year","Month","Day","date"):=NULL]
O3 <- O3[X != 'NaN']
#set the key by day 
setkey(O3, day)
O3[, O3.im := na.locf(O3, na.rm = F)]
#describe(O3[, list(O3, O3.im)])

#NO
NO <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/NO_D.csv")
NO$date<-paste(NO$Day,NO$Month,NO$Year,sep="/")
NO[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
NO[, c := as.numeric(format(day, "%Y")) ]
NO[,c("Year","Month","Day","date"):=NULL]
NO <- NO[X != 'NaN']
#set the key by day 
setkey(NO, day)
NO[, NO.im := na.locf(NO, na.rm = F)]
#describe(NO[, list(NO, NO.im)])

# #add overall met file
# #WS_D #WD_D #SR_D #SO2 #RH_D #Rain_D #O3_D #NO2 
# setkey(Temp,stn,day)
# setkey(WD,stn,day)
# mt <- merge(Temp, WD[,list(stn,day,WD.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(WS,stn,day)
# mt <- merge(mt, WS[,list(stn,day,WS.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(SR,stn,day)
# mt <- merge(mt, SR[,list(stn,day,SR.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(SO2,stn,day)
# mt <- merge(mt, SO2[,list(stn,day,SO2.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(RH,stn,day)
# mt <- merge(mt, RH[,list(stn,day,RH.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(Rain,stn,day)
# mt <- merge(mt, Rain[,list(stn,day,Rain.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(O3,stn,day)
# mt <- merge(mt, O3[,list(stn,day,O3.im)], all.x = T)  
# setkey(mt,stn,day)
# setkey(NO,stn,day)
# mt <- merge(mt, NO[,list(stn,day,NO.im)], all.x = T)  

#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
#aqua<- aqua[yr == "2003"]
#system.time(aqua[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(1:6,25:28),with=FALSE]
#bkaq<-copy(aqua)

#create single aod point per aodid per day (this addresses cartesean error below)
aqua <-aqua %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM )


#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
aod2003 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2003,aodid,day)
setkey(aqua,aodid,day)
m1 <- merge(aod2003, aqua, all.x = T)  


#add land use and X,Y
setkey(m1,aodid)
setkey(wlu,aodid)
m2<-merge(m1,wlu,all.x = T)
#take out Junk to fix lat/long
#l=seq(names(m2));names(l)=names(m2);l
m2<-m2[, c(3,4,8,9,21,22) := NULL]
#clean points with no lu data (on borders and in golan)
m2 <- m2[!is.na(reg_num)]

#add back lat/long and metreg
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m2,aodid)
m2<-merge(m2,grid,all.x = T)
#names(m2)



#----> Spatial
#Join PBL
setkey(pbl , day, pblid)
setkey(m2, day, pblid)
m3<-left_join(m2, pbl)

#add season
m3$month <- as.numeric(format(m3$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3$season<-recode(m3$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3$seasonSW<-recode(m3$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
m3[, m := as.numeric(format(day, "%m")) ]
#add year
m3[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(m3,  ndviid, c, m)
m3 <- merge(m3, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/dust.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
setkey(m3 , day, stn)
setkey(dust2, day, stn)
m3 <- merge(m3, dust2[,list(day,stn,Dust)], all.x = T)
m3<-m3[is.na(Dust), Dust:= 0]

#---------> Temporal additions- calculate met per met region
metreg <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL_stn_metreg.csv")
#WS_D #WD_D #SR_D #SO2 #RH_D #Rain_D #O3_D #NO2 
#Temp
setkey(metreg,stn)
setkey(Temp,stn)
Temp <- merge(Temp, metreg[,list(stn,metreg)], all.x = T)
temp.a <-Temp %>%
    group_by(day,metreg) %>%
    summarise(tempa = mean(Temp)) 
setkey(m2,metreg,day)
setkey(temp.a,metreg,day)
m3 <- merge(m2, temp.a, all.x = T)
summary(m3)







#########-------------------weights ############
#clean
summary(m3)
#delete areas in desset that have missing Temp
#m3 <- m3[tempa != 'NA']
#create weights
m3<-m3[,obs:=1]
m3[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+Dust+temp+as.factor(month),family=binomial,data=m3)
m3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m3$wt <- 1/m3$prob
m3$normwt <- m3$wt/mean(m3$wt)

#----------> Cleanup
m3<-m3[, c("prob","wt") := NULL]


#########-------------------############
#add meanPM per grid per day

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
pmall2003<- PM25[c==2003]

pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m3, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m3,aodid,day)
m4 <- merge(m3,closestaodse,all.x = T)

m4_NA<- m4[is.na(closestmean),]
m4_NA[,closestmean := NULL]
m4_good<- m4[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m4_NA, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m4_NA,aodid,day)
m4x <- merge(m4_NA,closestaodse,all.x = T)

m5<-rbindlist(list(m4x,m4_good))
setnames(m5,"closestmean","meanPM")



#########-------------------############
#add meanPM per grid per day

#PM10
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pmall2003<- PM10[c==2003]

pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m3, aodid)
aod.m <- makepointsmatrix(m3[m3[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m5, pmall2003 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,closestaodse,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

closestaodse<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, pmall2003 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 9, maxdistance = 90000, nearestmean = TRUE)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,closest :=NULL]
closestaodse[,closestknn :=NULL]
closestaodse[,closestnobs:=NULL]

#join to DB
setkey(closestaodse,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,closestaodse,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM10")








#------------> save mods
#mod3
saveRDS(m7,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2003.rds")
#mod2
m7.m2 <- m7[!is.na(aod)]
saveRDS(m7.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")





#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
m7days <- sort(unique(m7.m2$day))

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")


#--------->mod1
########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m7.m2,aodid)
aod.m <- makepointsmatrix(m7.m2[m7.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m7days,], m7.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.rds")
