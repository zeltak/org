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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")


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


#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")

###load Terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
#terra<- terra[yr == "2003"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
#system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(1:6,25:28),with=FALSE]
#bkaq<-copy(terra)

#create single aod point per aodid per day (this addresses cartesean error below)
terra <-terra %>%
    group_by(aodid,day) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,UN,WV,day,x_aod_ITM, y_aod_ITM )


#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
aod2003 <- data.table(expand.grid(aodid = ilgreen[, unique(aodid)], day = days))
setkey(aod2003,aodid,day)
setkey(terra,aodid,day)
m1 <- merge(aod2003, terra, all.x = T)  


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
#add back lat/long and metreg (after PBL join nuked some)
m3[,c("latitude","longitude"):=NULL]
grid<-ilgreen[,c("lat_aod","long_aod","aodid", "x_aod_ITM", "y_aod_ITM","metreg"),with=FALSE]
setkey(grid,aodid)
setkey(m3,aodid)
m3<-merge(m3,grid[,list(aodid,lat_aod, long_aod)],all.x = T)

#---------> Temporal additions- calculate met per met region
l=seq(names(m3));names(l)=names(m3);l
m3x<-m3[,c(1:4,29,30),with=FALSE]

#Temp

met.m <- makepointsmatrix(Temp, "X", "Y", "stn")
setkey(m3x, aodid)
lu.m <- makepointsmatrix(m3x[m3x[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m3x, Temp[, list(day,Temp.im,stn)], 
                            "aodid", "stn", "meanT", "Temp.im", knearest = 5, maxdistance = NA)
setkey(m3x,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m3x, closestaodse[,list(day,Temp.im,aodid)], all.x = T)

#WD
met.m <- makepointsmatrix(WD, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, WD[, list(day,WD.im,stn)], 
                          "aodid", "stn", "meanT", "WD.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,WD.im,aodid)], all.x = T)

#WS
met.m <- makepointsmatrix(WS, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, WS[, list(day,WS.im,stn)], 
                          "aodid", "stn", "meanT", "WS.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,WS.im,aodid)], all.x = T)

#SR
met.m <- makepointsmatrix(SR, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, SR[, list(day,SR.im,stn)], 
                          "aodid", "stn", "meanT", "SR.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,SR.im,aodid)], all.x = T)

#O3
met.m <- makepointsmatrix(O3, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, O3[, list(day,O3.im,stn)], 
                          "aodid", "stn", "meanT", "O3.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,O3.im,aodid)], all.x = T)

#NO
met.m <- makepointsmatrix(NO, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, NO[, list(day,NO.im,stn)], 
                          "aodid", "stn", "meanT", "NO.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,NO.im,aodid)], all.x = T)

#Rain
met.m <- makepointsmatrix(Rain, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, Rain[, list(day,Rain.im,stn)], 
                          "aodid", "stn", "meanT", "Rain.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,Rain.im,aodid)], all.x = T)

#SO2
met.m <- makepointsmatrix(SO2, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, SO2[, list(day,SO2.im,stn)], 
                          "aodid", "stn", "meanT", "SO2.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,SO2.im,aodid)], all.x = T)

#RH
met.m <- makepointsmatrix(RH, "X", "Y", "stn")
lu.m <- makepointsmatrix(m4[m4[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            m4, RH[, list(day,RH.im,stn)], 
                          "aodid", "stn", "meanT", "RH.im", knearest = 5, maxdistance = NA)
setkey(m4,aodid,day)
setkey(closestaodse,aodid,day)
m4 <- merge(m4, closestaodse[,list(day,RH.im,aodid)], all.x = T)

##merage all back in
m4[, c("stn", "c", "x_aod_ITM","y_aod_ITM") := NULL]
setkey(m4,aodid,day)
setkey(m3,aodid,day)
m5 <- merge(m3, m4, all.x = T)



#########-------------------weights ############
m5<-m5[,obs:=1]
m5[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+Dust+Temp.im+WS.im+as.factor(month),family=binomial,data=m5)
m5$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
m5$wt <- 1/m5$prob
m5$normwt <- m5$wt/mean(m5$wt)
#Cleanup
m5<-m5[, c("prob","wt") := NULL]

#clean 
rm(m1)
rm(m2)
rm(m3)
rm(m3x)
rm(m4)
rm(w1)
gc()


#----------------------------------> PM Data


#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 900 ]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")

## PM <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
## #pre-post PM
## PM=transform(PM, day = as.Date(paste(Year,Month,Day,1,sep="/")))
## head(PM)
## PM=as.data.table(PM)
## data=PM[,list(day,stn,PM25),with=TRUE]
## setkey(data,stn,day)
## library(DataCombine)
## Data1 <- slide(data, Var = "PM25", GroupVar = "stn",
##                slideBy = 1)
## Data2 <- slide(Data1, Var = "PM25", GroupVar = "stn",
##                slideBy = -1)



#calculate meanPM per grid per day to each station (excluding first station)
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 < 20200 ]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")

#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m5, aodid)
aod.m <- makepointsmatrix(m5[m5[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            m5, PM25 [, list(day,PM25,stn)], 
                            "aodid", "stn", "closest","PM25",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
pmj1[,PM25 :=NULL]
pmj1[,closest :=NULL]
pmj1[,closestknn :=NULL]
pmj1[,closestnobs:=NULL]

#join to DB
setkey(pmj1,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,pmj1,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

pmj2<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, PM25[, list(day,PM25,stn)], 
                            "aodid", "stn", "closest", "PM25", knearest = 15, maxdistance = 120000,nearestmean = TRUE)
#cleanup
pmj2[,PM25 :=NULL]
pmj2[,closest :=NULL]
pmj2[,closestknn :=NULL]
pmj2[,closestnobs:=NULL]

#join to DB
setkey(pmj2,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,pmj2,all.x = T)

m7<-rbindlist(list(m6x,m6_good))
setnames(m7,"closestmean","meanPM")

#check missing spatial covergae 
## describe(m7$meanPM)
## mmm <- m7[is.na(meanPM)]

## #aggregate and write to csv
## write.csv (x <-m7 %>%
##     group_by(aodid) %>%
##     summarise(lat_aod = mean(lat_aod.x, na.rm=TRUE),  long_aod = mean(long_aod.x, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) , "/home/zeltak/ZH_tmp/m7tst.csv")


#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(m5, aodid)
aod.m <- makepointsmatrix(m5[m5[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            m5, PM10 [, list(day,PM10,stn)], 
                            "aodid", "stn", "closest","PM10",knearest = 5, maxdistance = 30000, nearestmean = T)

#cleanup
pmj1[,PM10 :=NULL]
pmj1[,closest :=NULL]
pmj1[,closestknn :=NULL]
pmj1[,closestnobs:=NULL]

#join to DB
setkey(pmj1,aodid,day)
setkey(m5,aodid,day)
m6 <- merge(m5,pmj1,all.x = T)

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]

pmj2<- nearestbyday(aod.m  ,pm.m , 
                            m6_NA, PM10[, list(day,PM10,stn)], 
                            "aodid", "stn", "closest", "PM10", knearest = 15, maxdistance = 120000,nearestmean = TRUE)
#cleanup
pmj2[,PM10 :=NULL]
pmj2[,closest :=NULL]
pmj2[,closestknn :=NULL]
pmj2[,closestnobs:=NULL]

#join to DB
setkey(pmj2,aodid,day)
setkey(m6_NA,aodid,day)
m6x <- merge(m6_NA,pmj2,all.x = T)

setnames(m6x,"closestmean","meanPM10")
setnames(m6_good,"closestmean","meanPM10")

m8<-rbindlist(list(m6x,m6_good))
m8x<-m8[,c(1,2,52),with=FALSE]

setkey(m7,aodid,day)
setkey(m8x,aodid,day)
m9 <- merge(m7,m8x,all.x = T)



#----------> save mods 2+3
#clean
m9[,c("ndviid","pblid","pop","area","date","month","lat_ndvi","long_ndvi","lat_aod.y","long_aod.y"):=NULL]
saveRDS(m9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.TR.rds")
#m9<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.TR.rds")


#--------->mod2
m9.m2 <- m9[!is.na(aod)]
#calculate  prev/post day
#sort PM data
setkey(m9.m2,aodid,day)
m9x<-as.data.frame(m9.m2)
# next day PM
Data1 <- slide(m9x, Var = "aod", GroupVar = "aodid",
               slideBy = 1)
#prev day PM 
Data2 <- slide(Data1, Var = "aod", GroupVar = "aodid",
               slideBy = -1)

data1<-as.data.table(Data1)
data2<-as.data.table(Data2)
setkey(data1,day,aodid)
setkey(data2,day,aodid)
setnames(data1,"aod1","aodpre")
setnames(data2,"aod-1","aodpost")
rm(m9.m2)
rm(m9x)
gc()
m9.m2 <- merge(data1, data2[,list(aodid,day, aodpost)], all.x = T)

saveRDS(m9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod2.TR.rds")
#m9.m2<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod2.TR.rds")


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m9.m2,aodid)
aod.m <- makepointsmatrix(m9.m2[m9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% m9days,], m9.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

# create moniotor mpm.pm25
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
pm2.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#note this function is the one excluding the first station
closestaodse<- nearestbydayM1(pm2.m  ,pm.m , 
                            PM25, PM25 [, list(day,PM25,stn)], 
                            "stn", "stn", "closest","PM25",knearest = 5, maxdistance = 20000, nearestmean = T)
setkey(closestaodse,stn,day)
setkey(PM25,stn,day)
PM25 <- merge(PM25,closestaodse[,list(day,closestmean,stn)],all.x = T)
setnames(PM25,"closestmean","m1.mpm")
setkey(PM25,stn,day)
setkey(PM25.m1,stn,day)
PM25.m1 <- merge(PM25.m1, PM25[,list(stn,day,m1.mpm )] , all.x = T)

# create moniotor mpm.pm10
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
pm2.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#note this function is the one excluding the first station
closestaodse<- nearestbydayM1(pm2.m  ,pm.m , 
                            PM10, PM10 [, list(day,PM10,stn)], 
                            "stn", "stn", "closest","PM10",knearest = 5, maxdistance = 20000, nearestmean = T)
setkey(closestaodse,stn,day)
setkey(PM10,stn,day)
PM10 <- merge(PM10,closestaodse[,list(day,closestmean,stn)],all.x = T)
setnames(PM10,"closestmean","m1.mpm10")
setkey(PM10,stn,day)
setkey(PM25.m1,stn,day)
PM25.m1 <- merge(PM25.m1, PM10[,list(stn,day,m1.mpm10 )] , all.x = T)



#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.TR.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
setkey(m9.m2,aodid)
aod.m <- makepointsmatrix(m9.m2[m9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% m9days,], m9.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

# create moniotor mpm
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
pm2.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#note this function is the one excluding the first station
closestaodse<- nearestbydayM1(pm2.m  ,pm.m , 
                            PM10, PM10 [, list(day,PM10,stn)], 
                            "stn", "stn", "closest","PM10",knearest = 5, maxdistance = 20000, nearestmean = T)
setkey(closestaodse,stn,day)
setkey(PM10,stn,day)
PM10 <- merge(PM10,closestaodse[,list(day,closestmean,stn)],all.x = T)
setnames(PM10,"closestmean","m1.mpm10")


setkey(PM10,stn,day)
setkey(PM10.m1,stn,day)
PM10.m1 <- merge(PM10.m1, PM10[,list(stn,day,m1.mpm10 )] , all.x = T)

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM10.TR.rds")






