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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2003:2003){
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
Temp <- Temp[c == 2003]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2003]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2003]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2003]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2003]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2003"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
aod2003 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2003,aodid,day)
setkey(terra,aodid,day)
aodf.2003<-left_join(aod2003,terra)

#add land use and X,Y
setkey(aodf.2003,aodid)
setkey(wlu,aodid)
aodf.2003.lu<-left_join(aodf.2003,wlu)
aodf.2003.lu<-aodf.2003.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2003.lu <- aodf.2003.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2003.tmp <-aodf.2003.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2003<- Temp[c==2003]
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2003.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2003.tmp[aodf.2003.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2003.tmp, met2003[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2003.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2003.tmp.s1 <- merge(aodf.2003.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2003<- WS[c==2003]
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2003.tmp.s1, met2003[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2003.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2003.tmp.s2 <- merge(aodf.2003.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2003<- WD[c==2003]
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2003.tmp.s2, met2003[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2003.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2003.tmp.s3 <- merge(aodf.2003.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2003<- Rain[c==2003]
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2003.tmp.s3, met2003[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2003.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2003.tmp.s4 <- merge(aodf.2003.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2003<- RH[c==2003]
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2003.tmp.s4, met2003[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2003.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2003.tmp.s4 <- merge(aodf.2003.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2003.lu,aodid,day)
setkey(aodf.2003.tmp.s4,aodid,day)
x1<- left_join(aodf.2003.lu,aodf.2003.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2003.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2003.tmp.s6$month <- as.numeric(format(aodf.2003.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2003.tmp.s6$season<-recode(aodf.2003.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2003.tmp.s6$seasonSW<-recode(aodf.2003.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2003.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2003.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2003.tmp.s6,  ndviid, c, m)
aodf.2003.tmp.s8 <- merge(aodf.2003.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2003]

setkey(aodf.2003.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2003.tmp.s9 <- merge(aodf.2003.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2003.tmp.s9<-aodf.2003.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2003.tmp.s9<-aodf.2003.tmp.s9[,]
aodf.2003.tmp.s9 <- aodf.2003.tmp.s9[Temp != 'NA']
#create weights
aodf.2003.tmp.s9<-aodf.2003.tmp.s9[,obs:=1]
aodf.2003.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2003.tmp.s9)
aodf.2003.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2003.tmp.s9$wt <- 1/aodf.2003.tmp.s9$prob
aodf.2003.tmp.s9$normwt <- aodf.2003.tmp.s9$wt/mean(aodf.2003.tmp.s9$wt)
aodf.2003.tmp.s9<-aodf.2003.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2003<- PM25[c==2003]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2003[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2003 <- pmall2003[pmall2003$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2003, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2003.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2003.tmp.s9[aodf.2003.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2003.tmp.s9, pmall2003 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2003.tmp.s9,aodid,day)
aodf.2003.tmp.s9 <- merge(aodf.2003.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2003<- PM10[c==2003]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2003[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2003 <- pm10all2003[pm10all2003$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2003, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2003.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2003.tmp.s9[aodf.2003.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2003.tmp.s9, pm10all2003 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2003.tmp.s9,aodid,day)
aodf.2003.tmp.s9 <- merge(aodf.2003.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2003.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2003.rds")
#mod2
aodf.2003.tmp.s9.m2 <- aodf.2003.tmp.s9[!is.na(aod)]
saveRDS(aodf.2003.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2003.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2003.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2003.tmp.s9.m2[aodf.2003.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2003.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2003.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2003.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2003.tmp.s9.m2[aodf.2003.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2003.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2003.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2004:2004){
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
Temp <- Temp[c == 2004]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2004]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2004]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2004]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2004]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2004"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
aod2004 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2004,aodid,day)
setkey(terra,aodid,day)
aodf.2004<-left_join(aod2004,terra)

#add land use and X,Y
setkey(aodf.2004,aodid)
setkey(wlu,aodid)
aodf.2004.lu<-left_join(aodf.2004,wlu)
aodf.2004.lu<-aodf.2004.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2004.lu <- aodf.2004.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2004.tmp <-aodf.2004.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2004<- Temp[c==2004]
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2004.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2004.tmp[aodf.2004.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2004.tmp, met2004[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2004.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2004.tmp.s1 <- merge(aodf.2004.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2004<- WS[c==2004]
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2004.tmp.s1, met2004[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2004.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2004.tmp.s2 <- merge(aodf.2004.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2004<- WD[c==2004]
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2004.tmp.s2, met2004[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2004.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2004.tmp.s3 <- merge(aodf.2004.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2004<- Rain[c==2004]
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2004.tmp.s3, met2004[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2004.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2004.tmp.s4 <- merge(aodf.2004.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2004<- RH[c==2004]
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2004.tmp.s4, met2004[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2004.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2004.tmp.s4 <- merge(aodf.2004.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2004.lu,aodid,day)
setkey(aodf.2004.tmp.s4,aodid,day)
x1<- left_join(aodf.2004.lu,aodf.2004.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2004.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2004.tmp.s6$month <- as.numeric(format(aodf.2004.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2004.tmp.s6$season<-recode(aodf.2004.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2004.tmp.s6$seasonSW<-recode(aodf.2004.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2004.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2004.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2004.tmp.s6,  ndviid, c, m)
aodf.2004.tmp.s8 <- merge(aodf.2004.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2004]

setkey(aodf.2004.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2004.tmp.s9 <- merge(aodf.2004.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2004.tmp.s9<-aodf.2004.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2004.tmp.s9<-aodf.2004.tmp.s9[,]
aodf.2004.tmp.s9 <- aodf.2004.tmp.s9[Temp != 'NA']
#create weights
aodf.2004.tmp.s9<-aodf.2004.tmp.s9[,obs:=1]
aodf.2004.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2004.tmp.s9)
aodf.2004.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2004.tmp.s9$wt <- 1/aodf.2004.tmp.s9$prob
aodf.2004.tmp.s9$normwt <- aodf.2004.tmp.s9$wt/mean(aodf.2004.tmp.s9$wt)
aodf.2004.tmp.s9<-aodf.2004.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2004<- PM25[c==2004]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2004[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2004 <- pmall2004[pmall2004$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2004, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2004.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2004.tmp.s9[aodf.2004.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2004.tmp.s9, pmall2004 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2004.tmp.s9,aodid,day)
aodf.2004.tmp.s9 <- merge(aodf.2004.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2004<- PM10[c==2004]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2004[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2004 <- pm10all2004[pm10all2004$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2004, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2004.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2004.tmp.s9[aodf.2004.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2004.tmp.s9, pm10all2004 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2004.tmp.s9,aodid,day)
aodf.2004.tmp.s9 <- merge(aodf.2004.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2004.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2004.rds")
#mod2
aodf.2004.tmp.s9.m2 <- aodf.2004.tmp.s9[!is.na(aod)]
saveRDS(aodf.2004.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2004.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2004.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2004.tmp.s9.m2[aodf.2004.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2004.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2004.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2004.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2004.tmp.s9.m2[aodf.2004.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2004.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2004.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2005:2005){
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
Temp <- Temp[c == 2005]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2005]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2005]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2005]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2005]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2005"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), 1)
#create date range
aod2005 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2005,aodid,day)
setkey(terra,aodid,day)
aodf.2005<-left_join(aod2005,terra)

#add land use and X,Y
setkey(aodf.2005,aodid)
setkey(wlu,aodid)
aodf.2005.lu<-left_join(aodf.2005,wlu)
aodf.2005.lu<-aodf.2005.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2005.lu <- aodf.2005.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2005.tmp <-aodf.2005.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2005<- Temp[c==2005]
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2005.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2005.tmp[aodf.2005.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2005.tmp, met2005[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2005.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2005.tmp.s1 <- merge(aodf.2005.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2005<- WS[c==2005]
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2005.tmp.s1, met2005[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2005.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2005.tmp.s2 <- merge(aodf.2005.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2005<- WD[c==2005]
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2005.tmp.s2, met2005[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2005.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2005.tmp.s3 <- merge(aodf.2005.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2005<- Rain[c==2005]
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2005.tmp.s3, met2005[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2005.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2005.tmp.s4 <- merge(aodf.2005.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2005<- RH[c==2005]
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2005.tmp.s4, met2005[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2005.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2005.tmp.s4 <- merge(aodf.2005.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2005.lu,aodid,day)
setkey(aodf.2005.tmp.s4,aodid,day)
x1<- left_join(aodf.2005.lu,aodf.2005.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2005.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2005.tmp.s6$month <- as.numeric(format(aodf.2005.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2005.tmp.s6$season<-recode(aodf.2005.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2005.tmp.s6$seasonSW<-recode(aodf.2005.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2005.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2005.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2005.tmp.s6,  ndviid, c, m)
aodf.2005.tmp.s8 <- merge(aodf.2005.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2005]

setkey(aodf.2005.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2005.tmp.s9 <- merge(aodf.2005.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2005.tmp.s9<-aodf.2005.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2005.tmp.s9<-aodf.2005.tmp.s9[,]
aodf.2005.tmp.s9 <- aodf.2005.tmp.s9[Temp != 'NA']
#create weights
aodf.2005.tmp.s9<-aodf.2005.tmp.s9[,obs:=1]
aodf.2005.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2005.tmp.s9)
aodf.2005.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2005.tmp.s9$wt <- 1/aodf.2005.tmp.s9$prob
aodf.2005.tmp.s9$normwt <- aodf.2005.tmp.s9$wt/mean(aodf.2005.tmp.s9$wt)
aodf.2005.tmp.s9<-aodf.2005.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2005<- PM25[c==2005]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2005[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2005 <- pmall2005[pmall2005$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2005, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2005.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2005.tmp.s9[aodf.2005.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2005.tmp.s9, pmall2005 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2005.tmp.s9,aodid,day)
aodf.2005.tmp.s9 <- merge(aodf.2005.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2005<- PM10[c==2005]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2005[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2005 <- pm10all2005[pm10all2005$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2005, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2005.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2005.tmp.s9[aodf.2005.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2005.tmp.s9, pm10all2005 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2005.tmp.s9,aodid,day)
aodf.2005.tmp.s9 <- merge(aodf.2005.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2005.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2005.rds")
#mod2
aodf.2005.tmp.s9.m2 <- aodf.2005.tmp.s9[!is.na(aod)]
saveRDS(aodf.2005.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2005.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2005.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2005.tmp.s9.m2[aodf.2005.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2005.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2005.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2005.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2005.tmp.s9.m2[aodf.2005.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2005.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2005.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2006:2006){
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
Temp <- Temp[c == 2006]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2006]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2006]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2006]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2006]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2006"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
aod2006 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2006,aodid,day)
setkey(terra,aodid,day)
aodf.2006<-left_join(aod2006,terra)

#add land use and X,Y
setkey(aodf.2006,aodid)
setkey(wlu,aodid)
aodf.2006.lu<-left_join(aodf.2006,wlu)
aodf.2006.lu<-aodf.2006.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2006.lu <- aodf.2006.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2006.tmp <-aodf.2006.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2006<- Temp[c==2006]
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2006.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2006.tmp[aodf.2006.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2006.tmp, met2006[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2006.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2006.tmp.s1 <- merge(aodf.2006.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2006<- WS[c==2006]
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2006.tmp.s1, met2006[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2006.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2006.tmp.s2 <- merge(aodf.2006.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2006<- WD[c==2006]
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2006.tmp.s2, met2006[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2006.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2006.tmp.s3 <- merge(aodf.2006.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2006<- Rain[c==2006]
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2006.tmp.s3, met2006[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2006.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2006.tmp.s4 <- merge(aodf.2006.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2006<- RH[c==2006]
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2006.tmp.s4, met2006[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2006.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2006.tmp.s4 <- merge(aodf.2006.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2006.lu,aodid,day)
setkey(aodf.2006.tmp.s4,aodid,day)
x1<- left_join(aodf.2006.lu,aodf.2006.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2006.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2006.tmp.s6$month <- as.numeric(format(aodf.2006.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2006.tmp.s6$season<-recode(aodf.2006.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2006.tmp.s6$seasonSW<-recode(aodf.2006.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2006.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2006.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2006.tmp.s6,  ndviid, c, m)
aodf.2006.tmp.s8 <- merge(aodf.2006.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2006]

setkey(aodf.2006.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2006.tmp.s9 <- merge(aodf.2006.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2006.tmp.s9<-aodf.2006.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2006.tmp.s9<-aodf.2006.tmp.s9[,]
aodf.2006.tmp.s9 <- aodf.2006.tmp.s9[Temp != 'NA']
#create weights
aodf.2006.tmp.s9<-aodf.2006.tmp.s9[,obs:=1]
aodf.2006.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2006.tmp.s9)
aodf.2006.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2006.tmp.s9$wt <- 1/aodf.2006.tmp.s9$prob
aodf.2006.tmp.s9$normwt <- aodf.2006.tmp.s9$wt/mean(aodf.2006.tmp.s9$wt)
aodf.2006.tmp.s9<-aodf.2006.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2006<- PM25[c==2006]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2006[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2006 <- pmall2006[pmall2006$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2006, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2006.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2006.tmp.s9[aodf.2006.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2006.tmp.s9, pmall2006 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2006.tmp.s9,aodid,day)
aodf.2006.tmp.s9 <- merge(aodf.2006.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2006<- PM10[c==2006]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2006[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2006 <- pm10all2006[pm10all2006$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2006, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2006.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2006.tmp.s9[aodf.2006.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2006.tmp.s9, pm10all2006 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2006.tmp.s9,aodid,day)
aodf.2006.tmp.s9 <- merge(aodf.2006.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2006.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2006.rds")
#mod2
aodf.2006.tmp.s9.m2 <- aodf.2006.tmp.s9[!is.na(aod)]
saveRDS(aodf.2006.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2006.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2006.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2006.tmp.s9.m2[aodf.2006.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2006.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2006.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2006.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2006.tmp.s9.m2[aodf.2006.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2006.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2006.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2007:2007){
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
Temp <- Temp[c == 2007]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2007]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2007]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2007]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2007]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2007"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
aod2007 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2007,aodid,day)
setkey(terra,aodid,day)
aodf.2007<-left_join(aod2007,terra)

#add land use and X,Y
setkey(aodf.2007,aodid)
setkey(wlu,aodid)
aodf.2007.lu<-left_join(aodf.2007,wlu)
aodf.2007.lu<-aodf.2007.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2007.lu <- aodf.2007.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2007.tmp <-aodf.2007.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2007<- Temp[c==2007]
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2007.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2007.tmp[aodf.2007.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2007.tmp, met2007[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2007.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2007.tmp.s1 <- merge(aodf.2007.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2007<- WS[c==2007]
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2007.tmp.s1, met2007[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2007.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2007.tmp.s2 <- merge(aodf.2007.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2007<- WD[c==2007]
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2007.tmp.s2, met2007[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2007.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2007.tmp.s3 <- merge(aodf.2007.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2007<- Rain[c==2007]
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2007.tmp.s3, met2007[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2007.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2007.tmp.s4 <- merge(aodf.2007.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2007<- RH[c==2007]
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2007.tmp.s4, met2007[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2007.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2007.tmp.s4 <- merge(aodf.2007.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2007.lu,aodid,day)
setkey(aodf.2007.tmp.s4,aodid,day)
x1<- left_join(aodf.2007.lu,aodf.2007.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2007.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2007.tmp.s6$month <- as.numeric(format(aodf.2007.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2007.tmp.s6$season<-recode(aodf.2007.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2007.tmp.s6$seasonSW<-recode(aodf.2007.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2007.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2007.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2007.tmp.s6,  ndviid, c, m)
aodf.2007.tmp.s8 <- merge(aodf.2007.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2007]

setkey(aodf.2007.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2007.tmp.s9 <- merge(aodf.2007.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2007.tmp.s9<-aodf.2007.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2007.tmp.s9<-aodf.2007.tmp.s9[,]
aodf.2007.tmp.s9 <- aodf.2007.tmp.s9[Temp != 'NA']
#create weights
aodf.2007.tmp.s9<-aodf.2007.tmp.s9[,obs:=1]
aodf.2007.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2007.tmp.s9)
aodf.2007.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2007.tmp.s9$wt <- 1/aodf.2007.tmp.s9$prob
aodf.2007.tmp.s9$normwt <- aodf.2007.tmp.s9$wt/mean(aodf.2007.tmp.s9$wt)
aodf.2007.tmp.s9<-aodf.2007.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2007<- PM25[c==2007]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2007[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2007 <- pmall2007[pmall2007$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2007, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2007.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2007.tmp.s9[aodf.2007.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2007.tmp.s9, pmall2007 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2007.tmp.s9,aodid,day)
aodf.2007.tmp.s9 <- merge(aodf.2007.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2007<- PM10[c==2007]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2007[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2007 <- pm10all2007[pm10all2007$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2007, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2007.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2007.tmp.s9[aodf.2007.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2007.tmp.s9, pm10all2007 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2007.tmp.s9,aodid,day)
aodf.2007.tmp.s9 <- merge(aodf.2007.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2007.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2007.rds")
#mod2
aodf.2007.tmp.s9.m2 <- aodf.2007.tmp.s9[!is.na(aod)]
saveRDS(aodf.2007.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2007.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2007.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2007.tmp.s9.m2[aodf.2007.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2007.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2007.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2007.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2007.tmp.s9.m2[aodf.2007.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2007.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2007.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2008:2008){
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
Temp <- Temp[c == 2008]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2008]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2008]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2008]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2008]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2008"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), 1)
#create date range
aod2008 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2008,aodid,day)
setkey(terra,aodid,day)
aodf.2008<-left_join(aod2008,terra)

#add land use and X,Y
setkey(aodf.2008,aodid)
setkey(wlu,aodid)
aodf.2008.lu<-left_join(aodf.2008,wlu)
aodf.2008.lu<-aodf.2008.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2008.lu <- aodf.2008.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2008.tmp <-aodf.2008.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2008<- Temp[c==2008]
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2008.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2008.tmp[aodf.2008.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2008.tmp, met2008[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2008.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2008.tmp.s1 <- merge(aodf.2008.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2008<- WS[c==2008]
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2008.tmp.s1, met2008[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2008.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2008.tmp.s2 <- merge(aodf.2008.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2008<- WD[c==2008]
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2008.tmp.s2, met2008[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2008.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2008.tmp.s3 <- merge(aodf.2008.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2008<- Rain[c==2008]
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2008.tmp.s3, met2008[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2008.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2008.tmp.s4 <- merge(aodf.2008.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2008<- RH[c==2008]
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2008.tmp.s4, met2008[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2008.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2008.tmp.s4 <- merge(aodf.2008.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2008.lu,aodid,day)
setkey(aodf.2008.tmp.s4,aodid,day)
x1<- left_join(aodf.2008.lu,aodf.2008.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2008.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2008.tmp.s6$month <- as.numeric(format(aodf.2008.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2008.tmp.s6$season<-recode(aodf.2008.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2008.tmp.s6$seasonSW<-recode(aodf.2008.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2008.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2008.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2008.tmp.s6,  ndviid, c, m)
aodf.2008.tmp.s8 <- merge(aodf.2008.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2008]

setkey(aodf.2008.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2008.tmp.s9 <- merge(aodf.2008.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2008.tmp.s9<-aodf.2008.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2008.tmp.s9<-aodf.2008.tmp.s9[,]
aodf.2008.tmp.s9 <- aodf.2008.tmp.s9[Temp != 'NA']
#create weights
aodf.2008.tmp.s9<-aodf.2008.tmp.s9[,obs:=1]
aodf.2008.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2008.tmp.s9)
aodf.2008.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2008.tmp.s9$wt <- 1/aodf.2008.tmp.s9$prob
aodf.2008.tmp.s9$normwt <- aodf.2008.tmp.s9$wt/mean(aodf.2008.tmp.s9$wt)
aodf.2008.tmp.s9<-aodf.2008.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2008<- PM25[c==2008]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2008[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2008 <- pmall2008[pmall2008$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2008, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2008.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2008.tmp.s9[aodf.2008.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2008.tmp.s9, pmall2008 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2008.tmp.s9,aodid,day)
aodf.2008.tmp.s9 <- merge(aodf.2008.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2008<- PM10[c==2008]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2008[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2008 <- pm10all2008[pm10all2008$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2008, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2008.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2008.tmp.s9[aodf.2008.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2008.tmp.s9, pm10all2008 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2008.tmp.s9,aodid,day)
aodf.2008.tmp.s9 <- merge(aodf.2008.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2008.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2008.rds")
#mod2
aodf.2008.tmp.s9.m2 <- aodf.2008.tmp.s9[!is.na(aod)]
saveRDS(aodf.2008.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2008.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2008.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2008.tmp.s9.m2[aodf.2008.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2008.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2008.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2008.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2008.tmp.s9.m2[aodf.2008.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2008.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2008.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2009:2009){
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
Temp <- Temp[c == 2009]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2009]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2009]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2009]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2009]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2009"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
aod2009 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2009,aodid,day)
setkey(terra,aodid,day)
aodf.2009<-left_join(aod2009,terra)

#add land use and X,Y
setkey(aodf.2009,aodid)
setkey(wlu,aodid)
aodf.2009.lu<-left_join(aodf.2009,wlu)
aodf.2009.lu<-aodf.2009.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2009.lu <- aodf.2009.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2009.tmp <-aodf.2009.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2009<- Temp[c==2009]
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2009.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2009.tmp[aodf.2009.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2009.tmp, met2009[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2009.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2009.tmp.s1 <- merge(aodf.2009.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2009<- WS[c==2009]
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2009.tmp.s1, met2009[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2009.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2009.tmp.s2 <- merge(aodf.2009.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2009<- WD[c==2009]
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2009.tmp.s2, met2009[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2009.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2009.tmp.s3 <- merge(aodf.2009.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2009<- Rain[c==2009]
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2009.tmp.s3, met2009[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2009.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2009.tmp.s4 <- merge(aodf.2009.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2009<- RH[c==2009]
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2009.tmp.s4, met2009[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2009.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2009.tmp.s4 <- merge(aodf.2009.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2009.lu,aodid,day)
setkey(aodf.2009.tmp.s4,aodid,day)
x1<- left_join(aodf.2009.lu,aodf.2009.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2009.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2009.tmp.s6$month <- as.numeric(format(aodf.2009.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2009.tmp.s6$season<-recode(aodf.2009.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2009.tmp.s6$seasonSW<-recode(aodf.2009.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2009.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2009.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2009.tmp.s6,  ndviid, c, m)
aodf.2009.tmp.s8 <- merge(aodf.2009.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2009]

setkey(aodf.2009.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2009.tmp.s9 <- merge(aodf.2009.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2009.tmp.s9<-aodf.2009.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2009.tmp.s9<-aodf.2009.tmp.s9[,]
aodf.2009.tmp.s9 <- aodf.2009.tmp.s9[Temp != 'NA']
#create weights
aodf.2009.tmp.s9<-aodf.2009.tmp.s9[,obs:=1]
aodf.2009.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2009.tmp.s9)
aodf.2009.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2009.tmp.s9$wt <- 1/aodf.2009.tmp.s9$prob
aodf.2009.tmp.s9$normwt <- aodf.2009.tmp.s9$wt/mean(aodf.2009.tmp.s9$wt)
aodf.2009.tmp.s9<-aodf.2009.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2009<- PM25[c==2009]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2009[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2009 <- pmall2009[pmall2009$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2009, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2009.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2009.tmp.s9[aodf.2009.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2009.tmp.s9, pmall2009 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2009.tmp.s9,aodid,day)
aodf.2009.tmp.s9 <- merge(aodf.2009.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2009<- PM10[c==2009]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2009[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2009 <- pm10all2009[pm10all2009$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2009, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2009.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2009.tmp.s9[aodf.2009.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2009.tmp.s9, pm10all2009 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2009.tmp.s9,aodid,day)
aodf.2009.tmp.s9 <- merge(aodf.2009.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2009.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2009.rds")
#mod2
aodf.2009.tmp.s9.m2 <- aodf.2009.tmp.s9[!is.na(aod)]
saveRDS(aodf.2009.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2009.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2009.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2009.tmp.s9.m2[aodf.2009.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2009.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2009.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2009.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2009.tmp.s9.m2[aodf.2009.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2009.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2009.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2010:2010){
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
Temp <- Temp[c == 2010]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2010]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2010]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2010]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2010]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2010"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
#create date range
aod2010 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2010,aodid,day)
setkey(terra,aodid,day)
aodf.2010<-left_join(aod2010,terra)

#add land use and X,Y
setkey(aodf.2010,aodid)
setkey(wlu,aodid)
aodf.2010.lu<-left_join(aodf.2010,wlu)
aodf.2010.lu<-aodf.2010.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2010.lu <- aodf.2010.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2010.tmp <-aodf.2010.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2010<- Temp[c==2010]
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2010.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2010.tmp[aodf.2010.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2010.tmp, met2010[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2010.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2010.tmp.s1 <- merge(aodf.2010.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2010<- WS[c==2010]
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2010.tmp.s1, met2010[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2010.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2010.tmp.s2 <- merge(aodf.2010.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2010<- WD[c==2010]
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2010.tmp.s2, met2010[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2010.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2010.tmp.s3 <- merge(aodf.2010.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2010<- Rain[c==2010]
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2010.tmp.s3, met2010[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2010.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2010.tmp.s4 <- merge(aodf.2010.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2010<- RH[c==2010]
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2010.tmp.s4, met2010[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2010.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2010.tmp.s4 <- merge(aodf.2010.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2010.lu,aodid,day)
setkey(aodf.2010.tmp.s4,aodid,day)
x1<- left_join(aodf.2010.lu,aodf.2010.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2010.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2010.tmp.s6$month <- as.numeric(format(aodf.2010.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2010.tmp.s6$season<-recode(aodf.2010.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2010.tmp.s6$seasonSW<-recode(aodf.2010.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2010.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2010.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2010.tmp.s6,  ndviid, c, m)
aodf.2010.tmp.s8 <- merge(aodf.2010.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2010]

setkey(aodf.2010.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2010.tmp.s9 <- merge(aodf.2010.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2010.tmp.s9<-aodf.2010.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2010.tmp.s9<-aodf.2010.tmp.s9[,]
aodf.2010.tmp.s9 <- aodf.2010.tmp.s9[Temp != 'NA']
#create weights
aodf.2010.tmp.s9<-aodf.2010.tmp.s9[,obs:=1]
aodf.2010.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2010.tmp.s9)
aodf.2010.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2010.tmp.s9$wt <- 1/aodf.2010.tmp.s9$prob
aodf.2010.tmp.s9$normwt <- aodf.2010.tmp.s9$wt/mean(aodf.2010.tmp.s9$wt)
aodf.2010.tmp.s9<-aodf.2010.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2010<- PM25[c==2010]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2010[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2010 <- pmall2010[pmall2010$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2010, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2010.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2010.tmp.s9[aodf.2010.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2010.tmp.s9, pmall2010 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2010.tmp.s9,aodid,day)
aodf.2010.tmp.s9 <- merge(aodf.2010.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2010<- PM10[c==2010]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2010[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2010 <- pm10all2010[pm10all2010$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2010, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2010.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2010.tmp.s9[aodf.2010.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2010.tmp.s9, pm10all2010 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2010.tmp.s9,aodid,day)
aodf.2010.tmp.s9 <- merge(aodf.2010.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2010.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2010.rds")
#mod2
aodf.2010.tmp.s9.m2 <- aodf.2010.tmp.s9[!is.na(aod)]
saveRDS(aodf.2010.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2010.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2010.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2010.tmp.s9.m2[aodf.2010.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2010.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2010.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2010.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2010.tmp.s9.m2[aodf.2010.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2010.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2010.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2011:2011){
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
Temp <- Temp[c == 2011]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2011]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2011]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2011]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2011]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2011"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
aod2011 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2011,aodid,day)
setkey(terra,aodid,day)
aodf.2011<-left_join(aod2011,terra)

#add land use and X,Y
setkey(aodf.2011,aodid)
setkey(wlu,aodid)
aodf.2011.lu<-left_join(aodf.2011,wlu)
aodf.2011.lu<-aodf.2011.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2011.lu <- aodf.2011.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2011.tmp <-aodf.2011.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2011<- Temp[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2011.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2011.tmp[aodf.2011.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2011.tmp, met2011[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2011.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s1 <- merge(aodf.2011.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2011<- WS[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2011.tmp.s1, met2011[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2011.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s2 <- merge(aodf.2011.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2011<- WD[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2011.tmp.s2, met2011[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2011.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s3 <- merge(aodf.2011.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2011<- Rain[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2011.tmp.s3, met2011[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2011.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s4 <- merge(aodf.2011.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2011<- RH[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2011.tmp.s4, met2011[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2011.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s4 <- merge(aodf.2011.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2011.lu,aodid,day)
setkey(aodf.2011.tmp.s4,aodid,day)
x1<- left_join(aodf.2011.lu,aodf.2011.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2011.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2011.tmp.s6$month <- as.numeric(format(aodf.2011.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2011.tmp.s6$season<-recode(aodf.2011.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2011.tmp.s6$seasonSW<-recode(aodf.2011.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2011.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2011.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2011.tmp.s6,  ndviid, c, m)
aodf.2011.tmp.s8 <- merge(aodf.2011.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2011]

setkey(aodf.2011.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2011.tmp.s9 <- merge(aodf.2011.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[,]
aodf.2011.tmp.s9 <- aodf.2011.tmp.s9[Temp != 'NA']
#create weights
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[,obs:=1]
aodf.2011.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2011.tmp.s9)
aodf.2011.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2011.tmp.s9$wt <- 1/aodf.2011.tmp.s9$prob
aodf.2011.tmp.s9$normwt <- aodf.2011.tmp.s9$wt/mean(aodf.2011.tmp.s9$wt)
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2011<- PM25[c==2011]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2011[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2011 <- pmall2011[pmall2011$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2011, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2011.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9[aodf.2011.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2011.tmp.s9, pmall2011 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2011.tmp.s9,aodid,day)
aodf.2011.tmp.s9 <- merge(aodf.2011.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2011<- PM10[c==2011]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2011[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2011 <- pm10all2011[pm10all2011$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2011, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2011.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9[aodf.2011.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2011.tmp.s9, pm10all2011 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2011.tmp.s9,aodid,day)
aodf.2011.tmp.s9 <- merge(aodf.2011.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2011.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2011.rds")
#mod2
aodf.2011.tmp.s9.m2 <- aodf.2011.tmp.s9[!is.na(aod)]
saveRDS(aodf.2011.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2011.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2011.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9.m2[aodf.2011.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2011.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2011.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2011.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9.m2[aodf.2011.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2011.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2011.rds")



#clear workspace
rm(list = ls())
gc()

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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday_MPM.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/snippets/nearestbyday.r")


########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2012:2012){
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
Temp <- Temp[c == 2012]


#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD_D.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
WD <- WD[X != 'NaN']
WD <- WD[WD != 'NaN']
WD <- WD[c == 2012]

#WS
WS <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="/")
WS[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS <- WS[X != 'NaN']
WS <- WS[WS != 'NaN']
WS <- WS[c == 2012]


#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH <- RH[X != 'NaN']
RH <- RH[RH != 'NaN']
RH <- RH[c == 2012]


#Rain
Rain <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Rain_D.csv")
Rain$date<-paste(Rain$Day,Rain$Month,Rain$Year,sep="/")
Rain[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Rain[, c := as.numeric(format(day, "%Y")) ]
Rain[,c("Year","Month","Day","date"):=NULL]
Rain <- Rain[X != 'NaN']
Rain<- Rain[Rain != 'NaN']
Rain<- Rain[c == 2012]



#########-------------------############
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load terra
#load aod data
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
terra <- terra[terra$aodid %in% ilgreen$aodid, ] 
terra<- terra[yr == "2012"]
#system.time(terra[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(terra[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(terra));names(l)=names(terra);l
terra<-terra[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
aod2012 <- data.table(expand.grid(aodid = terra[, unique(aodid)], day = days))
setkey(aod2012,aodid,day)
setkey(terra,aodid,day)
aodf.2012<-left_join(aod2012,terra)

#add land use and X,Y
setkey(aodf.2012,aodid)
setkey(wlu,aodid)
aodf.2012.lu<-left_join(aodf.2012,wlu)
aodf.2012.lu<-aodf.2012.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2012.lu <- aodf.2012.lu[!is.na(pblid)]



#########-------------------############
#create temporal dataset
aodf.2012.tmp <-aodf.2012.lu[,c(1,2,9,10),with=FALSE]


#Temp
#xtract year met
met2012<- Temp[c==2012]
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
#create aod matrix
setkey(aodf.2012.tmp, aodid)
lu.m <- makepointsmatrix(aodf.2012.tmp[aodf.2012.tmp[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2012.tmp, met2012[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 7, maxdistance = NA)

setkey(aodf.2012.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2012.tmp.s1 <- merge(aodf.2012.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2012<- WS[c==2012]
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2012.tmp.s1, met2012[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 7, maxdistance = NA)

setkey(aodf.2012.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2012.tmp.s2 <- merge(aodf.2012.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)

#WD
#xtract year met
met2012<- WD[c==2012]
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2012.tmp.s2, met2012[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 7, maxdistance = NA)

setkey(aodf.2012.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2012.tmp.s3 <- merge(aodf.2012.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2012<- Rain[c==2012]
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2012.tmp.s3, met2012[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 7, maxdistance = NA)

setkey(aodf.2012.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2012.tmp.s4 <- merge(aodf.2012.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2012<- RH[c==2012]
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
closestaodse<- nearestbydayMEAN(lu.m ,met.m , 
                            aodf.2012.tmp.s4, met2012[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 7, maxdistance = NA)

setkey(aodf.2012.tmp.s4,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2012.tmp.s4 <- merge(aodf.2012.tmp.s4, closestaodse[,list(day,RH,aodid)], all.x = T)


#join back LU
setkey(aodf.2012.lu,aodid,day)
setkey(aodf.2012.tmp.s4,aodid,day)
x1<- left_join(aodf.2012.lu,aodf.2012.tmp.s4)


#Join PBL
setkey(pbl , day, pblid)
setkey(x1, day, pblid)
aodf.2012.tmp.s6<-left_join(x1, pbl)


#add season
aodf.2012.tmp.s6$month <- as.numeric(format(aodf.2012.tmp.s6$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
aodf.2012.tmp.s6$season<-recode(aodf.2012.tmp.s6$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
aodf.2012.tmp.s6$seasonSW<-recode(aodf.2012.tmp.s6$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
#add month
aodf.2012.tmp.s6[, m := as.numeric(format(day, "%m")) ]
#add year
aodf.2012.tmp.s6[, c := as.numeric(format(day, "%Y")) ]
#join NDVI to aod
setkey(ndvi, ndviid, c, m )
setkey(aodf.2012.tmp.s6,  ndviid, c, m)
aodf.2012.tmp.s8 <- merge(aodf.2012.tmp.s6, ndvi, all.x = T)

#add dust days
dust2<-fread("/media/NAS/Uni/Data/Israel/Dust/DDAqTer28.5.2014.csv")
dust2$date<-paste(dust2$Day,dust2$Month,dust2$Year,sep="/")
dust2[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
dust2[,c("Year","Month","Day","Max","date"):=NULL]
setnames(dust2,"StationID","stn")
dust2[, c := as.numeric(format(day, "%Y")) ]
dust2<- dust2[c==2012]

setkey(aodf.2012.tmp.s8 , day, stn)
setkey(dust2, day, stn)
aodf.2012.tmp.s9 <- merge(aodf.2012.tmp.s8, dust2[,list(day,stn,Dust)], all.x = T)
aodf.2012.tmp.s9<-aodf.2012.tmp.s9[is.na(Dust), Dust:= 0]


#########-------------------############
#clean
aodf.2012.tmp.s9<-aodf.2012.tmp.s9[,]
aodf.2012.tmp.s9 <- aodf.2012.tmp.s9[Temp != 'NA']
#create weights
aodf.2012.tmp.s9<-aodf.2012.tmp.s9[,obs:=1]
aodf.2012.tmp.s9[is.na(aod), obs:= 0]
#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2012.tmp.s9)
aodf.2012.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2012.tmp.s9$wt <- 1/aodf.2012.tmp.s9$prob
aodf.2012.tmp.s9$normwt <- aodf.2012.tmp.s9$wt/mean(aodf.2012.tmp.s9$wt)
aodf.2012.tmp.s9<-aodf.2012.tmp.s9[, c(53,54) := NULL]


#########-------------------############
#add meanPM per grid per day

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
pmall2012<- PM25[c==2012]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2012[,c("PM25","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pmall2012 <- pmall2012[pmall2012$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pmall2012, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2012.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2012.tmp.s9[aodf.2012.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2012.tmp.s9, pmall2012 [, list(day,PM25,stn)], 
                            "aodid", "stn", "meanPM25", "PM25", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM25 :=NULL]
closestaodse[,meanPM25 :=NULL]
closestaodse[,meanPM25knn:=NULL]
closestaodse[,meanPM25nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2012.tmp.s9,aodid,day)
aodf.2012.tmp.s9 <- merge(aodf.2012.tmp.s9,closestaodse,all.x = T)



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
#PM10_m means avialble obs per year
PM10[, PM10_n := length(na.omit(PM10)),by=list(stn,c)]
#clear non PM10 days
PM10<-PM10[!is.na(PM10)]
#clear non continous stations
PM10 <- PM10[PM10_n > 5  , ]
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
pm10all2012<- PM10[c==2012]
#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pm10all2012[,c("PM10","stn"),with=F]),.(stn),nrow))
table_temp<-table_temp[V1 > 364]
pm10all2012 <- pm10all2012[pm10all2012$stn %in% table_temp$stn, ] 
#create PM matrix
pm.m <- makepointsmatrix(pm10all2012, "x_stn_ITM", "y_stn_ITM", "stn")
### create aod grid
setkey(aodf.2012.tmp.s9, aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2012.tmp.s9[aodf.2012.tmp.s9[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaodse<- nearestbydayMEAN(aod.m  ,pm.m , 
                            aodf.2012.tmp.s9, pm10all2012 [, list(day,PM10,stn)], 
                            "aodid", "stn", "meanPM10", "PM10", knearest = 6, maxdistance = NA)
#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)
#cleanup
closestaodse[,PM10 :=NULL]
closestaodse[,meanPM10 :=NULL]
closestaodse[,meanPM10knn:=NULL]
closestaodse[,meanPM10nobs:=NULL]
#join to DB
setkey(closestaodse,aodid,day)
setkey(aodf.2012.tmp.s9,aodid,day)
aodf.2012.tmp.s9 <- merge(aodf.2012.tmp.s9,closestaodse,all.x = T)



###save mods
#mod3
saveRDS(aodf.2012.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.TR.2012.rds")
#mod2
aodf.2012.tmp.s9.m2 <- aodf.2012.tmp.s9[!is.na(aod)]
saveRDS(aodf.2012.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.TR.2012.rds")
#mod1

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2012.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2012.tmp.s9.m2[aodf.2012.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2012.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2012.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
setkey(aodf.2012.tmp.s9.m2,aodid)
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2012.tmp.s9.m2[aodf.2012.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2012.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 7, maxdistance = 1500)
closestaod[,i.stn :=NULL]
closestaod[,closestaodknn :=NULL]
closestaod[,closestaodnobs:=NULL]
setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]
#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR10.2012.rds")



#clear workspace
rm(list = ls())
gc()
