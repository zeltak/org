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
source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")

########### import datasets
#import NDVI
ndvid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndviid_aodid.csv")
ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
#import PBL
pbl <-  fread("/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/fianlpblXY_2002.csv")
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





#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid.csv")

###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 
aqua<- aqua[yr == "2011"]
system.time(aqua[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
#clean
l=seq(names(aqua));names(l)=names(aqua);l
aqua<-aqua[, c(2,3,7:24,26,27,28) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
aod2011 <- data.table(expand.grid(aodid = aqua[, unique(aodid)], day = days))
setkey(aod2011,aodid,day)
setkey(aqua,aodid,day)
aodf.2011<-left_join(aod2011,aqua)

#add land use and X,Y
setkey(aodf.2011,aodid)
setkey(wlu,aodid)
aodf.2011.lu<-left_join(aodf.2011,wlu)
aodf.2011.lu<-aodf.2011.lu[, c(7:9) := NULL]
#clean points with no lu data (on borders and in golan)
aodf.2011.lu <- aodf.2011.lu[!is.na(pblid)]

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
closestaodse<- nearestbyday(lu.m ,met.m , 
                            aodf.2011.tmp, met2011[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 5, maxdistance = NA)

setkey(aodf.2011.tmp,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s1 <- merge(aodf.2011.tmp, closestaodse[,list(day,Temp,aodid)], all.x = T)

#WS
#xtract year met
met2011<- WS[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            aodf.2011.tmp.s1, met2011[, list(day,WS,stn)], 
                            "aodid", "stn", "meanT", "WS", knearest = 5, maxdistance = NA)

setkey(aodf.2011.tmp.s1,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s2 <- merge(aodf.2011.tmp.s1, closestaodse[,list(day,WS,aodid)], all.x = T)




#WD
#xtract year met
met2011<- WD[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            aodf.2011.tmp.s2, met2011[, list(day,WD,stn)], 
                            "aodid", "stn", "meanT", "WD", knearest = 5, maxdistance = NA)

setkey(aodf.2011.tmp.s2,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s3 <- merge(aodf.2011.tmp.s2, closestaodse[,list(day,WD,aodid)], all.x = T)



#Rain
#xtract year met
met2011<- Rain[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            aodf.2011.tmp.s3, met2011[, list(day,Rain,stn)], 
                            "aodid", "stn", "meanT", "Rain", knearest = 5, maxdistance = NA)

setkey(aodf.2011.tmp.s3,aodid,day)
setkey(closestaodse,aodid,day)
aodf.2011.tmp.s4 <- merge(aodf.2011.tmp.s3, closestaodse[,list(day,Rain,aodid)], all.x = T)



#RH
#xtract year met
met2011<- RH[c==2011]
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
closestaodse<- nearestbyday(lu.m ,met.m , 
                            aodf.2011.tmp.s4, met2011[, list(day,RH,stn)], 
                            "aodid", "stn", "meanT", "RH", knearest = 5, maxdistance = NA)

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


#create weights
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[,obs:=1]
aodf.2011.tmp.s9[is.na(aod), obs:= 0]

#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(month),family=binomial,data=aodf.2011.tmp.s9)
aodf.2011.tmp.s9$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
aodf.2011.tmp.s9$wt <- 1/aodf.2011.tmp.s9$prob
aodf.2011.tmp.s9$normwt <- aodf.2011.tmp.s9$wt/mean(aodf.2011.tmp.s9$wt)
aodf.2011.tmp.s9<-aodf.2011.tmp.s9[, c(53,54) := NULL]

###save mods
#mod3
saveRDS(aodf.2011.tmp.s9,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2011.rds")
#mod2
aodf.2011.tmp.s9.m2 <- aodf.2011.tmp.s9[!is.na(aod)]
saveRDS(aodf.2011.tmp.s9.m2,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2011.rds")
#mod1
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
PM25<- PM25[c==2011]


#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9.m2[aodf.2011.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25, aodf.2011.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25[,list(stn,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.rds")



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
PM10<- PM10[c==2011]


#create PM matrix
pm.m <- makepointsmatrix(PM10, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
aod.m <- makepointsmatrix(aodf.2011.tmp.s9.m2[aodf.2011.tmp.s9.m2[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM10
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, aodf.2011.tmp.s9.m2, 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10[,list(stn,day,PM10)], closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2011.rds")
