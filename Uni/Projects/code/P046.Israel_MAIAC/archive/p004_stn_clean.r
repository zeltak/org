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


#PM
pm10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData10.csv")
pm10$date<-paste(pm10$Day,pm10$Month,pm10$Year,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm10[, c := as.numeric(format(day, "%Y")) ]
pm10[,c("Year","Month","Day","V10","date"):=NULL]
setnames(pm10,"StationID","x_stn_ITM")
setnames(pm10,"X","y_stn_ITM")
setnames(pm10,"Y", "stn")


#PM
pm25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData25.csv")
pm25$date<-paste(pm25$Day,pm25$Month,pm25$Year,sep="/")
pm25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm25[, c := as.numeric(format(day, "%Y")) ]
pm25[,c("Year","Month","Day","V10","date"):=NULL]
setnames(pm25,"StationID","x_stn_ITM")
setnames(pm25,"X","y_stn_ITM")
setnames(pm25,"Y", "stn")

#Temp
temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp.csv")
temp$date<-paste(temp$Day,temp$Month,temp$Year,sep="/")
temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
temp[, c := as.numeric(format(day, "%Y")) ]
temp[,c("Year","Month","Day","date"):=NULL]
setnames(temp,"StationID/AOD","stn")
setnames(temp,"X","x_stn_ITM")
setnames(temp,"Y", "y_stn_ITM")

#WD
WD <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/WD.csv")
WD$date<-paste(WD$Day,WD$Month,WD$Year,sep="/")
WD[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
WD[, c := as.numeric(format(day, "%Y")) ]
WD[,c("Year","Month","Day","date"):=NULL]
setnames(WD,"StationID/AOD","stn")
setnames(WD,"X","x_stn_ITM")
setnames(WD,"Y", "y_stn_ITM")

#RH
RH <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/RH.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="/")
RH[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","V10","Day","date"):=NULL]
setnames(RH,"StationID/AOD","stn")
setnames(RH,"X","x_stn_ITM")
setnames(RH,"Y", "y_stn_ITM")



RHx<-RH[,c(3,4,5),with=FALSE]
WDx<-WD[,c(3,4,5),with=FALSE]
tempx<-temp[,c(3,4,5),with=FALSE]
PM10x<-pm10[,c(3,4,5),with=FALSE]
PM25x<-pm25[,c(3,4,5),with=FALSE]

alls<-rbind(RHx,WDx,tempx,PM10x,PM25x)
describe(alls)

#export unique stn location to join to guid
alls_agg <- (alls[, list(
                        x_stn_ITM =  x_stn_ITM[1], 
                        y_stn_ITM =  y_stn_ITM[1]),
                        by = stn])
write.csv(alls_agg,"/home/zeltak/ZH_tmp/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY.csv")

#to create a date range based on start and end points use
days<-seq.Date(from = as.Date("2000-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
mg <- data.table(expand.grid(stn = alls[,unique(stn)], day = days))



setkey(pm10, day,stn)
setkey(mg, day,stn)
J1 <- merge(mg,pm10, all.x = T)

setkey(temp, day,stn)
setkey(J1, day,stn)
J2 <- merge(J1,temp[,list(day,stn,Temp)], all.x = T)

describe(J2$Temp)
describe(temp$Temp)

setkey(pm25, day,stn)
setkey(J2, day,stn)
J3 <- merge(J2,pm25[,list(day,stn,PM25)], all.x = T)

describe(J3$PM25)
describe(pm25$PM25)

setkey(RH, day,stn)
setkey(J3, day,stn)
J4 <- merge(J3,RH[,list(day,stn,RH)], all.x = T)

describe(J4$RH)
describe(RH$RH)


setkey(WD, day,stn)
setkey(J4, day,stn)
J5 <- merge(J4,WD[,list(day,stn,WD)], all.x = T)
describe(J5$WD)
describe(WD$WD)

#remove below year 2002 which we dont need
J5[, c := as.numeric(format(day, "%Y")) ]
J6 <- J5[c >= 2002]


#import LU
lu1<-fread("/home/zeltak/ZH_tmp/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#import stn keytable
stnkey<-fread("/home/zeltak/ZH_tmp/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILstnXY_aodid.csv")

setkey(stnkey,stn)
setkey(J6,stn)
J7 <- merge(J6,stnkey[,list(stn, aodid)], all.x = T)



setkey(lu1,aodid)
setkey(J7,aodid)
J8 <- merge(J7,lu1, all.x = T)
describe(J8)
#delete with missing aodid
J9 <- J8[!is.na(long_aod)]

#create pm25 dataset
pm <- J9[!is.na(PM25)]

describe(pm)




