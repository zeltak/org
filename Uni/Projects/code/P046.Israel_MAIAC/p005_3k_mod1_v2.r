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


########### import datasets
#import full XY

fullxy<-fread("/media/NAS/Uni/Data/Israel/IPA_stations/pmstn_miss.csv")
setnames(fullxy,"V2","stn")
setnames(fullxy,"V3","x_stn_ITM")
setnames(fullxy,"V4", "y_stn_ITM")
fullxy<-fullxy[,c(2:4),with=FALSE]


##################################
#PM10
##################################

#PM10
pm10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData10.csv")
pm10$date<-paste(pm10$Day,pm10$Month,pm10$Year,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm10[, c := as.numeric(format(day, "%Y")) ]
pm10[,c("Year","Month","Day","V10","date","X","StationID"):=NULL]
setnames(pm10,"Y","stn")

#add full X,Y values
setkey(fullxy , stn)
setkey(pm10, stn)
pm10 <- merge(pm10, fullxy, all.x = T)
pm10 <- pm10[x_stn_ITM != 'NA']
pm10[,length(na.omit(PM10)),by=list(stn,c)]
pm10[, pm10_miss := length(na.omit(PM10)),by=list(stn,c)]
pm10<-pm10[!is.na(PM10)]
pm10<-na.omit(pm10)





# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm10, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod terra matrix
itm<-read.csv("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/Archive/pwITM.csv")
itm<-as.data.table(itm)
setkey(allbestpred , aodid)
setkey(itm, aodid)
allbestpred <- merge(allbestpred, itm[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
allbestpred$aodid<-as.character(allbestpred$aodid)
aod.m <- makepointsmatrix(allbestpred, "x_aod_ITM", "y_aod_ITM", "aodid")


########### join aqua to pm10
closestaod <- nearestbyday(pm.m, aod.m, 
                              pm10, allbestpred [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 4200)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm10,stn,day)
setkey(closestaod,stn,day)
pm10.m1 <- merge(pm10, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# pm10.m1 <- pm10.m1[aod != "NA"]


########
##pm10
########

#lme formulas
#base raw
m1t.formula <- as.formula(PM10~ aod) 
m1t.formula <- as.formula(PM10~ aod+(1+aod|day)) 
#base+regions
out.m1 = lm(m1t.formula ,data =  pm10.m1,na.action = na.exclude)
pm10.m1$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=pm10.m1))





###PM25



########### import datasets
#import full XY

fullxy<-fread("/media/NAS/Uni/Data/Israel/IPA_stations/pmstn_miss.csv")
setnames(fullxy,"V2","stn")
setnames(fullxy,"V3","x_stn_ITM")
setnames(fullxy,"V4", "y_stn_ITM")
fullxy<-fullxy[,c(2:4),with=FALSE]


##################################
#PM25
##################################

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData25.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","V10","date","X","StationID"):=NULL]
setnames(PM25,"Y","stn")

#add full X,Y values
setkey(fullxy , stn)
setkey(PM25, stn)
PM25 <- merge(PM25, fullxy, all.x = T)
PM25 <- PM25[x_stn_ITM != 'NA']
PM25[,length(na.omit(PM25)),by=list(stn,c)]
PM25[, PM25_miss := length(na.omit(PM25)),by=list(stn,c)]
PM25<-PM25[!is.na(PM25)]
PM25<-na.omit(PM25)


# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod terra matrix
itm<-read.csv("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/Archive/pwITM.csv")
itm<-as.data.table(itm)
setkey(allbestpred , aodid)
setkey(itm, aodid)
allbestpred <- merge(allbestpred, itm[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
allbestpred$aodid<-as.character(allbestpred$aodid)
aod.m <- makepointsmatrix(allbestpred, "x_aod_ITM", "y_aod_ITM", "aodid")


########### join aqua to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                              PM25, allbestpred [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 3000)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# PM25.m1 <- PM25.m1[aod != "NA"]


########
##PM25
########

#lme formulas
#base raw
m1t.formula <- as.formula(PM25~ aod) 
m1t.formula <- as.formula(PM25~ aod+(1+aod|day)) 
#base+regions
out.m1 = lm(m1t.formula ,data =  PM25.m1,na.action = na.exclude)
PM25.m1$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=PM25.m1))



out.m1 = lmer(m1t.formula ,data =  PM25.m1,na.action = na.exclude)
PM25.m1$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=PM25.m1))

