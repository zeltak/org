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
library(DataCombine)
library(readr)



spatial <- fread("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/fishnet with spatial data/fishnet_all.csv")
setnames(spatial,"OBJECTID","gid")
#bring back XY
xy<-as.data.table(read.dbf("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/fishnet/fishnet.dbf"))
setnames(xy,"OBJECTID","gid")
setkey(spatial,gid)
setkey(xy ,gid)
spatialxy <- merge(spatial,xy[,list(gid,X,Y)], all.x = T)  


#create big grid
days<-seq.Date(from = as.Date("2002-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
sfull <- data.table(expand.grid(gid = spatialxy[, unique(gid)], day = days))
setkey(sfull,gid)
setkey(spatialxy ,gid)
sfull <- merge(sfull,spatialxy, all.x = T)  


#load function for geomerge
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")

#load met
Temp <- fread("//media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/data/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="-")
Temp[, day:=as.Date(strptime(date, "%d-%m-%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp$X<- NULL
Temp$Y<- NULL
Temp <- Temp[Temp != 'NaN']

#Temp
met.m <- makepointsmatrix(Temp, "X", "Y", "stn")
sfull$gid<-as.character(sfull$gid)
setkey(sfull, gid)
str(sfull)
lu.m <- makepointsmatrix(sfull[sfull[,unique(gid)], list(X, Y, gid), mult = "first"], "X", "Y", "gid")
out <- nearestbyday(lu.m ,met.m , 
                            sfull, Temp[, list(day,Temp,stn)], 
                            "gid", "stn", "cloesetSTN", "Temp", knearest = 6, maxdistance = 15000)
#head(aqua.met)
setkey(sfull,gid,day)
setkey(out,gid,day)
sfull.tmp <- merge(sfull, out[,list(day,Temp,gid)], all.x = T)
#summary(aqm2.2009$tempavg)



#load met
RH <- fread("//media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/data/RH_D.csv")
RH$date<-paste(RH$Day,RH$Month,RH$Year,sep="-")
RH[, day:=as.Date(strptime(date, "%d-%m-%Y"))]
RH[, c := as.numeric(format(day, "%Y")) ]
RH[,c("Year","Month","Day","date"):=NULL]
RH$X<- NULL
RH$Y<- NULL
RH <- RH[RH != 'NaN']

#RH
met.m <- makepointsmatrix(RH, "X", "Y", "stn")
sfull.tmp$gid<-as.character(sfull.tmp$gid)
setkey(sfull.tmp, gid)
str(sfull.tmp)
lu.m <- makepointsmatrix(sfull.tmp[sfull.tmp[,unique(gid)], list(X, Y, gid), mult = "first"], "X", "Y", "gid")
out <- nearestbyday(lu.m ,met.m , 
                            sfull.tmp, RH[, list(day,RH,stn)], 
                            "gid", "stn", "cloesetSTN", "RH", knearest = 6, maxdistance = 15000)
#head(aqua.met)
setkey(sfull.tmp,gid,day)
setkey(out,gid,day)
sfull.tmp <- merge(sfull.tmp, out[,list(day,RH,gid)], all.x = T)
#summary(aqm2.2009$RHavg)

#load met
WS <- fread("//media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/data/WS_D.csv")
WS$date<-paste(WS$Day,WS$Month,WS$Year,sep="-")
WS[, day:=as.Date(strptime(date, "%d-%m-%Y"))]
WS[, c := as.numeric(format(day, "%Y")) ]
WS[,c("Year","Month","Day","date"):=NULL]
WS$X<- NULL
WS$Y<- NULL
WS <- WS[WS != 'NaN']

#WS
met.m <- makepointsmatrix(WS, "X", "Y", "stn")
sfull.tmp$gid<-as.character(sfull.tmp$gid)
setkey(sfull.tmp, gid)
str(sfull.tmp)
lu.m <- makepointsmatrix(sfull.tmp[sfull.tmp[,unique(gid)], list(X, Y, gid), mult = "first"], "X", "Y", "gid")
out <- nearestbyday(lu.m ,met.m , 
                            sfull.tmp, WS[, list(day,WS,stn)], 
                            "gid", "stn", "cloesetSTN", "WS", knearest = 6, maxdistance = 15000)
#head(aqua.met)
setkey(sfull.tmp,gid,day)
setkey(out,gid,day)
sfull.tmp <- merge(sfull.tmp, out[,list(day,WS,gid)], all.x = T)
#summary(aqm2.2009$WSavg)



##no2
no2key <- fread("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/no2/noxXYTLVgid.csv")
## deletes column 
no2key[,6 := NULL]
no2key[,x := NULL]
no2key[,MEAN_temp_ := NULL]
no2key[,Distance := NULL]

#import No2
no2 <- fread("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/no2/NO2_D.csv")
#aggregate up so we are left with only 1 stn per point
no2xy<-no2 %>%
    group_by(stn) %>%
    summarise(X = mean(X), Y=mean(Y))
write.csv(no2xy,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/no2/NO2_DXY.csv")
no2$date<-paste(no2$Day,no2$Month,no2$Year,sep="-")
no2[, day:=as.Date(strptime(date, "%d-%m-%Y"))]

# :)
use <- c("YAF","PTR","TMM","ANT","IRD","AMI","DAV") 
no2<- no2[stn %in% use]



##setkey arranges the object by a column
setkey(no2key,stn)
setkey(no2,stn)
no2xy<-merge(no2,no2key,all.x=T)
summary(no2xy)
no2xy<-select(no2xy,NO2,X.x,Y.x,day,OBJECTID,stn)
#configures a column name
setnames(no2xy,"X.x","X")
setnames(no2xy,"Y.x","Y")
setnames(no2xy,"OBJECTID","gid")

no2xy$gid<-as.character(no2xy$gid)

setkey(no2xy,gid,day)
setkey(sfull.tmp,gid,day)
no2xy.tmp <-merge(no2xy,sfull.tmp,all.x=T)

saveRDS(no2xy.tmp,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/mod1.rds")
saveRDS(sfull.tmp,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/mod2.rds")


