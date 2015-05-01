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

source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")


#----------------------------------> PM Data
#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25_northeast_2012_matlab.csv")
PM25 [PM25  == -999] <- NA
PM25$datex<-paste(PM25$day,PM25$month,PM25$year,sep="/")
PM25[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]
PM25[,datex:=NULL]
PM25<-PM25[!is.na(PM25)]
setnames(PM25,"sitecode","SiteCode")
PM25$SiteCode<-as.character(PM25$SiteCode)



#load y2012
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

stage3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2012.rds")
s3<-as.data.table(stage3)
rm(stage3)
gc()
s3<-s3[, c(41:49) := NULL]
tx <- fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/met_humidity_0014.csv")
tx<-filter(tx,year == 2012)
tx$datex<-paste(tx$day,tx$month,tx$year,sep="/")
tx[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]

setnames(s3,"nearest_NCDC_stn","stn")
s3$datex<-paste(s3$Day,s3$Month,s3$Year,sep="/")
s3[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]

setkey(s3,day,stn)
setkey(tx,day,stn)
s3 <- merge(s3,tx, all.x = T)
s3[s3 == -999] <- NA
gc()

#head(s3)
s3[,Day :=NULL]
s3[,PM25 :=NULL]
s3[,DOY :=NULL]
s3[,datex :=NULL]
setnames(s3,"AOD","aod")
s3$GUID<-as.character(s3$GUID)

#-------> meanPM25  for mod 2+3
#subset PM
PM25.2012<-filter(PM25,year==2012)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2012, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2012 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 12000, nearestmean= TRUE)

#summary(pmj1$closestmean)
#check
# summary(pmj1$closestmean)
# tst<-filter(pmj1,is.na(closestmean))
# tst <-tst %>%
#     group_by(GUID) %>%
#     summarise(lat_aod = mean(closestmean, na.rm=TRUE))
# head(tst)
# reg<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU_v2_reg.csv")
# reg$GUID<-as.character(reg$GUID)
# setkey(reg,GUID)
# setkey(tst,GUID)
# t4 <- merge(tst,reg, all.x = T)
# write.csv(t4,"~/ZH_tmp/tdtd.csv")

#cleanup
pmj1[,PM25 :=NULL]
pmj1[,closest :=NULL]
pmj1[,closestknn :=NULL]
pmj1[,closestnobs:=NULL]

#join to DB
setkey(pmj1,GUID,day)
setkey(s3,GUID,day)
m6 <- merge(s3,pmj1,all.x = T)
gc()

m6_NA<- m6[is.na(closestmean),]
m6_NA[,closestmean := NULL]
m6_good<- m6[!is.na(closestmean),]
rm(m6)
gc()

#keep only full stations
table_temp<-as.data.table(ddply(na.omit(PM25.2012[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 300]
PM25.2012f <- PM25.2012[PM25.2012$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2012f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2012f [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "meanPM", "PM25", knearest = 6, maxdistance = NA,nearestmean = TRUE)
summary(pmj2)
#cleanup
pmj2[,PM25 :=NULL]
pmj2[,meanPM :=NULL]
pmj2[,meanPMknn :=NULL]
pmj2[,meanPMnobs:=NULL]

#join to DB
setkey(pmj2,GUID,day)
setkey(m6_NA,GUID,day)
m6x <- merge(m6_NA,pmj2,all.x = T)
setnames(m6_good,"closestmean","meanPMmean")
s3<-rbindlist(list(m6x,m6_good))
setnames(s3,"meanPMmean","meanPM")
rm(m6x,m6_good,pmj2,pmj1)
gc()



#---------> save mods 2+3
#clean
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2012.rds")
#mod2
s2<- s3[!is.na(aod)]
rm(s3)
gc()
reg<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU_v2_reg.csv")
reg<-select(reg,GUID,region)
reg$GUID<-as.character(reg$GUID)
setkey(reg,GUID)
setkey(s2,GUID)
s2<-merge(s2,reg)
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2012.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2012[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)


setkey(PM25.2012,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25.2012[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2012.rds")
keep(PM25, sure=TRUE) 
gc()


