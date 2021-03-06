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
PM25 <- fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25_northeast_0014_matlab.csv")
PM25 [PM25  == -999] <- NA
PM25$datex<-paste(PM25$day,PM25$month,PM25$year,sep="/")
PM25[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]
PM25[,datex:=NULL]
PM25<-PM25[!is.na(PM25)]




#load y2010
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
stage3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2010.rds")
stage3[stage3 == -999] <- NA
s3<-as.data.table(stage3)
rm(stage3)
gc()
s3$datex<-paste(s3$Day,s3$Month,s3$Year,sep="/")
s3[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]
#head(s3)
s3[,Day :=NULL]
s3[,PM25 :=NULL]
s3[,DOY :=NULL]
s3[,datex :=NULL]
setnames(s3,"AOD","aod")
s3$GUID<-as.character(s3$GUID)

#-------> meanPM25  for mod 2+3
#subset PM
PM25.2010<-filter(PM25,year==2010)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2010, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2010 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120100, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2010[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2010f <- PM25.2010[PM25.2010$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2010f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2010f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2010.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2010.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2010[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2010.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2013
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

stage3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2013.rds")
colnames(stage3)<-c("Year","Month","Day","DOY","GUID","Lat","Long","AOD","hpbl","NDVI","dist_PE","pm25stge30_15k","pm25stlt30_3k","pm10stge30_15k","pm10stlt30_3k","noxstge30_15k","noxstlt30_3k","so2stge30_15k","so2stlt30_3k","pcthd_1km","pctpa_1km","pctsh_1km","pctgr_1km","elev_m","Mjrrdden_1km","pctmd_1km","pctld_1km","pctop_1km","pctdf_1km","pctmf_1km","pctev_1km","pctcr_1km","NOXsum","PM10sum","SO2sum","nei05nonpntcntypm25","pcturb_1km","SumOfEMISS","pop_sqkm","nearest_NCDC_stn","slp","visib","wdsp","RH","AH","lat_met","long_met","Temp_C","Dewp_C","PM25","SiteID","mean_pm25_100km")
stage3[stage3 == -999] <- NA
s3<-as.data.table(stage3)
rm(stage3)
gc()
s3$datex<-paste(s3$Day,s3$Month,s3$Year,sep="/")
s3[, day:=as.Date(strptime(datex, "%d/%m/%Y"))]
#head(s3)
s3[,Day :=NULL]
s3[,PM25 :=NULL]
s3[,DOY :=NULL]
s3[,datex :=NULL]
setnames(s3,"AOD","aod")
s3$GUID<-as.character(s3$GUID)

#-------> meanPM25  for mod 2+3
#subset PM
PM25.2013<-filter(PM25,year==2013)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2013, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2013 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120130, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2013[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2013f <- PM25.2013[PM25.2013$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2013f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2013f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2013.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2013.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2013[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2013.rds")
keep(PM25, sure=TRUE) 
gc()


