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



#load y2000
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2000.rdata")
stage3_2000[stage3_2000 == -999] <- NA
s3<-as.data.table(stage3_2000)
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
PM25.2000<-filter(PM25,year==2000)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2000, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2000 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120000, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2000[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2000f <- PM25.2000[PM25.2000$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2000f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2000f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2000.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2000.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2000[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2000.rds")
keep(PM25, sure=TRUE) 
gc()


























#load y2001
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2001.rdata")
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
PM25.2001<-filter(PM25,year==2001)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2001, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2001 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120010, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2001[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2001f <- PM25.2001[PM25.2001$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2001f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2001f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2001.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2001.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2001[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2001.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2002
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2002.rdata")
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
PM25.2002<-filter(PM25,year==2002)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2002, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2002 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120020, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2002[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2002f <- PM25.2002[PM25.2002$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2002f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2002f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2002.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2002.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2002[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2002.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2003
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2003.rdata")
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
PM25.2003<-filter(PM25,year==2003)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2003, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2003 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120030, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2003[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2003f <- PM25.2003[PM25.2003$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2003f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2003f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2003.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2003.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2003[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2003.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2004
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2004.rdata")
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
PM25.2004<-filter(PM25,year==2004)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2004, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2004 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120040, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2004[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2004f <- PM25.2004[PM25.2004$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2004f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2004f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2004.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2004.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2004[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2004.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2005
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2005.rdata")
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
PM25.2005<-filter(PM25,year==2005)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2005, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2005 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120050, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2005[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2005f <- PM25.2005[PM25.2005$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2005f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2005f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2005.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2005.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2005[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2005.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2006
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2006.rdata")
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
PM25.2006<-filter(PM25,year==2006)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2006, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2006 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120060, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2006[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2006f <- PM25.2006[PM25.2006$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2006f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2006f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2006.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2006.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2006[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2006.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2007
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2007.rdata")
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
PM25.2007<-filter(PM25,year==2007)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2007, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2007 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120070, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2007[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2007f <- PM25.2007[PM25.2007$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2007f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2007f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2007.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2007.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2007[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2007.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2008
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2008.rdata")
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
PM25.2008<-filter(PM25,year==2008)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2008, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2008 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120080, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2008[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2008f <- PM25.2008[PM25.2008$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2008f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2008f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2008.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2008.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2008[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2008.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2009
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2009.rdata")
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
PM25.2009<-filter(PM25,year==2009)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2009, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2009 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120090, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2009[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2009f <- PM25.2009[PM25.2009$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2009f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2009f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2009.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2009.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2009[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2009.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2010
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2010.rdata")
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




#load y2011
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2011.rdata")
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
PM25.2011<-filter(PM25,year==2011)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2011, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2011 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120110, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2011[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2011f <- PM25.2011[PM25.2011$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2011f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2011f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2011.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2011.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2011[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2011.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2012
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2012.rdata")
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
PM25.2012<-filter(PM25,year==2012)


########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2012, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2012 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120120, nearestmean= TRUE)

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
table_temp<-table_temp[V1 > 364]
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

setkey(PM25,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25[,list(SiteCode,day,PM25)], closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2012.rds")
keep(PM25, sure=TRUE) 
gc()




#load y2013
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2013.rdata")
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




#load y2014
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")

x<-load("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/combined_stage3_2014.rdata")
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
s3<-filter(s3,day < "2014-07-01")


#-------> meanPM25  for mod 2+3
#subset PM
PM25.2014<-filter(PM25,year==2014)
PM25.2014<-filter(PM25.2014,day < "2014-07-01")

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25.2014, "Long_PM", "Lat_PM", "SiteCode")
setkey(s3,GUID)
aod.m3 <- makepointsmatrix(s3[s3[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj1<- nearestbyday(aod.m3  ,pm.m , 
                            s3, PM25.2014 [, list(day,PM25,SiteCode)], 
                            "GUID", "SiteCode", "closest","PM25",knearest = 15, maxdistance = 120140, nearestmean= TRUE)

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
table_temp<-as.data.table(ddply(na.omit(PM25.2014[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
PM25.2014f <- PM25.2014[PM25.2014$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.mx <- makepointsmatrix(PM25.2014f, "Long_PM", "Lat_PM", "SiteCode")
setkey(m6_NA,GUID)
aod.m3x <- makepointsmatrix(m6_NA[m6_NA[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")

pmj2<- nearestbyday(aod.m3x ,pm.mx , 
                            m6_NA, PM25.2014f [, list(day,PM25,SiteCode)], 
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
saveRDS(s3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2014.rds")
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
saveRDS(s2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2014.rds")
gc()


#--------->mod1
#to fix missing days issues resulting in cartesean error
m9days <- sort(unique(s2$day))
#create aod terra matrix
setkey(s2,GUID)
aod.m <- makepointsmatrix(s2[s2[,unique(GUID)], list(Long, Lat, GUID), mult = "first"], "Long", "Lat", "GUID")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25.2014[day %in% m9days,], s2, 
                           "SiteCode", "GUID", "closest", "aod", knearest = 6, maxdistance = 1500)
closestaod<-filter(closestaod,!is.na(Lat))
setkey(PM25.2014,SiteCode,day)
setkey(closestaod,SiteCode,day)
PM25.m1 <- merge(PM25.2014[,list(SiteCode,day,PM25)], closestaod, all.x = T, allow.cartesian=T)
PM25.m1<-PM25.m1[!is.na(aod)]
#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2014.rds")
keep(PM25, sure=TRUE) 
gc()




