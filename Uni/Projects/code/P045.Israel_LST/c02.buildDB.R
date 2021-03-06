#load libraries 
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#function to join on both space and time
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")

#import clipped grid
fullgrid<-fread("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/gridXY_IL.csv")

#load met
Temp<-fread("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/all_stations2.csv")
Temp<-filter(Temp,stn != "NA")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("V1","date","serial number","start"):=NULL]
Temp<-filter(Temp,c != "NA")
Temp<-filter(Temp, itm_e != "NA")
#summary(Temp)


# try2 <- Temp[is.na(day)]
# head(try2)
# #create full grid
# try3 <-try2 %>%
#     group_by(stn) %>%
#      summarise(data = n())

#load LST data
aqua.2003<-readRDS("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/lst.AQ.2003.rds")
aqua.2003<-as.data.table(aqua.2003)
# #get rid of dplyr tbl_df until bug gets fixed
# aqua.2003<-as.data.frame(aqua.2003)
# aqua.2003<-as.data.table(aqua.2003)

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
days2003 <- data.table(expand.grid(lstid = fullgrid[, unique(lstid)], day = days))
#merge
setkey(aqua.2003,lstid,day)
setkey(days2003 ,lstid,day)
db2003 <- merge(days2003,aqua.2003, all.x = T)

#subset fgird, take out unwanted variables/columns
fullgrid<-select(fullgrid,lstid ,  stn,   itm_e ,     itm_n ,   ELEVATION,  ASPECT  ,   roadden ,   DENS_POP,   ndviid  ,   long_ndvi , lat_ndvi  )

#######spatial 
#bring in all spatial components
    #merge
    setkey(db2003,lstid)
    setkey(fullgrid ,lstid)
    db2003 <- merge(db2003,fullgrid, all.x = T)  
gc()
head(db2003)

#add month
db2003[, m := as.numeric(format(day, "%m")) ]
#add season
#1-winter, 2-spring,3-summer,4-autum
db2003$season<-recode(db2003$m,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
db2003$seasonSW<-recode(db2003$m,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")


#join NDVI to lst
fin.ndvi<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")
fin.ndvi<-filter(fin.ndvi,c==2003)
#add ndvi
setkey(db2003,ndviid,m)
setkey(fin.ndvi,ndviid,m)
db2003 <- merge(db2003, fin.ndvi[,list(ndviid,ndvi,m)], all.x = T)
gc()
#summary(db2003)


# #fix ITM data
# db2003$itm_e <- gsub(",", "",db2003$itm_e)
# db2003$itm_e<-as.numeric(db2003$itm_e)
# db2003$itm_n <- gsub(",", "",db2003$itm_n)
# db2003$itm_n<-as.numeric(db2003$itm_n)


#################
# need to subset the datasets to day and night datasets!
#########

#Temp
Temp2003<-filter(Temp,c==2003)
temp2003tc<-select(Temp2003,stn,tempcmean,lat_stn=  lat ,long_stn=  long,day)
temp2003tc<-na.omit(temp2003tc)
temp2003tc$stn<-as.character(temp2003tc$stn)

#spatio temporal join
#matrix for temperature 
met.m <- makepointsmatrix(temp2003tc, "long_stn", "lat_stn", "stn")
setkey(db2003, lstid)
lu.m <- makepointsmatrix(db2003[db2003[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")

#runthescript

closestaodse<- nearestbyday(lu.m ,met.m , 
                            db2003, temp2003tc[, list(day,tempcmean,stn)], 
                            "lstid", "stn", "meanT", "tempcmean", knearest = 7, maxdistance = 50000)


setkey(db2003,lstid,day)
setkey(closestaodse,lstid,day)
db2003 <- merge(db2003, closestaodse[,list(day,tempcmean,lstid)], all.x = T)




#rhmean
Temp2003<-filter(Temp,c==2003)
temp2003tc<-select(Temp2003,stn,rhmean,lat_stn=  lat ,long_stn=  long,day)
temp2003tc<-na.omit(temp2003tc)
temp2003tc$stn<-as.character(temp2003tc$stn)

#spatio temporal join
#matrix for temperature 
met.m <- makepointsmatrix(temp2003tc, "long_stn", "lat_stn", "stn")
setkey(db2003, lstid)
lu.m <- makepointsmatrix(db2003[db2003[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")

#runthescript

closestaodse<- nearestbyday(lu.m ,met.m , 
                            db2003, temp2003tc[, list(day,rhmean,stn)], 
                            "lstid", "stn", "meanT", "rhmean", knearest = 7, maxdistance = 50000)


setkey(db2003,lstid,day)
setkey(closestaodse,lstid,day)
db2003 <- merge(db2003, closestaodse[,list(day,rhmean,lstid)], all.x = T)



#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD, tmin, tmax
#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD
#####ADDD WS AND WD


#Temp
Temp2003<-filter(Temp,c==2003)
temp2003tc<-select(Temp2003,stn,tempcmean,lat_stn=  lat ,long_stn=  long,day)
temp2003tc<-na.omit(temp2003tc)
temp2003tc$stn<-as.character(temp2003tc$stn)


#-------> mean Ta  for mod 2+3
#spatio temporal join
#matrix for temperature 
met.m <- makepointsmatrix(temp2003tc, "long_stn", "lat_stn", "stn")
setkey(db2003, lstid)
lu.m <- makepointsmatrix(db2003[db2003[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")


closestmta<- nearestbyday(lu.m ,met.m , 
                            db2003, temp2003tc[, list(day,tempcmean,stn)], 
                            "lstid", "stn", "stn.near", "tempcmean", knearest = 7, maxdistance = 50000, nearestmean = T)

#join to DB
setkey(db2003,lstid,day)
setkey(closestmta,lstid,day)
db2003 <- merge(db2003, closestmta[,list(day,stn.nearmean,lstid)], all.x = T)
setnames(db2003,"stn.nearmean","meanTa")
gc()


#save
gc()
saveRDS(db2003,"/PATH/mod3.AQ.2003.rds")
gc()

# # take out missing night LST >>> mod3 night
# # take out missing day LST >>> mod3 night
# #create mod 2 file
# db2003.m2 <- db2003[!is.na(lst)]
# #rm m3
# rm(db2003)
# gc()
# #save mod2
# saveRDS(db2003.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
# gc()
# 
# #--------->mod1
# #Ta
# #to fix missing days issues resulting in cartesean error
# db2003days <- sort(unique(db2003.m2$day))
# 
# #Ta import again
# Ta<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/Ta.rds")
# Ta<-filter(Ta,c==2003)
# Ta10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/Ta10.rds")
# Ta10<-filter(Ta10,c==2003)
# 
# ########### join lst to Ta
# #create Ta matrix
# Ta.m <- makepointsmatrix(Ta, "long_Ta", "lat_Ta", "stn")
# #create lst terra matrix
# db2003.m2$lstid<-as.character(db2003.m2$lstid)
# setkey(db2003.m2,lstid)
# lst.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")
# 
# 
# 
# #run function
# closestlst <- nearestbyday(Ta.m, lst.m, 
#                            Ta[day %in% db2003days,], db2003.m2, 
#                            "stn", "lstid", "closest", "lst", knearest = 9, maxdistance = 1500)
# 
# 
# #closestlst[,i.stn :=NULL]
# closestlst[,closestknn :=NULL]
# 
# setkey(Ta,stn,day)
# setkey(closestlst,stn,day)
# Ta.m1 <- merge(Ta, closestlst, all.x = T)
# Ta.m1<-Ta.m1[!is.na(lst)]
# 
# #save mod 1
# saveRDS(Ta.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.Ta.rds")
# 
# ########### join lst to Ta10
# #create Ta matrix
# Ta.m <- makepointsmatrix(Ta10, "long_Ta10", "lat_Ta10", "stn")
# #create lst terra matrix
# db2003.m2$lstid<-as.character(db2003.m2$lstid)
# setkey(db2003.m2,lstid)
# lst.m <- makepointsmatrix(db2003.m2[db2003.m2[,unique(lstid)], list(long_lst, lat_lst, lstid), mult = "first"], "long_lst", "lat_lst", "lstid")
# 
# #run function
# closestlst <- nearestbyday(Ta.m, lst.m, 
#                            Ta10[day %in% db2003days,], db2003.m2, 
#                            "stn", "lstid", "closest", "lst", knearest = 9, maxdistance = 1500)
# 
# 
# #closestlst[,i.stn :=NULL]
# closestlst[,closestknn :=NULL]
# 
# setkey(Ta10,stn,day)
# setkey(closestlst,stn,day)
# Ta10.m1 <- merge(Ta10, closestlst, all.x = T)
# Ta10.m1<-Ta10.m1[!is.na(lst)]
# 
# #save mod 1
# saveRDS(Ta10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.Ta10.rds")
# 
# #cleanup
# keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
# gc()
