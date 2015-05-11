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


###########2003


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
    m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
  }
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2003))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2003<- met[c==2003]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2003.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2003 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2003.rds")




###########2004


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2004.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2004<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2004))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2004<- met[c==2004]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2004.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2004.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2004)
#str(am2.lu.nd.pb)
setkey(met2004 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2004 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2004.rds")




###########2005


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2005.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2005<-seq.Date(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2005))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2005<- met[c==2005]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2005.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2005.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2005)
#str(am2.lu.nd.pb)
setkey(met2005 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2005 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2005.rds")




###########2006


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2006.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2006<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2006))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2006<- met[c==2006]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2006.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2006.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2006)
#str(am2.lu.nd.pb)
setkey(met2006 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2006 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2006.rds")




###########2007


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2007.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2007<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2007))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2007<- met[c==2007]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2007.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2007.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2007)
#str(am2.lu.nd.pb)
setkey(met2007 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2007 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2007.rds")




###########2008


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2008.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2008<-seq.Date(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2008))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2008<- met[c==2008]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2008.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2008.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2008)
#str(am2.lu.nd.pb)
setkey(met2008 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2008 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2008.rds")




###########2009


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2009.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2009<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2009))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2009<- met[c==2009]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2009.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2009.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2009)
#str(am2.lu.nd.pb)
setkey(met2009 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2009 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2009.rds")




###########2010


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2010.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2010<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2010))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2010<- met[c==2010]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2010.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2010.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2010)
#str(am2.lu.nd.pb)
setkey(met2010 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2010 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2010.rds")




###########2011


monlpm<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_lpmid.csv")
test2<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")


setkey(monlpm,lpmid)
setkey(test2,lpmid)
test2<- merge(monlpm,test2,all.x=TRUE)

#since we subsetted the data to only NE+NY we need to get rid of monitors outside of clipped study area
if(!exists("m2_agg")){
  m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2011.rds")
}
test2.se <- test2[test2$guid %in% m2_agg$guid, ] 


#create full TS
days_2011<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = test2.se[, unique(lpmid)], day = days_2011))


setkey(test3.se,lpmid)
setkey(test2.se,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,test2.se,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2011<- met[c==2011]


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2011.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2011.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2011)
#str(am2.lu.nd.pb)
setkey(met2011 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2011 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

saveRDS(test4.se.ndv.pb.met,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2011.rds")




