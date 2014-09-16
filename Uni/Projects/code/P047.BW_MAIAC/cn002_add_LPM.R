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

cases<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy0308_guid_lpmid.csv")

############### devide to sets
#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = cases[, unique(lpmid)], day = days_2003))
setkey(test3.se,lpmid)
setkey(cases,lpmid)

#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365

test4.se<- merge(test3.se,cases,allow.cartesian=TRUE)


test4.se$m <- as.numeric(format(test4.se$day, "%m"))



##########################################################################3
#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
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

saveRDS(test4.se.ndv.pb.met,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2003.rds")
