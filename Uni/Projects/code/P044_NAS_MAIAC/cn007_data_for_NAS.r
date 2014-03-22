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

m4.formula<-as.formula(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl)+s(tden,WDSP)+s(tden,tempc)+ah_gm3+s(tden,visib))

##############################################
##############################################
#NAS 1x1km data
##############################################

nasPM<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN002_Exposure/nasPM.rds")

add<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_guid_2014.csv")
str(add)
l=seq(names(add));names(l)=names(add);l
add2<-add[,c("guid","nasid", "AddID", "AddStartDate", "AddEndDate" ,"SeasonStartMos", "SeasonEndMos"),with=FALSE]

add2$xstart<-substr(add2$AddStartDate, 1, 10)
add2$xend<-substr(add2$AddEndDate, 1, 10)
add2 <- add2[,daystart:=as.Date(strptime(xstart, "%d/%m/%Y"))]
add2 <- add2[,dayend:=as.Date(strptime(xend, "%d/%m/%Y"))]
add2[,xstart:=NULL]
add2[,xend:=NULL]
add2[,AddStartDate:=NULL]
add2[,AddEndDate:=NULL]

############send to 
write.csv(nasPM,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN008_LPM/NASpm.csv")
write.csv(add2,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN008_LPM/NASadd.csv")









##############################################
##############################################
#NAS LPM
##############################################



addlpmid<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_albertsXY_LPMID_2014.csv")

#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = addlpmid[, unique(lpmid)], day = days_2003))


setkey(test3.se,lpmid)
setkey(addlpmid,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,addlpmid,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))
test4.se$c <- as.numeric(format(test4.se$day, "%Y"))




##########################################################################3
######2003
#create yearly LPM files
test4.se2003<- test4.se[c==2003]



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
setkey(test4.se2003, m, ndviid)
test4.se.ndv <- merge(test4.se2003, ndvi, all.x = T)
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


###################
############# GET LPM pred 2003

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2003<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2003,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2003.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2004
#create yearly LPM files
test4.se2004<- test4.se[c==2004]



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
setkey(test4.se2004, m, ndviid)
test4.se.ndv <- merge(test4.se2004, ndvi, all.x = T)
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


###################
############# GET LPM pred 2004

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2004_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2004<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2004,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2004.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2005
#create yearly LPM files
test4.se2005<- test4.se[c==2005]



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
setkey(test4.se2005, m, ndviid)
test4.se.ndv <- merge(test4.se2005, ndvi, all.x = T)
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


###################
############# GET LPM pred 2005

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2005_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2005<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2005,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2005.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)

##########################################################################3
######2006
#create yearly LPM files
test4.se2006<- test4.se[c==2006]



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
setkey(test4.se2006, m, ndviid)
test4.se.ndv <- merge(test4.se2006, ndvi, all.x = T)
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


###################
############# GET LPM pred 2006

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2006_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2006<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2006,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2006.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)




##########################################################################3
######2007
#create yearly LPM files
test4.se2007<- test4.se[c==2007]



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
setkey(test4.se2007, m, ndviid)
test4.se.ndv <- merge(test4.se2007, ndvi, all.x = T)
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


###################
############# GET LPM pred 2007

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2007_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2007<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2007,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2007.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2008
#create yearly LPM files
test4.se2008<- test4.se[c==2008]



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
setkey(test4.se2008, m, ndviid)
test4.se.ndv <- merge(test4.se2008, ndvi, all.x = T)
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


###################
############# GET LPM pred 2008

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2008_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2008<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2008,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2008.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2009
#create yearly LPM files
test4.se2009<- test4.se[c==2009]



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
setkey(test4.se2009, m, ndviid)
test4.se.ndv <- merge(test4.se2009, ndvi, all.x = T)
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


###################
############# GET LPM pred 2009

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2009_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2009<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2009,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2009.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2010
#create yearly LPM files
test4.se2010<- test4.se[c==2010]



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
setkey(test4.se2010, m, ndviid)
test4.se.ndv <- merge(test4.se2010, ndvi, all.x = T)
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


###################
############# GET LPM pred 2010

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2010_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2010<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2010,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2010.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)


##########################################################################3
######2011
#create yearly LPM files
test4.se2011<- test4.se[c==2011]



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
setkey(test4.se2011, m, ndviid)
test4.se.ndv <- merge(test4.se2011, ndvi, all.x = T)
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


###################
############# GET LPM pred 2011

mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2011_st.rds")

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)
#l=seq(names(test4.se.ndv.pb.met));names(l)=names(test4.se.ndv.pb.met);l
save2011<-test4.se.ndv.pb.met[,c(1,6,10:16,48),with=FALSE]
saveRDS(save2011,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2011.rds")
keep("test4.se", "addlpmid","m4.formula",sure=TRUE)

##bind all years
save2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2003.rds")
save2004<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2004.rds")
save2005<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2005.rds")
save2006<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2006.rds")
save2007<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2007.rds")
save2008<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2008.rds")
save2009<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2009.rds")
save2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2010.rds")
save2011<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN000_workdir/NASpmST2011.rds")

allylpm<-rbind(save2003,save2004,save2005,save2006,save2007,save2008,save2009,save2010,save2011)

hist(allylpm$lpm)

#final save
write.csv(allylpm, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN008_LPM/allyearslpm.csv")


