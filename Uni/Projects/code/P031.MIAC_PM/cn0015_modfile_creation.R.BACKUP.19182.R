###############
#LIBS
###############
<<<<<<< HEAD
=======
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
library(lme4)
>>>>>>> f80d4c21eb27136a106459afdedc33b83c07bb52
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
<<<<<<< HEAD
#library(nlme)
library(lme4)

###########Paths
# set working directory
path.root <- "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/"
setwd(path.root); list.files()


path.keys <- paste(path.root, "FN007_Key_tables/", sep = "")

# clip to bounding coordinates of road network
#dat <- dat[long_aod > -99.5 & long_aod < -98.5 & lat_aod < 19.9 & lat_aod > 19, ]


###############
Imports
##############

lu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
str(met)


#convert date from 01JAN2000 format
met <- met [, date:=as.Date(strptime(date, "%d%b%Y"))]

#add temp in celsius
=======


###############
#FUNCTIONS 
###############


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####for first run only create a ndvi id keytable by sourcing this file
#source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/create ndvi_id.r")
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


###############
#Imports for use of all years
##############

#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
>>>>>>> f80d4c21eb27136a106459afdedc33b83c07bb52
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
<<<<<<< HEAD
describe(met)


aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, date:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)



describe(lu)
# covers all used grid cells
# set -9999 to missing
lu[,reg := NULL]
lu[,FIPS_1 := NULL]

#join Land use
names(lu)
names(aodmod2)
setkey(lu, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, lu, all.x = T)
head(am2.lu)

#clean it
am2.lu <- am2.lu[,guid.y :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]

mod1$dayofyr <- as.numeric(format(mod1$day, "%j"))
am2.lu <- am2.lu[, m := date, ]


ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]

####for first run only create a ndvi id keytable by sourcing this file
#####NEED TO CREATE MIMICING SAS LATER

#add closeltst ndviID

#join Land use
names(ndvi)
names(am2.lu)
setkey(ndvi, lat_aod, long_aod)
setkey(am2.lu, lat_aod, long_aod)
am2.lu <- merge(aodmod2, lu, all.x = T)
head(am2.lu)


=======


#PM
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/all_pm.csv")
pm <- pm[, day:=as.Date(strptime(Date, "%d%b%Y"))]
pm[, c := as.numeric(format(day, "%Y")) ]
#summary(pm)
pm[,c("Date","POC","State","CountyFIPS","CountyName"):=NULL]

#LU

lu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
poplu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/pop_lu.csv")
stack <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/EPA/NEI05_stacks/midatlneweng_nei05stacks.csv" )
setnames(stack,"guid_", "guid")
lc <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/1km_landcover.csv" )
ems <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/LU_emis_EPA.csv" )
setkey(lu, guid)
setkey(stack, guid)
setkey(lc, guid)
setkey(ems, guid)
setkey(poplu, guid)
lux <- merge(lu,stack ,all.x = T)
lux <- merge(lux,lc ,all.x = T)
lux <- merge(lux,ems ,all.x = T)
lux <- merge(lux,poplu ,all.x = T)
#summary(lux)
lux[,State_Abbreviation:=NULL]

#create ID's file
#names(lux)
luxID<-lux[,c(1,3,4,8,17,21,12),with=FALSE]
#create pure Land use file
#names(lux)
PLU <-lux[,c(1,2,3,4,12,18,24:51),with=FALSE]


###############################
#year 2003
###############################


#xtract year met
met2003<- met[c==2003]
#xtract year PM
pm2003<- pm[c==2003]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2003.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2003.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2003 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2003.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2003, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2003, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2003,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2003, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2003_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm , sure=TRUE) 
gc()



###############################
#year 2004
###############################


#xtract year met
met2004<- met[c==2004]
#xtract year PM
pm2004<- pm[c==2004]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2004.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2004.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2004)
#str(am2.lu.nd.pb)
setkey(met2004 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2004 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2004.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2004, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2004, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2004,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2004, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2004.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2004.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2004_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm , sure=TRUE) 
gc()


###############################
#year 2005
###############################


#xtract year met
met2005<- met[c==2005]
#xtract year PM
pm2005<- pm[c==2005]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2005.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2005.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2005)
#str(am2.lu.nd.pb)
setkey(met2005 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2005 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2005.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2005, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2005, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2005,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2005, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2005.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2005.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2005_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm , sure=TRUE) 
gc()


###############################
#year 2006
###############################


#xtract year met
met2006<- met[c==2006]
#xtract year PM
pm2006<- pm[c==2006]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2006.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2006.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2006)
#str(am2.lu.nd.pb)
setkey(met2006 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2006 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2006.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2006, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2006, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2006,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2006, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2006.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2006_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm, makepointsmatrix, sure=TRUE) 
gc()


###############################
#year 2007
###############################


#xtract year met
met2007<- met[c==2007]
#xtract year PM
pm2007<- pm[c==2007]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2007.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2007.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2007)
#str(am2.lu.nd.pb)
setkey(met2007 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2007 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2007.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2007, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2007, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2007,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2007, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2007.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2007.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2007_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm, makepointsmatrix, sure=TRUE) 
gc()


###############################
#year 2008
###############################


#xtract year met
met2008<- met[c==2008]
#xtract year PM
pm2008<- pm[c==2008]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2008.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2008.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2008)
#str(am2.lu.nd.pb)
setkey(met2008 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2008 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2008.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2008, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2008, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2008,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2008, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2008.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2008.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2008_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm, makepointsmatrix, sure=TRUE) 
gc()


###############################
#year 2009
###############################


#xtract year met
met2009<- met[c==2009]
#xtract year PM
pm2009<- pm[c==2009]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2009.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2009.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2009)
#str(am2.lu.nd.pb)
setkey(met2009 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2009 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2009.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2009, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2009, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2009,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2009, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2009.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2009.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2009_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm, makepointsmatrix, sure=TRUE) 
gc()

###############################
#year 2010
###############################


#xtract year met
met2010<- met[c==2010]
#xtract year PM
pm2010<- pm[c==2010]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2010.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2010.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2010)
#str(am2.lu.nd.pb)
setkey(met2010 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2010 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2010.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2010, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2010, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2010,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2010, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2010.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2010.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2010_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm , sure=TRUE) 
gc()


###############################
#year 2011
###############################


#xtract year met
met2011<- met[c==2011]
#xtract year PM
pm2011<- pm[c==2011]


#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2011.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


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
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2011.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2011)
#str(am2.lu.nd.pb)
setkey(met2011 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2011 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa2011.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2011, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2011, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2011,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2011, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2011.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2011.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2011_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm , sure=TRUE) 
gc()
>>>>>>> f80d4c21eb27136a106459afdedc33b83c07bb52




<<<<<<< HEAD
gc()
=======
>>>>>>> f80d4c21eb27136a106459afdedc33b83c07bb52
