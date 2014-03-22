###############
#LIBS
###############
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
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
#Imports for use of all years
##############

#LU


lu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
stack <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/EPA/NEI05_stacks/midatlneweng_nei05stacks.csv" )
setnames(stack,"guid_", "guid")
lc <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/1km_landcover.csv" )
ems <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/LU_emis_EPA.csv" )
setkey(lu, guid)
setkey(stack, guid)
setkey(lc, guid)
setkey(ems, guid)
lux <- merge(lu,stack ,all.x = T)
lux <- merge(lux,lc ,all.x = T)
lux <- merge(lux,ems ,all.x = T)
summary(lux)
lux[,State_Abbreviation:=NULL]

#create ID's file
names(lux)
luxID<-lux[,c(1,3,4,8,17,21),with=FALSE]

#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, date:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(date, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
met [, c := as.numeric(format(date, "%Y")) ]

#PM
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/all_pm.csv")
pm <- pm[, date:=as.Date(strptime(Date, "%d%b%Y"))]
pm[, c := as.numeric(format(date, "%Y")) ]
summary(pm)
pm[,c("Date","POC","State","CountyFIPS","CountyName"):=NULL]


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
aodmod2 <- aodmod2 [, date:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins
########################


#join Land use
setkey(lux, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, lux, all.x = T)
head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y","date.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
setnames(am2.lu,"date.x","date")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(date, "%m")) ]

####clean a bit
gc()


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]

####for first run only create a ndvi id keytable by sourcing this file
#/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/create ndvi_id.r
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
summary(am2.lu.nd$NDVI)


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2003.dbf"))
str(pbl)
pbl[, date := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, date:=as.Date(strptime(date, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
str(pbl)
str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , date, pblid)
setkey(am2.lu.nd, date, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]

saveRDS(am2.lu.nd.pb, "/home/zeltak/ZH_tmp/pa.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")


###met
str(met2003)
str(am2.lu.nd.pb)
setkey(met2003 , date, stn)
setkey(am2.lu.nd.pb, date, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2003 , all.x = T)
summary(am2.lu.nd.pb.met)




# need functions from LUR_code_snippets.R
if(!exists("nearestbyday")) warning("load nearestbyday - I know, not elegant!")

#create matirx and convert numeric ID to character ID
#met2003
# all.metm <- makepointsmatrix(met2003, "long_met", "lat_met", "stn")
# met2003[, day := date]
# met2003[, stnc := as.character(stn)]
# met2003[, stn := NULL]
# setnames(met2003,"stnc", "stn")
# str(all.metm)
# 
# #grid
# aodidm <- makepointsmatrix(am2.lu.nd.pb, "long_aod", "lat_aod", "guid")
# m2g<-copy(am2.lu.nd.pb)
# m2g[, guidc := as.character(guid)]
# m2g[, guid := NULL]
# m2g[, day := date]
# setnames(m2g,"guidc", "guid")
# 
# #9:11-10:11 takes 1 hour
# # use the nearestbyday() function to spatial merge
# closesttemp <- nearestbyday(aodidm , all.metm, 
#                             m2g, met2003[,c("day", "stn", "tempc"), with = F], 
#                             "guid", "stn", "closesttemp", "tempc", knearest = 2)
# 

#closesttempknn-which nearest neigbour rdid it take
#closesttempnobs-number of obsv that were avilable

###########
# import monitor data and spatial merge with nearestbyday()

#create PM matrix
pm2003[,day:=as.Date(date)]
pm.m <- makepointsmatrix(pm2003, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb)
m2g[, guidc := as.character(guid)]
m2g[, guid := NULL]
m2g[, day := date]
setnames(m2g,"guidc", "guid")
names(m2gx)
m2gx<-m2g[,c(55,6,5,54,8,1),with=FALSE]
#need to sort
setkey(m2gx, guid)
mod2.m <- makepointsmatrix(m2gx[m2gx[,unique(guid)], list(long_aod, lat_aod, guid), mult = "first"], "long_aod", "lat_aod", "guid")
str(m2gx)

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2003, m2gx [, list(day, guid, aod)], 
                           "SiteCode", "guid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge
setkey(pm2003,SiteCode,day)
setkey(closestaod,SiteCode,day)

mod1 <- merge(pm2003, closestaod, all.x = T)
head(mod1)
mod1 <- mod1[aod != "NA"]

m1_2003<-mod1

#clean data and exclude bad values
m1_2003 <- m1_2003[aod < 1.4 & NDVI < 1 , ]
m1_2003$logroad<-log(m1_2003$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|date/region))

#full model 1
out.m1_2003 = lmer(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|date/region),data =  m1_2003)
#generate prediction
m1_2003$predicted <- predict(out.m1_2003)
#get overall R2
mod1_reg <- lm(m1_2003$PM25~m1_2003$predicted)

mod1table$r2003[1] <-summary(mod1_reg)$r.squared





