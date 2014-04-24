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
library(car)
library(dplyr)


###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(model=character(40), r2002=numeric(40), r2003=numeric(40),
                        r2004=numeric(40),r2005=numeric(40),
                        r2006=numeric(40),r2007=numeric(40),
                        r2008=numeric(40),r2009=numeric(40),
                        r2010=numeric(40),r2011=numeric(40),
                        r2012=numeric(40), r2013=numeric(40),mean=numeric(40))

#name columns

mod1table$model<- c("allyears","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
                   "mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE",
                   "mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial",
                   "mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE",
                   "mod1CVLPM_Slope","mod1CVLPM_Slope_SE","mod1CVLPM_RMSPE",
                   "mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial",
                   "mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int",
                   "mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE",
                   "mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
                   "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                   "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")

mod1table$model[1] <-"allyears_Pm25"

#EDA PM25
#import all terra aod
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyears.RDS")

######REPORT 1
#raw correlaction of aod and PM stations for 2 years

summary(lm(PM25~aod,data=mod1PM25_2012))$r.squared
#Multiple R-squared:  0.004063
summary(lm(PM25~aod,data=mod1PM25_2002))
#Multiple R-squared:  0.006503
summary(lm(PM25~aod,data=mod1PM25_2004))
#Multiple R-squared:  0.009647
summary(lm(PM25~aod,data=mod1PM25_2007))
#Multiple R-squared:  0.03155


######REPORT 1
#raw correlaction of aod and PM stations for 2 years

summary(lm(PM10~aod,data=mod1PM10_2012))
#Multiple R-squared:  0.004063
summary(lm(PM10~aod,data=mod1PM10_2002))
#Multiple R-squared:  0.006503
summary(lm(PM10~aod,data=mod1PM10_2004))
#Multiple R-squared:  0.009647
summary(lm(PM10~aod,data=mod1PM10_2007))
#Multiple R-squared:  0.03155

### by season
m1.formula <- as.formula(PM25~aod)

#2002
seas2002<- mod1PM25_2002 %.% group_by(season) %.% do(function(df){summary(lm(m1.formula,data=df))})
names(seas2002)<-c("1-winter", "2-spring","3-summer","4-autum")
seas2002[["1-winter"]][8]
seas2002[["2-spring"]][8]
seas2002[["3-summer"]][8]
seas2002[["4-autum"]][8]

#2007
seas2007<- mod1PM25_2007 %.% group_by(season) %.% do(function(df){summary(lm(m1.formula,data=df))})
names(seas2007)<-c("1-winter", "2-spring","3-summer","4-autum")
seas2007[["1-winter"]][8]
seas2007[["2-spring"]][8]
seas2007[["3-summer"]][8]
seas2007[["4-autum"]][8]

#2004
seas2004<- mod1PM25_2004 %.% group_by(season) %.% do(function(df){summary(lm(m1.formula,data=df))})
names(seas2004)<-c("1-winter", "2-spring","3-summer","4-autum")
seas2004[["1-winter"]][8]
seas2004[["2-spring"]][8]
seas2004[["3-summer"]][8]
seas2004[["4-autum"]][8]

#2012
seas2012<- mod1PM25_2012 %.% group_by(season) %.% do(function(df){summary(lm(m1.formula,data=df))})
names(seas2012)<-c("1-winter", "2-spring","3-summer","4-autum")
seas2012[["1-winter"]][8]
seas2012[["2-spring"]][8]
seas2012[["3-summer"]][8]
seas2012[["4-autum"]][8]


##### run PM25 per station
mod1PM25_2012[,unique(stn)]

mod1PM25_2002[,]

#2012
stn2002<- mod1PM25_2002 %.% group_by(stn) %.% do(function(df){summary(lm(m1.formula,data=df))})
seas2002[[1]][8]
seas2002[[2]][8]
seas2002[[3]][8]
seas2002[[4]][8]
seas2002[[5]][8]









##########Report 2
#day proportions
# summary on terra
terra[, meanaod := mean(aod, na.rm = T), by=aodid]
# particular grid cell how many times was it obbserved
terra[, naod := sum(!is.na(aod)), by=aodid]
# proportion of day missing
numaodid <- terra[, length(unique(aodid))]
numaodid
# for each day what is the number of observed aod measures
terra[, daypropcloud := sum(!is.na(aod)) / numaodid, by=day]
terra[!is.na(aod), list(naod = .N), by = "aodid"][, describe(naod)] # we have some aod in 5924 of 5925 aodids (with elevation)
# indicator for whether it was observed
terra[, obs := as.numeric(!is.na(aod))]

#get the total unique AOD grids
terra[, length(unique(aodid))]
#describe the missing precentage by day
#.N= number of results
describe(terra[, .N/ 23630, by=day])




#############
######REPORT 3
#aeronet
#############

#import 12 closest aod stations to aeronet nes tziona
aodiderolist<-fread("/home/zeltak/ZH_tmp/nes_tzionaXY_aod.csv")


#aeronet 2007-8
aero<-fread("/media/NAS/Uni/Data/Israel/aeronet/2007_2008NeszionaL2_clean.csv")
aero[, day := as.Date(strptime(date, "%d/%m/%Y"))]


#subset terra to aot points
terra.aero<- terra [terra$aodid %in% aodiderolist$aodid, ] 
terra.aero<- terra.aero[yr > "2006"]
terra.aero<- terra.aero[yr < "2009"]
setkey(aero,day)
setkey(terra.aero,day)
temp1 <- merge(aero,terra.aero[,list(aodid,x_aod_ITM, y_aod_ITM,day,aod)], all.x = T)
summary(lm(aod~aot440,data=temp1))
#Multiple R-squared:  0.5775


#join to aot440 2012
#aeronet 2012
aero12<-fread("/media/NAS/Uni/Data/Israel/aeronet/120101_121231_Nes_Ziona.lev20")
setnames(aero12, names(aero12)[1], c("aerodate"))
aero12[, day := as.Date(strptime(aerodate, "%d:%m:%Y"))]
terra.aero<- terra [terra$aodid %in% aodiderolist$aodid, ] 
terra.aero<- terra.aero[yr == "2012"]
setkey(aero12,day)
setkey(terra.aero,day)
temp2 <- merge(aero12[,list(day,AOT_440,AOT_500)],terra.aero[,list(aodid,x_aod_ITM, y_aod_ITM,day,aod)], all.x = T)
summary(lm(aod~AOT_440,data=temp2))
#Multiple R-squared:  0.214


#join to aot440 2002
#aeronet 2002
aero02<-fread("/media/NAS/Uni/Data/Israel/aeronet/020101_021231_Nes_Ziona.lev20")
setnames(aero02, names(aero02)[1], c("aerodate"))
aero02[, day := as.Date(strptime(aerodate, "%d:%m:%Y"))]
terra.aero<- terra [terra$aodid %in% aodiderolist$aodid, ] 
terra.aero<- terra.aero[yr == "2002"]
setkey(aero02,day)
setkey(terra.aero,day)
temp02 <- merge(aero02[,list(day,AOT_440,AOT_500)],terra.aero[,list(aodid,x_aod_ITM, y_aod_ITM,day,aod)], all.x = T)
summary(lm(aod~AOT_440,data=temp02))
#Multiple R-squared:  0.1539

#join to aot440 2004
#aeronet 2004
aero04<-fread("/media/NAS/Uni/Data/Israel/aeronet/040101_041231_Nes_Ziona.lev20")
setnames(aero04, names(aero04)[1], c("aerodate"))
aero04[, day := as.Date(strptime(aerodate, "%d:%m:%Y"))]
terra.aero<- terra [terra$aodid %in% aodiderolist$aodid, ] 
terra.aero<- terra.aero[yr == "2004"]
setkey(aero04,day)
setkey(terra.aero,day)
temp04 <- merge(aero04[,list(day,AOT_440,AOT_500)],terra.aero[,list(aodid,x_aod_ITM, y_aod_ITM,day,aod)], all.x = T)
summary(lm(aod~AOT_440,data=temp04))
#Multiple R-squared:  0.3453





#EDA PM10


#pm10
pm10all<-readRDS("/home/zeltak/ZH_tmp/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all.RDS")
summary(pm10all)
pm10all[,DOW:=NULL]
pm10all[,Holiday:=NULL]
pm10all[,PM25:=NULL]
pm10all[,RH:=NULL]
pm10all[,WD:=NULL]
pm10all[,Temp:=NULL]
pm10all[,SO2:=NULL]
pm10all[,NO2:=NULL]
pm10all[,WS:=NULL]

pm10all<-na.omit(pm10all)
#clean
#pm10all<-pm10all[pm10 <= 500]
describe(pm10all$pm10)
summary(pm10all)



# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm10all, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm10all, terra [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm10all,stn,day)
setkey(closestaod,stn,day)
mod1pm10 <- merge(pm10all, closestaod, all.x = T)
#head(mod1)
mod1pm10 <- mod1pm10[aod != "NA"]
summary(mod1pm10)


mod1pm10_2002 <- mod1pm10[c == "2002"]
mod1pm10_2003 <- mod1pm10[c == "2003"]
mod1pm10_2004 <- mod1pm10[c == "2004"]
mod1pm10_2005 <- mod1pm10[c == "2005"]
mod1pm10_2006 <- mod1pm10[c == "2006"]
mod1pm10_2007 <- mod1pm10[c == "2007"]
mod1pm10_2008 <- mod1pm10[c == "2008"]
mod1pm10_2009 <- mod1pm10[c == "2009"]
mod1pm10_2010 <- mod1pm10[c == "2010"]
mod1pm10_2011 <- mod1pm10[c == "2011"]
mod1pm10_2012 <- mod1pm10[c == "2012"]





######REPORT 1
#raw correlaction of aod and PM stations for 2 years

summary(lm(PM10~aod,data=mod1pm10_2012))
#0.005431
summary(lm(PM10~aod,data=mod1pm10_2002))
#0.005326
summary(lm(PM10~aod,data=mod1pm10_2004))
#0.01678
summary(lm(PM10~aod,data=mod1pm10_2007))
# 0.06286


















































# describe aod
terra[, describe(aod)]
#simple histogram
ggplot(terra[, list(aod)], aes(aod)) + geom_histogram(binwidth = 0.01) + theme_bw(16)


# remove values that are too high to tease apart anthropogenic contribution
# MAY REVISIT IN THE FUTURE
# 962 point-days with this very high aod
#terra[aod <= 1.5, describe(cloud)]
# cloud
# n missing unique Mean
# 6247929 0 4 14.89
#
# 1 (47600, 1%), 3 (3933, 0%), 6 (397, 0%), 15 (6195999, 99%)
#terra <- terra[aod <= 1.5]

# this confirms there are aodIDs that are completely missing aod for a given year
terra[, yr := format(day, "%Y")]
terra[,list(nobsmissing = 5925 - length(unique(aodid))),by="yr"]

# pull out 2011 for checking
if(0){
  terra2011 <- terra[yr == "2011"]
  # plot out number of measures
  ggplot(terra[yr == 2004,list(nobs = .N, meanaod = mean(aod, na.rm = T),
                             long_aod = long_aod[1], lat_aod = lat_aod[1]),by=aodid], aes(long_aod, lat_aod, color = nobs)) +
    geom_point(size = 3.5, shape = 15)
}
