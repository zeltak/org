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



#EDA

#import all terra aod
terra<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_terraa/FN003_aod_allyears/aod_allyears.RDS")

######REPORT 1
#raw correlaction of aod and PM stations for 2 years

summary(lm(PM25~aod,data=mod1PM25_2012))
#Multiple R-squared:  0.004063
summary(lm(PM25~aod,data=mod1PM25_2002))
#Multiple R-squared:  0.006503
summary(lm(PM25~aod,data=mod1PM25_2004))
#Multiple R-squared:  0.009647
summary(lm(PM25~aod,data=mod1PM25_2007))
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





#EDA


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
