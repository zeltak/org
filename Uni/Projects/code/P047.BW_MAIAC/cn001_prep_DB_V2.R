#Prediction
library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
#library(rgdal)


#######################################
# for 1x1 grid
#######################################

loc<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN003_BW_data/bwfull.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(loc));names(l)=names(loc);
l
str(loc$FIPS)
head(loc,n=3)
locxy<-loc[,c("lat","long","uniqueid_y"),with=FALSE]
write.csv(locxy,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy.csv")


#######################################
# for 200x200 grid
#######################################

lpmxy<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/LPM_GRID_IDS.csv")
str(lpmxy)
ss <- lpmxy[long_aod > -73.566 & long_aod < -69.9 & lat_aod < 43 & lat_aod > 41.1, ]

write.csv(ss ,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/lpmxy.csv")

