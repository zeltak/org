library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(readr)

france <- read_csv("/media/zeltak/SHATZI/Calipso/CAL_Fr_day.csv")
us <- read_csv("/media/zeltak/SHATZI/Calipso/CAL_USA_day.csv")
head(us)
str(france)



france$lat_cal<-((france$Lat1+france$Lat2+france$Lat3)/3)
france$long_cal<-((france$Long1+france$Long2+france$Long3)/3)

france$date<-paste(france$day,france$month,france$year,sep="/")
france$day<- as.Date(strptime(france$date, "%d/%m/%Y"))

#create aodid
france$calid<-paste(france$long_cal,france$lat_cal,sep="-")

ugrid <-france %>%
    group_by(calid) %>%
    summarise(lat_cal = mean(lat_cal, na.rm=TRUE),  long_cal = mean(long_cal, na.rm=TRUE))
str(ugrid)

#export dbf for GIS
write.csv(ugrid,"~/ZH_tmp/france.csv")

france$month<-NULL
france$location_index <-NULL
france$height_index <-NULL
france$isAerosol <-NULL
france$date <-NULL


#when finished with csv calipso save as RDS

saveRDS(france,"/PATH/france.clapiso.RDS")


# aod france

#load aod data
aqua.2003<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2003.rds")
head(aqua.2003)

#for AOD israel
aquaIL<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")

head(aquaIL)

#usa AOD

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2010.rds")
usaod<-select(m2.all,GUID,day,Lat,Long,aod)
head(usaod)
