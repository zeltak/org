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
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")
# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#import clipped grid
fullgrid<-read_csv("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/france.grid.csv")

####################
## 2003
####################
out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v01.2003.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v02.2003.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v01.2003.csv")
out4 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v02.2003.csv")
out<-rbind(out1,out2,out3,out4)
rm(out1,out2,out3,out4)
gc()
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")
##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]
#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
out<-select(out,aodid,lat_aod,long_aod,aod,day,UN,QA,Year)
#create single aod point per aodid per day 
aqua <-out %>%
    group_by(aodid,day) %>%
    summarise(long_aod=mean(long_aod),lat_aod=mean(lat_aod),aod=mean(aod),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/AOD.AQ.2003.rds")
keep(fullgrid, sure=TRUE) 
gc()
