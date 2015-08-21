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
# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#import clipped grid
fullgrid<-read_csv("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/ISAREL.grid.csv")

####################
## 2003
####################
###Read 1 csv per tile
out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v01.2003.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v02.2003.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v01.2003.csv")
out4 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v02.2003.csv")
out<-rbind(out1,out2,out3,out4)
rm(out1,out2,out3,out4)
gc()
#create lstid and unique grid
setnames(out,"Lat","lat_lst")
setnames(out,"Lon","long_lst")
#create lstid
out$lstid<-paste(out$long_lst,out$lat_lst,sep="-")
##at this stage clip the data based france grid
out<- out[out$lstid %in% fullgrid$lstid, ]
#dates
#contatanate feilds
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
#create R date format
out$day<-as.Date(strptime(out$date, "%d/%m/%Y"))
out<-as.data.frame(out)
out<-select(out,lstid,lat_lst,long_lst,lst,day,Year)
#create single lst point per lstid per day 
aqua <-out %>%
    group_by(lstid,day) %>%
    summarise(long_lst=mean(long_lst),lat_lst=mean(lat_lst),lst=mean(lst),UN=mean(UN),QA=mean(QA),Year=mean(Year) )
#####NOTE wierd values in longtitude lie -0.00103270 are OK
saveRDS(aqua,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/lst.AQ.2003.rds")
keep(fullgrid, sure=TRUE) 
gc()
