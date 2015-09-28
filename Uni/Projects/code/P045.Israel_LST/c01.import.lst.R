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
library(readr)
#sourcing
# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#import clipped grid
fullgrid<-fread("/media/NAS/Uni/Projects/P045_Israel_LST/2.work/gridXY_IL.csv")

####################
## 2003
####################
###Read 1 csv per tile
out <- read_csv("/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/AqIsr_2003.csv")
#describe(out$NIGHT)
head(out)
#recode fill values of '0'- they are NA
# note: you can use comma or ampersand to represent AND condition
out[DAY==0, DAY:= "NA"]


#convert LST to temperature
#apply a scale factor
out$d.tempc<-out$DAY* 0.02 - 273.15
out$n.tempc<-out$NIGHT* 0.02 - 273.15
out$emissivity <- out$EMIS*0.002+0.49
##correction we dont apply anymore
#NTckin=  dtc/(emis_scale**0.25);
#NTckin=  ntc/(emis_scale**0.25);
out<-as.data.frame(out)
head(out)
#create lstid and unique grid
setnames(out,"Lat","lat_lst")
setnames(out,"Lon","long_lst")
names(out)
out$DAY<-NULL
out$NIGHT<-NULL
out$EMIS<-NULL
out<-as.data.frame(out)
#create lstid
out$lstid<-paste(out$long_lst,out$lat_lst,sep="-")

#create full unclipped grid
yy <-out %>%
    group_by(lstid) %>%
    summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE))

write.csv(yy,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/unclip.lstXY.csv")


##at this stage clip the data based france grid
outclip <- out[out$lstid %in% fullgrid$lstid, ]


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
