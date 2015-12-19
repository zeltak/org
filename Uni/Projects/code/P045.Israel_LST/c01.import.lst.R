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
summary(out$NIGHT)
head(out)
outdt<-as.data.table(out)
#recode fill values of '0'- they are NA
#head(out)
out$DAY[out$DAY == 0] <- "NA"
out$DAY<-as.numeric(out$DAY)
summary(out$DAY)

out$NIGHT[out$NIGHT == 0] <- "NA"
out$NIGHT<-as.numeric(out$NIGHT)
summary(out$NIGHT)



      
      #convert LST to temperature
      #apply a scale factor
      out$d.tempc<-(out$DAY* 0.02) - 273.15
      out$n.tempc<-(out$NIGHT* 0.02) - 273.15
      out$emissivity <- (out$EMIS*0.002) +0.49
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
      
      # #create full unclipped grid-only done during frist run to clip data in arcgis
      # yy <-out %>%
      #     group_by(lstid) %>%
      #     summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE))
      # 
      # write.csv(yy,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/unclip.lstXY.csv")
      
      
      ##at this stage clip the data based france grid
      outclip <- out[out$lstid %in% fullgrid$lstid, ]
      
# #create full unclipped grid-only done during frist run to clip data in arcgis
# yy <-outclip %>%
#     group_by(lstid) %>%
#     summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE))
# 
# write.csv(yy,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/check.lstXY.csv")
#


#contatanate feilds
outclip$date<-paste(outclip$Day,outclip$Month,outclip$Year,sep="/")
#create R date format
outclip$day<-as.Date(strptime(outclip$date, "%d/%m/%Y"))
outclip<-as.data.frame(outclip)
outclip<-select(outclip,lstid,lat_lst,long_lst,day,Year,d.tempc, n.tempc,emissivity)
#create single lst point per lstid per day 
saveRDS(outclip,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/lst.AQ.2003.rds")
keep(fullgrid, sure=TRUE) 
gc()

####################
## 2004
####################
###Read 1 csv per tile
out <- read_csv("/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/AqIsr_2004.csv")
summary(out$NIGHT)
head(out)
outdt<-as.data.table(out)
#recode fill values of '0'- they are NA
#head(out)
out$DAY[out$DAY == 0] <- "NA"
out$DAY<-as.numeric(out$DAY)
summary(out$DAY)

out$NIGHT[out$NIGHT == 0] <- "NA"
out$NIGHT<-as.numeric(out$NIGHT)
summary(out$NIGHT)



      
      #convert LST to temperature
      #apply a scale factor
      out$d.tempc<-(out$DAY* 0.02) - 273.15
      out$n.tempc<-(out$NIGHT* 0.02) - 273.15
      out$emissivity <- (out$EMIS*0.002) +0.49
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
      
      # #create full unclipped grid-only done during frist run to clip data in arcgis
      # yy <-out %>%
      #     group_by(lstid) %>%
      #     summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE))
      # 
      # write.csv(yy,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/unclip.lstXY.csv")
      
      
      ##at this stage clip the data based france grid
      outclip <- out[out$lstid %in% fullgrid$lstid, ]
      
# #create full unclipped grid-only done during frist run to clip data in arcgis
# yy <-outclip %>%
#     group_by(lstid) %>%
#     summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE))
# 
# write.csv(yy,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/check.lstXY.csv")
#


#contatanate feilds
outclip$date<-paste(outclip$Day,outclip$Month,outclip$Year,sep="/")
#create R date format
outclip$day<-as.Date(strptime(outclip$date, "%d/%m/%Y"))
outclip<-as.data.frame(outclip)
outclip<-select(outclip,lstid,lat_lst,long_lst,day,Year,d.tempc, n.tempc,emissivity)
#create single lst point per lstid per day 
saveRDS(outclip,"/media/NAS/Uni/Projects/P045_Israel_LST/2.work/lst.AQ.2004.rds")
keep(fullgrid, sure=TRUE) 
gc()
