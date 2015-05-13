###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)

# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

####################
#######TERRA
####################

allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/"

for(i in 2000:2013){
  allbestpredlist[[paste0("year_", i)]] <- read.csv(paste0(path.data, "MAIACTrIsr_", i, ".csv"), header=T)
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)
allbestpred<-as.data.table(allbestpred)
allbestpred<-select(allbestpred,Day,Month,Year,Lat,Lon,AOD)

#create aodid and unique grid
setnames(allbestpred,"Lat","lat_aod")
setnames(allbestpred,"Lon","long_aod")
setnames(allbestpred,"AOD","aod")
allbestpred$date<-paste(allbestpred$Day,allbestpred$Month,allbestpred$Year,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
grid<-grid[,c(4,5,9),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]
#export dbf for GIS
write.csv(grid,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/yaakov/ILgrid.csv")



#### add ITM x,y
saveRDS(allbestpred,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/yaakov/AOD_TR_0014.RDS")
keep(path.data, sure=TRUE) 
gc()


####################
#######AQUA
####################


allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/"

for(i in 2003:2013){
  allbestpredlist[[paste0("year_", i)]] <- read.csv(paste0(path.data, "MAIACAqIsr_", i, ".csv"), header=T)
  print(i)
} 

allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)
allbestpred<-as.data.table(allbestpred)
allbestpred<-select(allbestpred,Day,Month,Year,Lat,Lon,AOD)

#create aodid and unique grid
setnames(allbestpred,"Lat","lat_aod")
setnames(allbestpred,"Lon","long_aod")
setnames(allbestpred,"AOD","aod")
allbestpred$date<-paste(allbestpred$Day,allbestpred$Month,allbestpred$Year,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
grid<-grid[,c(4,5,9),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]
#export dbf for GIS
write.csv(grid,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/yaakov/ILgridAQ.csv")



#### add ITM x,y
saveRDS(allbestpred,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/yaakov/AOD_AQ_0014.RDS")
keep(path.data, sure=TRUE) 
  
  
day1<-filter(allbestpred,day=="2003-07-01")
write.csv(day1, "~/ZH_tmp/day1.csv")
day2<-filter(allbestpred,day=="2003-07-02")
write.csv(day2, "~/ZH_tmp/day2.csv")
day3<-filter(allbestpred,day=="2003-07-03")
write.csv(day3, "~/ZH_tmp/day3.csv")
day4<-filter(allbestpred,day=="2003-07-04")
write.csv(day4, "~/ZH_tmp/day4.csv")
day5<-filter(allbestpred,day=="2003-07-05")
write.csv(day5, "~/ZH_tmp/day5.csv")
day6<-filter(allbestpred,day=="2003-07-06")
write.csv(day6, "~/ZH_tmp/day6.csv")
day7<-filter(allbestpred,day=="2003-07-07")
write.csv(day7, "~/ZH_tmp/day7.csv")


day7<-filter(allbestpred,day=="2013-04-28")
write.csv(day7, "~/ZH_tmp/day7lg.csv")



day7<-filter(allbestpred,day=="2013-09-13")
write.csv(day7, "~/ZH_tmp/day7yk.csv")




dayy<-filter(allbestpred,day=="2003-08-01")
write.csv(dayy, "~/ZH_tmp/day8.csv")



