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
library(dplyr)
library(readr)

# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#import clipped grid

fullgrid<-read_csv("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/fullgrid.csv")


####################
#######Aqua
####################
Italy tiles 
## Tile h01
out <- list()
path.data<-"/media/NAS/Uni/Data/MV3/Out/"
for(i in 2003:2010){
  out[[paste0("year_", i)]] <- read_csv(paste0(path.data, "MAIAC_Aq.h00v01.", i, ".csv"))
  print(i)
} 
out <- rbindlist(out)
rm(out)
out<-as.data.table(out)
gc()
##at this stage clip the data based france grid
out<- out[out$aodid %in% fullgrid$aodid, ]
#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")
#dates
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
XXXXftile01v02<-select(out,XX,YY)
keep(ftile01v02,fullgrid, sure=TRUE) 
gc()









#### add ITM x,y
setkey(clippedaod, aodid)
setkey(clipgrid, aodid)
clippedaod[, yr := as.numeric(format(day, "%Y")) ]
clippedaod <- merge(clippedaod, clipgrid[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
#describe(zclippedaod)
saveRDS(clippedaod,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_TR_0014.RDS")
keep(path.data, sure=TRUE) 



####################
#######AQUA
####################


out <- list()
path.data<-"/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/"

for(i in 2002:2013){
  out[[paste0("year_", i)]] <- read.csv(paste0(path.data, "MAIACAqIsr_", i, ".csv"), header=T)
  print(i)
} 
out <- rbindlist(out)
rm(out)
out<-as.data.table(out)

#create aodid and unique grid
setnames(out,"Lat","lat_aod")
setnames(out,"Lon","long_aod")
setnames(out,"AOD","aod")
out$date<-paste(out$Day,out$Month,out$Year,sep="/")
out[, day:=as.Date(strptime(date, "%d/%m/%Y"))]

#create aodid
out$aodid<-paste(out$long_aod,out$lat_aod,sep="-")
grid <- unique(out, by="aodid")
grid<-grid[,c(5,6,29),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]

##at this stage clip the data based on israel layer and reimport
clipgrid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILclipgrid.csv")

clippedaod<- out[out$aodid %in% clipgrid$aodid, ] 
clippedaod<-clippedaod[,c(5:29),with=FALSE]
#keep(clippedaod,clipgrid, sure=TRUE) 
#gc()
#### add ITM x,y
setkey(clippedaod, aodid)
setkey(clipgrid, aodid)
clippedaod[, yr := as.numeric(format(day, "%Y")) ]
clippedaod <- merge(clippedaod, clipgrid[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
#describe(zclippedaod)
saveRDS(clippedaod,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")







