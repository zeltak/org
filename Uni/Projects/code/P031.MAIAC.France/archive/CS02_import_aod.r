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

# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#import clipped grid

fullgrid<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/fullgrid.csv")


####################
#######Aqua
####################

## Tile h01
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/MV3/Out/"
for(i in 2003:2013){
  allbestpredlist[[paste0("year_", i)]] <- read.csv(paste0(path.data, "MAIAC_Aq.h01v02.", i, ".csv"), header=T)
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)
allbestpred<-as.data.table(allbestpred)
#create aodid and unique grid
setnames(allbestpred,"Lat","lat_aod")
setnames(allbestpred,"Lon","long_aod")
setnames(allbestpred,"AOD","aod")
#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
##at this stage clip the data based france grid
clippedaod<- allbestpred[allbestpred$aodid %in% fullgrid$aodid, ]
#dates
allbestpred$date<-paste(allbestpred$Day,allbestpred$Month,allbestpred$Year,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
clippedaod<-clippedaod[,c(5:29),with=FALSE]
keep(clippedaod,clipgrid, sure=TRUE) 
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


allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/"

for(i in 2002:2013){
  allbestpredlist[[paste0("year_", i)]] <- read.csv(paste0(path.data, "MAIACAqIsr_", i, ".csv"), header=T)
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)
allbestpred<-as.data.table(allbestpred)

#create aodid and unique grid
setnames(allbestpred,"Lat","lat_aod")
setnames(allbestpred,"Lon","long_aod")
setnames(allbestpred,"AOD","aod")
allbestpred$date<-paste(allbestpred$Day,allbestpred$Month,allbestpred$Year,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
grid<-grid[,c(5,6,29),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]

##at this stage clip the data based on israel layer and reimport
clipgrid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILclipgrid.csv")

clippedaod<- allbestpred[allbestpred$aodid %in% clipgrid$aodid, ] 
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







