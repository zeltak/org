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
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

####################
#######TERRA
####################

allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Israel/modis3k/full_years/"



for(i in 2002:2013){
  allbestpredlist[[paste0("year_", i)]] <- read.csv(paste0(path.data, "C06_3k_", i, ".csv"), header=T)
  print(i)
} 


allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

allbestpred <-allbestpred [,c(1:3,6:8),with=FALSE]
#AOD=rbind(d02n,d03n,d04n,d05n,d06n,d07n,d08n,d09n,d10n,d11n,d12n,d13n)


#AOD[,13]=data.frame(stn=character(dim(AOD)[1]))
setnames(allbestpred,"Lat","lat_aod")
setnames(allbestpred,"Long","long_aod")
allbestpred <- allbestpred[long_aod > 34.1 & long_aod < 36  & lat_aod < 34 & lat_aod > 28, ]


#create aodid and unique grid
setnames(allbestpred,"DT_L_O","aod")
#use above function to trim whitespace
# allbestpred$lat_aod<- trim(allbestpred$lat_aod)
# allbestpred$long_aod<- trim(allbestpred$long_aod)
# allbestpred$V2 <- trim(allbestpred$V2)
allbestpred$date<-paste(allbestpred$day,allbestpred$month,allbestpred$yr,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
str(allbestpred)

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
# grid<-grid[,c(5,6,10),with=FALSE]
# grid[,long_aod:=as.numeric(long_aod)]
# grid[,lat_aod:=as.numeric(lat_aod)]
#export dbf for GIS
write.dbf(grid,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL3kgrid.dbf")
##at this stage clip the data based on israel layer and reimport
saveRDS(allbestpred,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014_3k.RDS")

