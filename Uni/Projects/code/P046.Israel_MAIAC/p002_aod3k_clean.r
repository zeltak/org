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

# for(i in 2003:2003){
#   allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "C06_3k_", i, ".csv"))
#   print(i)
# } 
# allbestpred <- rbindlist(allbestpredlist)
# rm(allbestpredlist)



d12n=read.table("/media/NAS/Uni/Data/Israel/modis3k/full_years/C06_3k_2012.csv",sep=",",header=TRUE) 
## create new data tables from C06 data for years 2002-2012 with lat,long,dtaod,dbaod,combinedaod ##
d12n<-as.data.table(d12n)

#create table with relevant data
# d02n=cbind(d02[,1:8],d02[,18:21])
# d03n=cbind(d03[,1:8],d03[,18:21])
# d04n=cbind(d04[,1:8],d04[,18:21])
# d05n=cbind(d05[,1:8],d05[,18:21])
# d06n=cbind(d06[,1:8],d06[,18:21])
# d07n=cbind(d07[,1:8],d07[,18:21])
# d08n=cbind(d08[,1:8],d08[,18:21])
# d09n=cbind(d09[,1:8],d09[,18:21])
# d10n=cbind(d10[,1:8],d10[,18:21])
# d11n= cbind(d11[,1:8],d11[,18:21])
# d12n=cbind(d12[,1:4],d12[,18:21])
# d13n=cbind(d13[,1:8],d13[,18:21])

d12n<-d12n[,c(1:3,6:8),with=FALSE]
#AOD=rbind(d02n,d03n,d04n,d05n,d06n,d07n,d08n,d09n,d10n,d11n,d12n,d13n)


#AOD[,13]=data.frame(stn=character(dim(AOD)[1]))
setnames(d12n,"Lat","lat_aod")
setnames(d12n,"Long","long_aod")
allbestpred <- d12n[long_aod > 34.1 & long_aod < 36  & lat_aod < 34 & lat_aod > 28, ]


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
#for MAIAC
clipgrid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILclipgrid.csv")

