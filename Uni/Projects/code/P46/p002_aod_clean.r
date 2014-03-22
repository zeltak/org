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
path.data<-"/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/"

for(i in 2000:2013){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "MAIACTrIsr_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)


#create aodid and unique grid
setnames(allbestpred,"V5","lat_aod")
setnames(allbestpred,"V6","long_aod")
setnames(allbestpred,"V7","aod")
#use above function to trim whitespace
allbestpred$lat_aod<- trim(allbestpred$lat_aod)
allbestpred$long_aod<- trim(allbestpred$long_aod)
allbestpred$V2 <- trim(allbestpred$V2)

allbestpred$date<-paste(allbestpred$V1,allbestpred$V2,allbestpred$V3,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
str(allbestpred)

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
grid<-grid[,c(5,6,10),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]
#export dbf for GIS
write.dbf(grid,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILgrid.dbf")
##at this stage clip the data based on israel layer and reimport
clipgrid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILclipgrid.csv")

clippedaod<- allbestpred[allbestpred$aodid %in% clipgrid$aodid, ] 
clippedaod<-clippedaod[,c(5,6,7,9,10),with=FALSE]
keep(clippedaod,clipgrid, sure=TRUE) 
gc()
#### add ITM x,y
setkey(clippedaod, aodid)
setkey(clipgrid, aodid)
clippedaod[, yr := as.numeric(format(day, "%Y")) ]
clippedaod <- merge(clippedaod, clipgrid[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
#describe(zclippedaod)
saveRDS(clippedaod,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyears.RDS")



######## EDA
#describe(clippedaod$aod)
#dat <- dat[AOD <= 1.5]
###########

# clippedaod[, yr := as.numeric(format(day, "%Y")) ]
# ylist <- list()
# path.data<-"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/"
# for(i in 2000:2013){
#   ylist[[ paste0('dat',i) ]] <- clippedaod[yr == i] 
#            print(i)
# } 
# 
# saveRDS(ylist[1],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2000.rds")
# saveRDS(ylist[2],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2001.rds")
# saveRDS(ylist[3],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2002.rds")
# saveRDS(ylist[4],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2003.rds")
# saveRDS(ylist[5],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2004.rds")
# saveRDS(ylist[6],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2005.rds")
# saveRDS(ylist[7],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2006.rds")
# saveRDS(ylist[8],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2007.rds")
# saveRDS(ylist[9],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2008.rds")
# saveRDS(ylist[10],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2009.rds")
# saveRDS(ylist[11],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2010.rds")
# saveRDS(ylist[12],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2011.rds")
# saveRDS(ylist[13],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2012.rds")
# saveRDS(ylist[14],"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2013.rds")


#old methos (use for lopp above instead)
dat2000 <- clippedaod[yr == "2000"]
dat2001 <- clippedaod[yr == "2001"]
dat2002 <- clippedaod[yr == "2002"]
dat2003 <- clippedaod[yr == "2003"]
dat2004 <- clippedaod[yr == "2004"]
dat2005 <- clippedaod[yr == "2005"]
dat2006 <- clippedaod[yr == "2006"]
dat2007 <- clippedaod[yr == "2007"]
dat2008 <- clippedaod[yr == "2008"]
dat2009 <- clippedaod[yr == "2009"]
dat2010 <- clippedaod[yr == "2010"]
dat2011 <- clippedaod[yr == "2011"]
dat2012 <- clippedaod[yr == "2012"]
dat2013 <- clippedaod[yr == "2013"]


saveRDS(dat2000,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2000.rds")
saveRDS(dat2001,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2001.rds")
saveRDS(dat2002,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2002.rds")
saveRDS(dat2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2003.rds")
saveRDS(dat2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2004.rds")
saveRDS(dat2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2005.rds")
saveRDS(dat2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2006.rds")
saveRDS(dat2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2007.rds")
saveRDS(dat2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2008.rds")
saveRDS(dat2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2009.rds")
saveRDS(dat2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2010.rds")
saveRDS(dat2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2011.rds")
saveRDS(dat2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2012.rds")
saveRDS(dat2013,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2013.rds")


keep(path.data, sure=TRUE) 



####################
#######AQUA
####################

allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/"

for(i in 2002:2013){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "MAIACAqIsr_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)


#create aodid and unique grid
setnames(allbestpred,"V5","lat_aod")
setnames(allbestpred,"V6","long_aod")
setnames(allbestpred,"V7","aod")
#use above function to trim whitespace
allbestpred$lat_aod<- trim(allbestpred$lat_aod)
allbestpred$long_aod<- trim(allbestpred$long_aod)
allbestpred$V2 <- trim(allbestpred$V2)

allbestpred$date<-paste(allbestpred$V1,allbestpred$V2,allbestpred$V3,sep="/")
allbestpred[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
str(allbestpred)

#create aodid
allbestpred$aodid<-paste(allbestpred$long_aod,allbestpred$lat_aod,sep="-")
grid <- unique(allbestpred, by="aodid")
grid<-grid[,c(5,6,10),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]
#export dbf for GIS
write.dbf(grid,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILgridAq.dbf")



##at this stage clip the data based on israel layer and reimport
clipgrid<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ILclipgrid.csv")

clippedaod<- allbestpred[allbestpred$aodid %in% clipgrid$aodid, ] 
clippedaod<-clippedaod[,c(5,6,7,9,10),with=FALSE]
keep(clippedaod,clipgrid, sure=TRUE) 
gc()
#### add ITM x,y
setkey(clippedaod, aodid)
setkey(clipgrid, aodid)
clippedaod <- merge(clippedaod, clipgrid[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
#describe(zclippedaod)
clippedaod[, yr := as.numeric(format(day, "%Y")) ]
saveRDS(clippedaod,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyearsAQ.RDS")

clippedaod<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_allyearsAQ.RDS")


######## EDA
#describe(clippedaod$aod)
#dat <- dat[AOD <= 1.5]
###########


#old methos (use for lopp above instead)
dat2002 <- clippedaod[yr == "2002"]
dat2003 <- clippedaod[yr == "2003"]
dat2004 <- clippedaod[yr == "2004"]
dat2005 <- clippedaod[yr == "2005"]
dat2006 <- clippedaod[yr == "2006"]
dat2007 <- clippedaod[yr == "2007"]
dat2008 <- clippedaod[yr == "2008"]
dat2009 <- clippedaod[yr == "2009"]
dat2010 <- clippedaod[yr == "2010"]
dat2011 <- clippedaod[yr == "2011"]
dat2012 <- clippedaod[yr == "2012"]
dat2013 <- clippedaod[yr == "2013"]



saveRDS(dat2002,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2002.rds")
saveRDS(dat2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2003.rds")
saveRDS(dat2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2004.rds")
saveRDS(dat2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2005.rds")
saveRDS(dat2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2006.rds")
saveRDS(dat2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2007.rds")
saveRDS(dat2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2008.rds")
saveRDS(dat2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2009.rds")
saveRDS(dat2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2010.rds")
saveRDS(dat2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2011.rds")
saveRDS(dat2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2012.rds")
saveRDS(dat2013,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaodAQ2013.rds")






