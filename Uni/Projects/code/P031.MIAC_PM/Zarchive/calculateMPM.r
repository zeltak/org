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
library(FNN)


basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")


##################################
#2009
##################################

#to create a date range based on start and end points use
days_2009<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2009))
setkey(mod3grid,guid)
setkey(basegrid,guid)
mod3grid <- merge(mod3grid,basegrid)

if(!exists("m2_agg")){
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2009.rds")
}

#subset to study area
mod3grid.se <- mod3grid [mod3grid $guid %in% m2_agg$guid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
saveRDS(mod3grid.se,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2009.rds")
#mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2009.rds")

#PM
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/pm_all_complete.csv")
pmid <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid.csv")
setkey(pm ,SiteCode)
setkey(pmid,SiteCode)
pmall <- merge(pm,pmid)
str(pmall)
#convert date from 01JAN2000 format
pmall <- pmall [, day:=as.Date(strptime(Date,"%d%b%Y"))]
pmall[, c := as.numeric(format(day, "%Y")) ]
#xtract year PM
pmall2009<- pmall[c==2009]

#create PM matrix
pm.m <- makepointsmatrix(pmall2009, "long_pm", "lat_pm", "SiteCode")

source("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday_MPM.r")


################
#seT1
################
mod3grid.seT1<-mod3grid.se[1:40000000,]

#create aod matrix
mod3grid.seT1[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT1, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT1[mod3grid.seT1[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT1<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT1, pmall2009 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT1[,meanPM :=NULL]
closestaodseT1[,meanPMknn:=NULL]
closestaodseT1[,meanPMnobs:=NULL]


saveRDS(closestaodseT1,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT1d2009.rds")
rm(closestaodseT1)
gc()


################
#seT2
################
mod3grid.seT2<-mod3grid.se[20000001:40000000,]

#create aod matrix
mod3grid.seT2[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT2, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT2[mod3grid.seT2[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT2<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT2, pmall2009 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT2[,meanPM :=NULL]
closestaodseT2[,meanPMknn:=NULL]
closestaodseT2[,meanPMnobs:=NULL]

saveRDS(closestaodseT2,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT2d2009.rds")
rm(closestaodseT2)
gc()



################
#seT3
################
mod3grid.seT3<-mod3grid.se[40000001:60000000,]

#create aod matrix
mod3grid.seT3[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT3, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT3[mod3grid.seT3[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT3<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT3, pmall2009 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT3[,meanPM :=NULL]
closestaodseT3[,meanPMknn:=NULL]
closestaodseT3[,meanPMnobs:=NULL]

saveRDS(closestaodseT3,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT3d2009.rds")

rm(closestaodseT3)
gc()


################
#seT4
################
mod3grid.seT4<-mod3grid.se[60000001:80000000,]

#create aod matrix
mod3grid.seT4[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT4, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT4[mod3grid.seT4[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT4<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT4, pmall2009 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT4[,meanPM :=NULL]
closestaodseT4[,meanPMknn:=NULL]
closestaodseT4[,meanPMnobs:=NULL]

saveRDS(closestaodseT4,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT4d2009.rds")

rm(closestaodseT4)
gc()



################
#seT5
################

cc<-dim(mod3grid.se)
mod3grid.seT5<-mod3grid.se[80000001:cc[1],]

#create aod matrix
mod3grid.seT5[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT5, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT5[mod3grid.seT5[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT5<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT5, pmall2009 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT5[,meanPM :=NULL]
closestaodseT5[,meanPMknn:=NULL]
closestaodseT5[,meanPMnobs:=NULL]

saveRDS(closestaodseT5,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT5d2009.rds")

rm(closestaodseT5)
gc()

######
#bind it
######

mpm2009<-rbind(closestaodseT1,closestaodseT2,closestaodseT3,closestaodseT4,closestaodseT5)
#return numric guid
mpm2009[, guid := as.integer(guidc)]
mpm2009[,guidc :=NULL]

saveRDS(mpm2009  , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2009.rds")

keep(basegrid, sure=TRUE) 
gc()


##################################
#2007
##################################
source("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday_MPM.r")


#to create a date range based on start and end points use
days_2007<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2007))
setkey(mod3grid,guid)
setkey(basegrid,guid)
mod3grid <- merge(mod3grid,basegrid)

if(!exists("m2_agg")){
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2007.rds")
}

#subset to study area
mod3grid.se <- mod3grid [mod3grid $guid %in% m2_agg$guid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
saveRDS(mod3grid.se,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2007.rds")

#met
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/pm_all_complete.csv")
pmid <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid.csv")
setkey(pm ,SiteCode)
setkey(pmid,SiteCode)
pmall <- merge(pm,pmid)
str(pmall)
#convert date from 01JAN2000 format
pmall <- pmall [, day:=as.Date(strptime(Date, "%d%b%Y"))]
pmall[, c := as.numeric(format(day, "%Y")) ]
#xtract year PM
pmall2007<- pmall[c==2007]

#create PM matrix
pm.m <- makepointsmatrix(pmall2007, "long_pm", "lat_pm", "SiteCode")



################
#seT1
################
mod3grid.seT1<-mod3grid.se[1:20000000,]

#create aod matrix
mod3grid.seT1[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT1, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT1[mod3grid.seT1[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT1<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT1, pmall2007 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT1[,meanPM :=NULL]
closestaodseT1[,meanPMknn:=NULL]
closestaodseT1[,meanPMnobs:=NULL]



################
#seT2
################
mod3grid.seT2<-mod3grid.se[20000001:40000000,]

#create aod matrix
mod3grid.seT2[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT2, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT2[mod3grid.seT2[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT2<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT2, pmall2007 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT2[,meanPM :=NULL]
closestaodseT2[,meanPMknn:=NULL]
closestaodseT2[,meanPMnobs:=NULL]




################
#seT3
################
mod3grid.seT3<-mod3grid.se[40000001:60000000,]

#create aod matrix
mod3grid.seT3[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT3, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT3[mod3grid.seT3[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT3<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT3, pmall2007 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT3[,meanPM :=NULL]
closestaodseT3[,meanPMknn:=NULL]
closestaodseT3[,meanPMnobs:=NULL]



################
#seT4
################
mod3grid.seT4<-mod3grid.se[60000001:80000000,]

#create aod matrix
mod3grid.seT4[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT4, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT4[mod3grid.seT4[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT4<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT4, pmall2007 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT4[,meanPM :=NULL]
closestaodseT4[,meanPMknn:=NULL]
closestaodseT4[,meanPMnobs:=NULL]


################
#seT5
################
mod3grid.seT5<-mod3grid.se[80000001:104036680 ,]

#create aod matrix
mod3grid.seT5[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.seT5, guidc)
mod3.m <- makepointsmatrix(mod3grid.seT5[mod3grid.seT5[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT5<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.seT5, pmall2007 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)


closestaodseT5[,meanPM :=NULL]
closestaodseT5[,meanPMknn:=NULL]
closestaodseT5[,meanPMnobs:=NULL]

######
#bind it
######

mpm2007<-rbind(closestaodseT1,closestaodseT2,closestaodseT3,closestaodseT4,closestaodseT5)
#return numric guid
mpm2007[, guid := as.integer(guidc)]
mpm2007[,guidc :=NULL]

saveRDS(mpm2007  , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2007.rds")

keep(basegrid, sure=TRUE) 
gc()


