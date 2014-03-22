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


basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")
basegrid [,V1 :=NULL]

##################################
#2011
##################################



#to create a date range based on start and end points use
days_2011<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2011))
setkey(mod3grid,guid)
setkey(basegrid,guid)
mod3grid <- merge(mod3grid,basegrid)
#check correct final db 
dcc<-dim(basegrid)
dcc[1]*365


source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday_MPM.r")
saveRDS(mod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")
#mod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")

#met
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
pmall2011<- pmall[c==2011]

#check missing
test<-unique(pmall2011$SiteCode)
length(test)*365
pmall2011_check <- data.table(ddply(pmall2011, .(SiteCode), transform, n = length(SiteCode)))
try2 <- pmall2011_check [n<365 , ]


#create PM matrix
pm.m <- makepointsmatrix(pmall2011, "long_pm", "lat_pm", "SiteCode")

################
#seT1
################
mod3gridT1<-mod3grid[1:20000000,]

#create aod matrix
mod3gridT1[, guidc := as.character(guid)]
#need to sort
setkey(mod3gridT1, guidc)
mod3.m <- makepointsmatrix(mod3gridT1[mod3gridT1[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT1<- nearestbyday(mod3.m ,pm.m , 
                              mod3gridT1, pmall2011 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 5, maxdistance = 1)

cor(closestaodseT1$PM25,closestaodseT1$meanPMmean)


closestaodseT1[,meanPM :=NULL]
closestaodseT1[,meanPMknn:=NULL]
closestaodseT1[,meanPMnobs:=NULL]


saveRDS(closestaodseT1,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT1d2011.rds")
rm(closestaodseT1)
gc()


################
#seT2
################
mod3gridT2<-mod3grid[20000001:40000000,]

#create aod matrix
mod3gridT2[, guidc := as.character(guid)]
#need to sort
setkey(mod3gridT2, guidc)
mod3.m <- makepointsmatrix(mod3gridT2[mod3gridT2[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT2<- nearestbyday(mod3.m ,pm.m , 
                              mod3gridT2, pmall2011 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 13, maxdistance = NA)


closestaodseT2[,meanPM :=NULL]
closestaodseT2[,meanPMknn:=NULL]
closestaodseT2[,meanPMnobs:=NULL]

saveRDS(closestaodseT2,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT2d2011.rds")
rm(closestaodseT2)
gc()



################
#seT3
################
mod3gridT3<-mod3grid[40000001:60000000,]

#create aod matrix
mod3gridT3[, guidc := as.character(guid)]
#need to sort
setkey(mod3gridT3, guidc)
mod3.m <- makepointsmatrix(mod3gridT3[mod3gridT3[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT3<- nearestbyday(mod3.m ,pm.m , 
                              mod3gridT3, pmall2011 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 13, maxdistance = NA)


closestaodseT3[,meanPM :=NULL]
closestaodseT3[,meanPMknn:=NULL]
closestaodseT3[,meanPMnobs:=NULL]

saveRDS(closestaodseT3,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT3d2011.rds")

rm(closestaodseT3)
gc()


################
#seT4
################
mod3gridT4<-mod3grid[60000001:80000000,]

#create aod matrix
mod3gridT4[, guidc := as.character(guid)]
#need to sort
setkey(mod3gridT4, guidc)
mod3.m <- makepointsmatrix(mod3gridT4[mod3gridT4[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT4<- nearestbyday(mod3.m ,pm.m , 
                              mod3gridT4, pmall2011 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 13, maxdistance = NA)


closestaodseT4[,meanPM :=NULL]
closestaodseT4[,meanPMknn:=NULL]
closestaodseT4[,meanPMnobs:=NULL]

saveRDS(closestaodseT4,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT4d2011.rds")

rm(closestaodseT4)
gc()



################
#seT5
################

cc<-dim(mod3grid)
mod3gridT5<-mod3grid[80000001:cc[1],]

#create aod matrix
mod3gridT5[, guidc := as.character(guid)]
#need to sort
setkey(mod3gridT5, guidc)
mod3.m <- makepointsmatrix(mod3gridT5[mod3gridT5[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodseT5<- nearestbyday(mod3.m ,pm.m , 
                              mod3gridT5, pmall2011 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 13, maxdistance = NA)


closestaodseT5[,meanPM :=NULL]
closestaodseT5[,meanPMknn:=NULL]
closestaodseT5[,meanPMnobs:=NULL]

saveRDS(closestaodseT5,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT5d2011.rds")

rm(closestaodseT5)
gc()

######
#bind it
######
closestaodseT1<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT1d2011.rds")
closestaodseT2<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT2d2011.rds")
closestaodseT3<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT3d2011.rds")
closestaodseT4<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT4d2011.rds")
closestaodseT5<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/closestaodseT5d2011.rds")


mpm2011<-rbind(closestaodseT1,closestaodseT2,closestaodseT3,closestaodseT4,closestaodseT5)
#return numric guid
mpm2011[, guid := as.integer(guidc)]
mpm2011[,guidc :=NULL]

saveRDS(mpm2011  , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2011.rds")

# keep(basegrid, sure=TRUE) 
# gc()
# 

