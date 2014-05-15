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


basegrid <-  fread("/media/NAS/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")


##################################
#2003
##################################


source("/media/NAS/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday_MPM.r")

# #to create a date range based on start and end points use
# days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
# #create date range
# mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2003))
# setkey(mod3grid,guid)
# setkey(basegrid,guid)
# mod3grid <- merge(mod3grid,basegrid)
# 
# if(!exists("m2_agg")){
#   m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
# }
# 
# #subset to study area
# mod3grid.se <- mod3grid [mod3grid $guid %in% m2_agg$guid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/media/NAS/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
#saveRDS(mod3grid.se,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")
mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")




#PM
pm <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/pm_all_complete.csv")
pmid <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid.csv")
setkey(pm ,SiteCode)
setkey(pmid,SiteCode)
pmall <- merge(pm,pmid)
str(pmall)
#convert date from 01JAN2000 format
pmall <- pmall [, day:=as.Date(strptime(Date,"%d%b%Y"))]
pmall[, c := as.numeric(format(day, "%Y")) ]
#xtract year PM
pmall2003<- pmall[c==2003]

table_temp<-as.data.table(ddply(na.omit(pmall2003[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]

pmall2003 <- pmall2003[pmall2003$SiteCode %in% table_temp$SiteCode, ] 

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")

#create PM matrix
pm.m <- makepointsmatrix(pmall2003, "long_pm", "lat_pm", "SiteCode")

################
#se
################

#create aod matrix
mod3grid.se[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.se, guidc)
mod3.m <- makepointsmatrix(mod3grid.se[mod3grid.se[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodse<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.se, pmall2003 [, list(day,PM25,SiteCode)], 
                              "guidc", "SiteCode", "meanPM", "PM25", knearest = 18, maxdistance = NA)

#check data completness
x1<-closestaodse[, .N, by=c("guidc")]
summary(x1)
#cleanup
closestaodse[,meanPM :=NULL]
closestaodse[,meanPMknn:=NULL]
closestaodse[,meanPMnobs:=NULL]

#return numric guid
closestaodse[, guid := as.integer(guidc)]
closestaodse[,guidc :=NULL]

saveRDS(closestaodse  , "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2003.rds")

mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")

setkey(closestaodse,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,closestaodse,all.x = T)

#check data completness
x1<-mod3grid[, .N, by=c("guid")]
summary(x1)

basegrid <-  fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)

saveRDS(midmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")

# setkey(pmall2003,day,mpmid)
# mpm_agg <- (pmall2003[, list(ampm =mean(PM25, na.rm = TRUE), 
#                              long_pm = long_pm[1],
#                              lat_pm = lat_pm [1]),
#                       by = list(day,mpmid)])  
# 
# # saveRDS(mpm_aggs,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2003.rds")
# # saveRDS(midmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2003.rds")
# #midmod3grid<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2003.rds")
# 
# setkey(midmod3grid,day,mpmid)
# setkey(mpm_agg,day,mpmid)
# nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)
# 
# nmidmod3grid [,bestmpm := meanPMmean]
# nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
# nmidmod3grid[,mpmid:=NULL]
# nmidmod3grid[, meanPMmean  :=NULL]
# nmidmod3grid[, ampm  :=NULL]
# nmidmod3grid[,  long_pm :=NULL]
# nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")


