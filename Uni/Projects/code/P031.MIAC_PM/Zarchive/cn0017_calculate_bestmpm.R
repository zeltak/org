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


basegrid <-  fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")


##################################
#2003
##################################


source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")

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

#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2003[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
pmall2003 <- pmall2003[pmall2003$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.m <- makepointsmatrix(pmall2003, "long_pm", "lat_pm", "SiteCode")
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



#2010
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


basegrid <-  fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")


##################################
#2010
##################################


source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")

# #to create a date range based on start and end points use
# days_2010<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
# #create date range
# mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2010))
# setkey(mod3grid,guid)
# setkey(basegrid,guid)
# mod3grid <- merge(mod3grid,basegrid)
# 
# if(!exists("m2_agg")){
#   m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2010.rds")
# }
# 
# #subset to study area
# mod3grid.se <- mod3grid [mod3grid $guid %in% m2_agg$guid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/media/NAS/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
#saveRDS(mod3grid.se,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2010.rds")
mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2010.rds")




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
pmall2010<- pmall[c==2010]

#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2010[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
pmall2010 <- pmall2010[pmall2010$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.m <- makepointsmatrix(pmall2010, "long_pm", "lat_pm", "SiteCode")
#create aod matrix
mod3grid.se[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.se, guidc)
mod3.m <- makepointsmatrix(mod3grid.se[mod3grid.se[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodse<- nearestbyday(mod3.m ,pm.m , 
                              mod3grid.se, pmall2010 [, list(day,PM25,SiteCode)], 
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

saveRDS(closestaodse  , "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2010.rds")

mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2010.rds")

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

saveRDS(midmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2010.rds")

#clear workspace
rm(list = ls())
gc()


###############
#2011
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


basegrid <-  fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")


##################################
#2011
##################################


source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")

# #to create a date range based on start and end points use
# days_2011<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
# #create date range
# mod3grid <- data.table(expand.grid(guid = basegrid[, unique(guid)], day = days_2011))
# setkey(mod3grid,guid)
# setkey(basegrid,guid)
# mod3grid <- merge(mod3grid,basegrid)
# 
# if(!exists("m2_agg")){
#   m2_agg<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2011.rds")
# }
# 
# #subset to study area
# mod3grid.se <- mod3grid [mod3grid $guid %in% m2_agg$guid, ] 
# import monitor data and spatial merge with nearestbyday()
#source("/media/NAS/Uni/Backups/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
#saveRDS(mod3grid.se,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")
mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")




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
pmall2011<- pmall[c==2011]

#keep only full stations
table_temp<-as.data.table(ddply(na.omit(pmall2011[,c("PM25","SiteCode"),with=F]),.(SiteCode),nrow))
table_temp<-table_temp[V1 > 364]
pmall2011 <- pmall2011[pmall2011$SiteCode %in% table_temp$SiteCode, ] 


#create PM matrix
pm.m <- makepointsmatrix(pmall2011, "long_pm", "lat_pm", "SiteCode")
#create aod matrix
mod3grid.se[, guidc := as.character(guid)]
#need to sort
setkey(mod3grid.se, guidc)
mod3.m <- makepointsmatrix(mod3grid.se[mod3grid.se[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

closestaodse<- nearestbyday(mod3.m ,pm.m , 
                            mod3grid.se, pmall2011 [, list(day,PM25,SiteCode)], 
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

saveRDS(closestaodse  , "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2011.rds")

mod3grid.se<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")

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

saveRDS(midmod3grid,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2011.rds")

#clear workspace
rm(list = ls())
gc()

