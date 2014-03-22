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
library(sqldf)

#met
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/pm_all_complete.csv")
pmid <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid_mpmid.csv")
setkey(pm ,SiteCode)
setkey(pmid,SiteCode)
pmall <- merge(pm,pmid)
#convert date from 01JAN2000 format
pmall <- pmall [, day:=as.Date(strptime(Date,"%d%b%Y"))]
pmall[, c := as.numeric(format(day, "%Y")) ]



###############
#2003
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2003.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2003.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2003<- pmall[c==2003]


setkey(pmall2003,day,mpmid)
mpm_agg <- (pmall2003[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2003.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2003.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2003.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)

nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")

keep(pmall, sure=TRUE) 
gc()




###############
#2004
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2004.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2004.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2004<- pmall[c==2004]


setkey(pmall2004,day,mpmid)
mpm_agg <- (pmall2004[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2004.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2004.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2004.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)

nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2004.rds")

keep(pmall, sure=TRUE) 
gc()




###############
#2005
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2005.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2005.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2005<- pmall[c==2005]


setkey(pmall2005,day,mpmid)
mpm_agg <- (pmall2005[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2005.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2005.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2005.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)

nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2005.rds")

keep(pmall, sure=TRUE) 
gc()




###############
#2006
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2006.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2006.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2006<- pmall[c==2006]


setkey(pmall2006,day,mpmid)
mpm_agg <- (pmall2006[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2006.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2006.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2006.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)

nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2006.rds")

keep(pmall, sure=TRUE) 
gc()



###############
#2007
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2007.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2007.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2007<- pmall[c==2007]


setkey(pmall2007,day,mpmid)
mpm_agg <- (pmall2007[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2007.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2007.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2007.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)

nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2007.rds")

keep(pmall, sure=TRUE) 
gc()

