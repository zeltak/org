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
#2008
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2008.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2008.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2008<- pmall[c==2008]


setkey(pmall2008,day,mpmid)
mpm_agg <- (pmall2008[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2008.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2008.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2008.rds")

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

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2008.rds")

keep(pmall, sure=TRUE) 
gc()



###############
#2009
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2009.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2009.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2009<- pmall[c==2009]


setkey(pmall2009,day,mpmid)
mpm_agg <- (pmall2009[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2009.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2009.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2009.rds")

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

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2009.rds")

keep(pmall, sure=TRUE) 
gc()




###############
#2010
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2010.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2010.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2010<- pmall[c==2010]


setkey(pmall2010,day,mpmid)
mpm_agg <- (pmall2010[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2010.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2010.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2010.rds")

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

saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2010.rds")

keep(pmall, sure=TRUE) 
gc()




###############
#2011
################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2011.rds")
mod3grid.se<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mod3fullgrid2011.rds")

setkey(mpmg,guid,day)
setkey(mod3grid.se,guid,day)
mod3grid <- merge(mod3grid.se,mpmg,allow.cartesian=TRUE,all.x = T)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")

setkey(basegrid,guid)
setkey(mod3grid,guid)
midmod3grid <- merge(mod3grid,basegrid[,list(guid, mpmid)],all.x = T)



#xtract year PM
pmall2011<- pmall[c==2011]

summary(pmall2011)

setkey(pmall2011,day,mpmid)
mpm_agg <- (pmall2011[, list(ampm =mean(PM25, na.rm = TRUE), 
                             long_pm = long_pm[1],
                             lat_pm = lat_pm [1]),
                      by = list(day,mpmid)])  

# saveRDS(mpm_aggs,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpm_aggs2011.rds")
# saveRDS(midmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2011.rds")
#midmod3grid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/midmod3grid2011.rds")

setkey(midmod3grid,day,mpmid)
setkey(mpm_agg,day,mpmid)
nmidmod3grid <- merge(midmod3grid,mpm_agg,all.x=TRUE)
summary(nmidmod3grid)


nmidmod3grid [,bestmpm := meanPMmean]
nmidmod3grid [!is.na(ampm),bestmpm  := ampm]
nmidmod3grid[,mpmid:=NULL]
nmidmod3grid[, meanPMmean  :=NULL]
nmidmod3grid[, ampm  :=NULL]
nmidmod3grid[,  long_pm :=NULL]
nmidmod3grid[,  lat_pm :=NULL]
nmidmod3grid[,  xalb :=NULL]
nmidmod3grid[,  yalb :=NULL]


saveRDS(nmidmod3grid,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2011.rds")

keep(pmall, sure=TRUE) 
gc()


#plot missings

# test<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2008.rds")
# try2 <-test[is.na(bestmpm) , ]
# names(try2)
# try2$d<-1
# 
# ggplot(try2, aes(long_aod, lat_aod, color = d)) + 
#   geom_point(size = 5, shape = 15) + 
#   #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
#   xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
#   scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
#   theme_bw() + 
#   ggtitle("Long term predictions")
# 

