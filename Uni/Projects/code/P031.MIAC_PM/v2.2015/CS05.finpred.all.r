
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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2000.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2000.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2000.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2000.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2000.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2000.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2000.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2000.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2001.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2001.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2001.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2001.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2001.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2001.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2001.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2001.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2002.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2002.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2002.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2002.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2002.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2002.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2002.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2002.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2003.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2003.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2003.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2003.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2003.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2003.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2003.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2003.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2004.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2004.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2004.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2004.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2004.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2004.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2004.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2004.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2005.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2005.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2005.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2005.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2005.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2005.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2005.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2005.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2006.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2006.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2006.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2006.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2006.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2006.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2006.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2006.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2007.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2007.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2007.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2007.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2007.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2007.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2007.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2007.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2008.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2008.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2008.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2008.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2008.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2008.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2008.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2008.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2009.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2009.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2009.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2009.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2009.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2009.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2009.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2009.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2010.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2010.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2010.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2010.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2010.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2010.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2010.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2010.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2011.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2011.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2011.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2011.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2011.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2011.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2011.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2011.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2012.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2012.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2012.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2012.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2012.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T,allow.cartesian=T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2012.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2012.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2012.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2013.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2013.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2013.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2013.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2013.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T,allow.cartesian=T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2013.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2013.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2013.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()



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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")



mod3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2014.pred3.rds")
res<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2014.rds")


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2014.rds")
setnames(m1.all,"closest","GUID")
m1.all<-m1.all[,c("GUID","day","PM25","pred.m1","SiteCode","Year"),with=FALSE]
#R2.m3
setkey(mod3,day,GUID)
setkey(m1.all,day,GUID)
m1.all <- merge(m1.all,mod3[, list(day,GUID,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM25", 'm3.I'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM25", 'm3.Ise'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM25", 'm3.slope'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM25", 'm3.slopese'] <-print(summary(lm(PM25~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2014.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2014.rds")
mod2<-mod2[,c("GUID","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(GUID, Long, Lat, day, pred.m3)]
setkey(mod3best, day, GUID)
setkey(mod2, day, GUID)
mod3best <- merge(mod3best, mod2[,list(GUID, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("GUID","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,GUID)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#recode negative
mod3best[bestpred  < 0 , bestpred  := 0.1]
summary(mod3best$bestpred)

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.PM25.bestpred.2014.rds")
#clean for maps
mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          Long =  Long[1], Lat = Lat[1]),by=GUID], "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2014.csv", row.names = F)

#export res to csv
write.csv(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/resALL.AQ.PM25.2014.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = Long[1], #use the first long and lat (by GUID)
                        utmy = Lat[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

rm(list = ls(all = TRUE))
gc()


