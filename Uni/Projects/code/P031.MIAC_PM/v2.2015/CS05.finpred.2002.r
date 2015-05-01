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
mod3best[bestpred  <= 0 , bestpred  := 0.1]
summary(mod3best$bestpred)


#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.PM25.bestpred.rds")

mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=GUID], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.PM25.LTPM.csv", row.names = F)

#export res to csv

write.csv(res,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ.PM25.csv")


#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by GUID)
                        utmy = y_aod_ITM[1]),by = GUID])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
