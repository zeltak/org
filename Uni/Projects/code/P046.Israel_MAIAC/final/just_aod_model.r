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


m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM25.rds")
m1.all$aodid<-paste(m1.all$long_aod,m1.all$lat_aod,sep="-")
badaod<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/bad_AOD_IL.csv")
badaod<-select(badaod,aodid)
m1.all <- m1.all[!m1.all$aodid %in% badaod$aodid]


#for paper noaod
m1.formula <- as.formula(PM25~ tempa.s+pbldag+elev.s+tden.s+pden.s+ndvi.s +p_os.s+dist2water.s +(1|day))
m1_sc <- lmer(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#spatial
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)



#for paper noaod
m1.formula <- as.formula(PM25~ tempa.s+pbldag+elev.s+tden.s+pden.s+ndvi.s +p_os.s+dist2water.s)
m1_sc <- lm(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#spatial
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)






#just aod
m1.formula <- as.formula(PM25~ aod +(1+aod|day))
m1_sc <- lmer(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#spatial
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
print(rmse(residuals(m1_sc)))




#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)

#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)