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

dust<-filter(data.m3,Dust ==1)
hist(dust$aod)


dust<-filter(data.m3,Dust ==0)
hist(dust$aod)




m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1C.PM25.AQ.rds")
#raw cor dust days
dust<-filter(m1.all,Dust ==1)
#for paper raw
m1_sc <- lm(PM25~ aod,data=dust)
dust[,pred.m1 := NULL]
dust$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=dust))$r.squared)

#raw non dust
ndust<-filter(m1.all,Dust ==0)
#for paper raw
m1_sc <- lm(PM25~ aod,data=ndust)
ndust[,pred.m1 := NULL]
ndust$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=ndust))$r.squared)



#raw non dust
ndust<-filter(m1.all,Dust ==0)
#for paper raw
m1_sc <- lm(PM25~ aod,data=ndust)
ndust[,pred.m1 := NULL]
ndust$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=ndust))$r.squared)



#reg per region
################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.all, c("reg5"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)


#Aeronet

library(data.table)
library(ggplot2)
library(mgcv)
library(reshape2)
# downloaded aeronet
aero2002 <- fread("/home/zeltak/ZH_tmp/030101_141231_Nes_Ziona/030101_141231_Nes_Ziona.lev20")
setnames(aero2002, names(aero2002)[1], c("aerodate"))
setnames(aero2002, "Water(cm)", c("aerowater"))
aero2002[, day := as.Date(strptime(aerodate, "%d:%m:%Y"))]
aero2002[, c := as.numeric(format(day, "%Y")) ]
aero.2004<-filter(aero2002, c == 2004)

# merge in to dat2004 to correlate with AOD
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
data.m3.2004 <-data.m3[c ==2004]
data.m3.2004 <- data.m3.2004[aodid %in% c("34.781-31.926", "34.791-21.917", "34.792-31.926","34.78-31.917"), ]
setkey(data.m3.2004,day)
setkey(aero.2004,day)
dat2004 <- merge(data.m3.2004, aero.2004[, list(day, Julian_Day, AOT_440)], by = "day", all.x = T)
# all points (to single aeronet site)
summary(lm(AOT_440 ~ aod, dat = dat2004, na.action = na.omit))

# merge all
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
data.m3 <- data.m3[aodid %in% c("34.781-31.926", "34.791-21.917", "34.792-31.926","34.78-31.917"), ]
setkey(data.m3,day)
setkey(aero2002,day)
dat <- merge(data.m3[, list(day,aodid,aod)], aero2002[, list(day, Julian_Day, AOT_440)], by = "day", all.x = T)
dat$aot<-as.numeric(dat$AOT_440)
# all points (to single aeronet site)
summary(lm(aot ~ aod, dat = dat, na.action = na.omit))
