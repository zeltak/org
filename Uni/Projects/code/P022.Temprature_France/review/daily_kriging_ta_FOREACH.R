# library(lme4)
# library(reshape)
# library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
# library(data.table)
library(reshape2)
# library(Hmisc)
# library(mgcv)
# library(gdata)
library(readr)
library(gstat)
library(automap)
library(magrittr)
library(foreach)
library(doMC)
registerDoMC(20)

lct = Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)

#setwd("/home/michael/Dropbox/BGU/Itai/daily_kriging")
#dat <- readRDS("mod2005.rds")
dat <- readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099_review/mod2005.rds")

dat = as.data.frame(dat)
# dat$date %<>% tolower %>% as.Date("%d%b%Y")
head(dat)
coordinates(dat) = ~ Longitude + Latitude
#define cord system- longlat is WGS84 unprojected
proj4string(dat) = "+proj=longlat"
# for the loop to null final

#start 11:20
# Small subset
# dat = dat[dat$date %in% unique(dat$date)[1:3], ]

final = foreach(i=unique(dat$date)) %dopar% {
  
  dat1 = dat[dat$date == i, ]
  dat1 = dat1[, c("num_insee", "tm", "elev_m", "pcturb", "NDVI")]
  dat1 = dat1[complete.cases(dat1@data), ]
  
  v = autofitVariogram(tm ~ elev_m + pcturb + NDVI, dat1)
  g = gstat(formula = tm ~ elev_m + pcturb + NDVI, model = v$var_model, data = dat1)
  # use CV LOOCV
  cv = gstat.cv(g)
  
  data.frame(
    lon = coordinates(dat1)[, 1],
    lat = coordinates(dat1)[, 2],
    date = i,
    obs = cv$observed,
    pred = cv$var1.pred,
    num_insee=dat1$num_insee
  )
  
}

final = do.call(rbind, final)




#create CV table
res <- data.frame(type=character(10), R2=numeric(10),Bias=numeric(10),RMSPE=numeric(10),s.r2=numeric(10),s.RMSPE=numeric(10),t.r2=numeric(10))
#name columns
res$type <- c("overall" , "mountain climate", "ocianic climate", "degraded ocianic climate", "mediterranean climate", "continental climate", "winter","summer","urban","rural")

saveRDS(final, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099_review/cv_result_foreach.rds")
final<-read_rds("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099_review/cv_result_foreach.rds")
#import back climate zone
final<-as.data.table(final)
climz<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metstnXY._czone.csv")
climz$id<-NULL
setkey(final,num_insee)
setkey(climz,num_insee)
final <- merge(final,climz[,list(num_insee,cid)], all.x = T)
m1.all.cv<-final
setnames(m1.all.cv,"obs","tm")
setnames(m1.all.cv,"pred","pred.m1.cv")





#overall
m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv)
res[res$type=="overall", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="overall", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv))$coef[2,1])
#RMSPE
res[res$type=="overall", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="overall", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="overall", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="overall", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)




#mountain climate

m1.all.cv.c1<-filter(m1.all.cv,cid==1)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c1)
res[res$type=="mountain climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c1))$r.squared)
res[res$type=="mountain climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c1))$coef[2,1])
#RMSPE
res[res$type=="mountain climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c1 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="mountain climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="mountain climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c1,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="mountain climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)




#ocianic climate

m1.all.cv.c2<-filter(m1.all.cv,cid==2)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c2)
res[res$type=="ocianic climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c2))$r.squared)
res[res$type=="ocianic climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c2))$coef[2,1])
#RMSPE
res[res$type=="ocianic climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c2 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="ocianic climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="ocianic climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c2,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="ocianic climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)




#degraded ocianic climate

m1.all.cv.c3<-filter(m1.all.cv,cid==3)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c3)
res[res$type=="degraded ocianic climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c3))$r.squared)
res[res$type=="degraded ocianic climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c3))$coef[2,1])
#RMSPE
res[res$type=="degraded ocianic climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c3 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="degraded ocianic climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="degraded ocianic climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c3,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="degraded ocianic climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#mediterranean climate

m1.all.cv.c4<-filter(m1.all.cv,cid==4)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c4)
res[res$type=="mediterranean climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c4))$r.squared)
res[res$type=="mediterranean climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c4))$coef[2,1])
#RMSPE
res[res$type=="mediterranean climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c4 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="mediterranean climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="mediterranean climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c4,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="mediterranean climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#continental climate

m1.all.cv.c5<-filter(m1.all.cv,cid==5)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c5)
res[res$type=="continental climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c5))$r.squared)
res[res$type=="continental climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c5))$coef[2,1])
#RMSPE
res[res$type=="continental climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c5 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="continental climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="continental climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c5,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="continental climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)








