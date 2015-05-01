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


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2006.rds")

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(m1_sc)))

#spatial
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(m1.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2006.rds")

#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
saveRDS(m1.all.cv,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1cv.TR.PM25.2006.rds")
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

gc()

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2006.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.rds")


#-------------->prepare for mod3
m2.all[, bimon := (Month + 1) %/% 2]
setkey(m2.all,day, GUID)
m2.all.2006<-m2.all[!is.na(meanPM)]
rm(m2.all)
gc()

#2006
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth <- lme(pred.m2 ~ meanPM,random = list(GUID= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2006 )
#correlate to see everything from mod2 and the mpm works
m2.all.2006[, pred.t31 := predict(m2.smooth)]
m2.all.2006[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2006))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2006 ,m2.all.2006$bimon == "1")
Tall_bimon2 <- subset(m2.all.2006 ,m2.all.2006$bimon == "2")
Tall_bimon3 <- subset(m2.all.2006 ,m2.all.2006$bimon == "3")
Tall_bimon4 <- subset(m2.all.2006 ,m2.all.2006$bimon == "4")
Tall_bimon5 <- subset(m2.all.2006 ,m2.all.2006$bimon == "5")
Tall_bimon6 <- subset(m2.all.2006 ,m2.all.2006$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2006$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2006,day, GUID)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(GUID= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2006  )
m2.all.2006[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2006))$r.squared) 

#------------------------>>>foo
#import mod3 
data.m3.2006  <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2006.rds")
#for PM25
data.m3.2006 <- select(data.m3.2006,day,GUID,Month,meanPM,Long,Lat)
data.m3.2006[, bimon := (Month + 1) %/% 2]
setkey(data.m3.2006,day, GUID)
data.m3.2006<-data.m3.2006[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2006$pred.m3.mix <-  predict(Final_pred_all,data.m3.2006)

#create unique grid
ugrid <-data.m3.2006 %>%
    group_by(GUID) %>%
    summarise(Long = mean(Long, na.rm=TRUE),  Lat = mean(Lat, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2006_bimon1 <- data.m3.2006[bimon == 1, ]
data.m3.2006_bimon2 <- data.m3.2006[bimon == 2, ]
data.m3.2006_bimon3 <- data.m3.2006[bimon == 3, ]
data.m3.2006_bimon4 <- data.m3.2006[bimon == 4, ]
data.m3.2006_bimon5 <- data.m3.2006[bimon == 5, ]
data.m3.2006_bimon6 <- data.m3.2006[bimon == 6, ]


#addin unique grid to each bimon           
uniq_gid_bimon1 <- ugrid
uniq_gid_bimon2 <- ugrid
uniq_gid_bimon3 <- ugrid
uniq_gid_bimon4 <- ugrid
uniq_gid_bimon5 <- ugrid
uniq_gid_bimon6 <- ugrid

#get predictions for Bimon residuals
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,GUID)
setkey(data.m3.2006_bimon1,GUID)
data.m3.2006_bimon1 <- merge(data.m3.2006_bimon1, uniq_gid_bimon1[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon2,GUID)
setkey(data.m3.2006_bimon2,GUID)
data.m3.2006_bimon2 <- merge(data.m3.2006_bimon2, uniq_gid_bimon2[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon3,GUID)
setkey(data.m3.2006_bimon3,GUID)
data.m3.2006_bimon3 <- merge(data.m3.2006_bimon3, uniq_gid_bimon3[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon4,GUID)
setkey(data.m3.2006_bimon4,GUID)
data.m3.2006_bimon4 <- merge(data.m3.2006_bimon4, uniq_gid_bimon4[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon5,GUID)
setkey(data.m3.2006_bimon5,GUID)
data.m3.2006_bimon5 <- merge(data.m3.2006_bimon5, uniq_gid_bimon5[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon6,GUID)
setkey(data.m3.2006_bimon6,GUID)
data.m3.2006_bimon6 <- merge(data.m3.2006_bimon6, uniq_gid_bimon6[,list(GUID,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2006_bimon1,data.m3.2006_bimon2,data.m3.2006_bimon3,data.m3.2006_bimon4,data.m3.2006_bimon5,data.m3.2006_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2006.pred3.rds")
saveRDS(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2006.rds")

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


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2007.rds")

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(m1_sc)))

#spatial
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(m1.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2007.rds")

#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
saveRDS(m1.all.cv,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1cv.TR.PM25.2007.rds")
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

gc()

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2007.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.rds")


#-------------->prepare for mod3
m2.all[, bimon := (Month + 1) %/% 2]
setkey(m2.all,day, GUID)
m2.all.2007<-m2.all[!is.na(meanPM)]
rm(m2.all)
gc()

#2007
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth <- lme(pred.m2 ~ meanPM,random = list(GUID= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2007 )
#correlate to see everything from mod2 and the mpm works
m2.all.2007[, pred.t31 := predict(m2.smooth)]
m2.all.2007[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2007))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2007 ,m2.all.2007$bimon == "1")
Tall_bimon2 <- subset(m2.all.2007 ,m2.all.2007$bimon == "2")
Tall_bimon3 <- subset(m2.all.2007 ,m2.all.2007$bimon == "3")
Tall_bimon4 <- subset(m2.all.2007 ,m2.all.2007$bimon == "4")
Tall_bimon5 <- subset(m2.all.2007 ,m2.all.2007$bimon == "5")
Tall_bimon6 <- subset(m2.all.2007 ,m2.all.2007$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2007$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2007,day, GUID)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(GUID= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2007  )
m2.all.2007[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2007))$r.squared) 

#------------------------>>>foo
#import mod3 
data.m3.2007  <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2007.rds")
#for PM25
data.m3.2007 <- select(data.m3.2007,day,GUID,Month,meanPM,Long,Lat)
data.m3.2007[, bimon := (Month + 1) %/% 2]
setkey(data.m3.2007,day, GUID)
data.m3.2007<-data.m3.2007[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2007$pred.m3.mix <-  predict(Final_pred_all,data.m3.2007)

#create unique grid
ugrid <-data.m3.2007 %>%
    group_by(GUID) %>%
    summarise(Long = mean(Long, na.rm=TRUE),  Lat = mean(Lat, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2007_bimon1 <- data.m3.2007[bimon == 1, ]
data.m3.2007_bimon2 <- data.m3.2007[bimon == 2, ]
data.m3.2007_bimon3 <- data.m3.2007[bimon == 3, ]
data.m3.2007_bimon4 <- data.m3.2007[bimon == 4, ]
data.m3.2007_bimon5 <- data.m3.2007[bimon == 5, ]
data.m3.2007_bimon6 <- data.m3.2007[bimon == 6, ]


#addin unique grid to each bimon           
uniq_gid_bimon1 <- ugrid
uniq_gid_bimon2 <- ugrid
uniq_gid_bimon3 <- ugrid
uniq_gid_bimon4 <- ugrid
uniq_gid_bimon5 <- ugrid
uniq_gid_bimon6 <- ugrid

#get predictions for Bimon residuals
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,GUID)
setkey(data.m3.2007_bimon1,GUID)
data.m3.2007_bimon1 <- merge(data.m3.2007_bimon1, uniq_gid_bimon1[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon2,GUID)
setkey(data.m3.2007_bimon2,GUID)
data.m3.2007_bimon2 <- merge(data.m3.2007_bimon2, uniq_gid_bimon2[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon3,GUID)
setkey(data.m3.2007_bimon3,GUID)
data.m3.2007_bimon3 <- merge(data.m3.2007_bimon3, uniq_gid_bimon3[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon4,GUID)
setkey(data.m3.2007_bimon4,GUID)
data.m3.2007_bimon4 <- merge(data.m3.2007_bimon4, uniq_gid_bimon4[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon5,GUID)
setkey(data.m3.2007_bimon5,GUID)
data.m3.2007_bimon5 <- merge(data.m3.2007_bimon5, uniq_gid_bimon5[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon6,GUID)
setkey(data.m3.2007_bimon6,GUID)
data.m3.2007_bimon6 <- merge(data.m3.2007_bimon6, uniq_gid_bimon6[,list(GUID,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2007_bimon1,data.m3.2007_bimon2,data.m3.2007_bimon3,data.m3.2007_bimon4,data.m3.2007_bimon5,data.m3.2007_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2007.pred3.rds")
saveRDS(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2007.rds")

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


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2009.rds")

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("PM25")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM25", 'm1.rmspe'] <- print(rmse(residuals(m1_sc)))

#spatial
spatialall<-m1.all %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(m1.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1C.TR.PM25.pred.2009.rds")

#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
saveRDS(m1.all.cv,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1cv.TR.PM25.2009.rds")
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM25", 'm1cv.Ise'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM25", 'm1cv.slope'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM25", 'm1cv.slopese'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(SiteCode) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

gc()

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2009.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.rds")


#-------------->prepare for mod3
m2.all[, bimon := (Month + 1) %/% 2]
setkey(m2.all,day, GUID)
m2.all.2009<-m2.all[!is.na(meanPM)]
rm(m2.all)
gc()

#2009
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth <- lme(pred.m2 ~ meanPM,random = list(GUID= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2009 )
#correlate to see everything from mod2 and the mpm works
m2.all.2009[, pred.t31 := predict(m2.smooth)]
m2.all.2009[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2009))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2009 ,m2.all.2009$bimon == "1")
Tall_bimon2 <- subset(m2.all.2009 ,m2.all.2009$bimon == "2")
Tall_bimon3 <- subset(m2.all.2009 ,m2.all.2009$bimon == "3")
Tall_bimon4 <- subset(m2.all.2009 ,m2.all.2009$bimon == "4")
Tall_bimon5 <- subset(m2.all.2009 ,m2.all.2009$bimon == "5")
Tall_bimon6 <- subset(m2.all.2009 ,m2.all.2009$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(Long,Lat),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2009$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2009,day, GUID)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(GUID= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2009  )
m2.all.2009[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2009))$r.squared) 

#------------------------>>>foo
#import mod3 
data.m3.2009  <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.Tr.2009.rds")
#for PM25
data.m3.2009 <- select(data.m3.2009,day,GUID,Month,meanPM,Long,Lat)
data.m3.2009[, bimon := (Month + 1) %/% 2]
setkey(data.m3.2009,day, GUID)
data.m3.2009<-data.m3.2009[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2009$pred.m3.mix <-  predict(Final_pred_all,data.m3.2009)

#create unique grid
ugrid <-data.m3.2009 %>%
    group_by(GUID) %>%
    summarise(Long = mean(Long, na.rm=TRUE),  Lat = mean(Lat, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2009_bimon1 <- data.m3.2009[bimon == 1, ]
data.m3.2009_bimon2 <- data.m3.2009[bimon == 2, ]
data.m3.2009_bimon3 <- data.m3.2009[bimon == 3, ]
data.m3.2009_bimon4 <- data.m3.2009[bimon == 4, ]
data.m3.2009_bimon5 <- data.m3.2009[bimon == 5, ]
data.m3.2009_bimon6 <- data.m3.2009[bimon == 6, ]


#addin unique grid to each bimon           
uniq_gid_bimon1 <- ugrid
uniq_gid_bimon2 <- ugrid
uniq_gid_bimon3 <- ugrid
uniq_gid_bimon4 <- ugrid
uniq_gid_bimon5 <- ugrid
uniq_gid_bimon6 <- ugrid

#get predictions for Bimon residuals
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,GUID)
setkey(data.m3.2009_bimon1,GUID)
data.m3.2009_bimon1 <- merge(data.m3.2009_bimon1, uniq_gid_bimon1[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon2,GUID)
setkey(data.m3.2009_bimon2,GUID)
data.m3.2009_bimon2 <- merge(data.m3.2009_bimon2, uniq_gid_bimon2[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon3,GUID)
setkey(data.m3.2009_bimon3,GUID)
data.m3.2009_bimon3 <- merge(data.m3.2009_bimon3, uniq_gid_bimon3[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon4,GUID)
setkey(data.m3.2009_bimon4,GUID)
data.m3.2009_bimon4 <- merge(data.m3.2009_bimon4, uniq_gid_bimon4[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon5,GUID)
setkey(data.m3.2009_bimon5,GUID)
data.m3.2009_bimon5 <- merge(data.m3.2009_bimon5, uniq_gid_bimon5[,list(GUID,gpred)], all.x = T)
setkey(uniq_gid_bimon6,GUID)
setkey(data.m3.2009_bimon6,GUID)
data.m3.2009_bimon6 <- merge(data.m3.2009_bimon6, uniq_gid_bimon6[,list(GUID,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2009_bimon1,data.m3.2009_bimon2,data.m3.2009_bimon3,data.m3.2009_bimon4,data.m3.2009_bimon5,data.m3.2009_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/mod3.TR.PM25.2009.pred3.rds")
saveRDS(res,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/res.2009.rds")
rm(list = ls(all = TRUE))
gc()







