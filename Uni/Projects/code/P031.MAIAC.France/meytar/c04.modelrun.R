#add all packages
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

###year 2003
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
res$type <- c("pm25")

#load data
#mod1 <-readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod1.AQ.2013.PMloc.rds")
summary(mod1)
#dim(mod1)
#clean data >> massimo script in stata 
names(mod1)

###base model
m1.raw.formula <- as.formula(pm25~ aod+(1+aod|day))


#example model
m1.formula <- as.formula(pm25 ~ aod
#temporal
+BLH+temp+ndvi+ws+TP
#LU
+PercRes+PercInd+PercUrbgr+PercTotbld+PercNat+PercAgr
#emis
+inds2010pm+emwo2010+emho2010+emag2010+traficpm
#traffc data
+nearmajorrd+roadsum+mjroadsum
#elevation
+elev+slope_200m+std_1km
#random component
+(1+aod+temp|day/climid))  
####
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)
mod1$pred.m1 <- predict(m1_sc)
res[res$type=="pm25", 'm1.R2'] <- print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)



#more parsimonius
#EXCLUDED:+PercRes +BLH +PercUrbgr+PercTotbld +inds2010pm+emwo2010+traficpm+roadsum +PercNat+emag2010

m1.formula <- as.formula(pm25 ~ aod
#temporal
+temp+ndvi+ws+TP
#LU
+PercInd+PercAgr
#emis
+emho2010
#traffc data
+nearmajorrd+mjroadsum
#elevation
+elev+slope_200m+std_1km
#random component
+(1+aod+temp|day/climid))  
####
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)
mod1$pred.m1 <- predict(m1_sc)
res[res$type=="pm25", 'm1.R2'] <- print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)


#more parsimonius with splines 
#EXCLUDED:+PercRes +BLH +PercUrbgr+PercTotbld +inds2010pm+emwo2010+traficpm+roadsum +PercNat+emag2010
m1.formula <- as.formula(pm25 ~ aod
#spline
#+ns(BLH,8)
#temporal
+temp+ndvi+ws+TP
#LU
+PercInd+PercAgr
#emis
+emho2010
#traffc data
+nearmajorrd+mjroadsum
#elevation
+elev+slope_200m+std_1km
#random component
+(1+aod+temp|day/climid))  
####
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)
mod1$pred.m1 <- predict(m1_sc)
res[res$type=="pm25", 'm1.R2'] <- print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)



#more parsimonius with splines 
#EXCLUDED:+PercRes +BLH +PercUrbgr+PercTotbld +inds2010pm+emwo2010+traficpm+roadsum +PercNat+emag2010
m1.formula <- as.formula(pm25 ~ aod
#spline
+ns(BLH,8)
#temporal
+temp+ndvi+ws+TP
#LU
+PercInd+PercAgr
#emis
+emho2010
#traffc data
+nearmajorrd+mjroadsum
#elevation
+elev+slope_200m+std_1km
#random component
+(1+aod+temp|day/climid))  
####
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)
mod1$pred.m1 <- predict(m1_sc)
res[res$type=="pm25", 'm1.R2'] <- print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
    #RMSPE
    res[res$type=="pm25", 'm1.rmspe'] <- print(rmse(residuals(m1_sc)))

    #spatial
    spatialall<-mod1 %>%
        group_by(SiteName) %>%
        dplyr::summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
    m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
    res[res$type=="pm25", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
    res[res$type=="pm25", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))


    #temporal
#temporal (take out daily PM from yearly mean)
tempoall<-left_join(mod1,spatialall)
    tempoall$delpm <-tempoall$pm25-tempoall$barpm
    tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
    mod_temporal <- lm(delpm ~ delpred, data=tempoall)
    res[res$type=="pm25", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
    

#save
    saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.predm1.rds")
    #save results
    saveRDS(res,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/results.AQ.2003.rds")


 #---------------->>>> CV
      #s1
      splits_s1 <- splitdf(mod1)
      test_s1 <- splits_s1$testset
      train_s1 <- splits_s1$trainset
      out_train_s1 <- lmer(m1.formula,data =  train_s1 )
      test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
      test_s1$iter<-"s1"
      #s2
      splits_s2 <- splitdf(mod1)
      test_s2 <- splits_s2$testset
      train_s2 <- splits_s2$trainset
      out_train_s2 <- lmer(m1.formula,data =  train_s2 )
      test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
      test_s2$iter<-"s2"
      #s3
      splits_s3 <- splitdf(mod1)
      test_s3 <- splits_s3$testset
      train_s3 <- splits_s3$trainset
      out_train_s3 <- lmer(m1.formula,data =  train_s3 )
      test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
      test_s3$iter<-"s3"
      #s4
      splits_s4 <- splitdf(mod1)
      test_s4 <- splits_s4$testset
      train_s4 <- splits_s4$trainset
      out_train_s4 <- lmer(m1.formula,data =  train_s4 )
      test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
      test_s4$iter<-"s4"
      #s5
      splits_s5 <- splitdf(mod1)
      test_s5 <- splits_s5$testset
      train_s5 <- splits_s5$trainset
      out_train_s5 <- lmer(m1.formula,data =  train_s5 )
      test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
      test_s5$iter<-"s5"
      #s6
      splits_s6 <- splitdf(mod1)
      test_s6 <- splits_s6$testset
      train_s6 <- splits_s6$trainset
      out_train_s6 <- lmer(m1.formula,data =  train_s6 )
      test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
      test_s6$iter<-"s6"
      #s7
      splits_s7 <- splitdf(mod1)
      test_s7 <- splits_s7$testset
      train_s7 <- splits_s7$trainset
      out_train_s7 <- lmer(m1.formula,data =  train_s7 )
      test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
      test_s7$iter<-"s7"
      #s8
      splits_s8 <- splitdf(mod1)
      test_s8 <- splits_s8$testset
      train_s8 <- splits_s8$trainset
      out_train_s8 <- lmer(m1.formula,data =  train_s8 )
      test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
      test_s8$iter<-"s8"
      #s9
      splits_s9 <- splitdf(mod1)
      test_s9 <- splits_s9$testset
      train_s9 <- splits_s9$trainset
      out_train_s9 <- lmer(m1.formula,data =  train_s9 )
      test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
      test_s9$iter<-"s9"
      #s10
      splits_s10 <- splitdf(mod1)
      test_s10 <- splits_s10$testset
      train_s10 <- splits_s10$trainset
      out_train_s10 <- lmer(m1.formula,data =  train_s10 )
      test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
      test_s10$iter<-"s10"

      #BIND 1 dataset
      mod1.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
      #save
      #saveRDS(mod1.cv,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.CV.rds")
      # cleanup (remove from WS) objects from CV
      rm(list = ls(pattern = "train_|test_"))
      #table updates
      m1.fit.all.cv<-lm(pm25~pred.m1.cv,data=mod1.cv)
      res[res$type=="pm25", 'm1cv.R2'] <- print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$r.squared)
      res[res$type=="pm25", 'm1cv.I'] <-print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[1,1])
      res[res$type=="pm25", 'm1cv.Ise'] <-print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[1,2])
      res[res$type=="pm25", 'm1cv.slope'] <-print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[2,1])
      res[res$type=="pm25", 'm1cv.slopese'] <-print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[2,2])
      #RMSPE
      res[res$type=="pm25", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))
      #spatial
      spatialall.cv<-mod1.cv %>%
          group_by(SiteName) %>%
          summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
      m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
      res[res$type=="pm25", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
      res[res$type=="pm25", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
      #temporal
      tempoall.cv<-left_join(mod1.cv,spatialall.cv)
      tempoall.cv$delpm <-tempoall.cv$pm25-tempoall.cv$barpm
      tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
      mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
      res[res$type=="pm25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

      #save results
      saveRDS(res,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/results.mod1.AQ.2003.rds")



#local stage
head(mod1)

#xplore svm
library(e1071)
 mod1.cv$res.m1<- mod1.cv$pm25-mod1.cv$pred.m1.cv
tryx <-svm(res.m1~ agfo2010+hoco2010+indu2010+traf2010+wood2010+MAJRDS_EU+ROADS_EU+ndviloc+elevloc+BLH+temp+ws+TP ,type="nu-regression",cross=10,data=mod1.cv)
mod1.cv$predsvmloc <- predict(object=tryx) #morning


## reg
mod1.cv$pred.m1.bothsvm <- mod1.cv$pred.m1 + mod1.cv$predsvmloc
res[res$type=="PM25", 'm1cvloc.R2'] <- print(summary(lm(pm25~pred.m1.bothsvm,data=mod1.cv))$r.squared)
res[res$type=="PM25", 'm1cvloc.Ise'] <-print(summary(lm(pm25~pred.m1.bothsvm,data=mod1.cv))$coef[1,2])
res[res$type=="PM25", 'm1cvloc.slope'] <-print(summary(lm(pm25~pred.m1.bothsvm,data=mod1.cv))$coef[2,1])
res[res$type=="PM25", 'm1cvloc.slopese'] <-print(summary(lm(pm25~pred.m1.bothsvm,data=mod1.cv))$coef[2,2])
#RMSPE
m1.fit.all.loc<-lm(pm25~pred.m1.bothsvm,data=mod1.cv)
res[res$type=="PM25", 'm1cvloc.rmspe'] <- print(rmse(residuals(m1.fit.all.loc)))

#spatial
spatialall.cv.loc<-mod1.cv %>%
    group_by(SiteName) %>%
    summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1.bothsvm, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM25", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(mod1.cv,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$pm25-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM25", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)
#saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resm1.AQ.PM25.rds")







### mod 2 (around 2-4 h)

mod2 <- readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod2.AQ.2013.rds")
mod2<-filter(mod2,ndvi > 0)
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
setkey(mod2,day, aodid)
mod2<-mod2[!is.na(meanPM25)]
mod2[, m := as.numeric(format(day, "%m")) ]
mod2[, bimon := (m + 1) %/% 2]
summary(mod2$pred.m2)
gc()
mod2 <- select(mod2,day,aodid,m,meanPM25,long_aod,lat_aod,bimon,pred.m2,aod)
saveRDS(mod2,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod2.AQ.2013.PM.predm2.rds")
#keep(mod2,res,rmse,splitdf, sure=TRUE) 




#run lme regression, this *should* include the thin plate spline yet will not run (computational limitations) thus we break it down into 2 components  
m2.smooth = lme(pred.m2 ~ meanPM25,random = list(aodid= ~1 + meanPM25),control=lmeControl(opt = "optim"), data= mod2 )
#correlate to see everything from mod2 and the mpm works
mod2[, pred.t31 := predict(m2.smooth)]
mod2[, resid  := residuals(m2.smooth)]
#check R2 
print(summary(lm(pred.m2~pred.t31,data=mod2))$r.squared)


#split the files to the separate bi monthly data sets
Tall_bimon1 <- subset(mod2 ,mod2$bimon == "1")
Tall_bimon2 <- subset(mod2 ,mod2$bimon == "2")
Tall_bimon3 <- subset(mod2 ,mod2$bimon == "3")
Tall_bimon4 <- subset(mod2 ,mod2$bimon == "4")
Tall_bimon5 <- subset(mod2 ,mod2$bimon == "5")
Tall_bimon6 <- subset(mod2 ,mod2$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
mod2$pred.t32 <- c(Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(mod2,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM25 ,random = list(aodid= ~1 + meanPM25 ),control=lmeControl(opt = "optim"),data= mod2  )
mod2[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=mod2))$r.squared) 



#mod 3 (5-8 h)
mod3 <- readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.AQ.2013.rds")
mod3[, m := as.numeric(format(day, "%m")) ]
mod3 <- select(mod3,day,aodid,m,meanPM25,long_aod,lat_aod)
mod3[, bimon := (m + 1) %/% 2]
setkey(mod3,day, aodid)
mod3<-mod3[!is.na(meanPM25)]
#generate m.3 mix model  predictions 
mod3$pred.m3.mix <-  predict(Final_pred_all,mod3)

#create unique grid
ugrid <-mod3 %>%
    group_by(aodid) %>%
    summarise(long_aod = mean(long_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 

#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
mod3_bimon1 <- mod3[bimon == 1, ]
mod3_bimon2 <- mod3[bimon == 2, ]
mod3_bimon3 <- mod3[bimon == 3, ]
mod3_bimon4 <- mod3[bimon == 4, ]
mod3_bimon5 <- mod3[bimon == 5, ]
mod3_bimon6 <- mod3[bimon == 6, ]


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
setkey(uniq_gid_bimon1,aodid)
setkey(mod3_bimon1,aodid)
mod3_bimon1 <- merge(mod3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(mod3_bimon2,aodid)
mod3_bimon2 <- merge(mod3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(mod3_bimon3,aodid)
mod3_bimon3 <- merge(mod3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(mod3_bimon4,aodid)
mod3_bimon4 <- merge(mod3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(mod3_bimon5,aodid)
mod3_bimon5 <- merge(mod3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(mod3_bimon6,aodid)
mod3_bimon6 <- merge(mod3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(mod3_bimon1,mod3_bimon2,mod3_bimon3,mod3_bimon4,mod3_bimon5,mod3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
summary(mod3$pred.m3)
saveRDS(mod3,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod3.pred.AQ.2003.rds")
keep(data.m3,mod3,res,rmse, sure=TRUE) 
gc()

#calculate stage 3 R2- CV ten folds approach will take 6 weeks...we don't currently do CV for stage 3.

mod1 <-readRDS("/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod1.AQ.2003.PM25.predm1.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","pm25","pred.m1","SiteName"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(pm25~pred.m3,data=mod1))
res[res$type=="pm25", 'm3.R2'] <- print(summary(lm(pm25~pred.m3,data=mod1))$r.squared)    
res[res$type=="pm25", 'm3.I'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[1,1])
res[res$type=="pm25", 'm3.Ise'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[1,2])
res[res$type=="pm25", 'm3.slope'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[2,1])
res[res$type=="pm25", 'm3.slopese'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[2,2])
#RMSPE
res[res$type=="pm25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-mod1 %>%
    group_by(SiteName) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="pm25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="pm25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$pm25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="pm25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
saveRDS(res, "/media/NAS/Uni/Projects/P059_SWISS_AOD/work/resALL.AQ.2003.PM25.rds")



#create final prediction data set for use in health outcome studies

#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P059_SWISS_AOD/work/mod2.AQ.2013.PM.predm2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, long_aod, lat_aod, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1[,list(aodid,day,pred.m1,pm25)], all.x = T,allow.cartesian = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
summary(mod3best$bestpred)
mod3best[bestpred < 0 , bestpred  := 0.5]
mod3best<-select(mod3best,day,aodid,long_aod,lat_aod,bestpred)
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/bestpred.AQ.2003.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))

#save for plotting in QGIS
out <- mod3best %>% group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), bestpred=mean(bestpred, na.rm=TRUE))
out<-na.omit(out)
write.csv(out,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/map.bestpred.AQ.2003.PM25.csv")
#save res
saveRDS(res,"/media/NAS/Uni/Projects/P059_SWISS_AOD/work/results.AQ.2003.rds")

keep(rmse,splitdf, sure=TRUE) 
gc()
