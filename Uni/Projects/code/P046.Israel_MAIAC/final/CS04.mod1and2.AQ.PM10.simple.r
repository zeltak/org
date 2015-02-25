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


m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM10.rds")
m1.all$aodid<-paste(m1.all$long_aod,m1.all$lat_aod,sep="-")
badaod<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/bad_AOD_IL.csv")
badaod<-select(badaod,aodid)
m1.all <- m1.all[!m1.all$aodid %in% badaod$aodid]


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
res$type <- c("PM10")

#BEST SO FAR
m1.formula <- as.formula(PM10~ aod+tempa.s
                        +pbldag
                        +RHa.s+O3a.s+Raina.s+NOa.s 
                        +elev.s+tden.s+pden.s+ndvi.s +p_os.s
                        #+dist2rail.s 
                        +dist2water.s 
                        #+dist2A1.s+Dist2road.s
                       #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  
#                        +aod*lat_aod.x
#                        +Dust*lat_aod.x
#                       +pbldag*lat_aod.x
                      +(1+aod|day/reg_num)) 

m1_sc <- lmer(m1.formula,data=m1.all,weights=normwt)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
res[res$type=="PM10", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM10", 'm1.rmspe'] <- print(rmse(residuals(m1_sc)))

#spatial
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM10", 'm1.R2.space'] <-print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM10", 'm1.rmspe.space'] <- print(rmse(residuals(m1.fit.all.s)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM10", 'm1.R2.time']<- print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(m1.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM10.pred.rds")

#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
#m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s8,test_s9))
saveRDS(m1.all.cv,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1cv.AQ.PM10.rds")
# cleanup (remove from WS) objects from CV
#rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM10~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM10", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM10", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM10", 'm1cv.Ise'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM10", 'm1cv.slope'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM10", 'm1cv.slopese'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM10", 'm1cv.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM10", 'm1cv.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM10", 'm1cv.rmspe.space'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM10-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM10", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


# #by season
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[seasonSW==1]))$r.squared)
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[seasonSW==2]))$r.squared)
# # seasons
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[season==1]))$r.squared)
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[season==2]))$r.squared)
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[season==3]))$r.squared)
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[season==4]))$r.squared)
# print(summary(lm(PM10~pred.m1.cv,data=m1.all.cv[Dust==1]))$r.squared)


### alternate LOOCV

# # cross-validation and model building
# # repeated leave x monitors out CV
# #neveruse <- c("PER")
# neveruse <- c("")
# mons <- unique(m1.all[!stn %in% neveruse, stn]); length(mons)
# xout <- 1 # number of monitors to hold out
# # how many combinations if we pull out xout mons
# ncol(combn(mons, xout))
# n.iter <- 81
# # we will compute mean of the other monitors using all monitoring data
# setkey(m1.all, stn)
# 
# # list to store scheme
# cvscheme <- list()
# cvout <- list()
# # set seed for reproducibility
# set.seed(20150112)
# 
# # cross-validation in parallel
# 
# registerDoParallel(14)
# # use a proper reproducible backend RNG
# registerDoRNG(1234)
# system.time({
#   iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
#   #system.time(for(i in 1:n.iter){
#   #mons.test <- mons[sample(length(mons), xout)]
#   mons.test <- combn(mons, xout)[,i]
#   cvscheme[[i]] <- mons.test
#   test <- m1.all[stn %in% mons.test, ]
#   train<- m1.all[!stn %in% mons.test, ]
#   # fit the model
#   print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
#   print(paste("training on", nrow(train), "records"))
#   trainmod <-  lmer(m1.formula, data =  train)
#   test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
#   test$itercv <- i  
#   # export these results
#   test[, list(day, stn, PM10, predcv, itercv)]
# }# end of cross-validation loop
# })
# summary(lm(PM10 ~ predcv, data = iter.out))
# # compute root mean squared error
# iter.out[, sqrt(mean((PM10 - predcv)^2))]



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.all.cv,stn)
setkey(luf,stn)
m1.all.cv.loc <- merge(m1.all.cv, luf, all.x = T)
#m1.all.cv.loc<-na.omit(m1.all.cv.loc)

#create residual mp3 variable
m1.all.cv.loc$res.m1<-m1.all.cv.loc$PM10-m1.all.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS.im)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.all.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
gam.out<-gam(res.m1~s(loc.tden)+s(tden)+s(loc_p_os)+s(loc.elev)+s(dA1)+s(dsea)+s(pbldag,tden),data=m1.all.cv.loc)


## reg
m1.all.cv.loc$pred.m1.loc <-predict(gam.out)
m1.all.cv.loc$pred.m1.both <- m1.all.cv.loc$pred.m1.cv + m1.all.cv.loc$pred.m1.loc
res[res$type=="PM10", 'm1cvloc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.all.cv.loc))$r.squared)
res[res$type=="PM10", 'm1cvloc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.all.cv.loc))$coef[1,1])
res[res$type=="PM10", 'm1cvloc.Ise'] <-print(summary(lm(PM10~pred.m1.both,data=m1.all.cv.loc))$coef[1,2])
res[res$type=="PM10", 'm1cvloc.slope'] <-print(summary(lm(PM10~pred.m1.both,data=m1.all.cv.loc))$coef[2,1])
res[res$type=="PM10", 'm1cvloc.slopese'] <-print(summary(lm(PM10~pred.m1.both,data=m1.all.cv.loc))$coef[2,2])
#RMSPE
res[res$type=="PM10", 'm1cvloc.rmspe'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv.loc<-m1.all.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1.both, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM10", 'm1cvloc.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM10", 'm1cvloc.R2.time'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(m1.all.cv.loc,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM10-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM10", 'm1cv.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/resm1.AQ.PM10.rds")

#### mod2 
m2.all <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod2.AQ.rds")
m2.all <- m2.all[!m2.all$aodid %in% badaod$aodid]

#generate predictions
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 300   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod2.AQ.PM10.pred.rds")


#check R2
m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM10.rds")
m1.all[,aodid:= paste(m1.all$long_aod.x,m1.all$lat_aod.x,sep="-")]
m1.all<-m1.all[,c("aodid","day","PM10","stn","c"),with=FALSE]
#R2.m3
setkey(m2.all,day,aodid)
setkey(m1.all,day,aodid)
m1.all <- merge(m1.all,m2.all[, list(day,aodid,pred.m2)], all.x = T)
m3.fit.all<- summary(lm(PM10~pred.m2,data=m1.all))
res[res$type=="PM10", 'm2.R2'] <- print(summary(lm(PM10~pred.m2,data=m1.all))$r.squared)

#map
m3d_agg <- (m2.all[, list(LTPM =mean(pred.m2, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.csv")


#-------------->prepare for mod3
m2.all[, bimon := (m + 1) %/% 2]
setkey(m2.all,day, aodid)
m2.all<-m2.all[!is.na(meanPM)]


#2003
#take out 2003
m2.all.2003<-m2.all[c ==2003]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2003 )
#correlate to see everything from mod2 and the mpm works
m2.all.2003[, pred.t31 := predict(m2.smooth)]
m2.all.2003[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2003))$r.squared)



#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2003 ,m2.all.2003$bimon == "1")
Tall_bimon2 <- subset(m2.all.2003 ,m2.all.2003$bimon == "2")
Tall_bimon3 <- subset(m2.all.2003 ,m2.all.2003$bimon == "3")
Tall_bimon4 <- subset(m2.all.2003 ,m2.all.2003$bimon == "4")
Tall_bimon5 <- subset(m2.all.2003 ,m2.all.2003$bimon == "5")
Tall_bimon6 <- subset(m2.all.2003 ,m2.all.2003$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2003$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2003,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2003  )
m2.all.2003[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2003))$r.squared) 
# 
# #map
# m3d_agg <- (m2.all.2003[, list(LTPM =mean(pred.t33, na.rm = TRUE), 
#                         x = x_aod_ITM[1], #use the first long and lat (by aodid)
#                         y = y_aod_ITM[1]),by = aodid])  
# P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
#   geom_point(size = 4, shape = 15) + 
#   xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
#   scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
#   theme_bw() + 
#   ggtitle("Long term predictions")
# P1
# m3d_agg<-filter(m3d_agg,!is.na(LTPM))
# write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.csv")

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2003 <-data.m3[c ==2003]
#for PM10
data.m3.2003 <- select(data.m3.2003,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2003[, bimon := (m + 1) %/% 2]
setkey(data.m3.2003,day, aodid)
data.m3.2003<-data.m3.2003[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2003$pred.m3.mix <-  predict(Final_pred_all,data.m3.2003)

#create unique grid
ugrid <-data.m3.2003 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2003_bimon1 <- data.m3.2003[bimon == 1, ]
data.m3.2003_bimon2 <- data.m3.2003[bimon == 2, ]
data.m3.2003_bimon3 <- data.m3.2003[bimon == 3, ]
data.m3.2003_bimon4 <- data.m3.2003[bimon == 4, ]
data.m3.2003_bimon5 <- data.m3.2003[bimon == 5, ]
data.m3.2003_bimon6 <- data.m3.2003[bimon == 6, ]


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
setkey(data.m3.2003_bimon1,aodid)
data.m3.2003_bimon1 <- merge(data.m3.2003_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2003_bimon2,aodid)
data.m3.2003_bimon2 <- merge(data.m3.2003_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2003_bimon3,aodid)
data.m3.2003_bimon3 <- merge(data.m3.2003_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2003_bimon4,aodid)
data.m3.2003_bimon4 <- merge(data.m3.2003_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2003_bimon5,aodid)
data.m3.2003_bimon5 <- merge(data.m3.2003_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2003_bimon6,aodid)
data.m3.2003_bimon6 <- merge(data.m3.2003_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2003_bimon1,data.m3.2003_bimon2,data.m3.2003_bimon3,data.m3.2003_bimon4,data.m3.2003_bimon5,data.m3.2003_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2003.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2004
#take out 2004
m2.all.2004<-m2.all[c ==2004]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2004 )
#correlate to see everything from mod2 and the mpm works
m2.all.2004[, pred.t31 := predict(m2.smooth)]
m2.all.2004[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2004))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2004 ,m2.all.2004$bimon == "1")
Tall_bimon2 <- subset(m2.all.2004 ,m2.all.2004$bimon == "2")
Tall_bimon3 <- subset(m2.all.2004 ,m2.all.2004$bimon == "3")
Tall_bimon4 <- subset(m2.all.2004 ,m2.all.2004$bimon == "4")
Tall_bimon5 <- subset(m2.all.2004 ,m2.all.2004$bimon == "5")
Tall_bimon6 <- subset(m2.all.2004 ,m2.all.2004$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2004$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2004,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2004  )
m2.all.2004[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2004))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2004 <-data.m3[c ==2004]
#for PM10
data.m3.2004 <- select(data.m3.2004,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2004[, bimon := (m + 1) %/% 2]
setkey(data.m3.2004,day, aodid)
data.m3.2004<-data.m3.2004[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2004$pred.m3.mix <-  predict(Final_pred_all,data.m3.2004)

#create unique grid
ugrid <-data.m3.2004 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2004_bimon1 <- data.m3.2004[bimon == 1, ]
data.m3.2004_bimon2 <- data.m3.2004[bimon == 2, ]
data.m3.2004_bimon3 <- data.m3.2004[bimon == 3, ]
data.m3.2004_bimon4 <- data.m3.2004[bimon == 4, ]
data.m3.2004_bimon5 <- data.m3.2004[bimon == 5, ]
data.m3.2004_bimon6 <- data.m3.2004[bimon == 6, ]


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
setkey(data.m3.2004_bimon1,aodid)
data.m3.2004_bimon1 <- merge(data.m3.2004_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2004_bimon2,aodid)
data.m3.2004_bimon2 <- merge(data.m3.2004_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2004_bimon3,aodid)
data.m3.2004_bimon3 <- merge(data.m3.2004_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2004_bimon4,aodid)
data.m3.2004_bimon4 <- merge(data.m3.2004_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2004_bimon5,aodid)
data.m3.2004_bimon5 <- merge(data.m3.2004_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2004_bimon6,aodid)
data.m3.2004_bimon6 <- merge(data.m3.2004_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2004_bimon1,data.m3.2004_bimon2,data.m3.2004_bimon3,data.m3.2004_bimon4,data.m3.2004_bimon5,data.m3.2004_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2004.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()


#2005
#take out 2005
m2.all.2005<-m2.all[c ==2005]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2005 )
#correlate to see everything from mod2 and the mpm works
m2.all.2005[, pred.t31 := predict(m2.smooth)]
m2.all.2005[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2005))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2005 ,m2.all.2005$bimon == "1")
Tall_bimon2 <- subset(m2.all.2005 ,m2.all.2005$bimon == "2")
Tall_bimon3 <- subset(m2.all.2005 ,m2.all.2005$bimon == "3")
Tall_bimon4 <- subset(m2.all.2005 ,m2.all.2005$bimon == "4")
Tall_bimon5 <- subset(m2.all.2005 ,m2.all.2005$bimon == "5")
Tall_bimon6 <- subset(m2.all.2005 ,m2.all.2005$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2005$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2005,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2005  )
m2.all.2005[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2005))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2005 <-data.m3[c ==2005]
#for PM10
data.m3.2005 <- select(data.m3.2005,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2005[, bimon := (m + 1) %/% 2]
setkey(data.m3.2005,day, aodid)
data.m3.2005<-data.m3.2005[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2005$pred.m3.mix <-  predict(Final_pred_all,data.m3.2005)

#create unique grid
ugrid <-data.m3.2005 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2005_bimon1 <- data.m3.2005[bimon == 1, ]
data.m3.2005_bimon2 <- data.m3.2005[bimon == 2, ]
data.m3.2005_bimon3 <- data.m3.2005[bimon == 3, ]
data.m3.2005_bimon4 <- data.m3.2005[bimon == 4, ]
data.m3.2005_bimon5 <- data.m3.2005[bimon == 5, ]
data.m3.2005_bimon6 <- data.m3.2005[bimon == 6, ]


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
setkey(data.m3.2005_bimon1,aodid)
data.m3.2005_bimon1 <- merge(data.m3.2005_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2005_bimon2,aodid)
data.m3.2005_bimon2 <- merge(data.m3.2005_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2005_bimon3,aodid)
data.m3.2005_bimon3 <- merge(data.m3.2005_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2005_bimon4,aodid)
data.m3.2005_bimon4 <- merge(data.m3.2005_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2005_bimon5,aodid)
data.m3.2005_bimon5 <- merge(data.m3.2005_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2005_bimon6,aodid)
data.m3.2005_bimon6 <- merge(data.m3.2005_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2005_bimon1,data.m3.2005_bimon2,data.m3.2005_bimon3,data.m3.2005_bimon4,data.m3.2005_bimon5,data.m3.2005_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]

saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2005.pred3.rds")
#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2006
#take out 2006
m2.all.2006<-m2.all[c ==2006]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2006 )
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
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

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
setkey(m2.all.2006,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2006  )
m2.all.2006[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2006))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2006 <-data.m3[c ==2006]
#for PM10
data.m3.2006 <- select(data.m3.2006,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2006[, bimon := (m + 1) %/% 2]
setkey(data.m3.2006,day, aodid)
data.m3.2006<-data.m3.2006[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2006$pred.m3.mix <-  predict(Final_pred_all,data.m3.2006)

#create unique grid
ugrid <-data.m3.2006 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


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
setkey(uniq_gid_bimon1,aodid)
setkey(data.m3.2006_bimon1,aodid)
data.m3.2006_bimon1 <- merge(data.m3.2006_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2006_bimon2,aodid)
data.m3.2006_bimon2 <- merge(data.m3.2006_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2006_bimon3,aodid)
data.m3.2006_bimon3 <- merge(data.m3.2006_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2006_bimon4,aodid)
data.m3.2006_bimon4 <- merge(data.m3.2006_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2006_bimon5,aodid)
data.m3.2006_bimon5 <- merge(data.m3.2006_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2006_bimon6,aodid)
data.m3.2006_bimon6 <- merge(data.m3.2006_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2006_bimon1,data.m3.2006_bimon2,data.m3.2006_bimon3,data.m3.2006_bimon4,data.m3.2006_bimon5,data.m3.2006_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2006.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2007
#take out 2007
m2.all.2007<-m2.all[c ==2007]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2007 )
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
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

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
setkey(m2.all.2007,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2007  )
m2.all.2007[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2007))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2007 <-data.m3[c ==2007]
#for PM10
data.m3.2007 <- select(data.m3.2007,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2007[, bimon := (m + 1) %/% 2]
setkey(data.m3.2007,day, aodid)
data.m3.2007<-data.m3.2007[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2007$pred.m3.mix <-  predict(Final_pred_all,data.m3.2007)

#create unique grid
ugrid <-data.m3.2007 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


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
setkey(uniq_gid_bimon1,aodid)
setkey(data.m3.2007_bimon1,aodid)
data.m3.2007_bimon1 <- merge(data.m3.2007_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2007_bimon2,aodid)
data.m3.2007_bimon2 <- merge(data.m3.2007_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2007_bimon3,aodid)
data.m3.2007_bimon3 <- merge(data.m3.2007_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2007_bimon4,aodid)
data.m3.2007_bimon4 <- merge(data.m3.2007_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2007_bimon5,aodid)
data.m3.2007_bimon5 <- merge(data.m3.2007_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2007_bimon6,aodid)
data.m3.2007_bimon6 <- merge(data.m3.2007_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2007_bimon1,data.m3.2007_bimon2,data.m3.2007_bimon3,data.m3.2007_bimon4,data.m3.2007_bimon5,data.m3.2007_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2007.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2008
#take out 2008
m2.all.2008<-m2.all[c ==2008]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2008 )
#correlate to see everything from mod2 and the mpm works
m2.all.2008[, pred.t31 := predict(m2.smooth)]
m2.all.2008[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2008))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2008 ,m2.all.2008$bimon == "1")
Tall_bimon2 <- subset(m2.all.2008 ,m2.all.2008$bimon == "2")
Tall_bimon3 <- subset(m2.all.2008 ,m2.all.2008$bimon == "3")
Tall_bimon4 <- subset(m2.all.2008 ,m2.all.2008$bimon == "4")
Tall_bimon5 <- subset(m2.all.2008 ,m2.all.2008$bimon == "5")
Tall_bimon6 <- subset(m2.all.2008 ,m2.all.2008$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2008$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2008,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2008  )
m2.all.2008[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2008))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2008 <-data.m3[c ==2008]
#for PM10
data.m3.2008 <- select(data.m3.2008,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2008[, bimon := (m + 1) %/% 2]
setkey(data.m3.2008,day, aodid)
data.m3.2008<-data.m3.2008[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2008$pred.m3.mix <-  predict(Final_pred_all,data.m3.2008)

#create unique grid
ugrid <-data.m3.2008 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2008_bimon1 <- data.m3.2008[bimon == 1, ]
data.m3.2008_bimon2 <- data.m3.2008[bimon == 2, ]
data.m3.2008_bimon3 <- data.m3.2008[bimon == 3, ]
data.m3.2008_bimon4 <- data.m3.2008[bimon == 4, ]
data.m3.2008_bimon5 <- data.m3.2008[bimon == 5, ]
data.m3.2008_bimon6 <- data.m3.2008[bimon == 6, ]


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
setkey(data.m3.2008_bimon1,aodid)
data.m3.2008_bimon1 <- merge(data.m3.2008_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2008_bimon2,aodid)
data.m3.2008_bimon2 <- merge(data.m3.2008_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2008_bimon3,aodid)
data.m3.2008_bimon3 <- merge(data.m3.2008_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2008_bimon4,aodid)
data.m3.2008_bimon4 <- merge(data.m3.2008_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2008_bimon5,aodid)
data.m3.2008_bimon5 <- merge(data.m3.2008_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2008_bimon6,aodid)
data.m3.2008_bimon6 <- merge(data.m3.2008_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2008_bimon1,data.m3.2008_bimon2,data.m3.2008_bimon3,data.m3.2008_bimon4,data.m3.2008_bimon5,data.m3.2008_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2008.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2009
#take out 2009
m2.all.2009<-m2.all[c ==2009]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2009 )
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
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

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
setkey(m2.all.2009,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2009  )
m2.all.2009[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2009))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2009 <-data.m3[c ==2009]
#for PM10
data.m3.2009 <- select(data.m3.2009,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2009[, bimon := (m + 1) %/% 2]
setkey(data.m3.2009,day, aodid)
data.m3.2009<-data.m3.2009[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2009$pred.m3.mix <-  predict(Final_pred_all,data.m3.2009)

#create unique grid
ugrid <-data.m3.2009 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


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
setkey(uniq_gid_bimon1,aodid)
setkey(data.m3.2009_bimon1,aodid)
data.m3.2009_bimon1 <- merge(data.m3.2009_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2009_bimon2,aodid)
data.m3.2009_bimon2 <- merge(data.m3.2009_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2009_bimon3,aodid)
data.m3.2009_bimon3 <- merge(data.m3.2009_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2009_bimon4,aodid)
data.m3.2009_bimon4 <- merge(data.m3.2009_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2009_bimon5,aodid)
data.m3.2009_bimon5 <- merge(data.m3.2009_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2009_bimon6,aodid)
data.m3.2009_bimon6 <- merge(data.m3.2009_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2009_bimon1,data.m3.2009_bimon2,data.m3.2009_bimon3,data.m3.2009_bimon4,data.m3.2009_bimon5,data.m3.2009_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2009.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2010
#take out 2010
m2.all.2010<-m2.all[c ==2010]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2010 )
#correlate to see everything from mod2 and the mpm works
m2.all.2010[, pred.t31 := predict(m2.smooth)]
m2.all.2010[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2010))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2010 ,m2.all.2010$bimon == "1")
Tall_bimon2 <- subset(m2.all.2010 ,m2.all.2010$bimon == "2")
Tall_bimon3 <- subset(m2.all.2010 ,m2.all.2010$bimon == "3")
Tall_bimon4 <- subset(m2.all.2010 ,m2.all.2010$bimon == "4")
Tall_bimon5 <- subset(m2.all.2010 ,m2.all.2010$bimon == "5")
Tall_bimon6 <- subset(m2.all.2010 ,m2.all.2010$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2010$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2010,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2010  )
m2.all.2010[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2010))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2010 <-data.m3[c ==2010]
#for PM10
data.m3.2010 <- select(data.m3.2010,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2010[, bimon := (m + 1) %/% 2]
setkey(data.m3.2010,day, aodid)
data.m3.2010<-data.m3.2010[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2010$pred.m3.mix <-  predict(Final_pred_all,data.m3.2010)

#create unique grid
ugrid <-data.m3.2010 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2010_bimon1 <- data.m3.2010[bimon == 1, ]
data.m3.2010_bimon2 <- data.m3.2010[bimon == 2, ]
data.m3.2010_bimon3 <- data.m3.2010[bimon == 3, ]
data.m3.2010_bimon4 <- data.m3.2010[bimon == 4, ]
data.m3.2010_bimon5 <- data.m3.2010[bimon == 5, ]
data.m3.2010_bimon6 <- data.m3.2010[bimon == 6, ]


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
setkey(data.m3.2010_bimon1,aodid)
data.m3.2010_bimon1 <- merge(data.m3.2010_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2010_bimon2,aodid)
data.m3.2010_bimon2 <- merge(data.m3.2010_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2010_bimon3,aodid)
data.m3.2010_bimon3 <- merge(data.m3.2010_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2010_bimon4,aodid)
data.m3.2010_bimon4 <- merge(data.m3.2010_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2010_bimon5,aodid)
data.m3.2010_bimon5 <- merge(data.m3.2010_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2010_bimon6,aodid)
data.m3.2010_bimon6 <- merge(data.m3.2010_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2010_bimon1,data.m3.2010_bimon2,data.m3.2010_bimon3,data.m3.2010_bimon4,data.m3.2010_bimon5,data.m3.2010_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2010.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2011
#take out 2011
m2.all.2011<-m2.all[c ==2011]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2011 )
#correlate to see everything from mod2 and the mpm works
m2.all.2011[, pred.t31 := predict(m2.smooth)]
m2.all.2011[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2011))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2011 ,m2.all.2011$bimon == "1")
Tall_bimon2 <- subset(m2.all.2011 ,m2.all.2011$bimon == "2")
Tall_bimon3 <- subset(m2.all.2011 ,m2.all.2011$bimon == "3")
Tall_bimon4 <- subset(m2.all.2011 ,m2.all.2011$bimon == "4")
Tall_bimon5 <- subset(m2.all.2011 ,m2.all.2011$bimon == "5")
Tall_bimon6 <- subset(m2.all.2011 ,m2.all.2011$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2011$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2011,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2011  )
m2.all.2011[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2011))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2011 <-data.m3[c ==2011]
#for PM10
data.m3.2011 <- select(data.m3.2011,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2011[, bimon := (m + 1) %/% 2]
setkey(data.m3.2011,day, aodid)
data.m3.2011<-data.m3.2011[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2011$pred.m3.mix <-  predict(Final_pred_all,data.m3.2011)

#create unique grid
ugrid <-data.m3.2011 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2011_bimon1 <- data.m3.2011[bimon == 1, ]
data.m3.2011_bimon2 <- data.m3.2011[bimon == 2, ]
data.m3.2011_bimon3 <- data.m3.2011[bimon == 3, ]
data.m3.2011_bimon4 <- data.m3.2011[bimon == 4, ]
data.m3.2011_bimon5 <- data.m3.2011[bimon == 5, ]
data.m3.2011_bimon6 <- data.m3.2011[bimon == 6, ]


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
setkey(data.m3.2011_bimon1,aodid)
data.m3.2011_bimon1 <- merge(data.m3.2011_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2011_bimon2,aodid)
data.m3.2011_bimon2 <- merge(data.m3.2011_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2011_bimon3,aodid)
data.m3.2011_bimon3 <- merge(data.m3.2011_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2011_bimon4,aodid)
data.m3.2011_bimon4 <- merge(data.m3.2011_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2011_bimon5,aodid)
data.m3.2011_bimon5 <- merge(data.m3.2011_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2011_bimon6,aodid)
data.m3.2011_bimon6 <- merge(data.m3.2011_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2011_bimon1,data.m3.2011_bimon2,data.m3.2011_bimon3,data.m3.2011_bimon4,data.m3.2011_bimon5,data.m3.2011_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2011.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2012
#take out 2012
m2.all.2012<-m2.all[c ==2012]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2012 )
#correlate to see everything from mod2 and the mpm works
m2.all.2012[, pred.t31 := predict(m2.smooth)]
m2.all.2012[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2012))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2012 ,m2.all.2012$bimon == "1")
Tall_bimon2 <- subset(m2.all.2012 ,m2.all.2012$bimon == "2")
Tall_bimon3 <- subset(m2.all.2012 ,m2.all.2012$bimon == "3")
Tall_bimon4 <- subset(m2.all.2012 ,m2.all.2012$bimon == "4")
Tall_bimon5 <- subset(m2.all.2012 ,m2.all.2012$bimon == "5")
Tall_bimon6 <- subset(m2.all.2012 ,m2.all.2012$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2012$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2012,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2012  )
m2.all.2012[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2012))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2012 <-data.m3[c ==2012]
#for PM10
data.m3.2012 <- select(data.m3.2012,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2012[, bimon := (m + 1) %/% 2]
setkey(data.m3.2012,day, aodid)
data.m3.2012<-data.m3.2012[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2012$pred.m3.mix <-  predict(Final_pred_all,data.m3.2012)

#create unique grid
ugrid <-data.m3.2012 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2012_bimon1 <- data.m3.2012[bimon == 1, ]
data.m3.2012_bimon2 <- data.m3.2012[bimon == 2, ]
data.m3.2012_bimon3 <- data.m3.2012[bimon == 3, ]
data.m3.2012_bimon4 <- data.m3.2012[bimon == 4, ]
data.m3.2012_bimon5 <- data.m3.2012[bimon == 5, ]
data.m3.2012_bimon6 <- data.m3.2012[bimon == 6, ]


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
setkey(data.m3.2012_bimon1,aodid)
data.m3.2012_bimon1 <- merge(data.m3.2012_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2012_bimon2,aodid)
data.m3.2012_bimon2 <- merge(data.m3.2012_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2012_bimon3,aodid)
data.m3.2012_bimon3 <- merge(data.m3.2012_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2012_bimon4,aodid)
data.m3.2012_bimon4 <- merge(data.m3.2012_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2012_bimon5,aodid)
data.m3.2012_bimon5 <- merge(data.m3.2012_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2012_bimon6,aodid)
data.m3.2012_bimon6 <- merge(data.m3.2012_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2012_bimon1,data.m3.2012_bimon2,data.m3.2012_bimon3,data.m3.2012_bimon4,data.m3.2012_bimon5,data.m3.2012_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2012.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()
#2013
#take out 2013
m2.all.2013<-m2.all[c ==2013]
gc()
#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2013 )
#correlate to see everything from mod2 and the mpm works
m2.all.2013[, pred.t31 := predict(m2.smooth)]
m2.all.2013[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2013))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2013 ,m2.all.2013$bimon == "1")
Tall_bimon2 <- subset(m2.all.2013 ,m2.all.2013$bimon == "2")
Tall_bimon3 <- subset(m2.all.2013 ,m2.all.2013$bimon == "3")
Tall_bimon4 <- subset(m2.all.2013 ,m2.all.2013$bimon == "4")
Tall_bimon5 <- subset(m2.all.2013 ,m2.all.2013$bimon == "5")
Tall_bimon6 <- subset(m2.all.2013 ,m2.all.2013$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2013$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2013,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2013  )
m2.all.2013[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM10", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2013))$r.squared) 

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2013 <-data.m3[c ==2013]
#for PM10
data.m3.2013 <- select(data.m3.2013,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2013[, bimon := (m + 1) %/% 2]
setkey(data.m3.2013,day, aodid)
data.m3.2013<-data.m3.2013[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2013$pred.m3.mix <-  predict(Final_pred_all,data.m3.2013)

#create unique grid
ugrid <-data.m3.2013 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2013_bimon1 <- data.m3.2013[bimon == 1, ]
data.m3.2013_bimon2 <- data.m3.2013[bimon == 2, ]
data.m3.2013_bimon3 <- data.m3.2013[bimon == 3, ]
data.m3.2013_bimon4 <- data.m3.2013[bimon == 4, ]
data.m3.2013_bimon5 <- data.m3.2013[bimon == 5, ]
data.m3.2013_bimon6 <- data.m3.2013[bimon == 6, ]


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
setkey(data.m3.2013_bimon1,aodid)
data.m3.2013_bimon1 <- merge(data.m3.2013_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2013_bimon2,aodid)
data.m3.2013_bimon2 <- merge(data.m3.2013_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2013_bimon3,aodid)
data.m3.2013_bimon3 <- merge(data.m3.2013_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2013_bimon4,aodid)
data.m3.2013_bimon4 <- merge(data.m3.2013_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2013_bimon5,aodid)
data.m3.2013_bimon5 <- merge(data.m3.2013_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2013_bimon6,aodid)
data.m3.2013_bimon6 <- merge(data.m3.2013_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2013_bimon1,data.m3.2013_bimon2,data.m3.2013_bimon3,data.m3.2013_bimon4,data.m3.2013_bimon5,data.m3.2013_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2013.pred3.rds")

#clean
keep(m2.all,data.m3,mod3,res,rmse, sure=TRUE) 
gc()


#create full database
data.m3.2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2003.pred3.rds")
data.m3.2004<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2004.pred3.rds")
data.m3.2005<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2005.pred3.rds")
data.m3.2006<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2006.pred3.rds")
data.m3.2007<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2007.pred3.rds")
data.m3.2008<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2008.pred3.rds")
data.m3.2009<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2009.pred3.rds")
data.m3.2010<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2010.pred3.rds")
data.m3.2011<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2011.pred3.rds")
data.m3.2012<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2012.pred3.rds")
data.m3.2013<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM10.2013.pred3.rds")
mod3<-rbindlist(list(data.m3.2003,data.m3.2004,data.m3.2005,data.m3.2006,data.m3.2007,data.m3.2008,data.m3.2009,data.m3.2010,data.m3.2011,data.m3.2012,data.m3.2013))


#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM10.pred.rds")
m1.all[,aodid:= paste(m1.all$long_aod.x,m1.all$lat_aod.x,sep="-")]
m1.all<-m1.all[,c("aodid","day","PM10","pred.m1","stn","c"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(m1.all,day,aodid)
m1.all <- merge(m1.all,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM10~pred.m3,data=m1.all))
res[res$type=="PM10", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.all))$r.squared)    
res[res$type=="PM10", 'm3.I'] <-print(summary(lm(PM10~pred.m3,data=m1.all))$coef[1,1])
res[res$type=="PM10", 'm3.Ise'] <-print(summary(lm(PM10~pred.m3,data=m1.all))$coef[1,2])
res[res$type=="PM10", 'm3.slope'] <-print(summary(lm(PM10~pred.m3,data=m1.all))$coef[2,1])
res[res$type=="PM10", 'm3.slopese'] <-print(summary(lm(PM10~pred.m3,data=m1.all))$coef[2,2])
#RMSPE
res[res$type=="PM10", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM10", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM10", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM10", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/resALL.AQ.PM10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod2.AQ.PM10.pred.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<-m1.all[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
summary(mod3best$bestpred)
mod3best[bestpred < 0 , bestpred  := 0.5]

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.PM10.bestpred.rds")

mod3best<-filter(mod3best,!is.na(bestpred))

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.PM10.LTPM.csv", row.names = F)

#export res to csv

write.csv(res,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/resALL.AQ.PM10.csv")


#alt expo gis
#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
write.csv(m3d_agg,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/bestpred.AQ.PM10.LTPM.csv")

#mod3map

m3d_agg <- (mod3[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
# 
# # rmse by monitor
# neveruse <- c("")
# m1.all[!stn %in% neveruse, list(rmse = sqrt(mean((pred.m3 - PM10)^2))),by=c("stn", "c")][order(rmse)]
# 
# #alt method
# rmse.p10 <- ddply(m1.all, c("stn"), 
#       function(x) {
#         sqrt(mean((x$PM10-x$pred.m1)^2))
#                       })
# rmse.p10<-as.data.table(rmse.p10)
# setnames(rmse.p10,"V1","RMSPE")
# setkey(rmse.p10,RMSPE)
# rmse.p10
# 
# 
# out<-m1.all[c==2008] %>%
#   group_by(stn) %>%
#   summarise (n = n()) 
# setkey(out,stn,c)
# head(out,n=550)
# 
# 
# 
# # plot a single year by day- KMR- North
# ggplot(m1.all[stn %in% c("KMR") & c == 2008,], aes(x = day)) + 
# #overall line
#   geom_line(aes(y = PM10), color = "black", alpha = 0.15) + 
#   #points
#   geom_point(aes(y = PM10), color = "blue", alpha = 0.7, size = 0.8) + 
#   geom_point(aes(y = pred.m3), color = "red", alpha = 0.7, size = 0.8) + 
#   #smoothers
#   geom_smooth(aes(y = pred.m3), color = "red", linetype = "dashed", width = 1.4, se = F, size=0.9) + 
#   geom_smooth(aes(y = PM10), color = "blue", se = F, size=0.9) + 
#   ylab(expression(paste(PM[2.5], "concentration (ug/", m^3, ")"))) + 
#   theme_bw() + theme(axis.title.x = element_blank())
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/KMR.PM10.2008.svg", width = 5.5, height = 3)
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/KMR.PM10.2008.png", width = 5.5, height = 3)
# 
# 
# # plot a single year by day- TIV- North
# ggplot(m1.all[stn %in% c("TIV") & c == 2008,], aes(x = day)) + 
# #overall line
#   geom_line(aes(y = PM10), color = "black", alpha = 0.15) + 
#   #points
#   geom_point(aes(y = PM10), color = "blue", alpha = 0.7, size = 0.8) + 
#   geom_point(aes(y = pred.m3), color = "red", alpha = 0.7, size = 0.8) + 
#   #smoothers
#   geom_smooth(aes(y = pred.m3), color = "red", linetype = "dashed", width = 1.4, se = F, size=0.9) + 
#   geom_smooth(aes(y = PM10), color = "blue", se = F, size=0.9) + 
#   ylab(expression(paste(PM[2.5], "concentration (ug/", m^3, ")"))) + 
#   theme_bw() + theme(axis.title.x = element_blank())
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/TIV.PM10.2008.svg", width = 5.5, height = 3)
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/TIV.PM10.2008.png", width = 5.5, height = 3)
# 
# 
# # plot a single year by day- SYO- south
# ggplot(m1.all[stn %in% c("SYO") & c == 2008,], aes(x = day)) + 
# #overall line
#   geom_line(aes(y = PM10), color = "black", alpha = 0.15) + 
#   #points
#   geom_point(aes(y = PM10), color = "blue", alpha = 0.7, size = 0.8) + 
#   geom_point(aes(y = pred.m3), color = "red", alpha = 0.7, size = 0.8) + 
#   #smoothers
#   geom_smooth(aes(y = pred.m3), color = "red", linetype = "dashed", width = 1.4, se = F, size=0.9) + 
#   geom_smooth(aes(y = PM10), color = "blue", se = F, size=0.9) + 
#   ylab(expression(paste(PM[2.5], "concentration (ug/", m^3, ")"))) + 
#   theme_bw() + theme(axis.title.x = element_blank())
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/SYO.PM10.2008.svg", width = 5.5, height = 3)
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/SYO.PM10.2008.png", width = 5.5, height = 3)
# 
# 
# # plot a single year by day- GDR- south
# ggplot(m1.all[stn %in% c("GDR") & c == 2008,], aes(x = day)) + 
# #overall line
#   geom_line(aes(y = PM10), color = "black", alpha = 0.15) + 
#   #points
#   geom_point(aes(y = PM10), color = "blue", alpha = 0.7, size = 0.8) + 
#   geom_point(aes(y = pred.m3), color = "red", alpha = 0.7, size = 0.8) + 
#   #smoothers
#   geom_smooth(aes(y = pred.m3), color = "red", linetype = "dashed", width = 1.4, se = F, size=0.9) + 
#   geom_smooth(aes(y = PM10), color = "blue", se = F, size=0.9) + 
#   ylab(expression(paste(PM[2.5], "concentration (ug/", m^3, ")"))) + 
#   theme_bw() + theme(axis.title.x = element_blank())
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/GDR.PM10.2008.svg", width = 5.5, height = 3)
# ggsave("/media/NAS/Uni/Projects/P046_Israel_MAIAC/4.Results/figures/f4/GDR.PM10.2008.png", width = 5.5, height = 3)
# 
# 
# 
