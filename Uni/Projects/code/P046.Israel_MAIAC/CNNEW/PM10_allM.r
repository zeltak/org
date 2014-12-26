## ###############
## #LIBS
## ###############

## library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


## #sourcing
## source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
## source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

## #if needed load res table
## #res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")


## #-------------------->> RES TABLE
## res <- matrix(nrow=9, ncol=45)
## res <- data.frame(res)
## colnames(res) <- c(
##           "year"
##           ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
##           ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
##           ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
##           ,"m2.R2" #mod2
##           ,"m3.t31","m3.t33" #mod3 tests
##           ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
##           ,"XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX"    )


## res$year <- c(2003:2011) 



## ### import data
## m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.rds")

## #subset to aqua and apply alexei cleaning methods
## #MaskAdjacency == "000"
## m1.2003<-m1.2003[ UN > 0 & UN < 0.04  ] 

## ################# clean BAD STN PM10 and check if improved model?
## raWDaf <- ddply(m1.2003, c( "stn"), 
##       function(x) {
##         mod1 <- lm(PM10 ~ aod, data=x)
##         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
##                    nsamps = length(summary(mod1)$resid))
## })
## raWDaf
## raWDaf<-as.data.table(raWDaf)
## bad<- raWDaf[R2< 0.05]
## bad[,badid := paste(stn,sep="-")]
## #################BAD STN
## m1.2003[,badid := paste(stn,sep="-")]
## ####Take out bad stations
## m1.2003 <- m1.2003[!(m1.2003$badid %in% bad$badid), ] 

## #get rid of missing
## m1.2003[,elev.s:= scale(elev)]
## m1.2003[,tden.s:= scale(tden)]
## m1.2003[,pden.s:= scale(pden)]
## m1.2003[,dist2A1.s:= scale(dist2A1)]
## m1.2003[,dist2water.s:= scale(dist2water)]
## m1.2003[,dist2rail.s:= scale(dist2rail)]
## m1.2003[,Dist2road.s:= scale(Dist2road)]
## m1.2003[,ndvi.s:= scale(ndvi)]
## m1.2003[,MeanPbl.s:= scale(MeanPbl)]
## m1.2003[,p_ind.s:= scale(p_ind)]
## m1.2003[,p_for.s:= scale(p_for)]
## m1.2003[,p_farm.s:= scale(p_farm)]
## m1.2003[,p_dos.s:= scale(p_dos)]
## m1.2003[,p_dev.s:= scale(p_dev)]
## m1.2003[,p_os.s:= scale(p_os)]
## m1.2003[,tempa.s:= scale(tempa)]
## m1.2003[,WDa.s:= scale(WDa)]
## m1.2003[,WSa.s:= scale(WSa)]
## m1.2003[,RHa.s:= scale(RHa)]
## m1.2003[,Raina.s:= scale(Raina)]
## m1.2003[,NO2a.s:= scale(NO2a)]



## m1.formula <- as.formula(PM10~ aod
##                         +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
##                         +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
##                         +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
##                          #+aod*Dust #interactions
##                          +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

## #full fit
## m1.fit.2003 <-  lmer(m1.formula,data=m1.2003,weights=normwt)
## m1.2003$pred.m1 <- predict(m1.fit.2003)
## res[res$year=="2003", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2003))$r.squared)
## #RMSPE
## res[res$year=="2003", 'm1.PE'] <- print(rmse(residuals(m1.fit.2003)))

## #spatial
## ###to check
## spatial2003<-m1.2003 %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2003.spat<- lm(barpm ~ barpred, data=spatial2003)
## res[res$year=="2003", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003))$r.squared)
## res[res$year=="2003", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2003.spat)))
       
## #temporal
## tempo2003<-left_join(m1.2003,spatial2003)
## tempo2003$delpm <-tempo2003$PM10-tempo2003$barpm
## tempo2003$delpred <-tempo2003$pred.m1-tempo2003$barpred
## mod_temporal <- lm(delpm ~ delpred, data=tempo2003)
## res[res$year=="2003", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003))$r.squared)
## saveRDS(m1.2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.pred.rds")



## #---------------->>>> CV
## #s1
## splits_s1 <- splitdf(m1.2003)
## test_s1 <- splits_s1$testset
## train_s1 <- splits_s1$trainset
## out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
## test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
## test_s1$iter<-"s1"
## #s2
## splits_s2 <- splitdf(m1.2003)
## test_s2 <- splits_s2$testset
## train_s2 <- splits_s2$trainset
## out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
## test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
## test_s2$iter<-"s2"
## #s3
## splits_s3 <- splitdf(m1.2003)
## test_s3 <- splits_s3$testset
## train_s3 <- splits_s3$trainset
## out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
## test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
## test_s3$iter<-"s3"
## #s4
## splits_s4 <- splitdf(m1.2003)
## test_s4 <- splits_s4$testset
## train_s4 <- splits_s4$trainset
## out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
## test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
## test_s4$iter<-"s4"
## #s5
## splits_s5 <- splitdf(m1.2003)
## test_s5 <- splits_s5$testset
## train_s5 <- splits_s5$trainset
## out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
## test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
## test_s5$iter<-"s5"
## #s6
## splits_s6 <- splitdf(m1.2003)
## test_s6 <- splits_s6$testset
## train_s6 <- splits_s6$trainset
## out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
## test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
## test_s6$iter<-"s6"
## #s7
## splits_s7 <- splitdf(m1.2003)
## test_s7 <- splits_s7$testset
## train_s7 <- splits_s7$trainset
## out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
## test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
## test_s7$iter<-"s7"
## #s8
## splits_s8 <- splitdf(m1.2003)
## test_s8 <- splits_s8$testset
## train_s8 <- splits_s8$trainset
## out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
## test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
## test_s8$iter<-"s8"
## #s9
## splits_s9 <- splitdf(m1.2003)
## test_s9 <- splits_s9$testset
## train_s9 <- splits_s9$trainset
## out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
## test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
## test_s9$iter<-"s9"
## #s10
## splits_s10 <- splitdf(m1.2003)
## test_s10 <- splits_s10$testset
## train_s10 <- splits_s10$trainset
## out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
## test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
## test_s10$iter<-"s10"

## #BIND 1 dataset
## m1.2003.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
## # cleanup (remove from WS) objects from CV
## rm(list = ls(pattern = "train_|test_"))
## #table updates
## m1.fit.2003.cv<-lm(PM10~pred.m1.cv,data=m1.2003.cv)
## res[res$year=="2003", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2003.cv))$r.squared)
## res[res$year=="2003", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2003.cv))$coef[1,1])
## res[res$year=="2003", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2003.cv))$coef[1,2])
## res[res$year=="2003", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2003.cv))$coef[2,1])
## res[res$year=="2003", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2003.cv))$coef[2,2])
## #RMSPE
## res[res$year=="2003", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2003.cv)))

## #spatial
## spatial2003.cv<-m1.2003.cv %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2003.cv.s <- lm(barpm ~ barpred, data=spatial2003.cv)
## res[res$year=="2003", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003.cv))$r.squared)
## res[res$year=="2003", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2003.cv.s)))
       
## #temporal
## tempo2003.cv<-left_join(m1.2003.cv,spatial2003.cv)
## tempo2003.cv$delpm <-tempo2003.cv$PM10-tempo2003.cv$barpm
## tempo2003.cv$delpred <-tempo2003.cv$pred.m1.cv-tempo2003.cv$barpred
## mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2003.cv)
## res[res$year=="2003", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003.cv))$r.squared)



## #-------->>> loc stage

## luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
## setnames(luf,"tden","loc.tden")
## setnames(luf,"elev50","loc.elev")

## #add 50m LU to CV data
## setkey(m1.2003.cv,stn)
## setkey(luf,stn)
## m1.2003.cv.loc <- merge(m1.2003.cv, luf, all.x = T)
## #m1.2003.cv.loc<-na.omit(m1.2003.cv.loc)

## #create residual mp3 variable
## m1.2003.cv.loc$res.m1<-m1.2003.cv.loc$PM10-m1.2003.cv.loc$pred.m1.cv

## #The GAM model
## gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2003.cv.loc)
## #plot(bp.model.ps)
## #summary(bp.model.ps)
## ## reg
## m1.2003.cv.loc$pred.m1.loc <-predict(gam.out)
## m1.2003.cv.loc$pred.m1.both <- m1.2003.cv.loc$pred.m1.cv + m1.2003.cv.loc$pred.m1.loc
## res[res$year=="2003", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2003.cv.loc))$r.squared)
## res[res$year=="2003", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2003.cv.loc))$coef[1,1])
## res[res$year=="2003", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2003.cv.loc))$coef[1,2])
## res[res$year=="2003", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2003.cv.loc))$coef[2,1])
## res[res$year=="2003", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2003.cv.loc))$coef[2,2])
## #RMSPE
## res[res$year=="2003", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2003.cv)))

## #spatial
## spatial2003.cv.loc<-m1.2003.cv.loc %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2003.cv.loc.s <- lm(barpm ~ barpred, data=spatial2003.cv.loc)
## res[res$year=="2003", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003.cv.loc))$r.squared)
## res[res$year=="2003", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2003.cv.loc.s)))
       
## #temporal
## tempo2003.loc.cv<-left_join(m1.2003.cv.loc,spatial2003.cv.loc)
## tempo2003.loc.cv$delpm <-tempo2003.loc.cv$PM10-tempo2003.loc.cv$barpm
## tempo2003.loc.cv$delpred <-tempo2003.loc.cv$pred.m1.both-tempo2003.loc.cv$barpred
## mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2003.loc.cv)
## res[res$year=="2003", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003.loc.cv))$r.squared)

## #############save midpoint
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2003.rds")
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
## saveRDS(m1.2003.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.predCV.rds")


## ###############
## #MOD2
## ###############
## m2.2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")

## m2.2003[,elev.s:= scale(elev)]
## m2.2003[,tden.s:= scale(tden)]
## m2.2003[,pden.s:= scale(pden)]
## m2.2003[,dist2A1.s:= scale(dist2A1)]
## m2.2003[,dist2water.s:= scale(dist2water)]
## m2.2003[,dist2rail.s:= scale(dist2rail)]
## m2.2003[,Dist2road.s:= scale(Dist2road)]
## m2.2003[,ndvi.s:= scale(ndvi)]
## m2.2003[,MeanPbl.s:= scale(MeanPbl)]
## m2.2003[,p_ind.s:= scale(p_ind)]
## m2.2003[,p_for.s:= scale(p_for)]
## m2.2003[,p_farm.s:= scale(p_farm)]
## m2.2003[,p_dos.s:= scale(p_dos)]
## m2.2003[,p_dev.s:= scale(p_dev)]
## m2.2003[,p_os.s:= scale(p_os)]
## m2.2003[,tempa.s:= scale(tempa)]
## m2.2003[,WDa.s:= scale(WDa)]
## m2.2003[,WSa.s:= scale(WSa)]
## m2.2003[,RHa.s:= scale(RHa)]
## m2.2003[,Raina.s:= scale(Raina)]
## m2.2003[,NO2a.s:= scale(NO2a)]



## #generate predictions
## m2.2003[, pred.m2 := predict(object=m1.fit.2003,newdata=m2.2003,allow.new.levels=TRUE,re.form=NULL)]
## describe(m2.2003$pred.m2)
## #delete implossible valuesOA[24~
## m2.2003 <- m2.2003[pred.m2 > 0.00000000000001 , ]
## m2.2003 <- m2.2003[pred.m2 < 1500   , ]

## saveRDS(m2.2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2003.pred2.rds")



## #-------------->prepare for mod3
## m2.2003[, bimon := (m + 1) %/% 2]
## setkey(m2.2003,day, aodid)
## m2.2003<-m2.2003[!is.na(meanPM10)]




## #run the lmer part regressing stage 2 pred Vs mean pm
## #in israel check per month, also check 30km band and other methods for meanpm
## m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2003 )
## #xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2003 )
## #correlate to see everything from mod2 and the mpm works
## m2.2003[, pred.t31 := predict(m2.smooth)]
## m2.2003[, resid  := residuals(m2.smooth)]
## res[res$year=="2003", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2003))$r.squared)


## #split the files to the separate bi monthly datsets
## T2003_bimon1 <- subset(m2.2003 ,m2.2003$bimon == "1")
## T2003_bimon2 <- subset(m2.2003 ,m2.2003$bimon == "2")
## T2003_bimon3 <- subset(m2.2003 ,m2.2003$bimon == "3")
## T2003_bimon4 <- subset(m2.2003 ,m2.2003$bimon == "4")
## T2003_bimon5 <- subset(m2.2003 ,m2.2003$bimon == "5")
## T2003_bimon6 <- subset(m2.2003 ,m2.2003$bimon == "6")

## #run the separate splines (smooth) for x and y for each bimon
## #whats the default band (distance) that the spline goes out and uses
## fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon1 )
## fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon2 )
## fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon3 )
## fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon4 )
## fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon5 )
## fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon6 )

## #get the predicted-fitted 
## Xpred_1 <- (T2003_bimon1$pred.t31 - fit2_1$fitted)
## Xpred_2 <- (T2003_bimon2$pred.t31 - fit2_2$fitted)
## Xpred_3 <- (T2003_bimon3$pred.t31 - fit2_3$fitted)
## Xpred_4 <- (T2003_bimon4$pred.t31 - fit2_4$fitted)
## Xpred_5 <- (T2003_bimon5$pred.t31 - fit2_5$fitted)
## Xpred_6 <- (T2003_bimon6$pred.t31 - fit2_6$fitted)

## #remerge to 1 file
## m2.2003$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
## #this is important so that its sorted as in the first gamm
## setkey(m2.2003,day, aodid)

## #rerun the lme on the predictions including the spatial spline (smooth)
## Final_pred_2003 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2003  )
## m2.2003[, pred.t33 := predict(Final_pred_2003)]
## #check correlations
## res[res$year=="2003", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2003))$r.squared) 


## #------------------------>>>
## #import mod3 
## data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2003.rds")

## #for PM10
## data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
## data.m3[, bimon := (m + 1) %/% 2]
## setkey(data.m3,day, aodid)
## data.m3<-data.m3[!is.na(meanPM10)]

## #generate m.3 initial pred
## data.m3$pred.m3.mix <-  predict(Final_pred_2003,data.m3)

## #create unique grid
## ugrid <-data.m3 %>%
##     group_by(aodid) %>%
##     summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


## #### PREDICT Gam part
## #split back into bimons to include the gam prediction in final prediction      	
## data.m3_bimon1 <- data.m3[bimon == 1, ]
## data.m3_bimon2 <- data.m3[bimon == 2, ]
## data.m3_bimon3 <- data.m3[bimon == 3, ]
## data.m3_bimon4 <- data.m3[bimon == 4, ]
## data.m3_bimon5 <- data.m3[bimon == 5, ]
## data.m3_bimon6 <- data.m3[bimon == 6, ]


## #addin unique grid to each bimon           
## uniq_gid_bimon1 <- ugrid
## uniq_gid_bimon2 <- ugrid
## uniq_gid_bimon3 <- ugrid
## uniq_gid_bimon4 <- ugrid
## uniq_gid_bimon5 <- ugrid
## uniq_gid_bimon6 <- ugrid

## #get predictions for Bimon residuals
## uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
## uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
## uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
## uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
## uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
## uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



## #merge things back togheter
## #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
## setkey(uniq_gid_bimon1,aodid)
## setkey(data.m3_bimon1,aodid)
## data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon2,aodid)
## setkey(data.m3_bimon2,aodid)
## data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon3,aodid)
## setkey(data.m3_bimon3,aodid)
## data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon4,aodid)
## setkey(data.m3_bimon4,aodid)
## data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon5,aodid)
## setkey(data.m3_bimon5,aodid)
## data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon6,aodid)
## setkey(data.m3_bimon6,aodid)
## data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

## #reattach all parts        
## mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
## # create pred.m3
## mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
## #hist(mod3$pred.m3)
## #describe(mod3$pred.m3)
## #recode negative into zero
## mod3 <- mod3[pred.m3 >= 0]
## saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2003.pred.rds")

## #clean
## keep(mod3,res,rmse, sure=TRUE) 
## gc()


## #########################
## #prepare for m3.R2
## #########################
## #load mod1
## mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.pred.rds")
## mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
## mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

## #R2.m3
## setkey(mod3,day,aodid)
## setkey(mod1,day,aodid)
## m1.2003 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
## m3.fit.2003<- summary(lm(PM10~pred.m3,data=m1.2003))
## res[res$year=="2003", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2003))$r.squared)    
## #RMSPE
## res[res$year=="2003", 'm3.PE'] <- print(rmse(residuals(m3.fit.2003)))

## #spatial
## ###to check
## spatial2003<-m1.2003 %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
## m1.fit.2003.spat<- lm(barpm ~ barpred, data=spatial2003)
## res[res$year=="2003", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003))$r.squared)
## res[res$year=="2003", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2003.spat)))
       
## #temporal
## tempo2003<-left_join(m1.2003,spatial2003)
## tempo2003$delpm <-tempo2003$PM10-tempo2003$barpm
## tempo2003$delpred <-tempo2003$pred.m3-tempo2003$barpred
## mod_temporal <- lm(delpm ~ delpred, data=tempo2003)
## res[res$year=="2003", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003))$r.squared)




## #############save midpoint
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2003.rds")
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

## #########################
## #import mod2
## mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2003.pred2.rds")
## mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

## #----------------> store the best available
## mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
## setkey(mod3best, day, aodid)
## setkey(mod2, day, aodid)
## mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
## #reload mod1
## mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2003.pred.rds")
## mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
## mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
## setkey(mod1,day,aodid)
## mod3best <- merge(mod3best, mod1, all.x = T)
## mod3best[,bestpred := pred.m3]
## mod3best[!is.na(pred.m2),bestpred := pred.m2]
## mod3best[!is.na(pred.m1),bestpred := pred.m1]
## #save
## saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2003.FINAL.rds")

## #save for GIS
## write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
##                           predvariance = var(bestpred, na.rm = T),
##                           predmin = min(bestpred, na.rm = T),
##                           predmax = max(bestpred, na.rm = T),
##                           npred = sum(!is.na(bestpred)),
##                           npred.m1 = sum(!is.na(pred.m1)),
##                           npred.m2 = sum(!is.na(pred.m2)),
##                           npred.m3 = sum(!is.na(pred.m3)),
##                           x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2003.csv", row.names = F)

## keep(res, sure=TRUE) 
## c()



###############
#LIBS
###############

## library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


## #sourcing
## source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
## source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

## #if needed load res table
## res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")


## ### import data
## m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.rds")

## #subset to aqua and apply alexei cleaning methods
## #MaskAdjacency == "000"
## ## m1.2004<-m1.2004[ UN > 0 & UN < 0.04  ] 

## ## ################# clean BAD STN PM10 and check if improved model?
## ## raWDaf <- ddply(m1.2004, c( "stn"), 
## ##       function(x) {
## ##         mod1 <- lm(PM10 ~ aod, data=x)
## ##         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
## ##                    nsamps = length(summary(mod1)$resid))
## ## })
## ## raWDaf
## ## raWDaf<-as.data.table(raWDaf)
## ## bad<- raWDaf[R2< 0.05]
## ## bad[,badid := paste(stn,sep="-")]
## ## #################BAD STN
## ## m1.2004[,badid := paste(stn,sep="-")]
## ## ####Take out bad stations
## ## m1.2004 <- m1.2004[!(m1.2004$badid %in% bad$badid), ] 

## #get rid of missing
## m1.2004[,elev.s:= scale(elev)]
## m1.2004[,tden.s:= scale(tden)]
## m1.2004[,pden.s:= scale(pden)]
## m1.2004[,dist2A1.s:= scale(dist2A1)]
## m1.2004[,dist2water.s:= scale(dist2water)]
## m1.2004[,dist2rail.s:= scale(dist2rail)]
## m1.2004[,Dist2road.s:= scale(Dist2road)]
## m1.2004[,ndvi.s:= scale(ndvi)]
## m1.2004[,MeanPbl.s:= scale(MeanPbl)]
## m1.2004[,p_ind.s:= scale(p_ind)]
## m1.2004[,p_for.s:= scale(p_for)]
## m1.2004[,p_farm.s:= scale(p_farm)]
## m1.2004[,p_dos.s:= scale(p_dos)]
## m1.2004[,p_dev.s:= scale(p_dev)]
## m1.2004[,p_os.s:= scale(p_os)]
## m1.2004[,tempa.s:= scale(tempa)]
## m1.2004[,WDa.s:= scale(WDa)]
## m1.2004[,WSa.s:= scale(WSa)]
## m1.2004[,RHa.s:= scale(RHa)]
## m1.2004[,Raina.s:= scale(Raina)]
## m1.2004[,NO2a.s:= scale(NO2a)]



## m1.formula <- as.formula(PM10~ aod
##                         +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
##                         +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
##                         +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
##                          #+aod*Dust #interactions
##                          +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

## #full fit
## m1.fit.2004 <-  lmer(m1.formula,data=m1.2004,weights=normwt)
## m1.2004$pred.m1 <- predict(m1.fit.2004)
## res[res$year=="2004", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2004))$r.squared)
## #RMSPE
## res[res$year=="2004", 'm1.PE'] <- print(rmse(residuals(m1.fit.2004)))

## #spatial
## ###to check
## spatial2004<-m1.2004 %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2004.spat<- lm(barpm ~ barpred, data=spatial2004)
## res[res$year=="2004", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004))$r.squared)
## res[res$year=="2004", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2004.spat)))
       
## #temporal
## tempo2004<-left_join(m1.2004,spatial2004)
## tempo2004$delpm <-tempo2004$PM10-tempo2004$barpm
## tempo2004$delpred <-tempo2004$pred.m1-tempo2004$barpred
## mod_temporal <- lm(delpm ~ delpred, data=tempo2004)
## res[res$year=="2004", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004))$r.squared)
## saveRDS(m1.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")



## #---------------->>>> CV
## #s1
## splits_s1 <- splitdf(m1.2004)
## test_s1 <- splits_s1$testset
## train_s1 <- splits_s1$trainset
## out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
## test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
## test_s1$iter<-"s1"
## #s2
## splits_s2 <- splitdf(m1.2004)
## test_s2 <- splits_s2$testset
## train_s2 <- splits_s2$trainset
## out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
## test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
## test_s2$iter<-"s2"
## #s3
## splits_s3 <- splitdf(m1.2004)
## test_s3 <- splits_s3$testset
## train_s3 <- splits_s3$trainset
## out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
## test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
## test_s3$iter<-"s3"
## #s4
## splits_s4 <- splitdf(m1.2004)
## test_s4 <- splits_s4$testset
## train_s4 <- splits_s4$trainset
## out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
## test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
## test_s4$iter<-"s4"
## #s5
## splits_s5 <- splitdf(m1.2004)
## test_s5 <- splits_s5$testset
## train_s5 <- splits_s5$trainset
## out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
## test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
## test_s5$iter<-"s5"
## #s6
## splits_s6 <- splitdf(m1.2004)
## test_s6 <- splits_s6$testset
## train_s6 <- splits_s6$trainset
## out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
## test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
## test_s6$iter<-"s6"
## #s7
## splits_s7 <- splitdf(m1.2004)
## test_s7 <- splits_s7$testset
## train_s7 <- splits_s7$trainset
## out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
## test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
## test_s7$iter<-"s7"
## #s8
## splits_s8 <- splitdf(m1.2004)
## test_s8 <- splits_s8$testset
## train_s8 <- splits_s8$trainset
## out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
## test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
## test_s8$iter<-"s8"
## #s9
## splits_s9 <- splitdf(m1.2004)
## test_s9 <- splits_s9$testset
## train_s9 <- splits_s9$trainset
## out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
## test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
## test_s9$iter<-"s9"
## #s10
## splits_s10 <- splitdf(m1.2004)
## test_s10 <- splits_s10$testset
## train_s10 <- splits_s10$trainset
## out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
## test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
## test_s10$iter<-"s10"

## #BIND 1 dataset
## m1.2004.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
## # cleanup (remove from WS) objects from CV
## rm(list = ls(pattern = "train_|test_"))
## #table updates
## m1.fit.2004.cv<-lm(PM10~pred.m1.cv,data=m1.2004.cv)
## res[res$year=="2004", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$r.squared)
## res[res$year=="2004", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[1,1])
## res[res$year=="2004", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[1,2])
## res[res$year=="2004", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[2,1])
## res[res$year=="2004", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[2,2])
## #RMSPE
## res[res$year=="2004", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

## #spatial
## spatial2004.cv<-m1.2004.cv %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2004.cv.s <- lm(barpm ~ barpred, data=spatial2004.cv)
## res[res$year=="2004", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv))$r.squared)
## res[res$year=="2004", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.s)))
       
## #temporal
## tempo2004.cv<-left_join(m1.2004.cv,spatial2004.cv)
## tempo2004.cv$delpm <-tempo2004.cv$PM10-tempo2004.cv$barpm
## tempo2004.cv$delpred <-tempo2004.cv$pred.m1.cv-tempo2004.cv$barpred
## mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2004.cv)
## res[res$year=="2004", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004.cv))$r.squared)



## #-------->>> loc stage

## luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
## setnames(luf,"tden","loc.tden")
## setnames(luf,"elev50","loc.elev")

## #add 50m LU to CV data
## setkey(m1.2004.cv,stn)
## setkey(luf,stn)
## m1.2004.cv.loc <- merge(m1.2004.cv, luf, all.x = T)
## #m1.2004.cv.loc<-na.omit(m1.2004.cv.loc)

## #create residual mp3 variable
## m1.2004.cv.loc$res.m1<-m1.2004.cv.loc$PM10-m1.2004.cv.loc$pred.m1.cv

## #The GAM model
## gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2004.cv.loc)
## #plot(bp.model.ps)
## #summary(bp.model.ps)
## ## reg
## m1.2004.cv.loc$pred.m1.loc <-predict(gam.out)
## m1.2004.cv.loc$pred.m1.both <- m1.2004.cv.loc$pred.m1.cv + m1.2004.cv.loc$pred.m1.loc
## res[res$year=="2004", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$r.squared)
## res[res$year=="2004", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[1,1])
## res[res$year=="2004", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[1,2])
## res[res$year=="2004", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[2,1])
## res[res$year=="2004", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[2,2])
## #RMSPE
## res[res$year=="2004", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

## #spatial
## spatial2004.cv.loc<-m1.2004.cv.loc %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
## m1.fit.2004.cv.loc.s <- lm(barpm ~ barpred, data=spatial2004.cv.loc)
## res[res$year=="2004", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv.loc))$r.squared)
## res[res$year=="2004", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.loc.s)))
       
## #temporal
## tempo2004.loc.cv<-left_join(m1.2004.cv.loc,spatial2004.cv.loc)
## tempo2004.loc.cv$delpm <-tempo2004.loc.cv$PM10-tempo2004.loc.cv$barpm
## tempo2004.loc.cv$delpred <-tempo2004.loc.cv$pred.m1.both-tempo2004.loc.cv$barpred
## mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2004.loc.cv)
## res[res$year=="2004", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004.loc.cv))$r.squared)

## #############save midpoint
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2004.rds")
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
## saveRDS(m1.2004.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.predCV.rds")


## ###############
## #MOD2
## ###############
## m2.2004<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")

## m2.2004[,elev.s:= scale(elev)]
## m2.2004[,tden.s:= scale(tden)]
## m2.2004[,pden.s:= scale(pden)]
## m2.2004[,dist2A1.s:= scale(dist2A1)]
## m2.2004[,dist2water.s:= scale(dist2water)]
## m2.2004[,dist2rail.s:= scale(dist2rail)]
## m2.2004[,Dist2road.s:= scale(Dist2road)]
## m2.2004[,ndvi.s:= scale(ndvi)]
## m2.2004[,MeanPbl.s:= scale(MeanPbl)]
## m2.2004[,p_ind.s:= scale(p_ind)]
## m2.2004[,p_for.s:= scale(p_for)]
## m2.2004[,p_farm.s:= scale(p_farm)]
## m2.2004[,p_dos.s:= scale(p_dos)]
## m2.2004[,p_dev.s:= scale(p_dev)]
## m2.2004[,p_os.s:= scale(p_os)]
## m2.2004[,tempa.s:= scale(tempa)]
## m2.2004[,WDa.s:= scale(WDa)]
## m2.2004[,WSa.s:= scale(WSa)]
## m2.2004[,RHa.s:= scale(RHa)]
## m2.2004[,Raina.s:= scale(Raina)]
## m2.2004[,NO2a.s:= scale(NO2a)]



## #generate predictions
## m2.2004[, pred.m2 := predict(object=m1.fit.2004,newdata=m2.2004,allow.new.levels=TRUE,re.form=NULL)]
## describe(m2.2004$pred.m2)
## #delete implossible valuesOA[24~
## m2.2004 <- m2.2004[pred.m2 > 0.00000000000001 , ]
## m2.2004 <- m2.2004[pred.m2 < 1500   , ]

## saveRDS(m2.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2004.pred2.rds")



## #-------------->prepare for mod3
## m2.2004[, bimon := (m + 1) %/% 2]
## setkey(m2.2004,day, aodid)
## m2.2004<-m2.2004[!is.na(meanPM10)]




## #run the lmer part regressing stage 2 pred Vs mean pm
## #in israel check per month, also check 30km band and other methods for meanpm
## m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2004 )
## #xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2004 )
## #correlate to see everything from mod2 and the mpm works
## m2.2004[, pred.t31 := predict(m2.smooth)]
## m2.2004[, resid  := residuals(m2.smooth)]
## res[res$year=="2004", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2004))$r.squared)


## #split the files to the separate bi monthly datsets
## T2004_bimon1 <- subset(m2.2004 ,m2.2004$bimon == "1")
## T2004_bimon2 <- subset(m2.2004 ,m2.2004$bimon == "2")
## T2004_bimon3 <- subset(m2.2004 ,m2.2004$bimon == "3")
## T2004_bimon4 <- subset(m2.2004 ,m2.2004$bimon == "4")
## T2004_bimon5 <- subset(m2.2004 ,m2.2004$bimon == "5")
## T2004_bimon6 <- subset(m2.2004 ,m2.2004$bimon == "6")

## #run the separate splines (smooth) for x and y for each bimon
## #whats the default band (distance) that the spline goes out and uses
## fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon1 )
## fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon2 )
## fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon3 )
## fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon4 )
## fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon5 )
## fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon6 )

## #get the predicted-fitted 
## Xpred_1 <- (T2004_bimon1$pred.t31 - fit2_1$fitted)
## Xpred_2 <- (T2004_bimon2$pred.t31 - fit2_2$fitted)
## Xpred_3 <- (T2004_bimon3$pred.t31 - fit2_3$fitted)
## Xpred_4 <- (T2004_bimon4$pred.t31 - fit2_4$fitted)
## Xpred_5 <- (T2004_bimon5$pred.t31 - fit2_5$fitted)
## Xpred_6 <- (T2004_bimon6$pred.t31 - fit2_6$fitted)

## #remerge to 1 file
## m2.2004$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
## #this is important so that its sorted as in the first gamm
## setkey(m2.2004,day, aodid)

## #rerun the lme on the predictions including the spatial spline (smooth)
## Final_pred_2004 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2004  )
## m2.2004[, pred.t33 := predict(Final_pred_2004)]
## #check correlations
## res[res$year=="2004", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2004))$r.squared) 


## #------------------------>>>
## #import mod3 
## data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2004.rds")

## #for PM10
## data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
## data.m3[, bimon := (m + 1) %/% 2]
## setkey(data.m3,day, aodid)
## data.m3<-data.m3[!is.na(meanPM10)]

## #generate m.3 initial pred
## data.m3$pred.m3.mix <-  predict(Final_pred_2004,data.m3)

## #create unique grid
## ugrid <-data.m3 %>%
##     group_by(aodid) %>%
##     summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


## #### PREDICT Gam part
## #split back into bimons to include the gam prediction in final prediction      	
## data.m3_bimon1 <- data.m3[bimon == 1, ]
## data.m3_bimon2 <- data.m3[bimon == 2, ]
## data.m3_bimon3 <- data.m3[bimon == 3, ]
## data.m3_bimon4 <- data.m3[bimon == 4, ]
## data.m3_bimon5 <- data.m3[bimon == 5, ]
## data.m3_bimon6 <- data.m3[bimon == 6, ]


## #addin unique grid to each bimon           
## uniq_gid_bimon1 <- ugrid
## uniq_gid_bimon2 <- ugrid
## uniq_gid_bimon3 <- ugrid
## uniq_gid_bimon4 <- ugrid
## uniq_gid_bimon5 <- ugrid
## uniq_gid_bimon6 <- ugrid

## #get predictions for Bimon residuals
## uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
## uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
## uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
## uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
## uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
## uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



## #merge things back togheter
## #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
## setkey(uniq_gid_bimon1,aodid)
## setkey(data.m3_bimon1,aodid)
## data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon2,aodid)
## setkey(data.m3_bimon2,aodid)
## data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon3,aodid)
## setkey(data.m3_bimon3,aodid)
## data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon4,aodid)
## setkey(data.m3_bimon4,aodid)
## data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon5,aodid)
## setkey(data.m3_bimon5,aodid)
## data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
## setkey(uniq_gid_bimon6,aodid)
## setkey(data.m3_bimon6,aodid)
## data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

## #reattach all parts        
## mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
## # create pred.m3
## mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
## #hist(mod3$pred.m3)
## #describe(mod3$pred.m3)
## #recode negative into zero
## mod3 <- mod3[pred.m3 >= 0]
## saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2004.pred.rds")

## #clean
## keep(mod3,res,rmse, sure=TRUE) 
## gc()


## #########################
## #prepare for m3.R2
## #########################
## #load mod1
## mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")
## mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
## mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

## #R2.m3
## setkey(mod3,day,aodid)
## setkey(mod1,day,aodid)
## m1.2004 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
## m3.fit.2004<- summary(lm(PM10~pred.m3,data=m1.2004))
## res[res$year=="2004", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2004))$r.squared)    
## #RMSPE
## res[res$year=="2004", 'm3.PE'] <- print(rmse(residuals(m3.fit.2004)))

## #spatial
## ###to check
## spatial2004<-m1.2004 %>%
##     group_by(stn) %>%
##     summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
## m1.fit.2004.spat<- lm(barpm ~ barpred, data=spatial2004)
## res[res$year=="2004", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004))$r.squared)
## res[res$year=="2004", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2004.spat)))
       
## #temporal
## tempo2004<-left_join(m1.2004,spatial2004)
## tempo2004$delpm <-tempo2004$PM10-tempo2004$barpm
## tempo2004$delpred <-tempo2004$pred.m3-tempo2004$barpred
## mod_temporal <- lm(delpm ~ delpred, data=tempo2004)
## res[res$year=="2004", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004))$r.squared)




## #############save midpoint
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2004.rds")
## saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

## #########################
## #import mod2
## mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2004.pred2.rds")
## mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

## #----------------> store the best available
## mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
## setkey(mod3best, day, aodid)
## setkey(mod2, day, aodid)
## mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
## #reload mod1
## mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")
## mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
## mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
## setkey(mod1,day,aodid)
## mod3best <- merge(mod3best, mod1, all.x = T)
## mod3best[,bestpred := pred.m3]
## mod3best[!is.na(pred.m2),bestpred := pred.m2]
## mod3best[!is.na(pred.m1),bestpred := pred.m1]
## #save
## saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2004.FINAL.rds")

## #save for GIS
## write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
##                           predvariance = var(bestpred, na.rm = T),
##                           predmin = min(bestpred, na.rm = T),
##                           predmax = max(bestpred, na.rm = T),
##                           npred = sum(!is.na(bestpred)),
##                           npred.m1 = sum(!is.na(pred.m1)),
##                           npred.m2 = sum(!is.na(pred.m2)),
##                           npred.m3 = sum(!is.na(pred.m3)),
##                           x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2004.csv", row.names = F)

## keep(res, sure=TRUE) 
## gc()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")



### import data
m1.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
## m1.2005<-m1.2005[ UN > 0 & UN < 0.04  ] 

## ################# clean BAD STN PM10 and check if improved model?
## raWDaf <- ddply(m1.2005, c( "stn"), 
##       function(x) {
##         mod1 <- lm(PM10 ~ aod, data=x)
##         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
##                    nsamps = length(summary(mod1)$resid))
## })
## raWDaf
## raWDaf<-as.data.table(raWDaf)
## bad<- raWDaf[R2< 0.05]
## bad[,badid := paste(stn,sep="-")]
## #################BAD STN
## m1.2005[,badid := paste(stn,sep="-")]
## ####Take out bad stations
## m1.2005 <- m1.2005[!(m1.2005$badid %in% bad$badid), ] 

#get rid of missing
m1.2005[,elev.s:= scale(elev)]
m1.2005[,tden.s:= scale(tden)]
m1.2005[,pden.s:= scale(pden)]
m1.2005[,dist2A1.s:= scale(dist2A1)]
m1.2005[,dist2water.s:= scale(dist2water)]
m1.2005[,dist2rail.s:= scale(dist2rail)]
m1.2005[,Dist2road.s:= scale(Dist2road)]
m1.2005[,ndvi.s:= scale(ndvi)]
m1.2005[,MeanPbl.s:= scale(MeanPbl)]
m1.2005[,p_ind.s:= scale(p_ind)]
m1.2005[,p_for.s:= scale(p_for)]
m1.2005[,p_farm.s:= scale(p_farm)]
m1.2005[,p_dos.s:= scale(p_dos)]
m1.2005[,p_dev.s:= scale(p_dev)]
m1.2005[,p_os.s:= scale(p_os)]
m1.2005[,tempa.s:= scale(tempa)]
m1.2005[,WDa.s:= scale(WDa)]
m1.2005[,WSa.s:= scale(WSa)]
m1.2005[,RHa.s:= scale(RHa)]
m1.2005[,Raina.s:= scale(Raina)]
m1.2005[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2005 <-  lmer(m1.formula,data=m1.2005,weights=normwt)
m1.2005$pred.m1 <- predict(m1.fit.2005)
res[res$year=="2005", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2005))$r.squared)
#RMSPE
res[res$year=="2005", 'm1.PE'] <- print(rmse(residuals(m1.fit.2005)))

#spatial
###to check
spatial2005<-m1.2005 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.spat<- lm(barpm ~ barpred, data=spatial2005)
res[res$year=="2005", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005))$r.squared)
res[res$year=="2005", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2005.spat)))
       
#temporal
tempo2005<-left_join(m1.2005,spatial2005)
tempo2005$delpm <-tempo2005$PM10-tempo2005$barpm
tempo2005$delpred <-tempo2005$pred.m1-tempo2005$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2005)
res[res$year=="2005", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005))$r.squared)
saveRDS(m1.2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2005)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2005)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2005)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2005)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2005)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2005)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2005)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2005)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2005)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2005)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2005.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2005.cv<-lm(PM10~pred.m1.cv,data=m1.2005.cv)
res[res$year=="2005", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2005.cv))$r.squared)
res[res$year=="2005", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2005.cv))$coef[1,1])
res[res$year=="2005", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2005.cv))$coef[1,2])
res[res$year=="2005", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2005.cv))$coef[2,1])
res[res$year=="2005", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2005.cv))$coef[2,2])
#RMSPE
res[res$year=="2005", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2005.cv)))

#spatial
spatial2005.cv<-m1.2005.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.cv.s <- lm(barpm ~ barpred, data=spatial2005.cv)
res[res$year=="2005", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005.cv))$r.squared)
res[res$year=="2005", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2005.cv.s)))
       
#temporal
tempo2005.cv<-left_join(m1.2005.cv,spatial2005.cv)
tempo2005.cv$delpm <-tempo2005.cv$PM10-tempo2005.cv$barpm
tempo2005.cv$delpred <-tempo2005.cv$pred.m1.cv-tempo2005.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2005.cv)
res[res$year=="2005", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2005.cv,stn)
setkey(luf,stn)
m1.2005.cv.loc <- merge(m1.2005.cv, luf, all.x = T)
#m1.2005.cv.loc<-na.omit(m1.2005.cv.loc)

#create residual mp3 variable
m1.2005.cv.loc$res.m1<-m1.2005.cv.loc$PM10-m1.2005.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2005.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2005.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2005.cv.loc$pred.m1.both <- m1.2005.cv.loc$pred.m1.cv + m1.2005.cv.loc$pred.m1.loc
res[res$year=="2005", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2005.cv.loc))$r.squared)
res[res$year=="2005", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2005.cv.loc))$coef[1,1])
res[res$year=="2005", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2005.cv.loc))$coef[1,2])
res[res$year=="2005", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2005.cv.loc))$coef[2,1])
res[res$year=="2005", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2005.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2005", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2005.cv)))

#spatial
spatial2005.cv.loc<-m1.2005.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.cv.loc.s <- lm(barpm ~ barpred, data=spatial2005.cv.loc)
res[res$year=="2005", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005.cv.loc))$r.squared)
res[res$year=="2005", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2005.cv.loc.s)))
       
#temporal
tempo2005.loc.cv<-left_join(m1.2005.cv.loc,spatial2005.cv.loc)
tempo2005.loc.cv$delpm <-tempo2005.loc.cv$PM10-tempo2005.loc.cv$barpm
tempo2005.loc.cv$delpred <-tempo2005.loc.cv$pred.m1.both-tempo2005.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2005.loc.cv)
res[res$year=="2005", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2005.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2005.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.predCV.rds")


###############
#MOD2
###############
m2.2005<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2005.rds")

m2.2005[,elev.s:= scale(elev)]
m2.2005[,tden.s:= scale(tden)]
m2.2005[,pden.s:= scale(pden)]
m2.2005[,dist2A1.s:= scale(dist2A1)]
m2.2005[,dist2water.s:= scale(dist2water)]
m2.2005[,dist2rail.s:= scale(dist2rail)]
m2.2005[,Dist2road.s:= scale(Dist2road)]
m2.2005[,ndvi.s:= scale(ndvi)]
m2.2005[,MeanPbl.s:= scale(MeanPbl)]
m2.2005[,p_ind.s:= scale(p_ind)]
m2.2005[,p_for.s:= scale(p_for)]
m2.2005[,p_farm.s:= scale(p_farm)]
m2.2005[,p_dos.s:= scale(p_dos)]
m2.2005[,p_dev.s:= scale(p_dev)]
m2.2005[,p_os.s:= scale(p_os)]
m2.2005[,tempa.s:= scale(tempa)]
m2.2005[,WDa.s:= scale(WDa)]
m2.2005[,WSa.s:= scale(WSa)]
m2.2005[,RHa.s:= scale(RHa)]
m2.2005[,Raina.s:= scale(Raina)]
m2.2005[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2005[, pred.m2 := predict(object=m1.fit.2005,newdata=m2.2005,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2005$pred.m2)
#delete implossible valuesOA[24~
m2.2005 <- m2.2005[pred.m2 > 0.00000000000001 , ]
m2.2005 <- m2.2005[pred.m2 < 1500   , ]

saveRDS(m2.2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2005.pred2.rds")



#-------------->prepare for mod3
m2.2005[, bimon := (m + 1) %/% 2]
setkey(m2.2005,day, aodid)
m2.2005<-m2.2005[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2005 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2005 )
#correlate to see everything from mod2 and the mpm works
m2.2005[, pred.t31 := predict(m2.smooth)]
m2.2005[, resid  := residuals(m2.smooth)]
res[res$year=="2005", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2005))$r.squared)


#split the files to the separate bi monthly datsets
T2005_bimon1 <- subset(m2.2005 ,m2.2005$bimon == "1")
T2005_bimon2 <- subset(m2.2005 ,m2.2005$bimon == "2")
T2005_bimon3 <- subset(m2.2005 ,m2.2005$bimon == "3")
T2005_bimon4 <- subset(m2.2005 ,m2.2005$bimon == "4")
T2005_bimon5 <- subset(m2.2005 ,m2.2005$bimon == "5")
T2005_bimon6 <- subset(m2.2005 ,m2.2005$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2005_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2005_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2005_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2005_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2005_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2005_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2005_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2005$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2005,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2005 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2005  )
m2.2005[, pred.t33 := predict(Final_pred_2005)]
#check correlations
res[res$year=="2005", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2005))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2005.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2005,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2005.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2005 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2005<- summary(lm(PM10~pred.m3,data=m1.2005))
res[res$year=="2005", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2005))$r.squared)    
#RMSPE
res[res$year=="2005", 'm3.PE'] <- print(rmse(residuals(m3.fit.2005)))

#spatial
###to check
spatial2005<-m1.2005 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2005.spat<- lm(barpm ~ barpred, data=spatial2005)
res[res$year=="2005", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005))$r.squared)
res[res$year=="2005", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2005.spat)))
       
#temporal
tempo2005<-left_join(m1.2005,spatial2005)
tempo2005$delpm <-tempo2005$PM10-tempo2005$barpm
tempo2005$delpred <-tempo2005$pred.m3-tempo2005$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2005)
res[res$year=="2005", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2005.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2005.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2005.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2005.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2005.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")


### import data
m1.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2006<-m1.2006[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2006, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2006[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2006 <- m1.2006[!(m1.2006$badid %in% bad$badid), ] 

#get rid of missing
m1.2006[,elev.s:= scale(elev)]
m1.2006[,tden.s:= scale(tden)]
m1.2006[,pden.s:= scale(pden)]
m1.2006[,dist2A1.s:= scale(dist2A1)]
m1.2006[,dist2water.s:= scale(dist2water)]
m1.2006[,dist2rail.s:= scale(dist2rail)]
m1.2006[,Dist2road.s:= scale(Dist2road)]
m1.2006[,ndvi.s:= scale(ndvi)]
m1.2006[,MeanPbl.s:= scale(MeanPbl)]
m1.2006[,p_ind.s:= scale(p_ind)]
m1.2006[,p_for.s:= scale(p_for)]
m1.2006[,p_farm.s:= scale(p_farm)]
m1.2006[,p_dos.s:= scale(p_dos)]
m1.2006[,p_dev.s:= scale(p_dev)]
m1.2006[,p_os.s:= scale(p_os)]
m1.2006[,tempa.s:= scale(tempa)]
m1.2006[,WDa.s:= scale(WDa)]
m1.2006[,WSa.s:= scale(WSa)]
m1.2006[,RHa.s:= scale(RHa)]
m1.2006[,Raina.s:= scale(Raina)]
m1.2006[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2006 <-  lmer(m1.formula,data=m1.2006,weights=normwt)
m1.2006$pred.m1 <- predict(m1.fit.2006)
res[res$year=="2006", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2006))$r.squared)
#RMSPE
res[res$year=="2006", 'm1.PE'] <- print(rmse(residuals(m1.fit.2006)))

#spatial
###to check
spatial2006<-m1.2006 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.spat<- lm(barpm ~ barpred, data=spatial2006)
res[res$year=="2006", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006))$r.squared)
res[res$year=="2006", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2006.spat)))
       
#temporal
tempo2006<-left_join(m1.2006,spatial2006)
tempo2006$delpm <-tempo2006$PM10-tempo2006$barpm
tempo2006$delpred <-tempo2006$pred.m1-tempo2006$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2006)
res[res$year=="2006", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006))$r.squared)
saveRDS(m1.2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2006)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2006)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2006)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2006)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2006)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2006)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2006)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2006)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2006)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2006)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2006.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2006.cv<-lm(PM10~pred.m1.cv,data=m1.2006.cv)
res[res$year=="2006", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2006.cv))$r.squared)
res[res$year=="2006", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2006.cv))$coef[1,1])
res[res$year=="2006", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2006.cv))$coef[1,2])
res[res$year=="2006", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2006.cv))$coef[2,1])
res[res$year=="2006", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2006.cv))$coef[2,2])
#RMSPE
res[res$year=="2006", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2006.cv)))

#spatial
spatial2006.cv<-m1.2006.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.cv.s <- lm(barpm ~ barpred, data=spatial2006.cv)
res[res$year=="2006", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006.cv))$r.squared)
res[res$year=="2006", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2006.cv.s)))
       
#temporal
tempo2006.cv<-left_join(m1.2006.cv,spatial2006.cv)
tempo2006.cv$delpm <-tempo2006.cv$PM10-tempo2006.cv$barpm
tempo2006.cv$delpred <-tempo2006.cv$pred.m1.cv-tempo2006.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2006.cv)
res[res$year=="2006", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2006.cv,stn)
setkey(luf,stn)
m1.2006.cv.loc <- merge(m1.2006.cv, luf, all.x = T)
#m1.2006.cv.loc<-na.omit(m1.2006.cv.loc)

#create residual mp3 variable
m1.2006.cv.loc$res.m1<-m1.2006.cv.loc$PM10-m1.2006.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2006.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2006.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2006.cv.loc$pred.m1.both <- m1.2006.cv.loc$pred.m1.cv + m1.2006.cv.loc$pred.m1.loc
res[res$year=="2006", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2006.cv.loc))$r.squared)
res[res$year=="2006", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2006.cv.loc))$coef[1,1])
res[res$year=="2006", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2006.cv.loc))$coef[1,2])
res[res$year=="2006", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2006.cv.loc))$coef[2,1])
res[res$year=="2006", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2006.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2006", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2006.cv)))

#spatial
spatial2006.cv.loc<-m1.2006.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.cv.loc.s <- lm(barpm ~ barpred, data=spatial2006.cv.loc)
res[res$year=="2006", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006.cv.loc))$r.squared)
res[res$year=="2006", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2006.cv.loc.s)))
       
#temporal
tempo2006.loc.cv<-left_join(m1.2006.cv.loc,spatial2006.cv.loc)
tempo2006.loc.cv$delpm <-tempo2006.loc.cv$PM10-tempo2006.loc.cv$barpm
tempo2006.loc.cv$delpred <-tempo2006.loc.cv$pred.m1.both-tempo2006.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2006.loc.cv)
res[res$year=="2006", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2006.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2006.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.predCV.rds")


###############
#MOD2
###############
m2.2006<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2006.rds")

m2.2006[,elev.s:= scale(elev)]
m2.2006[,tden.s:= scale(tden)]
m2.2006[,pden.s:= scale(pden)]
m2.2006[,dist2A1.s:= scale(dist2A1)]
m2.2006[,dist2water.s:= scale(dist2water)]
m2.2006[,dist2rail.s:= scale(dist2rail)]
m2.2006[,Dist2road.s:= scale(Dist2road)]
m2.2006[,ndvi.s:= scale(ndvi)]
m2.2006[,MeanPbl.s:= scale(MeanPbl)]
m2.2006[,p_ind.s:= scale(p_ind)]
m2.2006[,p_for.s:= scale(p_for)]
m2.2006[,p_farm.s:= scale(p_farm)]
m2.2006[,p_dos.s:= scale(p_dos)]
m2.2006[,p_dev.s:= scale(p_dev)]
m2.2006[,p_os.s:= scale(p_os)]
m2.2006[,tempa.s:= scale(tempa)]
m2.2006[,WDa.s:= scale(WDa)]
m2.2006[,WSa.s:= scale(WSa)]
m2.2006[,RHa.s:= scale(RHa)]
m2.2006[,Raina.s:= scale(Raina)]
m2.2006[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2006[, pred.m2 := predict(object=m1.fit.2006,newdata=m2.2006,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2006$pred.m2)
#delete implossible valuesOA[24~
m2.2006 <- m2.2006[pred.m2 > 0.00000000000001 , ]
m2.2006 <- m2.2006[pred.m2 < 1500   , ]

saveRDS(m2.2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2006.pred2.rds")



#-------------->prepare for mod3
m2.2006[, bimon := (m + 1) %/% 2]
setkey(m2.2006,day, aodid)
m2.2006<-m2.2006[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2006 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2006 )
#correlate to see everything from mod2 and the mpm works
m2.2006[, pred.t31 := predict(m2.smooth)]
m2.2006[, resid  := residuals(m2.smooth)]
res[res$year=="2006", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2006))$r.squared)


#split the files to the separate bi monthly datsets
T2006_bimon1 <- subset(m2.2006 ,m2.2006$bimon == "1")
T2006_bimon2 <- subset(m2.2006 ,m2.2006$bimon == "2")
T2006_bimon3 <- subset(m2.2006 ,m2.2006$bimon == "3")
T2006_bimon4 <- subset(m2.2006 ,m2.2006$bimon == "4")
T2006_bimon5 <- subset(m2.2006 ,m2.2006$bimon == "5")
T2006_bimon6 <- subset(m2.2006 ,m2.2006$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2006_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2006_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2006_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2006_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2006_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2006_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2006_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2006$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2006,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2006 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2006  )
m2.2006[, pred.t33 := predict(Final_pred_2006)]
#check correlations
res[res$year=="2006", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2006))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2006.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2006,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2006.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2006 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2006<- summary(lm(PM10~pred.m3,data=m1.2006))
res[res$year=="2006", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2006))$r.squared)    
#RMSPE
res[res$year=="2006", 'm3.PE'] <- print(rmse(residuals(m3.fit.2006)))

#spatial
###to check
spatial2006<-m1.2006 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2006.spat<- lm(barpm ~ barpred, data=spatial2006)
res[res$year=="2006", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006))$r.squared)
res[res$year=="2006", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2006.spat)))
       
#temporal
tempo2006<-left_join(m1.2006,spatial2006)
tempo2006$delpm <-tempo2006$PM10-tempo2006$barpm
tempo2006$delpred <-tempo2006$pred.m3-tempo2006$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2006)
res[res$year=="2006", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2006.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2006.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2006.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2006.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2006.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")



### import data
m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2007<-m1.2007[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2007, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2007[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2007 <- m1.2007[!(m1.2007$badid %in% bad$badid), ] 

#get rid of missing
m1.2007[,elev.s:= scale(elev)]
m1.2007[,tden.s:= scale(tden)]
m1.2007[,pden.s:= scale(pden)]
m1.2007[,dist2A1.s:= scale(dist2A1)]
m1.2007[,dist2water.s:= scale(dist2water)]
m1.2007[,dist2rail.s:= scale(dist2rail)]
m1.2007[,Dist2road.s:= scale(Dist2road)]
m1.2007[,ndvi.s:= scale(ndvi)]
m1.2007[,MeanPbl.s:= scale(MeanPbl)]
m1.2007[,p_ind.s:= scale(p_ind)]
m1.2007[,p_for.s:= scale(p_for)]
m1.2007[,p_farm.s:= scale(p_farm)]
m1.2007[,p_dos.s:= scale(p_dos)]
m1.2007[,p_dev.s:= scale(p_dev)]
m1.2007[,p_os.s:= scale(p_os)]
m1.2007[,tempa.s:= scale(tempa)]
m1.2007[,WDa.s:= scale(WDa)]
m1.2007[,WSa.s:= scale(WSa)]
m1.2007[,RHa.s:= scale(RHa)]
m1.2007[,Raina.s:= scale(Raina)]
m1.2007[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2007 <-  lmer(m1.formula,data=m1.2007,weights=normwt)
m1.2007$pred.m1 <- predict(m1.fit.2007)
res[res$year=="2007", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2007))$r.squared)
#RMSPE
res[res$year=="2007", 'm1.PE'] <- print(rmse(residuals(m1.fit.2007)))

#spatial
###to check
spatial2007<-m1.2007 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.spat<- lm(barpm ~ barpred, data=spatial2007)
res[res$year=="2007", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007))$r.squared)
res[res$year=="2007", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2007.spat)))
       
#temporal
tempo2007<-left_join(m1.2007,spatial2007)
tempo2007$delpm <-tempo2007$PM10-tempo2007$barpm
tempo2007$delpred <-tempo2007$pred.m1-tempo2007$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2007)
res[res$year=="2007", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007))$r.squared)
saveRDS(m1.2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2007)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2007)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2007)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2007)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2007)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2007)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2007)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2007)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2007)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2007)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2007.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2007.cv<-lm(PM10~pred.m1.cv,data=m1.2007.cv)
res[res$year=="2007", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2007.cv))$r.squared)
res[res$year=="2007", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2007.cv))$coef[1,1])
res[res$year=="2007", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2007.cv))$coef[1,2])
res[res$year=="2007", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2007.cv))$coef[2,1])
res[res$year=="2007", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2007.cv))$coef[2,2])
#RMSPE
res[res$year=="2007", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2007.cv)))

#spatial
spatial2007.cv<-m1.2007.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.cv.s <- lm(barpm ~ barpred, data=spatial2007.cv)
res[res$year=="2007", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007.cv))$r.squared)
res[res$year=="2007", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2007.cv.s)))
       
#temporal
tempo2007.cv<-left_join(m1.2007.cv,spatial2007.cv)
tempo2007.cv$delpm <-tempo2007.cv$PM10-tempo2007.cv$barpm
tempo2007.cv$delpred <-tempo2007.cv$pred.m1.cv-tempo2007.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2007.cv)
res[res$year=="2007", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2007.cv,stn)
setkey(luf,stn)
m1.2007.cv.loc <- merge(m1.2007.cv, luf, all.x = T)
#m1.2007.cv.loc<-na.omit(m1.2007.cv.loc)

#create residual mp3 variable
m1.2007.cv.loc$res.m1<-m1.2007.cv.loc$PM10-m1.2007.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2007.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2007.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2007.cv.loc$pred.m1.both <- m1.2007.cv.loc$pred.m1.cv + m1.2007.cv.loc$pred.m1.loc
res[res$year=="2007", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2007.cv.loc))$r.squared)
res[res$year=="2007", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2007.cv.loc))$coef[1,1])
res[res$year=="2007", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2007.cv.loc))$coef[1,2])
res[res$year=="2007", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2007.cv.loc))$coef[2,1])
res[res$year=="2007", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2007.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2007", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2007.cv)))

#spatial
spatial2007.cv.loc<-m1.2007.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.cv.loc.s <- lm(barpm ~ barpred, data=spatial2007.cv.loc)
res[res$year=="2007", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007.cv.loc))$r.squared)
res[res$year=="2007", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2007.cv.loc.s)))
       
#temporal
tempo2007.loc.cv<-left_join(m1.2007.cv.loc,spatial2007.cv.loc)
tempo2007.loc.cv$delpm <-tempo2007.loc.cv$PM10-tempo2007.loc.cv$barpm
tempo2007.loc.cv$delpred <-tempo2007.loc.cv$pred.m1.both-tempo2007.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2007.loc.cv)
res[res$year=="2007", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2007.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2007.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.predCV.rds")


###############
#MOD2
###############
m2.2007<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.rds")

m2.2007[,elev.s:= scale(elev)]
m2.2007[,tden.s:= scale(tden)]
m2.2007[,pden.s:= scale(pden)]
m2.2007[,dist2A1.s:= scale(dist2A1)]
m2.2007[,dist2water.s:= scale(dist2water)]
m2.2007[,dist2rail.s:= scale(dist2rail)]
m2.2007[,Dist2road.s:= scale(Dist2road)]
m2.2007[,ndvi.s:= scale(ndvi)]
m2.2007[,MeanPbl.s:= scale(MeanPbl)]
m2.2007[,p_ind.s:= scale(p_ind)]
m2.2007[,p_for.s:= scale(p_for)]
m2.2007[,p_farm.s:= scale(p_farm)]
m2.2007[,p_dos.s:= scale(p_dos)]
m2.2007[,p_dev.s:= scale(p_dev)]
m2.2007[,p_os.s:= scale(p_os)]
m2.2007[,tempa.s:= scale(tempa)]
m2.2007[,WDa.s:= scale(WDa)]
m2.2007[,WSa.s:= scale(WSa)]
m2.2007[,RHa.s:= scale(RHa)]
m2.2007[,Raina.s:= scale(Raina)]
m2.2007[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2007[, pred.m2 := predict(object=m1.fit.2007,newdata=m2.2007,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2007$pred.m2)
#delete implossible valuesOA[24~
m2.2007 <- m2.2007[pred.m2 > 0.00000000000001 , ]
m2.2007 <- m2.2007[pred.m2 < 1500   , ]

saveRDS(m2.2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2007.pred2.rds")



#-------------->prepare for mod3
m2.2007[, bimon := (m + 1) %/% 2]
setkey(m2.2007,day, aodid)
m2.2007<-m2.2007[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2007 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2007 )
#correlate to see everything from mod2 and the mpm works
m2.2007[, pred.t31 := predict(m2.smooth)]
m2.2007[, resid  := residuals(m2.smooth)]
res[res$year=="2007", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2007))$r.squared)


#split the files to the separate bi monthly datsets
T2007_bimon1 <- subset(m2.2007 ,m2.2007$bimon == "1")
T2007_bimon2 <- subset(m2.2007 ,m2.2007$bimon == "2")
T2007_bimon3 <- subset(m2.2007 ,m2.2007$bimon == "3")
T2007_bimon4 <- subset(m2.2007 ,m2.2007$bimon == "4")
T2007_bimon5 <- subset(m2.2007 ,m2.2007$bimon == "5")
T2007_bimon6 <- subset(m2.2007 ,m2.2007$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2007_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2007_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2007_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2007_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2007_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2007_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2007$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2007,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2007 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2007  )
m2.2007[, pred.t33 := predict(Final_pred_2007)]
#check correlations
res[res$year=="2007", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2007))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2007.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2007,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2007.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2007 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2007<- summary(lm(PM10~pred.m3,data=m1.2007))
res[res$year=="2007", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2007))$r.squared)    
#RMSPE
res[res$year=="2007", 'm3.PE'] <- print(rmse(residuals(m3.fit.2007)))

#spatial
###to check
spatial2007<-m1.2007 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2007.spat<- lm(barpm ~ barpred, data=spatial2007)
res[res$year=="2007", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007))$r.squared)
res[res$year=="2007", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2007.spat)))
       
#temporal
tempo2007<-left_join(m1.2007,spatial2007)
tempo2007$delpm <-tempo2007$PM10-tempo2007$barpm
tempo2007$delpred <-tempo2007$pred.m3-tempo2007$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2007)
res[res$year=="2007", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2007.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2007.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2007.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2007.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2007.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")





### import data
m1.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2008<-m1.2008[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2008, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2008[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2008 <- m1.2008[!(m1.2008$badid %in% bad$badid), ] 

#get rid of missing
m1.2008[,elev.s:= scale(elev)]
m1.2008[,tden.s:= scale(tden)]
m1.2008[,pden.s:= scale(pden)]
m1.2008[,dist2A1.s:= scale(dist2A1)]
m1.2008[,dist2water.s:= scale(dist2water)]
m1.2008[,dist2rail.s:= scale(dist2rail)]
m1.2008[,Dist2road.s:= scale(Dist2road)]
m1.2008[,ndvi.s:= scale(ndvi)]
m1.2008[,MeanPbl.s:= scale(MeanPbl)]
m1.2008[,p_ind.s:= scale(p_ind)]
m1.2008[,p_for.s:= scale(p_for)]
m1.2008[,p_farm.s:= scale(p_farm)]
m1.2008[,p_dos.s:= scale(p_dos)]
m1.2008[,p_dev.s:= scale(p_dev)]
m1.2008[,p_os.s:= scale(p_os)]
m1.2008[,tempa.s:= scale(tempa)]
m1.2008[,WDa.s:= scale(WDa)]
m1.2008[,WSa.s:= scale(WSa)]
m1.2008[,RHa.s:= scale(RHa)]
m1.2008[,Raina.s:= scale(Raina)]
m1.2008[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2008 <-  lmer(m1.formula,data=m1.2008,weights=normwt)
m1.2008$pred.m1 <- predict(m1.fit.2008)
res[res$year=="2008", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2008))$r.squared)
#RMSPE
res[res$year=="2008", 'm1.PE'] <- print(rmse(residuals(m1.fit.2008)))

#spatial
###to check
spatial2008<-m1.2008 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.spat<- lm(barpm ~ barpred, data=spatial2008)
res[res$year=="2008", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008))$r.squared)
res[res$year=="2008", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2008.spat)))
       
#temporal
tempo2008<-left_join(m1.2008,spatial2008)
tempo2008$delpm <-tempo2008$PM10-tempo2008$barpm
tempo2008$delpred <-tempo2008$pred.m1-tempo2008$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2008)
res[res$year=="2008", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008))$r.squared)
saveRDS(m1.2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2008)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2008)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2008)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2008)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2008)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2008)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2008)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2008)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2008)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2008)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2008.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2008.cv<-lm(PM10~pred.m1.cv,data=m1.2008.cv)
res[res$year=="2008", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2008.cv))$r.squared)
res[res$year=="2008", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2008.cv))$coef[1,1])
res[res$year=="2008", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2008.cv))$coef[1,2])
res[res$year=="2008", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2008.cv))$coef[2,1])
res[res$year=="2008", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2008.cv))$coef[2,2])
#RMSPE
res[res$year=="2008", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2008.cv)))

#spatial
spatial2008.cv<-m1.2008.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.cv.s <- lm(barpm ~ barpred, data=spatial2008.cv)
res[res$year=="2008", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008.cv))$r.squared)
res[res$year=="2008", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2008.cv.s)))
       
#temporal
tempo2008.cv<-left_join(m1.2008.cv,spatial2008.cv)
tempo2008.cv$delpm <-tempo2008.cv$PM10-tempo2008.cv$barpm
tempo2008.cv$delpred <-tempo2008.cv$pred.m1.cv-tempo2008.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2008.cv)
res[res$year=="2008", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2008.cv,stn)
setkey(luf,stn)
m1.2008.cv.loc <- merge(m1.2008.cv, luf, all.x = T)
#m1.2008.cv.loc<-na.omit(m1.2008.cv.loc)

#create residual mp3 variable
m1.2008.cv.loc$res.m1<-m1.2008.cv.loc$PM10-m1.2008.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2008.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2008.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2008.cv.loc$pred.m1.both <- m1.2008.cv.loc$pred.m1.cv + m1.2008.cv.loc$pred.m1.loc
res[res$year=="2008", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2008.cv.loc))$r.squared)
res[res$year=="2008", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2008.cv.loc))$coef[1,1])
res[res$year=="2008", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2008.cv.loc))$coef[1,2])
res[res$year=="2008", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2008.cv.loc))$coef[2,1])
res[res$year=="2008", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2008.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2008", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2008.cv)))

#spatial
spatial2008.cv.loc<-m1.2008.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.cv.loc.s <- lm(barpm ~ barpred, data=spatial2008.cv.loc)
res[res$year=="2008", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008.cv.loc))$r.squared)
res[res$year=="2008", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2008.cv.loc.s)))
       
#temporal
tempo2008.loc.cv<-left_join(m1.2008.cv.loc,spatial2008.cv.loc)
tempo2008.loc.cv$delpm <-tempo2008.loc.cv$PM10-tempo2008.loc.cv$barpm
tempo2008.loc.cv$delpred <-tempo2008.loc.cv$pred.m1.both-tempo2008.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2008.loc.cv)
res[res$year=="2008", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2008.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2008.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.predCV.rds")


###############
#MOD2
###############
m2.2008<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2008.rds")

m2.2008[,elev.s:= scale(elev)]
m2.2008[,tden.s:= scale(tden)]
m2.2008[,pden.s:= scale(pden)]
m2.2008[,dist2A1.s:= scale(dist2A1)]
m2.2008[,dist2water.s:= scale(dist2water)]
m2.2008[,dist2rail.s:= scale(dist2rail)]
m2.2008[,Dist2road.s:= scale(Dist2road)]
m2.2008[,ndvi.s:= scale(ndvi)]
m2.2008[,MeanPbl.s:= scale(MeanPbl)]
m2.2008[,p_ind.s:= scale(p_ind)]
m2.2008[,p_for.s:= scale(p_for)]
m2.2008[,p_farm.s:= scale(p_farm)]
m2.2008[,p_dos.s:= scale(p_dos)]
m2.2008[,p_dev.s:= scale(p_dev)]
m2.2008[,p_os.s:= scale(p_os)]
m2.2008[,tempa.s:= scale(tempa)]
m2.2008[,WDa.s:= scale(WDa)]
m2.2008[,WSa.s:= scale(WSa)]
m2.2008[,RHa.s:= scale(RHa)]
m2.2008[,Raina.s:= scale(Raina)]
m2.2008[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2008[, pred.m2 := predict(object=m1.fit.2008,newdata=m2.2008,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2008$pred.m2)
#delete implossible valuesOA[24~
m2.2008 <- m2.2008[pred.m2 > 0.00000000000001 , ]
m2.2008 <- m2.2008[pred.m2 < 1500   , ]

saveRDS(m2.2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2008.pred2.rds")



#-------------->prepare for mod3
m2.2008[, bimon := (m + 1) %/% 2]
setkey(m2.2008,day, aodid)
m2.2008<-m2.2008[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2008 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2008 )
#correlate to see everything from mod2 and the mpm works
m2.2008[, pred.t31 := predict(m2.smooth)]
m2.2008[, resid  := residuals(m2.smooth)]
res[res$year=="2008", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2008))$r.squared)


#split the files to the separate bi monthly datsets
T2008_bimon1 <- subset(m2.2008 ,m2.2008$bimon == "1")
T2008_bimon2 <- subset(m2.2008 ,m2.2008$bimon == "2")
T2008_bimon3 <- subset(m2.2008 ,m2.2008$bimon == "3")
T2008_bimon4 <- subset(m2.2008 ,m2.2008$bimon == "4")
T2008_bimon5 <- subset(m2.2008 ,m2.2008$bimon == "5")
T2008_bimon6 <- subset(m2.2008 ,m2.2008$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2008_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2008_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2008_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2008_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2008_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2008_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2008_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2008$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2008,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2008 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2008  )
m2.2008[, pred.t33 := predict(Final_pred_2008)]
#check correlations
res[res$year=="2008", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2008))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2008.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2008,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2008.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2008 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2008<- summary(lm(PM10~pred.m3,data=m1.2008))
res[res$year=="2008", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2008))$r.squared)    
#RMSPE
res[res$year=="2008", 'm3.PE'] <- print(rmse(residuals(m3.fit.2008)))

#spatial
###to check
spatial2008<-m1.2008 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2008.spat<- lm(barpm ~ barpred, data=spatial2008)
res[res$year=="2008", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008))$r.squared)
res[res$year=="2008", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2008.spat)))
       
#temporal
tempo2008<-left_join(m1.2008,spatial2008)
tempo2008$delpm <-tempo2008$PM10-tempo2008$barpm
tempo2008$delpred <-tempo2008$pred.m3-tempo2008$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2008)
res[res$year=="2008", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2008.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2008.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2008.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2008.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2008.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")





### import data
m1.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2009<-m1.2009[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2009, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2009[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2009 <- m1.2009[!(m1.2009$badid %in% bad$badid), ] 

#get rid of missing
m1.2009[,elev.s:= scale(elev)]
m1.2009[,tden.s:= scale(tden)]
m1.2009[,pden.s:= scale(pden)]
m1.2009[,dist2A1.s:= scale(dist2A1)]
m1.2009[,dist2water.s:= scale(dist2water)]
m1.2009[,dist2rail.s:= scale(dist2rail)]
m1.2009[,Dist2road.s:= scale(Dist2road)]
m1.2009[,ndvi.s:= scale(ndvi)]
m1.2009[,MeanPbl.s:= scale(MeanPbl)]
m1.2009[,p_ind.s:= scale(p_ind)]
m1.2009[,p_for.s:= scale(p_for)]
m1.2009[,p_farm.s:= scale(p_farm)]
m1.2009[,p_dos.s:= scale(p_dos)]
m1.2009[,p_dev.s:= scale(p_dev)]
m1.2009[,p_os.s:= scale(p_os)]
m1.2009[,tempa.s:= scale(tempa)]
m1.2009[,WDa.s:= scale(WDa)]
m1.2009[,WSa.s:= scale(WSa)]
m1.2009[,RHa.s:= scale(RHa)]
m1.2009[,Raina.s:= scale(Raina)]
m1.2009[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2009 <-  lmer(m1.formula,data=m1.2009,weights=normwt)
m1.2009$pred.m1 <- predict(m1.fit.2009)
res[res$year=="2009", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2009))$r.squared)
#RMSPE
res[res$year=="2009", 'm1.PE'] <- print(rmse(residuals(m1.fit.2009)))

#spatial
###to check
spatial2009<-m1.2009 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.spat<- lm(barpm ~ barpred, data=spatial2009)
res[res$year=="2009", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009))$r.squared)
res[res$year=="2009", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2009.spat)))
       
#temporal
tempo2009<-left_join(m1.2009,spatial2009)
tempo2009$delpm <-tempo2009$PM10-tempo2009$barpm
tempo2009$delpred <-tempo2009$pred.m1-tempo2009$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2009)
res[res$year=="2009", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009))$r.squared)
saveRDS(m1.2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2009)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2009)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2009)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2009)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2009)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2009)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2009)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2009)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2009)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2009)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2009.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2009.cv<-lm(PM10~pred.m1.cv,data=m1.2009.cv)
res[res$year=="2009", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2009.cv))$r.squared)
res[res$year=="2009", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2009.cv))$coef[1,1])
res[res$year=="2009", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2009.cv))$coef[1,2])
res[res$year=="2009", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2009.cv))$coef[2,1])
res[res$year=="2009", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2009.cv))$coef[2,2])
#RMSPE
res[res$year=="2009", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2009.cv)))

#spatial
spatial2009.cv<-m1.2009.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.cv.s <- lm(barpm ~ barpred, data=spatial2009.cv)
res[res$year=="2009", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009.cv))$r.squared)
res[res$year=="2009", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2009.cv.s)))
       
#temporal
tempo2009.cv<-left_join(m1.2009.cv,spatial2009.cv)
tempo2009.cv$delpm <-tempo2009.cv$PM10-tempo2009.cv$barpm
tempo2009.cv$delpred <-tempo2009.cv$pred.m1.cv-tempo2009.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2009.cv)
res[res$year=="2009", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2009.cv,stn)
setkey(luf,stn)
m1.2009.cv.loc <- merge(m1.2009.cv, luf, all.x = T)
#m1.2009.cv.loc<-na.omit(m1.2009.cv.loc)

#create residual mp3 variable
m1.2009.cv.loc$res.m1<-m1.2009.cv.loc$PM10-m1.2009.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2009.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2009.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2009.cv.loc$pred.m1.both <- m1.2009.cv.loc$pred.m1.cv + m1.2009.cv.loc$pred.m1.loc
res[res$year=="2009", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2009.cv.loc))$r.squared)
res[res$year=="2009", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2009.cv.loc))$coef[1,1])
res[res$year=="2009", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2009.cv.loc))$coef[1,2])
res[res$year=="2009", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2009.cv.loc))$coef[2,1])
res[res$year=="2009", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2009.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2009", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2009.cv)))

#spatial
spatial2009.cv.loc<-m1.2009.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.cv.loc.s <- lm(barpm ~ barpred, data=spatial2009.cv.loc)
res[res$year=="2009", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009.cv.loc))$r.squared)
res[res$year=="2009", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2009.cv.loc.s)))
       
#temporal
tempo2009.loc.cv<-left_join(m1.2009.cv.loc,spatial2009.cv.loc)
tempo2009.loc.cv$delpm <-tempo2009.loc.cv$PM10-tempo2009.loc.cv$barpm
tempo2009.loc.cv$delpred <-tempo2009.loc.cv$pred.m1.both-tempo2009.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2009.loc.cv)
res[res$year=="2009", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2009.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2009.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.predCV.rds")


###############
#MOD2
###############
m2.2009<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2009.rds")

m2.2009[,elev.s:= scale(elev)]
m2.2009[,tden.s:= scale(tden)]
m2.2009[,pden.s:= scale(pden)]
m2.2009[,dist2A1.s:= scale(dist2A1)]
m2.2009[,dist2water.s:= scale(dist2water)]
m2.2009[,dist2rail.s:= scale(dist2rail)]
m2.2009[,Dist2road.s:= scale(Dist2road)]
m2.2009[,ndvi.s:= scale(ndvi)]
m2.2009[,MeanPbl.s:= scale(MeanPbl)]
m2.2009[,p_ind.s:= scale(p_ind)]
m2.2009[,p_for.s:= scale(p_for)]
m2.2009[,p_farm.s:= scale(p_farm)]
m2.2009[,p_dos.s:= scale(p_dos)]
m2.2009[,p_dev.s:= scale(p_dev)]
m2.2009[,p_os.s:= scale(p_os)]
m2.2009[,tempa.s:= scale(tempa)]
m2.2009[,WDa.s:= scale(WDa)]
m2.2009[,WSa.s:= scale(WSa)]
m2.2009[,RHa.s:= scale(RHa)]
m2.2009[,Raina.s:= scale(Raina)]
m2.2009[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2009[, pred.m2 := predict(object=m1.fit.2009,newdata=m2.2009,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2009$pred.m2)
#delete implossible valuesOA[24~
m2.2009 <- m2.2009[pred.m2 > 0.00000000000001 , ]
m2.2009 <- m2.2009[pred.m2 < 1500   , ]

saveRDS(m2.2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2009.pred2.rds")



#-------------->prepare for mod3
m2.2009[, bimon := (m + 1) %/% 2]
setkey(m2.2009,day, aodid)
m2.2009<-m2.2009[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2009 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2009 )
#correlate to see everything from mod2 and the mpm works
m2.2009[, pred.t31 := predict(m2.smooth)]
m2.2009[, resid  := residuals(m2.smooth)]
res[res$year=="2009", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2009))$r.squared)


#split the files to the separate bi monthly datsets
T2009_bimon1 <- subset(m2.2009 ,m2.2009$bimon == "1")
T2009_bimon2 <- subset(m2.2009 ,m2.2009$bimon == "2")
T2009_bimon3 <- subset(m2.2009 ,m2.2009$bimon == "3")
T2009_bimon4 <- subset(m2.2009 ,m2.2009$bimon == "4")
T2009_bimon5 <- subset(m2.2009 ,m2.2009$bimon == "5")
T2009_bimon6 <- subset(m2.2009 ,m2.2009$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2009_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2009_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2009_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2009_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2009_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2009_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2009_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2009$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2009,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2009 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2009  )
m2.2009[, pred.t33 := predict(Final_pred_2009)]
#check correlations
res[res$year=="2009", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2009))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2009.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2009,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2009.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2009 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2009<- summary(lm(PM10~pred.m3,data=m1.2009))
res[res$year=="2009", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2009))$r.squared)    
#RMSPE
res[res$year=="2009", 'm3.PE'] <- print(rmse(residuals(m3.fit.2009)))

#spatial
###to check
spatial2009<-m1.2009 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2009.spat<- lm(barpm ~ barpred, data=spatial2009)
res[res$year=="2009", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009))$r.squared)
res[res$year=="2009", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2009.spat)))
       
#temporal
tempo2009<-left_join(m1.2009,spatial2009)
tempo2009$delpm <-tempo2009$PM10-tempo2009$barpm
tempo2009$delpred <-tempo2009$pred.m3-tempo2009$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2009)
res[res$year=="2009", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2009.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2009.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2009.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2009.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2009.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")





### import data
m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2010<-m1.2010[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2010, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2010[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2010 <- m1.2010[!(m1.2010$badid %in% bad$badid), ] 

#get rid of missing
m1.2010[,elev.s:= scale(elev)]
m1.2010[,tden.s:= scale(tden)]
m1.2010[,pden.s:= scale(pden)]
m1.2010[,dist2A1.s:= scale(dist2A1)]
m1.2010[,dist2water.s:= scale(dist2water)]
m1.2010[,dist2rail.s:= scale(dist2rail)]
m1.2010[,Dist2road.s:= scale(Dist2road)]
m1.2010[,ndvi.s:= scale(ndvi)]
m1.2010[,MeanPbl.s:= scale(MeanPbl)]
m1.2010[,p_ind.s:= scale(p_ind)]
m1.2010[,p_for.s:= scale(p_for)]
m1.2010[,p_farm.s:= scale(p_farm)]
m1.2010[,p_dos.s:= scale(p_dos)]
m1.2010[,p_dev.s:= scale(p_dev)]
m1.2010[,p_os.s:= scale(p_os)]
m1.2010[,tempa.s:= scale(tempa)]
m1.2010[,WDa.s:= scale(WDa)]
m1.2010[,WSa.s:= scale(WSa)]
m1.2010[,RHa.s:= scale(RHa)]
m1.2010[,Raina.s:= scale(Raina)]
m1.2010[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2010 <-  lmer(m1.formula,data=m1.2010,weights=normwt)
m1.2010$pred.m1 <- predict(m1.fit.2010)
res[res$year=="2010", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2010))$r.squared)
#RMSPE
res[res$year=="2010", 'm1.PE'] <- print(rmse(residuals(m1.fit.2010)))

#spatial
###to check
spatial2010<-m1.2010 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.spat<- lm(barpm ~ barpred, data=spatial2010)
res[res$year=="2010", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010))$r.squared)
res[res$year=="2010", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2010.spat)))
       
#temporal
tempo2010<-left_join(m1.2010,spatial2010)
tempo2010$delpm <-tempo2010$PM10-tempo2010$barpm
tempo2010$delpred <-tempo2010$pred.m1-tempo2010$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2010)
res[res$year=="2010", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010))$r.squared)
saveRDS(m1.2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2010)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2010)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2010)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2010)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2010)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2010)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2010)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2010)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2010)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2010)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2010.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2010.cv<-lm(PM10~pred.m1.cv,data=m1.2010.cv)
res[res$year=="2010", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2010.cv))$r.squared)
res[res$year=="2010", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2010.cv))$coef[1,1])
res[res$year=="2010", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2010.cv))$coef[1,2])
res[res$year=="2010", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2010.cv))$coef[2,1])
res[res$year=="2010", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2010.cv))$coef[2,2])
#RMSPE
res[res$year=="2010", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2010.cv)))

#spatial
spatial2010.cv<-m1.2010.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.cv.s <- lm(barpm ~ barpred, data=spatial2010.cv)
res[res$year=="2010", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010.cv))$r.squared)
res[res$year=="2010", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2010.cv.s)))
       
#temporal
tempo2010.cv<-left_join(m1.2010.cv,spatial2010.cv)
tempo2010.cv$delpm <-tempo2010.cv$PM10-tempo2010.cv$barpm
tempo2010.cv$delpred <-tempo2010.cv$pred.m1.cv-tempo2010.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2010.cv)
res[res$year=="2010", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2010.cv,stn)
setkey(luf,stn)
m1.2010.cv.loc <- merge(m1.2010.cv, luf, all.x = T)
#m1.2010.cv.loc<-na.omit(m1.2010.cv.loc)

#create residual mp3 variable
m1.2010.cv.loc$res.m1<-m1.2010.cv.loc$PM10-m1.2010.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2010.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2010.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2010.cv.loc$pred.m1.both <- m1.2010.cv.loc$pred.m1.cv + m1.2010.cv.loc$pred.m1.loc
res[res$year=="2010", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2010.cv.loc))$r.squared)
res[res$year=="2010", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2010.cv.loc))$coef[1,1])
res[res$year=="2010", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2010.cv.loc))$coef[1,2])
res[res$year=="2010", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2010.cv.loc))$coef[2,1])
res[res$year=="2010", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2010.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2010", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2010.cv)))

#spatial
spatial2010.cv.loc<-m1.2010.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.cv.loc.s <- lm(barpm ~ barpred, data=spatial2010.cv.loc)
res[res$year=="2010", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010.cv.loc))$r.squared)
res[res$year=="2010", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2010.cv.loc.s)))
       
#temporal
tempo2010.loc.cv<-left_join(m1.2010.cv.loc,spatial2010.cv.loc)
tempo2010.loc.cv$delpm <-tempo2010.loc.cv$PM10-tempo2010.loc.cv$barpm
tempo2010.loc.cv$delpred <-tempo2010.loc.cv$pred.m1.both-tempo2010.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2010.loc.cv)
res[res$year=="2010", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2010.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2010.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.predCV.rds")


###############
#MOD2
###############
m2.2010<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.rds")

m2.2010[,elev.s:= scale(elev)]
m2.2010[,tden.s:= scale(tden)]
m2.2010[,pden.s:= scale(pden)]
m2.2010[,dist2A1.s:= scale(dist2A1)]
m2.2010[,dist2water.s:= scale(dist2water)]
m2.2010[,dist2rail.s:= scale(dist2rail)]
m2.2010[,Dist2road.s:= scale(Dist2road)]
m2.2010[,ndvi.s:= scale(ndvi)]
m2.2010[,MeanPbl.s:= scale(MeanPbl)]
m2.2010[,p_ind.s:= scale(p_ind)]
m2.2010[,p_for.s:= scale(p_for)]
m2.2010[,p_farm.s:= scale(p_farm)]
m2.2010[,p_dos.s:= scale(p_dos)]
m2.2010[,p_dev.s:= scale(p_dev)]
m2.2010[,p_os.s:= scale(p_os)]
m2.2010[,tempa.s:= scale(tempa)]
m2.2010[,WDa.s:= scale(WDa)]
m2.2010[,WSa.s:= scale(WSa)]
m2.2010[,RHa.s:= scale(RHa)]
m2.2010[,Raina.s:= scale(Raina)]
m2.2010[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2010[, pred.m2 := predict(object=m1.fit.2010,newdata=m2.2010,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2010$pred.m2)
#delete implossible valuesOA[24~
m2.2010 <- m2.2010[pred.m2 > 0.00000000000001 , ]
m2.2010 <- m2.2010[pred.m2 < 1500   , ]

saveRDS(m2.2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2010.pred2.rds")



#-------------->prepare for mod3
m2.2010[, bimon := (m + 1) %/% 2]
setkey(m2.2010,day, aodid)
m2.2010<-m2.2010[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2010 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2010 )
#correlate to see everything from mod2 and the mpm works
m2.2010[, pred.t31 := predict(m2.smooth)]
m2.2010[, resid  := residuals(m2.smooth)]
res[res$year=="2010", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2010))$r.squared)


#split the files to the separate bi monthly datsets
T2010_bimon1 <- subset(m2.2010 ,m2.2010$bimon == "1")
T2010_bimon2 <- subset(m2.2010 ,m2.2010$bimon == "2")
T2010_bimon3 <- subset(m2.2010 ,m2.2010$bimon == "3")
T2010_bimon4 <- subset(m2.2010 ,m2.2010$bimon == "4")
T2010_bimon5 <- subset(m2.2010 ,m2.2010$bimon == "5")
T2010_bimon6 <- subset(m2.2010 ,m2.2010$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2010_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2010_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2010_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2010_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2010_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2010_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2010_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2010$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2010,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2010 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2010  )
m2.2010[, pred.t33 := predict(Final_pred_2010)]
#check correlations
res[res$year=="2010", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2010))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2010.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2010,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2010.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2010 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2010<- summary(lm(PM10~pred.m3,data=m1.2010))
res[res$year=="2010", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2010))$r.squared)    
#RMSPE
res[res$year=="2010", 'm3.PE'] <- print(rmse(residuals(m3.fit.2010)))

#spatial
###to check
spatial2010<-m1.2010 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2010.spat<- lm(barpm ~ barpred, data=spatial2010)
res[res$year=="2010", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010))$r.squared)
res[res$year=="2010", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2010.spat)))
       
#temporal
tempo2010<-left_join(m1.2010,spatial2010)
tempo2010$delpm <-tempo2010$PM10-tempo2010$barpm
tempo2010$delpred <-tempo2010$pred.m3-tempo2010$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2010)
res[res$year=="2010", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2010.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2010.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2010.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2010.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2010.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")





### import data
m1.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2011<-m1.2011[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2011, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2011[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2011 <- m1.2011[!(m1.2011$badid %in% bad$badid), ] 

#get rid of missing
m1.2011[,elev.s:= scale(elev)]
m1.2011[,tden.s:= scale(tden)]
m1.2011[,pden.s:= scale(pden)]
m1.2011[,dist2A1.s:= scale(dist2A1)]
m1.2011[,dist2water.s:= scale(dist2water)]
m1.2011[,dist2rail.s:= scale(dist2rail)]
m1.2011[,Dist2road.s:= scale(Dist2road)]
m1.2011[,ndvi.s:= scale(ndvi)]
m1.2011[,MeanPbl.s:= scale(MeanPbl)]
m1.2011[,p_ind.s:= scale(p_ind)]
m1.2011[,p_for.s:= scale(p_for)]
m1.2011[,p_farm.s:= scale(p_farm)]
m1.2011[,p_dos.s:= scale(p_dos)]
m1.2011[,p_dev.s:= scale(p_dev)]
m1.2011[,p_os.s:= scale(p_os)]
m1.2011[,tempa.s:= scale(tempa)]
m1.2011[,WDa.s:= scale(WDa)]
m1.2011[,WSa.s:= scale(WSa)]
m1.2011[,RHa.s:= scale(RHa)]
m1.2011[,Raina.s:= scale(Raina)]
m1.2011[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2011 <-  lmer(m1.formula,data=m1.2011,weights=normwt)
m1.2011$pred.m1 <- predict(m1.fit.2011)
res[res$year=="2011", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2011))$r.squared)
#RMSPE
res[res$year=="2011", 'm1.PE'] <- print(rmse(residuals(m1.fit.2011)))

#spatial
###to check
spatial2011<-m1.2011 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.spat<- lm(barpm ~ barpred, data=spatial2011)
res[res$year=="2011", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011))$r.squared)
res[res$year=="2011", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2011.spat)))
       
#temporal
tempo2011<-left_join(m1.2011,spatial2011)
tempo2011$delpm <-tempo2011$PM10-tempo2011$barpm
tempo2011$delpred <-tempo2011$pred.m1-tempo2011$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2011)
res[res$year=="2011", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011))$r.squared)
saveRDS(m1.2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2011)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2011)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2011)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2011)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2011)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2011)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2011)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2011)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2011)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2011)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2011.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2011.cv<-lm(PM10~pred.m1.cv,data=m1.2011.cv)
res[res$year=="2011", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2011.cv))$r.squared)
res[res$year=="2011", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2011.cv))$coef[1,1])
res[res$year=="2011", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2011.cv))$coef[1,2])
res[res$year=="2011", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2011.cv))$coef[2,1])
res[res$year=="2011", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2011.cv))$coef[2,2])
#RMSPE
res[res$year=="2011", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2011.cv)))

#spatial
spatial2011.cv<-m1.2011.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.cv.s <- lm(barpm ~ barpred, data=spatial2011.cv)
res[res$year=="2011", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011.cv))$r.squared)
res[res$year=="2011", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2011.cv.s)))
       
#temporal
tempo2011.cv<-left_join(m1.2011.cv,spatial2011.cv)
tempo2011.cv$delpm <-tempo2011.cv$PM10-tempo2011.cv$barpm
tempo2011.cv$delpred <-tempo2011.cv$pred.m1.cv-tempo2011.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2011.cv)
res[res$year=="2011", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2011.cv,stn)
setkey(luf,stn)
m1.2011.cv.loc <- merge(m1.2011.cv, luf, all.x = T)
#m1.2011.cv.loc<-na.omit(m1.2011.cv.loc)

#create residual mp3 variable
m1.2011.cv.loc$res.m1<-m1.2011.cv.loc$PM10-m1.2011.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2011.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2011.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2011.cv.loc$pred.m1.both <- m1.2011.cv.loc$pred.m1.cv + m1.2011.cv.loc$pred.m1.loc
res[res$year=="2011", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2011.cv.loc))$r.squared)
res[res$year=="2011", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2011.cv.loc))$coef[1,1])
res[res$year=="2011", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2011.cv.loc))$coef[1,2])
res[res$year=="2011", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2011.cv.loc))$coef[2,1])
res[res$year=="2011", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2011.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2011", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2011.cv)))

#spatial
spatial2011.cv.loc<-m1.2011.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.cv.loc.s <- lm(barpm ~ barpred, data=spatial2011.cv.loc)
res[res$year=="2011", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011.cv.loc))$r.squared)
res[res$year=="2011", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2011.cv.loc.s)))
       
#temporal
tempo2011.loc.cv<-left_join(m1.2011.cv.loc,spatial2011.cv.loc)
tempo2011.loc.cv$delpm <-tempo2011.loc.cv$PM10-tempo2011.loc.cv$barpm
tempo2011.loc.cv$delpred <-tempo2011.loc.cv$pred.m1.both-tempo2011.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2011.loc.cv)
res[res$year=="2011", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2011.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2011.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.predCV.rds")


###############
#MOD2
###############
m2.2011<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2011.rds")

m2.2011[,elev.s:= scale(elev)]
m2.2011[,tden.s:= scale(tden)]
m2.2011[,pden.s:= scale(pden)]
m2.2011[,dist2A1.s:= scale(dist2A1)]
m2.2011[,dist2water.s:= scale(dist2water)]
m2.2011[,dist2rail.s:= scale(dist2rail)]
m2.2011[,Dist2road.s:= scale(Dist2road)]
m2.2011[,ndvi.s:= scale(ndvi)]
m2.2011[,MeanPbl.s:= scale(MeanPbl)]
m2.2011[,p_ind.s:= scale(p_ind)]
m2.2011[,p_for.s:= scale(p_for)]
m2.2011[,p_farm.s:= scale(p_farm)]
m2.2011[,p_dos.s:= scale(p_dos)]
m2.2011[,p_dev.s:= scale(p_dev)]
m2.2011[,p_os.s:= scale(p_os)]
m2.2011[,tempa.s:= scale(tempa)]
m2.2011[,WDa.s:= scale(WDa)]
m2.2011[,WSa.s:= scale(WSa)]
m2.2011[,RHa.s:= scale(RHa)]
m2.2011[,Raina.s:= scale(Raina)]
m2.2011[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2011[, pred.m2 := predict(object=m1.fit.2011,newdata=m2.2011,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2011$pred.m2)
#delete implossible valuesOA[24~
m2.2011 <- m2.2011[pred.m2 > 0.00000000000001 , ]
m2.2011 <- m2.2011[pred.m2 < 1500   , ]

saveRDS(m2.2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2011.pred2.rds")



#-------------->prepare for mod3
m2.2011[, bimon := (m + 1) %/% 2]
setkey(m2.2011,day, aodid)
m2.2011<-m2.2011[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2011 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2011 )
#correlate to see everything from mod2 and the mpm works
m2.2011[, pred.t31 := predict(m2.smooth)]
m2.2011[, resid  := residuals(m2.smooth)]
res[res$year=="2011", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2011))$r.squared)


#split the files to the separate bi monthly datsets
T2011_bimon1 <- subset(m2.2011 ,m2.2011$bimon == "1")
T2011_bimon2 <- subset(m2.2011 ,m2.2011$bimon == "2")
T2011_bimon3 <- subset(m2.2011 ,m2.2011$bimon == "3")
T2011_bimon4 <- subset(m2.2011 ,m2.2011$bimon == "4")
T2011_bimon5 <- subset(m2.2011 ,m2.2011$bimon == "5")
T2011_bimon6 <- subset(m2.2011 ,m2.2011$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2011_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2011_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2011_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2011_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2011_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2011_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2011_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2011$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2011,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2011 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2011  )
m2.2011[, pred.t33 := predict(Final_pred_2011)]
#check correlations
res[res$year=="2011", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2011))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2011.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2011,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2011.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2011 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2011<- summary(lm(PM10~pred.m3,data=m1.2011))
res[res$year=="2011", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2011))$r.squared)    
#RMSPE
res[res$year=="2011", 'm3.PE'] <- print(rmse(residuals(m3.fit.2011)))

#spatial
###to check
spatial2011<-m1.2011 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2011.spat<- lm(barpm ~ barpred, data=spatial2011)
res[res$year=="2011", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011))$r.squared)
res[res$year=="2011", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2011.spat)))
       
#temporal
tempo2011<-left_join(m1.2011,spatial2011)
tempo2011$delpm <-tempo2011$PM10-tempo2011$barpm
tempo2011$delpred <-tempo2011$pred.m3-tempo2011$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2011)
res[res$year=="2011", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2011.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2011.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2011.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2011.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2011.csv", row.names = F)

keep(res, sure=TRUE) 
c()



###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")



### import data
m1.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2012<-m1.2012[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2012, c( "stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2012[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2012 <- m1.2012[!(m1.2012$badid %in% bad$badid), ] 

#get rid of missing
m1.2012[,elev.s:= scale(elev)]
m1.2012[,tden.s:= scale(tden)]
m1.2012[,pden.s:= scale(pden)]
m1.2012[,dist2A1.s:= scale(dist2A1)]
m1.2012[,dist2water.s:= scale(dist2water)]
m1.2012[,dist2rail.s:= scale(dist2rail)]
m1.2012[,Dist2road.s:= scale(Dist2road)]
m1.2012[,ndvi.s:= scale(ndvi)]
m1.2012[,MeanPbl.s:= scale(MeanPbl)]
m1.2012[,p_ind.s:= scale(p_ind)]
m1.2012[,p_for.s:= scale(p_for)]
m1.2012[,p_farm.s:= scale(p_farm)]
m1.2012[,p_dos.s:= scale(p_dos)]
m1.2012[,p_dev.s:= scale(p_dev)]
m1.2012[,p_os.s:= scale(p_os)]
m1.2012[,tempa.s:= scale(tempa)]
m1.2012[,WDa.s:= scale(WDa)]
m1.2012[,WSa.s:= scale(WSa)]
m1.2012[,RHa.s:= scale(RHa)]
m1.2012[,Raina.s:= scale(Raina)]
m1.2012[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2012 <-  lmer(m1.formula,data=m1.2012,weights=normwt)
m1.2012$pred.m1 <- predict(m1.fit.2012)
res[res$year=="2012", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2012))$r.squared)
#RMSPE
res[res$year=="2012", 'm1.PE'] <- print(rmse(residuals(m1.fit.2012)))

#spatial
###to check
spatial2012<-m1.2012 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.spat<- lm(barpm ~ barpred, data=spatial2012)
res[res$year=="2012", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012))$r.squared)
res[res$year=="2012", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2012.spat)))
       
#temporal
tempo2012<-left_join(m1.2012,spatial2012)
tempo2012$delpm <-tempo2012$PM10-tempo2012$barpm
tempo2012$delpred <-tempo2012$pred.m1-tempo2012$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2012)
res[res$year=="2012", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012))$r.squared)
saveRDS(m1.2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.pred.rds")



#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2012)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2012)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2012)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2012)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2012)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2012)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2012)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2012)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2012)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2012)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2012.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2012.cv<-lm(PM10~pred.m1.cv,data=m1.2012.cv)
res[res$year=="2012", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2012.cv))$r.squared)
res[res$year=="2012", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2012.cv))$coef[1,1])
res[res$year=="2012", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2012.cv))$coef[1,2])
res[res$year=="2012", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2012.cv))$coef[2,1])
res[res$year=="2012", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2012.cv))$coef[2,2])
#RMSPE
res[res$year=="2012", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2012.cv)))

#spatial
spatial2012.cv<-m1.2012.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.cv.s <- lm(barpm ~ barpred, data=spatial2012.cv)
res[res$year=="2012", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012.cv))$r.squared)
res[res$year=="2012", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2012.cv.s)))
       
#temporal
tempo2012.cv<-left_join(m1.2012.cv,spatial2012.cv)
tempo2012.cv$delpm <-tempo2012.cv$PM10-tempo2012.cv$barpm
tempo2012.cv$delpred <-tempo2012.cv$pred.m1.cv-tempo2012.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2012.cv)
res[res$year=="2012", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012.cv))$r.squared)



#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2012.cv,stn)
setkey(luf,stn)
m1.2012.cv.loc <- merge(m1.2012.cv, luf, all.x = T)
#m1.2012.cv.loc<-na.omit(m1.2012.cv.loc)

#create residual mp3 variable
m1.2012.cv.loc$res.m1<-m1.2012.cv.loc$PM10-m1.2012.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2012.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2012.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2012.cv.loc$pred.m1.both <- m1.2012.cv.loc$pred.m1.cv + m1.2012.cv.loc$pred.m1.loc
res[res$year=="2012", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2012.cv.loc))$r.squared)
res[res$year=="2012", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2012.cv.loc))$coef[1,1])
res[res$year=="2012", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2012.cv.loc))$coef[1,2])
res[res$year=="2012", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2012.cv.loc))$coef[2,1])
res[res$year=="2012", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2012.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2012", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2012.cv)))

#spatial
spatial2012.cv.loc<-m1.2012.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.cv.loc.s <- lm(barpm ~ barpred, data=spatial2012.cv.loc)
res[res$year=="2012", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012.cv.loc))$r.squared)
res[res$year=="2012", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2012.cv.loc.s)))
       
#temporal
tempo2012.loc.cv<-left_join(m1.2012.cv.loc,spatial2012.cv.loc)
tempo2012.loc.cv$delpm <-tempo2012.loc.cv$PM10-tempo2012.loc.cv$barpm
tempo2012.loc.cv$delpred <-tempo2012.loc.cv$pred.m1.both-tempo2012.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2012.loc.cv)
res[res$year=="2012", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2012.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2012.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.predCV.rds")


###############
#MOD2
###############
m2.2012<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2012.rds")

m2.2012[,elev.s:= scale(elev)]
m2.2012[,tden.s:= scale(tden)]
m2.2012[,pden.s:= scale(pden)]
m2.2012[,dist2A1.s:= scale(dist2A1)]
m2.2012[,dist2water.s:= scale(dist2water)]
m2.2012[,dist2rail.s:= scale(dist2rail)]
m2.2012[,Dist2road.s:= scale(Dist2road)]
m2.2012[,ndvi.s:= scale(ndvi)]
m2.2012[,MeanPbl.s:= scale(MeanPbl)]
m2.2012[,p_ind.s:= scale(p_ind)]
m2.2012[,p_for.s:= scale(p_for)]
m2.2012[,p_farm.s:= scale(p_farm)]
m2.2012[,p_dos.s:= scale(p_dos)]
m2.2012[,p_dev.s:= scale(p_dev)]
m2.2012[,p_os.s:= scale(p_os)]
m2.2012[,tempa.s:= scale(tempa)]
m2.2012[,WDa.s:= scale(WDa)]
m2.2012[,WSa.s:= scale(WSa)]
m2.2012[,RHa.s:= scale(RHa)]
m2.2012[,Raina.s:= scale(Raina)]
m2.2012[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2012[, pred.m2 := predict(object=m1.fit.2012,newdata=m2.2012,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2012$pred.m2)
#delete implossible valuesOA[24~
m2.2012 <- m2.2012[pred.m2 > 0.00000000000001 , ]
m2.2012 <- m2.2012[pred.m2 < 1500   , ]

saveRDS(m2.2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2012.pred2.rds")



#-------------->prepare for mod3
m2.2012[, bimon := (m + 1) %/% 2]
setkey(m2.2012,day, aodid)
m2.2012<-m2.2012[!is.na(meanPM10)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2012 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2012 )
#correlate to see everything from mod2 and the mpm works
m2.2012[, pred.t31 := predict(m2.smooth)]
m2.2012[, resid  := residuals(m2.smooth)]
res[res$year=="2012", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2012))$r.squared)


#split the files to the separate bi monthly datsets
T2012_bimon1 <- subset(m2.2012 ,m2.2012$bimon == "1")
T2012_bimon2 <- subset(m2.2012 ,m2.2012$bimon == "2")
T2012_bimon3 <- subset(m2.2012 ,m2.2012$bimon == "3")
T2012_bimon4 <- subset(m2.2012 ,m2.2012$bimon == "4")
T2012_bimon5 <- subset(m2.2012 ,m2.2012$bimon == "5")
T2012_bimon6 <- subset(m2.2012 ,m2.2012$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2012_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2012_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2012_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2012_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2012_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2012_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2012_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2012$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2012,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2012 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2012  )
m2.2012[, pred.t33 := predict(Final_pred_2012)]
#check correlations
res[res$year=="2012", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2012))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2012.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2012,data.m3)

#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction      	
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2012.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2012 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2012<- summary(lm(PM10~pred.m3,data=m1.2012))
res[res$year=="2012", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2012))$r.squared)    
#RMSPE
res[res$year=="2012", 'm3.PE'] <- print(rmse(residuals(m3.fit.2012)))

#spatial
###to check
spatial2012<-m1.2012 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2012.spat<- lm(barpm ~ barpred, data=spatial2012)
res[res$year=="2012", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012))$r.squared)
res[res$year=="2012", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2012.spat)))
       
#temporal
tempo2012<-left_join(m1.2012,spatial2012)
tempo2012$delpm <-tempo2012$PM10-tempo2012$barpm
tempo2012$delpred <-tempo2012$pred.m3-tempo2012$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2012)
res[res$year=="2012", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2012.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2012.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2012.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2012.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2012.csv", row.names = F)

keep(res, sure=TRUE) 
c()

