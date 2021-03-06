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
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


#-------------------->> RES TABLE
res <- matrix(nrow=2, ncol=44)
res <- data.frame(res)
colnames(res) <- c(
          ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
         ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"m2.R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
          ,"XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX")
          
res$type <- c("PM25","PM10")


m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.AQ.rds")

m1.all[,elev.s:= scale(elev)]
m1.all[,tden.s:= scale(tden)]
m1.all[,pden.s:= scale(pden)]
m1.all[,dist2A1.s:= scale(dist2A1)]
m1.all[,dist2water.s:= scale(dist2water)]
m1.all[,dist2rail.s:= scale(dist2rail)]
m1.all[,Dist2road.s:= scale(Dist2road)]
m1.all[,ndvi.s:= scale(ndvi)]
m1.all[,MeanPbl.s:= scale(MeanPbl)]
m1.all[,p_ind.s:= scale(p_ind)]
m1.all[,p_for.s:= scale(p_for)]
m1.all[,p_farm.s:= scale(p_farm)]
m1.all[,p_dos.s:= scale(p_dos)]
m1.all[,p_dev.s:= scale(p_dev)]
m1.all[,p_os.s:= scale(p_os)]
m1.all[,tempa.s:= scale(Temp.im)]
m1.all[,WDa.s:= scale(WD.im)]
m1.all[,WSa.s:= scale(WS.im)]
m1.all[,RHa.s:= scale(RH.im)]
m1.all[,Raina.s:= scale(Rain.im)]
m1.all[,NOa.s:= scale(NO.im)]
m1.all[,O3a.s:= scale(O3.im)]
m1.all[,SO2a.s:= scale(SO2.im)]
################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.all, c("stn","m"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,m,sep="-")]
#################BAD STN
m1.all[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badid %in% bad$badid), ] 

summary(m1.all)
#clear missings
m1.all<- m1.all[!is.na(m1.mpm)]


m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WDa.s+WSa.s+Dust#+MeanPbl.s #temporal
                        #+RHa.s+ NOa.s +O3a.s 
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +dist2rail.s +dist2water.s +dist2A1.s
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                        #+ns(day)
                        #+as.factor(season)
                        #+ aod:as.factor(elev.s)
                        #+ as.factor(reg_num) + aod:as.factor(reg_num)
                        #+aod*Dust #interactions
                        #+ns(tempa.s)
                        #+meanPM10 
                        #+meanPM
                        #+ndvi.s*season
                        #+aod*WSa.s
                        #+aod*tempa.s
                        +m1.mpm
                        +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 
#[1] 0.7455536
#1] 0.7893207 (with meanPM)
#full fit
m1.fit.all <-  lmer(m1.formula,data=m1.all,weights=normwt)
m1.all$pred.m1 <- predict(m1.fit.all)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM25", 'm1.PE'] <- print(rmse(residuals(m1.fit.all)))

#spatial
###to check
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(m1.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.allYEARS.pred.rds")



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
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM25", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM25", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM25", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM25", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM25", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM25", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)



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
m1.all.cv.loc$res.m1<-m1.all.cv.loc$PM25-m1.all.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS.im)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.all.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.all.cv.loc$pred.m1.loc <-predict(gam.out)
m1.all.cv.loc$pred.m1.both <- m1.all.cv.loc$pred.m1.cv + m1.all.cv.loc$pred.m1.loc
res[res$type=="PM25", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.all.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.all.cv.loc))$coef[1,1])
res[res$type=="PM25", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.all.cv.loc))$coef[1,2])
res[res$type=="PM25", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.all.cv.loc))$coef[2,1])
res[res$type=="PM25", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.all.cv.loc))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv.loc<-m1.all.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1.both, na.rm=TRUE)) 
m1.fit.all.cv.loc.s <- lm(barpm ~ barpred, data=spatialall.cv.loc)
res[res$type=="PM25", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv.loc))$r.squared)
res[res$type=="PM25", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.all.cv.loc.s)))
       
#temporal
tempoall.loc.cv<-left_join(m1.all.cv.loc,spatialall.cv.loc)
tempoall.loc.cv$delpm <-tempoall.loc.cv$PM25-tempoall.loc.cv$barpm
tempoall.loc.cv$delpred <-tempoall.loc.cv$pred.m1.both-tempoall.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempoall.loc.cv)
res[res$type=="PM25", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ.allYEARS.rds")
saveRDS(m1.all.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.allYEARS.predCV.rds")


###############
#MOD2
###############

m2.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")
m2.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")
m2.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2005.rds")
m2.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2006.rds")
m2.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.rds")
m2.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2008.rds")
m2.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2009.rds")
m2.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.rds")
m2.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2011.rds")
m2.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2012.rds")
m2.2013 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2013.rds")


m2.all <- rbindlist(list(m2.2003,m2.2004,m2.2005,m2.2006,m2.2007,m2.2008,m2.2009,m2.2010,m2.2011,m2.2012,m2.2013), fill=TRUE)



m2.all[,elev.s:= scale(elev)]
m2.all[,tden.s:= scale(tden)]
m2.all[,pden.s:= scale(pden)]
m2.all[,dist2A1.s:= scale(dist2A1)]
m2.all[,dist2water.s:= scale(dist2water)]
m2.all[,dist2rail.s:= scale(dist2rail)]
m2.all[,Dist2road.s:= scale(Dist2road)]
m2.all[,ndvi.s:= scale(ndvi)]
m2.all[,MeanPbl.s:= scale(MeanPbl)]
m2.all[,p_ind.s:= scale(p_ind)]
m2.all[,p_for.s:= scale(p_for)]
m2.all[,p_farm.s:= scale(p_farm)]
m2.all[,p_dos.s:= scale(p_dos)]
m2.all[,p_dev.s:= scale(p_dev)]
m2.all[,p_os.s:= scale(p_os)]
m2.all[,tempa.s:= scale(tempa)]
m2.all[,WDa.s:= scale(WDa)]
m2.all[,WSa.s:= scale(WSa)]
m2.all[,RHa.s:= scale(RHa)]
m2.all[,Raina.s:= scale(Raina)]
m2.all[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.all[, pred.m2 := predict(object=m1.fit.all,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.all$pred.m2)
#delete implossible valuesOA[24~
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 1500   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.allYEARS.pred2.rds")

#-------------->prepare for mod3
m2.all[, bimon := (m + 1) %/% 2]
setkey(m2.all,day, aodid)
m2.all<-m2.all[!is.na(meanPM)]


#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all )
#xm2.smooth = lmer(pred.m2 ~ meanPM25+(1+ meanPM25|aodid), data= m2.all )
#correlate to see everything from mod2 and the mpm works
m2.all[, pred.t31 := predict(m2.smooth)]
m2.all[, resid  := residuals(m2.smooth)]
res[res$type=="PM25", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.all))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all ,m2.all$bimon == "1")
Tall_bimon2 <- subset(m2.all ,m2.all$bimon == "2")
Tall_bimon3 <- subset(m2.all ,m2.all$bimon == "3")
Tall_bimon4 <- subset(m2.all ,m2.all$bimon == "4")
Tall_bimon5 <- subset(m2.all ,m2.all$bimon == "5")
Tall_bimon6 <- subset(m2.all ,m2.all$bimon == "6")

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
m2.all$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all  )
m2.all[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.all.rds")

#for PM25
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_all,data.m3)

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
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.allYEARS.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.allYEARS.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.all <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
res[res$type=="PM25", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
#RMSPE
res[res$type=="PM25", 'm3.PE'] <- print(rmse(residuals(m3.fit.all)))

#spatial
###to check
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ.allYEARS.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.allYEARS.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.allYEARS.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.allYEARS.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ.allYEARS.csv", row.names = F)







