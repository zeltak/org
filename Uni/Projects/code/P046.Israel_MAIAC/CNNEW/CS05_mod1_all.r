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
res <- matrix(nrow=9, ncol=45)
res <- data.frame(res)
colnames(res) <- c(
          "year"
          ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
          ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"mod2_R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"mod3_pm_mod3","mod3_int"
          ,"mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE"
          ,"mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
          "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                   "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial"
                   )
res$year <- c(2003:2011); 



### import data
m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")

### subset to aqua and apply alexei cleaning methods
m1.2003<-m1.2003[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2003, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2003[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2003 <- m1.2003[!(m1.2003$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2003)
m1.2003$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2003))#0.74
#get rid of missing
m1.2003 <- na.omit(m1.2003)
m1.2003[,elev.s:= scale(elev)]
m1.2003[,tden.s:= scale(tden)]
m1.2003[,pden.s:= scale(pden)]
m1.2003[,dist2A1.s:= scale(dist2A1)]
m1.2003[,dist2water.s:= scale(dist2water)]
m1.2003[,dist2rail.s:= scale(dist2rail)]
m1.2003[,Dist2road.s:= scale(Dist2road)]
m1.2003[,ndvi.s:= scale(ndvi)]
m1.2003[,MeanPbl.s:= scale(MeanPbl)]
m1.2003[,p_ind.s:= scale(p_ind)]
m1.2003[,p_for.s:= scale(p_for)]
m1.2003[,p_farm.s:= scale(p_farm)]
m1.2003[,p_dos.s:= scale(p_dos)]
m1.2003[,p_dev.s:= scale(p_dev)]
m1.2003[,p_os.s:= scale(p_os)]
m1.2003[,Temp.s:= scale(Temp)]
m1.2003[,WD.s:= scale(WD)]
m1.2003[,WS.s:= scale(WS)]
m1.2003[,RH.s:= scale(RH)]
m1.2003[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2003 <-  lmer(m1.formula,data=m1.2003,weights=normwt)
m1.2003$pred.m1 <- predict(m1.fit.2003)
res[res$year=="2003", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2003))$r.squared)
#RMSPE
res[res$year=="2003", 'm1.PE'] <- print(rmse(residuals(m1.fit.2003)))

#spatial
###to check
spatial2003<-m1.2003 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2003.spat<- lm(barpm ~ barpred, data=spatial2003)
res[res$year=="2003", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003))$r.squared)
res[res$year=="2003", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2003.spat)))
       
#temporal
tempo2003<-left_join(m1.2003,spatial2003)
tempo2003$delpm <-tempo2003$PM25-tempo2003$barpm
tempo2003$delpred <-tempo2003$pred.m1-tempo2003$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2003)
res[res$year=="2003", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003))$r.squared)
saveRDS(m1.2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.pred.rds")


#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2003)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2003)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2003)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2003)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2003)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2003)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2003)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2003)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2003)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2003)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2003.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2003.cv<-lm(PM25~pred.m1.cv,data=m1.2003.cv)
res[res$year=="2003", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2003.cv))$r.squared)
res[res$year=="2003", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2003.cv))$coef[1,1])
res[res$year=="2003", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2003.cv))$coef[1,2])
res[res$year=="2003", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2003.cv))$coef[2,1])
res[res$year=="2003", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2003.cv))$coef[2,2])
#RMSPE
res[res$year=="2003", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2003.cv)))

#spatial
spatial2003.cv<-m1.2003.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2003.cv.s <- lm(barpm ~ barpred, data=spatial2003.cv)
res[res$year=="2003", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003.cv))$r.squared)
res[res$year=="2003", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2003.cv.s)))
       
#temporal
tempo2003.cv<-left_join(m1.2003.cv,spatial2003.cv)
tempo2003.cv$delpm <-tempo2003.cv$PM25-tempo2003.cv$barpm
tempo2003.cv$delpred <-tempo2003.cv$pred.m1.cv-tempo2003.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2003.cv)
res[res$year=="2003", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003.cv))$r.squared)



#-------->>> loc stage
luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2003.cv,stn)
setkey(luf,stn)
m1.2003.cv.loc <- merge(m1.2003.cv, luf, all.x = T)
m1.2003.cv.loc<-na.omit(m1.2003.cv.loc)

#create residual mp3 variable
m1.2003.cv.loc$res.m1<-m1.2003.cv.loc$PM25-m1.2003.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2003.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2003.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2003.cv.loc$pred.m1.both <- m1.2003.cv.loc$pred.m1.cv + m1.2003.cv.loc$pred.m1.loc
res[res$year=="2003", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2003.cv.loc))$r.squared)
res[res$year=="2003", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2003.cv.loc))$coef[1,1])
res[res$year=="2003", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2003.cv.loc))$coef[1,2])
res[res$year=="2003", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2003.cv.loc))$coef[2,1])
res[res$year=="2003", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2003.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2003", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2003.cv)))

#spatial
spatial2003.cv.loc<-m1.2003.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2003.cv.loc.s <- lm(barpm ~ barpred, data=spatial2003.cv.loc)
res[res$year=="2003", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2003.cv.loc))$r.squared)
res[res$year=="2003", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2003.cv.loc.s)))
       
#temporal
tempo2003.loc.cv<-left_join(m1.2003.cv.loc,spatial2003.cv.loc)
tempo2003.loc.cv$delpm <-tempo2003.loc.cv$PM25-tempo2003.loc.cv$barpm
tempo2003.loc.cv$delpred <-tempo2003.loc.cv$pred.m1.both-tempo2003.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2003.loc.cv)
res[res$year=="2003", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2003.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2003.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2003.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.predCV.rds")


###############
#MOD2
###############
m2.2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")


m2.2003[,elev.s:= scale(elev)]
m2.2003[,tden.s:= scale(tden)]
m2.2003[,pden.s:= scale(pden)]
m2.2003[,dist2A1.s:= scale(dist2A1)]
m2.2003[,dist2water.s:= scale(dist2water)]
m2.2003[,dist2rail.s:= scale(dist2rail)]
m2.2003[,Dist2road.s:= scale(Dist2road)]
m2.2003[,ndvi.s:= scale(ndvi)]
m2.2003[,MeanPbl.s:= scale(MeanPbl)]
m2.2003[,p_ind.s:= scale(p_ind)]
m2.2003[,p_for.s:= scale(p_for)]
m2.2003[,p_farm.s:= scale(p_farm)]
m2.2003[,p_dos.s:= scale(p_dos)]
m2.2003[,p_dev.s:= scale(p_dev)]
m2.2003[,p_os.s:= scale(p_os)]
m2.2003[,Temp.s:= scale(Temp)]
m2.2003[,WD.s:= scale(WD)]
m2.2003[,WS.s:= scale(WS)]
m2.2003[,RH.s:= scale(RH)]
m2.2003[,Rain.s:= scale(Rain)]

#generate predictions
m2.2003[, pred.m2 := predict(object=m1.fit.2003,newdata=m2.2003,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2003$pred.m2)
#delete implossible values
m2.2003 <- m2.2003[pred.m2 > 0.00000000000001 , ]
m2.2003 <- m2.2003[pred.m2 < 500   , ]

saveRDS(m2.2003,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")

#test
# describe(m2.2003$pred.m2)
# hist(m2.2003$pred.m2)

#######
#M2 R2
######
# m1.2003[,aodid:= paste(m1.2003$long_aod,m1.2003$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2003,aodid,day)
# setkey(m2.2003,aodid,day)
# m.1.2.pred <- merge(m1.2003, m2.2003[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2003[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2003.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2003.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()


#$$2004
### import data
m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.rds")

### subset to aqua and apply alexei cleaning methods
m1.2004<-m1.2004[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2004, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2004[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2004 <- m1.2004[!(m1.2004$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2004)
m1.2004$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2004))#0.74
#get rid of missing
m1.2004 <- na.omit(m1.2004)
m1.2004[,elev.s:= scale(elev)]
m1.2004[,tden.s:= scale(tden)]
m1.2004[,pden.s:= scale(pden)]
m1.2004[,dist2A1.s:= scale(dist2A1)]
m1.2004[,dist2water.s:= scale(dist2water)]
m1.2004[,dist2rail.s:= scale(dist2rail)]
m1.2004[,Dist2road.s:= scale(Dist2road)]
m1.2004[,ndvi.s:= scale(ndvi)]
m1.2004[,MeanPbl.s:= scale(MeanPbl)]
m1.2004[,p_ind.s:= scale(p_ind)]
m1.2004[,p_for.s:= scale(p_for)]
m1.2004[,p_farm.s:= scale(p_farm)]
m1.2004[,p_dos.s:= scale(p_dos)]
m1.2004[,p_dev.s:= scale(p_dev)]
m1.2004[,p_os.s:= scale(p_os)]
m1.2004[,Temp.s:= scale(Temp)]
m1.2004[,WD.s:= scale(WD)]
m1.2004[,WS.s:= scale(WS)]
m1.2004[,RH.s:= scale(RH)]
m1.2004[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2004 <-  lmer(m1.formula,data=m1.2004,weights=normwt)
m1.2004$pred.m1 <- predict(m1.fit.2004)
res[res$year=="2004", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2004))$r.squared)
#RMSPE
res[res$year=="2004", 'm1.PE'] <- print(rmse(residuals(m1.fit.2004)))

#spatial
###to check
spatial2004<-m1.2004 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.spat<- lm(barpm ~ barpred, data=spatial2004)
res[res$year=="2004", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004))$r.squared)
res[res$year=="2004", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2004.spat)))
       
#temporal
tempo2004<-left_join(m1.2004,spatial2004)
tempo2004$delpm <-tempo2004$PM25-tempo2004$barpm
tempo2004$delpred <-tempo2004$pred.m1-tempo2004$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2004)
res[res$year=="2004", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004))$r.squared)
saveRDS(m1.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.pred.rds")


#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.2004)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.2004)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.2004)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.2004)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.2004)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.2004)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.2004)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.2004)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.2004)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.2004)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.2004.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.2004.cv<-lm(PM25~pred.m1.cv,data=m1.2004.cv)
res[res$year=="2004", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2004.cv))$r.squared)
res[res$year=="2004", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2004.cv))$coef[1,1])
res[res$year=="2004", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2004.cv))$coef[1,2])
res[res$year=="2004", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2004.cv))$coef[2,1])
res[res$year=="2004", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2004.cv))$coef[2,2])
#RMSPE
res[res$year=="2004", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

#spatial
spatial2004.cv<-m1.2004.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.cv.s <- lm(barpm ~ barpred, data=spatial2004.cv)
res[res$year=="2004", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv))$r.squared)
res[res$year=="2004", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.s)))
       
#temporal
tempo2004.cv<-left_join(m1.2004.cv,spatial2004.cv)
tempo2004.cv$delpm <-tempo2004.cv$PM25-tempo2004.cv$barpm
tempo2004.cv$delpred <-tempo2004.cv$pred.m1.cv-tempo2004.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempo2004.cv)
res[res$year=="2004", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004.cv))$r.squared)



#-------->>> loc stage
luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")

#add 50m LU to CV data
setkey(m1.2004.cv,stn)
setkey(luf,stn)
m1.2004.cv.loc <- merge(m1.2004.cv, luf, all.x = T)
m1.2004.cv.loc<-na.omit(m1.2004.cv.loc)

#create residual mp3 variable
m1.2004.cv.loc$res.m1<-m1.2004.cv.loc$PM25-m1.2004.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2004.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2004.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2004.cv.loc$pred.m1.both <- m1.2004.cv.loc$pred.m1.cv + m1.2004.cv.loc$pred.m1.loc
res[res$year=="2004", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2004.cv.loc))$r.squared)
res[res$year=="2004", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2004.cv.loc))$coef[1,1])
res[res$year=="2004", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2004.cv.loc))$coef[1,2])
res[res$year=="2004", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2004.cv.loc))$coef[2,1])
res[res$year=="2004", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2004.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2004", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

#spatial
spatial2004.cv.loc<-m1.2004.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.cv.loc.s <- lm(barpm ~ barpred, data=spatial2004.cv.loc)
res[res$year=="2004", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv.loc))$r.squared)
res[res$year=="2004", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.loc.s)))
       
#temporal
tempo2004.loc.cv<-left_join(m1.2004.cv.loc,spatial2004.cv.loc)
tempo2004.loc.cv$delpm <-tempo2004.loc.cv$PM25-tempo2004.loc.cv$barpm
tempo2004.loc.cv$delpred <-tempo2004.loc.cv$pred.m1.both-tempo2004.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2004.loc.cv)
res[res$year=="2004", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2004.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2004.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.predCV.rds")


###############
#MOD2
###############
m2.2004<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")


m2.2004[,elev.s:= scale(elev)]
m2.2004[,tden.s:= scale(tden)]
m2.2004[,pden.s:= scale(pden)]
m2.2004[,dist2A1.s:= scale(dist2A1)]
m2.2004[,dist2water.s:= scale(dist2water)]
m2.2004[,dist2rail.s:= scale(dist2rail)]
m2.2004[,Dist2road.s:= scale(Dist2road)]
m2.2004[,ndvi.s:= scale(ndvi)]
m2.2004[,MeanPbl.s:= scale(MeanPbl)]
m2.2004[,p_ind.s:= scale(p_ind)]
m2.2004[,p_for.s:= scale(p_for)]
m2.2004[,p_farm.s:= scale(p_farm)]
m2.2004[,p_dos.s:= scale(p_dos)]
m2.2004[,p_dev.s:= scale(p_dev)]
m2.2004[,p_os.s:= scale(p_os)]
m2.2004[,Temp.s:= scale(Temp)]
m2.2004[,WD.s:= scale(WD)]
m2.2004[,WS.s:= scale(WS)]
m2.2004[,RH.s:= scale(RH)]
m2.2004[,Rain.s:= scale(Rain)]

#generate predictions
m2.2004[, pred.m2 := predict(object=m1.fit.2004,newdata=m2.2004,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2004$pred.m2)
#delete implossible values
m2.2004 <- m2.2004[pred.m2 > 0.00000000000001 , ]
m2.2004 <- m2.2004[pred.m2 < 500   , ]

saveRDS(m2.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")

#test
# describe(m2.2004$pred.m2)
# hist(m2.2004$pred.m2)

#######
#M2 R2
######
# m1.2004[,aodid:= paste(m1.2004$long_aod,m1.2004$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2004,aodid,day)
# setkey(m2.2004,aodid,day)
# m.1.2.pred <- merge(m1.2004, m2.2004[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2004[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2004.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2004.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2004.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()




### import data
m1.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.rds")

### subset to aqua and apply alexei cleaning methods
m1.2005<-m1.2005[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2005, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2005[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2005 <- m1.2005[!(m1.2005$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2005)
m1.2005$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2005))#0.74
#get rid of missing
m1.2005 <- na.omit(m1.2005)
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
m1.2005[,Temp.s:= scale(Temp)]
m1.2005[,WD.s:= scale(WD)]
m1.2005[,WS.s:= scale(WS)]
m1.2005[,RH.s:= scale(RH)]
m1.2005[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2005 <-  lmer(m1.formula,data=m1.2005,weights=normwt)
m1.2005$pred.m1 <- predict(m1.fit.2005)
res[res$year=="2005", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2005))$r.squared)
#RMSPE
res[res$year=="2005", 'm1.PE'] <- print(rmse(residuals(m1.fit.2005)))

#spatial
###to check
spatial2005<-m1.2005 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.spat<- lm(barpm ~ barpred, data=spatial2005)
res[res$year=="2005", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005))$r.squared)
res[res$year=="2005", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2005.spat)))
       
#temporal
tempo2005<-left_join(m1.2005,spatial2005)
tempo2005$delpm <-tempo2005$PM25-tempo2005$barpm
tempo2005$delpred <-tempo2005$pred.m1-tempo2005$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2005)
res[res$year=="2005", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005))$r.squared)
saveRDS(m1.2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.pred.rds")


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
m1.fit.2005.cv<-lm(PM25~pred.m1.cv,data=m1.2005.cv)
res[res$year=="2005", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2005.cv))$r.squared)
res[res$year=="2005", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2005.cv))$coef[1,1])
res[res$year=="2005", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2005.cv))$coef[1,2])
res[res$year=="2005", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2005.cv))$coef[2,1])
res[res$year=="2005", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2005.cv))$coef[2,2])
#RMSPE
res[res$year=="2005", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2005.cv)))

#spatial
spatial2005.cv<-m1.2005.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.cv.s <- lm(barpm ~ barpred, data=spatial2005.cv)
res[res$year=="2005", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005.cv))$r.squared)
res[res$year=="2005", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2005.cv.s)))
       
#temporal
tempo2005.cv<-left_join(m1.2005.cv,spatial2005.cv)
tempo2005.cv$delpm <-tempo2005.cv$PM25-tempo2005.cv$barpm
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
m1.2005.cv.loc<-na.omit(m1.2005.cv.loc)

#create residual mp3 variable
m1.2005.cv.loc$res.m1<-m1.2005.cv.loc$PM25-m1.2005.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2005.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2005.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2005.cv.loc$pred.m1.both <- m1.2005.cv.loc$pred.m1.cv + m1.2005.cv.loc$pred.m1.loc
res[res$year=="2005", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2005.cv.loc))$r.squared)
res[res$year=="2005", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2005.cv.loc))$coef[1,1])
res[res$year=="2005", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2005.cv.loc))$coef[1,2])
res[res$year=="2005", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2005.cv.loc))$coef[2,1])
res[res$year=="2005", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2005.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2005", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2005.cv)))

#spatial
spatial2005.cv.loc<-m1.2005.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.cv.loc.s <- lm(barpm ~ barpred, data=spatial2005.cv.loc)
res[res$year=="2005", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005.cv.loc))$r.squared)
res[res$year=="2005", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2005.cv.loc.s)))
       
#temporal
tempo2005.loc.cv<-left_join(m1.2005.cv.loc,spatial2005.cv.loc)
tempo2005.loc.cv$delpm <-tempo2005.loc.cv$PM25-tempo2005.loc.cv$barpm
tempo2005.loc.cv$delpred <-tempo2005.loc.cv$pred.m1.both-tempo2005.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2005.loc.cv)
res[res$year=="2005", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2005.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2005.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.predCV.rds")


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
m2.2005[,Temp.s:= scale(Temp)]
m2.2005[,WD.s:= scale(WD)]
m2.2005[,WS.s:= scale(WS)]
m2.2005[,RH.s:= scale(RH)]
m2.2005[,Rain.s:= scale(Rain)]

#generate predictions
m2.2005[, pred.m2 := predict(object=m1.fit.2005,newdata=m2.2005,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2005$pred.m2)
#delete implossible values
m2.2005 <- m2.2005[pred.m2 > 0.00000000000001 , ]
m2.2005 <- m2.2005[pred.m2 < 500   , ]

saveRDS(m2.2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2005.rds")

#test
# describe(m2.2005$pred.m2)
# hist(m2.2005$pred.m2)

#######
#M2 R2
######
# m1.2005[,aodid:= paste(m1.2005$long_aod,m1.2005$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2005,aodid,day)
# setkey(m2.2005,aodid,day)
# m.1.2.pred <- merge(m1.2005, m2.2005[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2005[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2005.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2005.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2005.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()




### import data
m1.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2006.rds")

### subset to aqua and apply alexei cleaning methods
m1.2006<-m1.2006[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2006, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2006[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2006 <- m1.2006[!(m1.2006$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2006)
m1.2006$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2006))#0.74
#get rid of missing
m1.2006 <- na.omit(m1.2006)
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
m1.2006[,Temp.s:= scale(Temp)]
m1.2006[,WD.s:= scale(WD)]
m1.2006[,WS.s:= scale(WS)]
m1.2006[,RH.s:= scale(RH)]
m1.2006[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2006 <-  lmer(m1.formula,data=m1.2006,weights=normwt)
m1.2006$pred.m1 <- predict(m1.fit.2006)
res[res$year=="2006", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2006))$r.squared)
#RMSPE
res[res$year=="2006", 'm1.PE'] <- print(rmse(residuals(m1.fit.2006)))

#spatial
###to check
spatial2006<-m1.2006 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.spat<- lm(barpm ~ barpred, data=spatial2006)
res[res$year=="2006", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006))$r.squared)
res[res$year=="2006", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2006.spat)))
       
#temporal
tempo2006<-left_join(m1.2006,spatial2006)
tempo2006$delpm <-tempo2006$PM25-tempo2006$barpm
tempo2006$delpred <-tempo2006$pred.m1-tempo2006$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2006)
res[res$year=="2006", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006))$r.squared)
saveRDS(m1.2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2006.pred.rds")


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
m1.fit.2006.cv<-lm(PM25~pred.m1.cv,data=m1.2006.cv)
res[res$year=="2006", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2006.cv))$r.squared)
res[res$year=="2006", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2006.cv))$coef[1,1])
res[res$year=="2006", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2006.cv))$coef[1,2])
res[res$year=="2006", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2006.cv))$coef[2,1])
res[res$year=="2006", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2006.cv))$coef[2,2])
#RMSPE
res[res$year=="2006", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2006.cv)))

#spatial
spatial2006.cv<-m1.2006.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.cv.s <- lm(barpm ~ barpred, data=spatial2006.cv)
res[res$year=="2006", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006.cv))$r.squared)
res[res$year=="2006", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2006.cv.s)))
       
#temporal
tempo2006.cv<-left_join(m1.2006.cv,spatial2006.cv)
tempo2006.cv$delpm <-tempo2006.cv$PM25-tempo2006.cv$barpm
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
m1.2006.cv.loc<-na.omit(m1.2006.cv.loc)

#create residual mp3 variable
m1.2006.cv.loc$res.m1<-m1.2006.cv.loc$PM25-m1.2006.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2006.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2006.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2006.cv.loc$pred.m1.both <- m1.2006.cv.loc$pred.m1.cv + m1.2006.cv.loc$pred.m1.loc
res[res$year=="2006", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2006.cv.loc))$r.squared)
res[res$year=="2006", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2006.cv.loc))$coef[1,1])
res[res$year=="2006", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2006.cv.loc))$coef[1,2])
res[res$year=="2006", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2006.cv.loc))$coef[2,1])
res[res$year=="2006", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2006.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2006", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2006.cv)))

#spatial
spatial2006.cv.loc<-m1.2006.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2006.cv.loc.s <- lm(barpm ~ barpred, data=spatial2006.cv.loc)
res[res$year=="2006", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2006.cv.loc))$r.squared)
res[res$year=="2006", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2006.cv.loc.s)))
       
#temporal
tempo2006.loc.cv<-left_join(m1.2006.cv.loc,spatial2006.cv.loc)
tempo2006.loc.cv$delpm <-tempo2006.loc.cv$PM25-tempo2006.loc.cv$barpm
tempo2006.loc.cv$delpred <-tempo2006.loc.cv$pred.m1.both-tempo2006.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2006.loc.cv)
res[res$year=="2006", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2006.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2006.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2006.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2006.predCV.rds")


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
m2.2006[,Temp.s:= scale(Temp)]
m2.2006[,WD.s:= scale(WD)]
m2.2006[,WS.s:= scale(WS)]
m2.2006[,RH.s:= scale(RH)]
m2.2006[,Rain.s:= scale(Rain)]

#generate predictions
m2.2006[, pred.m2 := predict(object=m1.fit.2006,newdata=m2.2006,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2006$pred.m2)
#delete implossible values
m2.2006 <- m2.2006[pred.m2 > 0.00000000000001 , ]
m2.2006 <- m2.2006[pred.m2 < 500   , ]

saveRDS(m2.2006,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2006.rds")

#test
# describe(m2.2006$pred.m2)
# hist(m2.2006$pred.m2)

#######
#M2 R2
######
# m1.2006[,aodid:= paste(m1.2006$long_aod,m1.2006$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2006,aodid,day)
# setkey(m2.2006,aodid,day)
# m.1.2.pred <- merge(m1.2006, m2.2006[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2006[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2006.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2006.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2006.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()






### import data
m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.rds")

### subset to aqua and apply alexei cleaning methods
m1.2007<-m1.2007[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2007, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2007[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2007 <- m1.2007[!(m1.2007$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2007)
m1.2007$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2007))#0.74
#get rid of missing
m1.2007 <- na.omit(m1.2007)
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
m1.2007[,Temp.s:= scale(Temp)]
m1.2007[,WD.s:= scale(WD)]
m1.2007[,WS.s:= scale(WS)]
m1.2007[,RH.s:= scale(RH)]
m1.2007[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2007 <-  lmer(m1.formula,data=m1.2007,weights=normwt)
m1.2007$pred.m1 <- predict(m1.fit.2007)
res[res$year=="2007", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2007))$r.squared)
#RMSPE
res[res$year=="2007", 'm1.PE'] <- print(rmse(residuals(m1.fit.2007)))

#spatial
###to check
spatial2007<-m1.2007 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.spat<- lm(barpm ~ barpred, data=spatial2007)
res[res$year=="2007", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007))$r.squared)
res[res$year=="2007", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2007.spat)))
       
#temporal
tempo2007<-left_join(m1.2007,spatial2007)
tempo2007$delpm <-tempo2007$PM25-tempo2007$barpm
tempo2007$delpred <-tempo2007$pred.m1-tempo2007$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2007)
res[res$year=="2007", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007))$r.squared)
saveRDS(m1.2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.pred.rds")


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
m1.fit.2007.cv<-lm(PM25~pred.m1.cv,data=m1.2007.cv)
res[res$year=="2007", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2007.cv))$r.squared)
res[res$year=="2007", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2007.cv))$coef[1,1])
res[res$year=="2007", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2007.cv))$coef[1,2])
res[res$year=="2007", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2007.cv))$coef[2,1])
res[res$year=="2007", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2007.cv))$coef[2,2])
#RMSPE
res[res$year=="2007", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2007.cv)))

#spatial
spatial2007.cv<-m1.2007.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.cv.s <- lm(barpm ~ barpred, data=spatial2007.cv)
res[res$year=="2007", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007.cv))$r.squared)
res[res$year=="2007", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2007.cv.s)))
       
#temporal
tempo2007.cv<-left_join(m1.2007.cv,spatial2007.cv)
tempo2007.cv$delpm <-tempo2007.cv$PM25-tempo2007.cv$barpm
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
m1.2007.cv.loc<-na.omit(m1.2007.cv.loc)

#create residual mp3 variable
m1.2007.cv.loc$res.m1<-m1.2007.cv.loc$PM25-m1.2007.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2007.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2007.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2007.cv.loc$pred.m1.both <- m1.2007.cv.loc$pred.m1.cv + m1.2007.cv.loc$pred.m1.loc
res[res$year=="2007", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2007.cv.loc))$r.squared)
res[res$year=="2007", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2007.cv.loc))$coef[1,1])
res[res$year=="2007", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2007.cv.loc))$coef[1,2])
res[res$year=="2007", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2007.cv.loc))$coef[2,1])
res[res$year=="2007", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2007.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2007", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2007.cv)))

#spatial
spatial2007.cv.loc<-m1.2007.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2007.cv.loc.s <- lm(barpm ~ barpred, data=spatial2007.cv.loc)
res[res$year=="2007", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2007.cv.loc))$r.squared)
res[res$year=="2007", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2007.cv.loc.s)))
       
#temporal
tempo2007.loc.cv<-left_join(m1.2007.cv.loc,spatial2007.cv.loc)
tempo2007.loc.cv$delpm <-tempo2007.loc.cv$PM25-tempo2007.loc.cv$barpm
tempo2007.loc.cv$delpred <-tempo2007.loc.cv$pred.m1.both-tempo2007.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2007.loc.cv)
res[res$year=="2007", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2007.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2007.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2007.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.predCV.rds")


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
m2.2007[,Temp.s:= scale(Temp)]
m2.2007[,WD.s:= scale(WD)]
m2.2007[,WS.s:= scale(WS)]
m2.2007[,RH.s:= scale(RH)]
m2.2007[,Rain.s:= scale(Rain)]

#generate predictions
m2.2007[, pred.m2 := predict(object=m1.fit.2007,newdata=m2.2007,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2007$pred.m2)
#delete implossible values
m2.2007 <- m2.2007[pred.m2 > 0.00000000000001 , ]
m2.2007 <- m2.2007[pred.m2 < 500   , ]

saveRDS(m2.2007,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.rds")

#test
# describe(m2.2007$pred.m2)
# hist(m2.2007$pred.m2)

#######
#M2 R2
######
# m1.2007[,aodid:= paste(m1.2007$long_aod,m1.2007$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2007,aodid,day)
# setkey(m2.2007,aodid,day)
# m.1.2.pred <- merge(m1.2007, m2.2007[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2007[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2007.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2007.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2007.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()






### import data
m1.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2008.rds")

### subset to aqua and apply alexei cleaning methods
m1.2008<-m1.2008[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2008, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2008[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2008 <- m1.2008[!(m1.2008$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2008)
m1.2008$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2008))#0.74
#get rid of missing
m1.2008 <- na.omit(m1.2008)
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
m1.2008[,Temp.s:= scale(Temp)]
m1.2008[,WD.s:= scale(WD)]
m1.2008[,WS.s:= scale(WS)]
m1.2008[,RH.s:= scale(RH)]
m1.2008[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2008 <-  lmer(m1.formula,data=m1.2008,weights=normwt)
m1.2008$pred.m1 <- predict(m1.fit.2008)
res[res$year=="2008", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2008))$r.squared)
#RMSPE
res[res$year=="2008", 'm1.PE'] <- print(rmse(residuals(m1.fit.2008)))

#spatial
###to check
spatial2008<-m1.2008 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.spat<- lm(barpm ~ barpred, data=spatial2008)
res[res$year=="2008", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008))$r.squared)
res[res$year=="2008", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2008.spat)))
       
#temporal
tempo2008<-left_join(m1.2008,spatial2008)
tempo2008$delpm <-tempo2008$PM25-tempo2008$barpm
tempo2008$delpred <-tempo2008$pred.m1-tempo2008$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2008)
res[res$year=="2008", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008))$r.squared)
saveRDS(m1.2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2008.pred.rds")


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
m1.fit.2008.cv<-lm(PM25~pred.m1.cv,data=m1.2008.cv)
res[res$year=="2008", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2008.cv))$r.squared)
res[res$year=="2008", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2008.cv))$coef[1,1])
res[res$year=="2008", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2008.cv))$coef[1,2])
res[res$year=="2008", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2008.cv))$coef[2,1])
res[res$year=="2008", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2008.cv))$coef[2,2])
#RMSPE
res[res$year=="2008", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2008.cv)))

#spatial
spatial2008.cv<-m1.2008.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.cv.s <- lm(barpm ~ barpred, data=spatial2008.cv)
res[res$year=="2008", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008.cv))$r.squared)
res[res$year=="2008", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2008.cv.s)))
       
#temporal
tempo2008.cv<-left_join(m1.2008.cv,spatial2008.cv)
tempo2008.cv$delpm <-tempo2008.cv$PM25-tempo2008.cv$barpm
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
m1.2008.cv.loc<-na.omit(m1.2008.cv.loc)

#create residual mp3 variable
m1.2008.cv.loc$res.m1<-m1.2008.cv.loc$PM25-m1.2008.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2008.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2008.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2008.cv.loc$pred.m1.both <- m1.2008.cv.loc$pred.m1.cv + m1.2008.cv.loc$pred.m1.loc
res[res$year=="2008", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2008.cv.loc))$r.squared)
res[res$year=="2008", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2008.cv.loc))$coef[1,1])
res[res$year=="2008", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2008.cv.loc))$coef[1,2])
res[res$year=="2008", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2008.cv.loc))$coef[2,1])
res[res$year=="2008", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2008.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2008", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2008.cv)))

#spatial
spatial2008.cv.loc<-m1.2008.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2008.cv.loc.s <- lm(barpm ~ barpred, data=spatial2008.cv.loc)
res[res$year=="2008", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2008.cv.loc))$r.squared)
res[res$year=="2008", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2008.cv.loc.s)))
       
#temporal
tempo2008.loc.cv<-left_join(m1.2008.cv.loc,spatial2008.cv.loc)
tempo2008.loc.cv$delpm <-tempo2008.loc.cv$PM25-tempo2008.loc.cv$barpm
tempo2008.loc.cv$delpred <-tempo2008.loc.cv$pred.m1.both-tempo2008.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2008.loc.cv)
res[res$year=="2008", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2008.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2008.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2008.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2008.predCV.rds")


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
m2.2008[,Temp.s:= scale(Temp)]
m2.2008[,WD.s:= scale(WD)]
m2.2008[,WS.s:= scale(WS)]
m2.2008[,RH.s:= scale(RH)]
m2.2008[,Rain.s:= scale(Rain)]

#generate predictions
m2.2008[, pred.m2 := predict(object=m1.fit.2008,newdata=m2.2008,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2008$pred.m2)
#delete implossible values
m2.2008 <- m2.2008[pred.m2 > 0.00000000000001 , ]
m2.2008 <- m2.2008[pred.m2 < 500   , ]

saveRDS(m2.2008,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2008.rds")

#test
# describe(m2.2008$pred.m2)
# hist(m2.2008$pred.m2)

#######
#M2 R2
######
# m1.2008[,aodid:= paste(m1.2008$long_aod,m1.2008$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2008,aodid,day)
# setkey(m2.2008,aodid,day)
# m.1.2.pred <- merge(m1.2008, m2.2008[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2008[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2008.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2008.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2008.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()





### import data
m1.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2009.rds")

### subset to aqua and apply alexei cleaning methods
m1.2009<-m1.2009[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2009, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2009[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2009 <- m1.2009[!(m1.2009$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2009)
m1.2009$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2009))#0.74
#get rid of missing
m1.2009 <- na.omit(m1.2009)
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
m1.2009[,Temp.s:= scale(Temp)]
m1.2009[,WD.s:= scale(WD)]
m1.2009[,WS.s:= scale(WS)]
m1.2009[,RH.s:= scale(RH)]
m1.2009[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2009 <-  lmer(m1.formula,data=m1.2009,weights=normwt)
m1.2009$pred.m1 <- predict(m1.fit.2009)
res[res$year=="2009", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2009))$r.squared)
#RMSPE
res[res$year=="2009", 'm1.PE'] <- print(rmse(residuals(m1.fit.2009)))

#spatial
###to check
spatial2009<-m1.2009 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.spat<- lm(barpm ~ barpred, data=spatial2009)
res[res$year=="2009", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009))$r.squared)
res[res$year=="2009", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2009.spat)))
       
#temporal
tempo2009<-left_join(m1.2009,spatial2009)
tempo2009$delpm <-tempo2009$PM25-tempo2009$barpm
tempo2009$delpred <-tempo2009$pred.m1-tempo2009$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2009)
res[res$year=="2009", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009))$r.squared)
saveRDS(m1.2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2009.pred.rds")


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
m1.fit.2009.cv<-lm(PM25~pred.m1.cv,data=m1.2009.cv)
res[res$year=="2009", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2009.cv))$r.squared)
res[res$year=="2009", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2009.cv))$coef[1,1])
res[res$year=="2009", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2009.cv))$coef[1,2])
res[res$year=="2009", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2009.cv))$coef[2,1])
res[res$year=="2009", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2009.cv))$coef[2,2])
#RMSPE
res[res$year=="2009", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2009.cv)))

#spatial
spatial2009.cv<-m1.2009.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.cv.s <- lm(barpm ~ barpred, data=spatial2009.cv)
res[res$year=="2009", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009.cv))$r.squared)
res[res$year=="2009", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2009.cv.s)))
       
#temporal
tempo2009.cv<-left_join(m1.2009.cv,spatial2009.cv)
tempo2009.cv$delpm <-tempo2009.cv$PM25-tempo2009.cv$barpm
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
m1.2009.cv.loc<-na.omit(m1.2009.cv.loc)

#create residual mp3 variable
m1.2009.cv.loc$res.m1<-m1.2009.cv.loc$PM25-m1.2009.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2009.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2009.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2009.cv.loc$pred.m1.both <- m1.2009.cv.loc$pred.m1.cv + m1.2009.cv.loc$pred.m1.loc
res[res$year=="2009", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2009.cv.loc))$r.squared)
res[res$year=="2009", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2009.cv.loc))$coef[1,1])
res[res$year=="2009", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2009.cv.loc))$coef[1,2])
res[res$year=="2009", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2009.cv.loc))$coef[2,1])
res[res$year=="2009", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2009.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2009", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2009.cv)))

#spatial
spatial2009.cv.loc<-m1.2009.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2009.cv.loc.s <- lm(barpm ~ barpred, data=spatial2009.cv.loc)
res[res$year=="2009", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2009.cv.loc))$r.squared)
res[res$year=="2009", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2009.cv.loc.s)))
       
#temporal
tempo2009.loc.cv<-left_join(m1.2009.cv.loc,spatial2009.cv.loc)
tempo2009.loc.cv$delpm <-tempo2009.loc.cv$PM25-tempo2009.loc.cv$barpm
tempo2009.loc.cv$delpred <-tempo2009.loc.cv$pred.m1.both-tempo2009.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2009.loc.cv)
res[res$year=="2009", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2009.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2009.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2009.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2009.predCV.rds")


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
m2.2009[,Temp.s:= scale(Temp)]
m2.2009[,WD.s:= scale(WD)]
m2.2009[,WS.s:= scale(WS)]
m2.2009[,RH.s:= scale(RH)]
m2.2009[,Rain.s:= scale(Rain)]

#generate predictions
m2.2009[, pred.m2 := predict(object=m1.fit.2009,newdata=m2.2009,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2009$pred.m2)
#delete implossible values
m2.2009 <- m2.2009[pred.m2 > 0.00000000000001 , ]
m2.2009 <- m2.2009[pred.m2 < 500   , ]

saveRDS(m2.2009,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2009.rds")

#test
# describe(m2.2009$pred.m2)
# hist(m2.2009$pred.m2)

#######
#M2 R2
######
# m1.2009[,aodid:= paste(m1.2009$long_aod,m1.2009$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2009,aodid,day)
# setkey(m2.2009,aodid,day)
# m.1.2.pred <- merge(m1.2009, m2.2009[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2009[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2009.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2009.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2009.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()




### import data
m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.rds")

### subset to aqua and apply alexei cleaning methods
m1.2010<-m1.2010[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2010, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2010[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2010 <- m1.2010[!(m1.2010$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2010)
m1.2010$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2010))#0.74
#get rid of missing
m1.2010 <- na.omit(m1.2010)
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
m1.2010[,Temp.s:= scale(Temp)]
m1.2010[,WD.s:= scale(WD)]
m1.2010[,WS.s:= scale(WS)]
m1.2010[,RH.s:= scale(RH)]
m1.2010[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2010 <-  lmer(m1.formula,data=m1.2010,weights=normwt)
m1.2010$pred.m1 <- predict(m1.fit.2010)
res[res$year=="2010", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2010))$r.squared)
#RMSPE
res[res$year=="2010", 'm1.PE'] <- print(rmse(residuals(m1.fit.2010)))

#spatial
###to check
spatial2010<-m1.2010 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.spat<- lm(barpm ~ barpred, data=spatial2010)
res[res$year=="2010", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010))$r.squared)
res[res$year=="2010", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2010.spat)))
       
#temporal
tempo2010<-left_join(m1.2010,spatial2010)
tempo2010$delpm <-tempo2010$PM25-tempo2010$barpm
tempo2010$delpred <-tempo2010$pred.m1-tempo2010$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2010)
res[res$year=="2010", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010))$r.squared)
saveRDS(m1.2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.pred.rds")


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
m1.fit.2010.cv<-lm(PM25~pred.m1.cv,data=m1.2010.cv)
res[res$year=="2010", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2010.cv))$r.squared)
res[res$year=="2010", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2010.cv))$coef[1,1])
res[res$year=="2010", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2010.cv))$coef[1,2])
res[res$year=="2010", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2010.cv))$coef[2,1])
res[res$year=="2010", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2010.cv))$coef[2,2])
#RMSPE
res[res$year=="2010", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2010.cv)))

#spatial
spatial2010.cv<-m1.2010.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.cv.s <- lm(barpm ~ barpred, data=spatial2010.cv)
res[res$year=="2010", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010.cv))$r.squared)
res[res$year=="2010", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2010.cv.s)))
       
#temporal
tempo2010.cv<-left_join(m1.2010.cv,spatial2010.cv)
tempo2010.cv$delpm <-tempo2010.cv$PM25-tempo2010.cv$barpm
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
m1.2010.cv.loc<-na.omit(m1.2010.cv.loc)

#create residual mp3 variable
m1.2010.cv.loc$res.m1<-m1.2010.cv.loc$PM25-m1.2010.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2010.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2010.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2010.cv.loc$pred.m1.both <- m1.2010.cv.loc$pred.m1.cv + m1.2010.cv.loc$pred.m1.loc
res[res$year=="2010", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2010.cv.loc))$r.squared)
res[res$year=="2010", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2010.cv.loc))$coef[1,1])
res[res$year=="2010", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2010.cv.loc))$coef[1,2])
res[res$year=="2010", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2010.cv.loc))$coef[2,1])
res[res$year=="2010", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2010.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2010", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2010.cv)))

#spatial
spatial2010.cv.loc<-m1.2010.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2010.cv.loc.s <- lm(barpm ~ barpred, data=spatial2010.cv.loc)
res[res$year=="2010", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010.cv.loc))$r.squared)
res[res$year=="2010", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2010.cv.loc.s)))
       
#temporal
tempo2010.loc.cv<-left_join(m1.2010.cv.loc,spatial2010.cv.loc)
tempo2010.loc.cv$delpm <-tempo2010.loc.cv$PM25-tempo2010.loc.cv$barpm
tempo2010.loc.cv$delpred <-tempo2010.loc.cv$pred.m1.both-tempo2010.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2010.loc.cv)
res[res$year=="2010", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2010.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2010.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.predCV.rds")


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
m2.2010[,Temp.s:= scale(Temp)]
m2.2010[,WD.s:= scale(WD)]
m2.2010[,WS.s:= scale(WS)]
m2.2010[,RH.s:= scale(RH)]
m2.2010[,Rain.s:= scale(Rain)]

#generate predictions
m2.2010[, pred.m2 := predict(object=m1.fit.2010,newdata=m2.2010,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2010$pred.m2)
#delete implossible values
m2.2010 <- m2.2010[pred.m2 > 0.00000000000001 , ]
m2.2010 <- m2.2010[pred.m2 < 500   , ]

saveRDS(m2.2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.rds")

#test
# describe(m2.2010$pred.m2)
# hist(m2.2010$pred.m2)

#######
#M2 R2
######
# m1.2010[,aodid:= paste(m1.2010$long_aod,m1.2010$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2010,aodid,day)
# setkey(m2.2010,aodid,day)
# m.1.2.pred <- merge(m1.2010, m2.2010[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2010[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2010.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2010.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2010.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()





### import data
m1.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.rds")

### subset to aqua and apply alexei cleaning methods
m1.2011<-m1.2011[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2011, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2011[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2011 <- m1.2011[!(m1.2011$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2011)
m1.2011$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2011))#0.74
#get rid of missing
m1.2011 <- na.omit(m1.2011)
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
m1.2011[,Temp.s:= scale(Temp)]
m1.2011[,WD.s:= scale(WD)]
m1.2011[,WS.s:= scale(WS)]
m1.2011[,RH.s:= scale(RH)]
m1.2011[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2011 <-  lmer(m1.formula,data=m1.2011,weights=normwt)
m1.2011$pred.m1 <- predict(m1.fit.2011)
res[res$year=="2011", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2011))$r.squared)
#RMSPE
res[res$year=="2011", 'm1.PE'] <- print(rmse(residuals(m1.fit.2011)))

#spatial
###to check
spatial2011<-m1.2011 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.spat<- lm(barpm ~ barpred, data=spatial2011)
res[res$year=="2011", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011))$r.squared)
res[res$year=="2011", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2011.spat)))
       
#temporal
tempo2011<-left_join(m1.2011,spatial2011)
tempo2011$delpm <-tempo2011$PM25-tempo2011$barpm
tempo2011$delpred <-tempo2011$pred.m1-tempo2011$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2011)
res[res$year=="2011", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011))$r.squared)
saveRDS(m1.2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.pred.rds")


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
m1.fit.2011.cv<-lm(PM25~pred.m1.cv,data=m1.2011.cv)
res[res$year=="2011", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2011.cv))$r.squared)
res[res$year=="2011", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2011.cv))$coef[1,1])
res[res$year=="2011", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2011.cv))$coef[1,2])
res[res$year=="2011", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2011.cv))$coef[2,1])
res[res$year=="2011", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2011.cv))$coef[2,2])
#RMSPE
res[res$year=="2011", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2011.cv)))

#spatial
spatial2011.cv<-m1.2011.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.cv.s <- lm(barpm ~ barpred, data=spatial2011.cv)
res[res$year=="2011", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011.cv))$r.squared)
res[res$year=="2011", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2011.cv.s)))
       
#temporal
tempo2011.cv<-left_join(m1.2011.cv,spatial2011.cv)
tempo2011.cv$delpm <-tempo2011.cv$PM25-tempo2011.cv$barpm
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
m1.2011.cv.loc<-na.omit(m1.2011.cv.loc)

#create residual mp3 variable
m1.2011.cv.loc$res.m1<-m1.2011.cv.loc$PM25-m1.2011.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2011.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2011.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2011.cv.loc$pred.m1.both <- m1.2011.cv.loc$pred.m1.cv + m1.2011.cv.loc$pred.m1.loc
res[res$year=="2011", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2011.cv.loc))$r.squared)
res[res$year=="2011", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2011.cv.loc))$coef[1,1])
res[res$year=="2011", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2011.cv.loc))$coef[1,2])
res[res$year=="2011", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2011.cv.loc))$coef[2,1])
res[res$year=="2011", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2011.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2011", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2011.cv)))

#spatial
spatial2011.cv.loc<-m1.2011.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2011.cv.loc.s <- lm(barpm ~ barpred, data=spatial2011.cv.loc)
res[res$year=="2011", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2011.cv.loc))$r.squared)
res[res$year=="2011", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2011.cv.loc.s)))
       
#temporal
tempo2011.loc.cv<-left_join(m1.2011.cv.loc,spatial2011.cv.loc)
tempo2011.loc.cv$delpm <-tempo2011.loc.cv$PM25-tempo2011.loc.cv$barpm
tempo2011.loc.cv$delpred <-tempo2011.loc.cv$pred.m1.both-tempo2011.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2011.loc.cv)
res[res$year=="2011", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2011.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2011.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2011.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.predCV.rds")


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
m2.2011[,Temp.s:= scale(Temp)]
m2.2011[,WD.s:= scale(WD)]
m2.2011[,WS.s:= scale(WS)]
m2.2011[,RH.s:= scale(RH)]
m2.2011[,Rain.s:= scale(Rain)]

#generate predictions
m2.2011[, pred.m2 := predict(object=m1.fit.2011,newdata=m2.2011,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2011$pred.m2)
#delete implossible values
m2.2011 <- m2.2011[pred.m2 > 0.00000000000001 , ]
m2.2011 <- m2.2011[pred.m2 < 500   , ]

saveRDS(m2.2011,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2011.rds")

#test
# describe(m2.2011$pred.m2)
# hist(m2.2011$pred.m2)

#######
#M2 R2
######
# m1.2011[,aodid:= paste(m1.2011$long_aod,m1.2011$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2011,aodid,day)
# setkey(m2.2011,aodid,day)
# m.1.2.pred <- merge(m1.2011, m2.2011[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2011[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2011.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2011.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2011.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()




### import data
m1.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2012.rds")

### subset to aqua and apply alexei cleaning methods
m1.2012<-m1.2012[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2012, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2012[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2012 <- m1.2012[!(m1.2012$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2012)
m1.2012$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2012))#0.74
#get rid of missing
m1.2012 <- na.omit(m1.2012)
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
m1.2012[,Temp.s:= scale(Temp)]
m1.2012[,WD.s:= scale(WD)]
m1.2012[,WS.s:= scale(WS)]
m1.2012[,RH.s:= scale(RH)]
m1.2012[,Rain.s:= scale(Rain)]



m1.formula <- as.formula(PM25~ aod
                        +Temp.s+WD.s+RH.s+WS.s+Dust+Rain.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2012 <-  lmer(m1.formula,data=m1.2012,weights=normwt)
m1.2012$pred.m1 <- predict(m1.fit.2012)
res[res$year=="2012", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.2012))$r.squared)
#RMSPE
res[res$year=="2012", 'm1.PE'] <- print(rmse(residuals(m1.fit.2012)))

#spatial
###to check
spatial2012<-m1.2012 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.spat<- lm(barpm ~ barpred, data=spatial2012)
res[res$year=="2012", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012))$r.squared)
res[res$year=="2012", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2012.spat)))
       
#temporal
tempo2012<-left_join(m1.2012,spatial2012)
tempo2012$delpm <-tempo2012$PM25-tempo2012$barpm
tempo2012$delpred <-tempo2012$pred.m1-tempo2012$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2012)
res[res$year=="2012", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012))$r.squared)
saveRDS(m1.2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2012.pred.rds")


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
m1.fit.2012.cv<-lm(PM25~pred.m1.cv,data=m1.2012.cv)
res[res$year=="2012", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.2012.cv))$r.squared)
res[res$year=="2012", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2012.cv))$coef[1,1])
res[res$year=="2012", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2012.cv))$coef[1,2])
res[res$year=="2012", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2012.cv))$coef[2,1])
res[res$year=="2012", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.2012.cv))$coef[2,2])
#RMSPE
res[res$year=="2012", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2012.cv)))

#spatial
spatial2012.cv<-m1.2012.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.cv.s <- lm(barpm ~ barpred, data=spatial2012.cv)
res[res$year=="2012", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012.cv))$r.squared)
res[res$year=="2012", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2012.cv.s)))
       
#temporal
tempo2012.cv<-left_join(m1.2012.cv,spatial2012.cv)
tempo2012.cv$delpm <-tempo2012.cv$PM25-tempo2012.cv$barpm
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
m1.2012.cv.loc<-na.omit(m1.2012.cv.loc)

#create residual mp3 variable
m1.2012.cv.loc$res.m1<-m1.2012.cv.loc$PM25-m1.2012.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WS)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2012.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2012.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2012.cv.loc$pred.m1.both <- m1.2012.cv.loc$pred.m1.cv + m1.2012.cv.loc$pred.m1.loc
res[res$year=="2012", 'm1cv.loc.R2'] <- print(summary(lm(PM25~pred.m1.both,data=m1.2012.cv.loc))$r.squared)
res[res$year=="2012", 'm1cv.loc.I'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2012.cv.loc))$coef[1,1])
res[res$year=="2012", 'm1cv.loc.I.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2012.cv.loc))$coef[1,2])
res[res$year=="2012", 'm1cv.loc.S'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2012.cv.loc))$coef[2,1])
res[res$year=="2012", 'm1cv.loc.S.se'] <-print(summary(lm(PM25~pred.m1.both,data=m1.2012.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2012", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2012.cv)))

#spatial
spatial2012.cv.loc<-m1.2012.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2012.cv.loc.s <- lm(barpm ~ barpred, data=spatial2012.cv.loc)
res[res$year=="2012", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2012.cv.loc))$r.squared)
res[res$year=="2012", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2012.cv.loc.s)))
       
#temporal
tempo2012.loc.cv<-left_join(m1.2012.cv.loc,spatial2012.cv.loc)
tempo2012.loc.cv$delpm <-tempo2012.loc.cv$PM25-tempo2012.loc.cv$barpm
tempo2012.loc.cv$delpred <-tempo2012.loc.cv$pred.m1.both-tempo2012.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2012.loc.cv)
res[res$year=="2012", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2012.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res2012.m1.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")
saveRDS(m1.2012.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2012.predCV.rds")


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
m2.2012[,Temp.s:= scale(Temp)]
m2.2012[,WD.s:= scale(WD)]
m2.2012[,WS.s:= scale(WS)]
m2.2012[,RH.s:= scale(RH)]
m2.2012[,Rain.s:= scale(Rain)]

#generate predictions
m2.2012[, pred.m2 := predict(object=m1.fit.2012,newdata=m2.2012,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2012$pred.m2)
#delete implossible values
m2.2012 <- m2.2012[pred.m2 > 0.00000000000001 , ]
m2.2012 <- m2.2012[pred.m2 < 500   , ]

saveRDS(m2.2012,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2012.rds")

#test
# describe(m2.2012$pred.m2)
# hist(m2.2012$pred.m2)

#######
#M2 R2
######
# m1.2012[,aodid:= paste(m1.2012$long_aod,m1.2012$lat_aod,sep="-")]
# #merge co located mod1 and mod2 grids
# setkey(m1.2012,aodid,day)
# setkey(m2.2012,aodid,day)
# m.1.2.pred <- merge(m1.2012, m2.2012[, list(aodid, day, pred.m2)], all.x = T)
# mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
# #cleanup and save current stages (workspace)
# summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2012[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2012.rds")
#map the predictions
write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/m2.2012.LTPM.csv")
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.2012.m2.png")
keep(res, rmse, splitdf, sure=TRUE) 
gc()


res
