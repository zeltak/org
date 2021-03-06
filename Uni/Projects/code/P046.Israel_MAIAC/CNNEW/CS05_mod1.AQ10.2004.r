
###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)


#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")


#-------------------->> RES TABLE
res <- matrix(nrow=9, ncol=45)
res <- data.frame(res)
colnames(res) <- c(
          "year"
          ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
          ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"m2.R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
          ,"XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX"    )


res$year <- c(2003:2011) 



### import data
m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.rds")

#subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
m1.2004<-m1.2004[ UN > 0 & UN < 0.04  ] 

################# clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(m1.2004, c( "stn"), 
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
m1.2004[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2004 <- m1.2004[!(m1.2004$badid %in% bad$badid), ] 

#get rid of missing
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
m1.2004[,tempa.s:= scale(tempa)]
m1.2004[,WDa.s:= scale(WDa)]
m1.2004[,WSa.s:= scale(WSa)]
m1.2004[,RHa.s:= scale(RHa)]
m1.2004[,Raina.s:= scale(Raina)]
m1.2004[,NO2a.s:= scale(NO2a)]



m1.formula <- as.formula(PM10~ aod
                        +tempa.s+WDa.s+WSa.s+Dust+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

#full fit
m1.fit.2004 <-  lmer(m1.formula,data=m1.2004,weights=normwt)
m1.2004$pred.m1 <- predict(m1.fit.2004)
res[res$year=="2004", 'm1.R2'] <- print(summary(lm(PM10~pred.m1,data=m1.2004))$r.squared)
#RMSPE
res[res$year=="2004", 'm1.PE'] <- print(rmse(residuals(m1.fit.2004)))

#spatial
###to check
spatial2004<-m1.2004 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.spat<- lm(barpm ~ barpred, data=spatial2004)
res[res$year=="2004", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004))$r.squared)
res[res$year=="2004", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2004.spat)))
       
#temporal
tempo2004<-left_join(m1.2004,spatial2004)
tempo2004$delpm <-tempo2004$PM10-tempo2004$barpm
tempo2004$delpred <-tempo2004$pred.m1-tempo2004$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2004)
res[res$year=="2004", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004))$r.squared)
saveRDS(m1.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")



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
m1.fit.2004.cv<-lm(PM10~pred.m1.cv,data=m1.2004.cv)
res[res$year=="2004", 'm1cv.R2'] <- print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$r.squared)
res[res$year=="2004", 'm1cv.I'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[1,1])
res[res$year=="2004", 'm1cv.I.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[1,2])
res[res$year=="2004", 'm1cv.S'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[2,1])
res[res$year=="2004", 'm1cv.S.se'] <-print(summary(lm(PM10~pred.m1.cv,data=m1.2004.cv))$coef[2,2])
#RMSPE
res[res$year=="2004", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

#spatial
spatial2004.cv<-m1.2004.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.cv.s <- lm(barpm ~ barpred, data=spatial2004.cv)
res[res$year=="2004", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv))$r.squared)
res[res$year=="2004", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.s)))
       
#temporal
tempo2004.cv<-left_join(m1.2004.cv,spatial2004.cv)
tempo2004.cv$delpm <-tempo2004.cv$PM10-tempo2004.cv$barpm
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
#m1.2004.cv.loc<-na.omit(m1.2004.cv.loc)

#create residual mp3 variable
m1.2004.cv.loc$res.m1<-m1.2004.cv.loc$PM10-m1.2004.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2004.cv.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m1.2004.cv.loc$pred.m1.loc <-predict(gam.out)
m1.2004.cv.loc$pred.m1.both <- m1.2004.cv.loc$pred.m1.cv + m1.2004.cv.loc$pred.m1.loc
res[res$year=="2004", 'm1cv.loc.R2'] <- print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$r.squared)
res[res$year=="2004", 'm1cv.loc.I'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[1,1])
res[res$year=="2004", 'm1cv.loc.I.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[1,2])
res[res$year=="2004", 'm1cv.loc.S'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[2,1])
res[res$year=="2004", 'm1cv.loc.S.se'] <-print(summary(lm(PM10~pred.m1.both,data=m1.2004.cv.loc))$coef[2,2])
#RMSPE
res[res$year=="2004", 'm1cv.loc.PE'] <- print(rmse(residuals(m1.fit.2004.cv)))

#spatial
spatial2004.cv.loc<-m1.2004.cv.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2004.cv.loc.s <- lm(barpm ~ barpred, data=spatial2004.cv.loc)
res[res$year=="2004", 'm1cv.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004.cv.loc))$r.squared)
res[res$year=="2004", 'm1cv.loc.PE.s'] <- print(rmse(residuals(m1.fit.2004.cv.loc.s)))
       
#temporal
tempo2004.loc.cv<-left_join(m1.2004.cv.loc,spatial2004.cv.loc)
tempo2004.loc.cv$delpm <-tempo2004.loc.cv$PM10-tempo2004.loc.cv$barpm
tempo2004.loc.cv$delpred <-tempo2004.loc.cv$pred.m1.both-tempo2004.loc.cv$barpred
mod_temporal.loc.cv <- lm(delpm ~ delpred, data=tempo2004.loc.cv)
res[res$year=="2004", 'm1cv.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004.loc.cv))$r.squared)

#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2004.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")
saveRDS(m1.2004.cv.loc,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.predCV.rds")


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
m2.2004[,tempa.s:= scale(tempa)]
m2.2004[,WDa.s:= scale(WDa)]
m2.2004[,WSa.s:= scale(WSa)]
m2.2004[,RHa.s:= scale(RHa)]
m2.2004[,Raina.s:= scale(Raina)]
m2.2004[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2004[, pred.m2 := predict(object=m1.fit.2004,newdata=m2.2004,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2004$pred.m2)
#delete implossible valuesOA[24~
m2.2004 <- m2.2004[pred.m2 > 0.00000000000001 , ]
m2.2004 <- m2.2004[pred.m2 < 1500   , ]

saveRDS(m2.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2004.pred2.rds")

#map the predictions
#aggregate by guid
## m2_agg <- m2.2004[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
## saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2004.rds")
#map the predictions
## write.csv(m2_agg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM2.AQ10.2004.csv")
## ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
##   geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
##   scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
## ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM2.AQ10.2004.png")

#prepare for mod3
m2.2004[, bimon := (m + 1) %/% 2]
setkey(m2.2004,day, aodid)
m2.2004<-m2.2004[!is.na(meanPM10)]



#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM10,random = list(aodid= ~1 + meanPM10),control=lmeControl(opt = "optim"), data= m2.2004 )
#xm2.smooth = lmer(pred.m2 ~ meanPM10+(1+ meanPM10|aodid), data= m2.2004 )
#correlate to see everything from mod2 and the mpm works
m2.2004[, pred.t31 := predict(m2.smooth)]
m2.2004[, resid  := residuals(m2.smooth)]
res[res$year=="2004", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2004))$r.squared)


#split the files to the separate bi monthly datsets
T2004_bimon1 <- subset(m2.2004 ,m2.2004$bimon == "1")
T2004_bimon2 <- subset(m2.2004 ,m2.2004$bimon == "2")
T2004_bimon3 <- subset(m2.2004 ,m2.2004$bimon == "3")
T2004_bimon4 <- subset(m2.2004 ,m2.2004$bimon == "4")
T2004_bimon5 <- subset(m2.2004 ,m2.2004$bimon == "5")
T2004_bimon6 <- subset(m2.2004 ,m2.2004$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2004_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2004_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2004_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2004_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2004_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2004_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2004_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2004$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2004,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2004 <- lme(pred.t32 ~ meanPM10 ,random = list(aodid= ~1 + meanPM10 ),control=lmeControl(opt = "optim"),data= m2.2004  )
m2.2004[, pred.t33 := predict(Final_pred_2004)]
#check correlations
res[res$year=="2004", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2004))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2004.rds")

#for PM10
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM10)]

#generate m.3 initial pred
data.m3$pred.m3.mix <-  predict(Final_pred_2004,data.m3)

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
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2004.pred.rds")

#clean
keep(mod3,res, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM10","pred.m1","stn"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2004 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2004<- summary(lm(PM10~pred.m3,data=m1.2004))
res[res$year=="2004", 'm3.R2'] <- print(summary(lm(PM10~pred.m3,data=m1.2004))$r.squared)    
#RMSPE
res[res$year=="2004", 'm3.PE'] <- print(rmse(residuals(m3.fit.2004)))

#spatial
###to check
spatial2004<-m1.2004 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2004.spat<- lm(barpm ~ barpred, data=spatial2004)
res[res$year=="2004", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2004))$r.squared)
res[res$year=="2004", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2004.spat)))
       
#temporal
tempo2004<-left_join(m1.2004,spatial2004)
tempo2004$delpm <-tempo2004$PM10-tempo2004$barpm
tempo2004$delpred <-tempo2004$pred.m3-tempo2004$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2004)
res[res$year=="2004", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2004))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ10.2004.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ10.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ10.2004.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ10.2004.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM10","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ10.2004.FINAL.rds")
#map the predictions
#aggregate by aodid
## m3d_agg <- (mod3[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
##                         x_aod_ITM = x_aod_ITM[1], #use the first long and lat (by aodid)
##                         y_aod_ITM = y_aod_ITM[1]),by = aodid])

## # plot
## ggplot(m3d_agg, aes(x_aod_ITM, y_aod_ITM, color = LTPM)) + 
##   geom_point(size = 3, shape = 15) + 
##   #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
##   xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
##   scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(15)) + #c("purple", "blue", "white", "red", "orange")) + 
##   theme_bw() + 
##   ggtitle("Long term predictions")

## ggsave(file="/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2004.png")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2004.csv", row.names = F)

keep(res, sure=TRUE) 
c()

