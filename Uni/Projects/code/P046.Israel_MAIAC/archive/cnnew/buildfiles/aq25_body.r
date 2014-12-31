#-------------------> Year 2010
#if needed load res table
#res<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ.rds")

### import data
m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.rds")

################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.2010, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.01]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2010[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2010 <- m1.2010[!(m1.2010$badid %in% bad$badid), ] 

#scale data
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


m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WDa.s+WSa.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s +p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                        #+aod*Dust #interactions
                         +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 

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
#m1.2010.cv.loc<-na.omit(m1.2010.cv.loc)

#create residual mp3 variable
m1.2010.cv.loc$res.m1<-m1.2010.cv.loc$PM25-m1.2010.cv.loc$pred.m1.cv

#The GAM model
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2010.cv.loc)
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
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ.2010.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ.rds")
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

saveRDS(m2.2010,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.pred2.rds")



#-------------->prepare for mod3
m2.2010[, bimon := (m + 1) %/% 2]
setkey(m2.2010,day, aodid)
m2.2010<-m2.2010[!is.na(meanPM25)]




#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM25,random = list(aodid= ~1 + meanPM25),control=lmeControl(opt = "optim"), data= m2.2010 )
#xm2.smooth = lmer(pred.m2 ~ meanPM25+(1+ meanPM25|aodid), data= m2.2010 )
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
Final_pred_2010 <- lme(pred.t32 ~ meanPM25 ,random = list(aodid= ~1 + meanPM25 ),control=lmeControl(opt = "optim"),data= m2.2010  )
m2.2010[, pred.t33 := predict(Final_pred_2010)]
#check correlations
res[res$year=="2010", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2010))$r.squared) 


#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2010.rds")

#for PM25
data.m3 <- data.m3[,c(1,2,5,29:32,52,53),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
data.m3<-data.m3[!is.na(meanPM25)]

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
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2010.pred.rds")

#clean
keep(mod3,res,rmse, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn","MeanPbl","WSa","WDa","tempa"),with=FALSE]

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
m1.2010 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.2010<- summary(lm(PM25~pred.m3,data=m1.2010))
res[res$year=="2010", 'm3.R2'] <- print(summary(lm(PM25~pred.m3,data=m1.2010))$r.squared)    
#RMSPE
res[res$year=="2010", 'm3.PE'] <- print(rmse(residuals(m3.fit.2010)))

#spatial
###to check
spatial2010<-m1.2010 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.2010.spat<- lm(barpm ~ barpred, data=spatial2010)
res[res$year=="2010", 'm3.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010))$r.squared)
res[res$year=="2010", 'm3.PE.s'] <- print(rmse(residuals(m1.fit.2010.spat)))
       
#temporal
tempo2010<-left_join(m1.2010,spatial2010)
tempo2010$delpm <-tempo2010$PM25-tempo2010$barpm
tempo2010$delpred <-tempo2010$pred.m3-tempo2010$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2010)
res[res$year=="2010", 'm3.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010))$r.squared)




#############save midpoint
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/res.AQ.2010.rds")
saveRDS(res, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.AQ.rds")

#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2010.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2010.FINAL.rds")

#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ.2010.csv", row.names = F)


#-------->>> loc stage

luf<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/local.csv")
setnames(luf,"tden","loc.tden")
setnames(luf,"elev50","loc.elev")
#rename dataset
m3.2010<-m1.2010
m3.2010<-na.omit(m3.2010)
#add 50m LU to CV data
setkey(m3.2010,stn)
setkey(luf,stn)
m3.2010.loc <- merge(m3.2010, luf, all.x = T)

#create residual mp3 variable
m3.2010.loc$res.m3<-m3.2010.loc$PM25-m3.2010.loc$pred.m3

#The GAM model
gam.out<-gam(res.m3~s(loc.tden)+s(loc.tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m3.2010.loc)
#plot(bp.model.ps)
#summary(bp.model.ps)
## reg
m3.2010.loc$pred.m3.loc <-predict(gam.out)
m3.2010.loc$pred.m3.both <- m3.2010.loc$pred.m3 + m3.2010.loc$pred.m3.loc
res[res$year=="2010", 'm3.loc.R2'] <- print(summary(lm(PM25~pred.m3.both,data=m3.2010.loc))$r.squared)
res[res$year=="2010", 'm3.loc.I'] <-print(summary(lm(PM25~pred.m3.both,data=m3.2010.loc))$coef[1,1])
res[res$year=="2010", 'm3.loc.I.se'] <-print(summary(lm(PM25~pred.m3.both,data=m3.2010.loc))$coef[1,2])
res[res$year=="2010", 'm3.loc.S'] <-print(summary(lm(PM25~pred.m3.both,data=m3.2010.loc))$coef[2,1])
res[res$year=="2010", 'm3.loc.S.se'] <-print(summary(lm(PM25~pred.m3.both,data=m3.2010.loc))$coef[2,2])
#RMSPE
res[res$year=="2010", 'm3.loc.PE'] <- print(rmse(residuals(m3.fit.2010)))

#spatial
spatial2010.loc<-m3.2010.loc %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m3.fit.2010.loc.s <- lm(barpm ~ barpred, data=spatial2010.loc)
res[res$year=="2010", 'm3.loc.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2010.loc))$r.squared)
res[res$year=="2010", 'm3.loc.PE.s'] <- print(rmse(residuals(m3.fit.2010.loc.s)))
       
#temporal
tempo2010.loc<-left_join(m3.2010.loc,spatial2010.loc)
tempo2010.loc$delpm <-tempo2010.loc$PM25-tempo2010.loc$barpm
tempo2010.loc$delpred <-tempo2010.loc$pred.m3.both-tempo2010.loc$barpred
mod_temporal.loc <- lm(delpm ~ delpred, data=tempo2010.loc)
res[res$year=="2010", 'm3.loc.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2010.loc))$r.squared)


keep(res, sure=TRUE) 
gc()


