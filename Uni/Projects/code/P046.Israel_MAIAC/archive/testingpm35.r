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
res$year <- c(2004:2011); 



### import data
m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.rds")
#bad aod?
#m1.2004<-m1.2004[ aod < 0.3 ] 
### subset to aqua and apply alexei cleaning methods
#MaskAdjacency == "000"
#m1.2004<-m1.2004[ UN > 0 & UN < 0.04]





################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.2004, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
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

ugrid <-m1.2004 %>%
    group_by(stn) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE))



setkey(raWDaf,stn)
setkey(ugrid,stn)
raw2 <- merge(raWDaf, ugrid, by.x = "stn")
write.csv(raw2,"/home/zeltak/ZH_tmp/rawdf2004.csv")



#check aod
m2.2004<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.rds")



aodagg <-m2.2004 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE), meanaod=mean(aod,na.rm=TRUE))

write.csv(aodagg,"/home/zeltak/ZH_tmp/aodagg2004.csv")


################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.2004, c("stn","m"), 
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
m1.2004[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.2004 <- m1.2004[!(m1.2004$badid %in% bad$badid), ] 


################# region
raWDaf <- ddply(m1.2004, c("metreg"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,season,sep="-")]
#################BAD STN
m1.2004[,badid := paste(stn,season,sep="-")]
####Take out bad stations
m1.2004 <- m1.2004[!(m1.2004$badid %in% bad$badid), ] 



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


m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WDa.s+WSa.s+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s #+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         #+aod*Dust #interactions
                         +(1+aod|day/reg_num)+(1|stn)) #0.812

#full fit
m1.fit.2004 <-  lmer(m1.formula,data=m1.2004,weights=normwt)
m1.2004$pred.m1 <- predict(m1.fit.2004)
 print(summary(lm(PM25~pred.m1,data=m1.2004))$r.squared)
#RMSPE
print(rmse(residuals(m1.fit.2004)))

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
gam.out<-gam(res.m1~s(loc.tden)+s(tden,MeanPbl)+s(loc.tden,WSa)+s(loc_p_os,fx=FALSE,k=4,bs='cr')+s(loc.elev,fx=FALSE,k=4,bs='cr')+s(dA1,fx=FALSE,k=4,bs='cr')+s(dsea,fx=FALSE,k=4,bs='cr'),data=m1.2004.cv.loc)
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
m2.2004[,tempa.s:= scale(tempa)]
m2.2004[,WDa.s:= scale(WDa)]
m2.2004[,WSa.s:= scale(WSa)]
m2.2004[,RHa.s:= scale(RHa)]
m2.2004[,Raina.s:= scale(Raina)]
m2.2004[,NO2a.s:= scale(NO2a)]



#generate predictions
m2.2004[, pred.m2 := predict(object=m1.fit.2004,newdata=m2.2004,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2004$pred.m2)
#delete implossible values
m2.2004 <- m2.2004[pred.m2 > 0.00000000000001 , ]
m2.2004 <- m2.2004[pred.m2 < 500   , ]

saveRDS(m2.2004,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2004.pred2.rds")

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
