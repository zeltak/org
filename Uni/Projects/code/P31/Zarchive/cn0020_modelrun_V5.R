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


###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(type=character(40), r2003=numeric(40),
                        r2004=numeric(40),r2005=numeric(40),
                        r2006=numeric(40),r2007=numeric(40),
                        r2008=numeric(40),r2009=numeric(40),
                        r2010=numeric(40),r2011=numeric(40),
                        r2012=numeric(40),mean=numeric(40))

#name columns

mod1table$type<- c("mod1_R2","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
                   "mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE",
                   "mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial",
                   "mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE",
                   "mod1CVLPM_Slope","mod1CVLPM_Slope_SE","mod1CVLPM_RMSPE",
                   "mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial",
                   "mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int",
                   "mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE",
                   "mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
                   "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                   "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")





##########################
#2006
##########################
keep(mod1table,basegrid,splitdf, sure=TRUE) 
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/CV_splits.r")

#import and clip data
mod1<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006.rds")
mod2<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2006.rds")
#clip to just NE and NY/NJ
mod2C <- mod2[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
mod1C <- mod1[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
mod2C[,pblid:=NULL]
mod2C[,ndviid:=NULL]
mod2C[,lat_met:=NULL]
mod2C[,long_met:=NULL]
#remove uneeded
rm(mod1)
rm(mod2)

###############
#MOD1
###############
###############
#switch to choose all area or cliped area for paper
m1_2006<-mod1C
rm(mod1C)
#m1_2006<-mod1
###############

#clean data and exclude bad values
m1_2006$logroad<-log(m1_2006$Mjrrdden_1 +.1)

#################################################################################
#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))
#full model 1
out.m1_2006 = lmer(m1.formula ,data =  m1_2006)
#################################################################################
#generate prediction
m1_2006$predicted <- predict(out.m1_2006)
#get overall R2
mod1_reg <- lm(m1_2006$PM25~m1_2006$predicted)
mod1table$r2006[1] <-summary(mod1_reg)$r.squared
saveRDS(m1_2006, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006_pred.m1.rds")

###############
#MOD1 CV
###############

#s1
splits_s1 <- splitdf(m1_2006)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,REform=NULL )
#s2
splits_s2 <- splitdf(m1_2006)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,REform=NULL )
#s3
splits_s3 <- splitdf(m1_2006)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,REform=NULL )
#s4
splits_s4 <- splitdf(m1_2006)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,REform=NULL )
#s5
splits_s5 <- splitdf(m1_2006)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,REform=NULL )
#s6
splits_s6 <- splitdf(m1_2006)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,REform=NULL )
#s7
splits_s7 <- splitdf(m1_2006)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,REform=NULL )
#s8
splits_s8 <- splitdf(m1_2006)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,REform=NULL )
#s9
splits_s9 <- splitdf(m1_2006)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,REform=NULL )
#s10
splits_s10 <- splitdf(m1_2006)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,REform=NULL )

####BIND ALL 10% into 1 dataset
mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))

# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "mod1d|out_|splits_"))
#get R2 etc
mod1CV_reg <- lm(mod1CV_all$PM25~mod1CV_all$predicted)
mod1table$r2006[2] <-summary(mod1CV_reg)$r.squared #R2
mod1table$r2006[3] <-summary(mod1CV_reg)$coef[1,1] #intercept
mod1table$r2006[4] <-summary(mod1CV_reg)$coef[1,2] #intercept SE
mod1table$r2006[5] <-summary(mod1CV_reg)$coef[2,1] #Slope
mod1table$r2006[6] <-summary(mod1CV_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2006[7]<- sqrt(mean(mod1CV_reg$residual^2))
#spatial
m1CV_agg <- (mod1CV_all[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])	
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2006[8] <- summary(mod1_spatial)$r.squared
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1CV_all,SiteCode)
dat <- merge(mod1CV_all,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2006[9] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2006[10]<- sqrt(mean(dat$spatresid^2))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#import 50x50LU terms
lu2 <-read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
lu <-read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50_MIA.dbf")
lu$guid<-as.factor(lu$guid)
lu<-as.data.table(lu)
lu2<-as.data.table(lu2)
lu[,c("long_pm","lat_pm","reg_name","reg_id","tdenden"):=NULL]
lu2[,c("LONGITUDE","LATITUDE_M","reg_name","reg_id","OID_"):=NULL]
lu2<- rename(lu2, c(SITECODE="SiteCode",guid_="guid")) 
setcolorder(lu2, c("SiteCode","guid","popden","pcturban","tden", "dist_pemis","dist_A1","elev"))
luf<-rbindlist(list(lu, lu2))
#add 50m LU to CV data
setkey(mod1CV_all,SiteCode)
setkey(luf,SiteCode)
mod1d_all_st <- merge(mod1CV_all, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
#create residual PM variable
mod1d_all_st$resm1<-mod1d_all_st$PM25-mod1d_all_st$predicted
#The GAM model
bp.model.ps<-gam(resm1~s(tden,popden)+s(tden,pbl)+s(tden,WDSP)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
#summary(bp.model.ps)
mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted+mod1d_all_st$Predlocm
#regression
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)
mod1table$r2006[11] <-summary(mod1d_reg_st)$r.squared
mod1table$r2006[12] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2006[13] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2006[14] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2006[15] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2006[16]<- sqrt(mean(mod1d_reg_st$residual^2))
#spatial
aggf<- ddply(mod1d_all_st, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])	
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2006[17] <- summary(mod1LPM_spatial)$r.squared
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2006[18] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2006[19]<- sqrt(mean(dat$spatresid^2))

#############saving point
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
#clean
keep(out.m1_2006,m1_2006,mod2C,mod1table, sure=TRUE) 
gc()




###############
#MOD2
###############
###############
#switch to choose all area or cliped area for paper
m2_2006<-mod2C
rm(mod2C)
###############
#m2_2006<-mod2
###############
m2_2006$logroad<-log(m2_2006$Mjrrdden_1 +.1)
#generate predictions
m2_2006[, predicted.m2 := predict(object=out.m1_2006,newdata=m2_2006,allow.new.levels=TRUE,REform=NULL)]
m2_2006 <- m2_2006[predicted.m2 > 0.00000000000001 , ]
#save mod2 with predictions
saveRDS(m2_2006, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2006_pred.m2.rds")
# #shorten data sets
m2_2006s<-m2_2006[,c(1:7,52),with=FALSE]

##get R2
m1<-m1_2006[,c(1,2,3,7,12),with=FALSE]
m2<-m2_2006s
#merge co located mod1 and mod2 grids
setkey(m1,guid,day)
setkey(m2,guid,day)
m12 <- merge(m1, m2[, list(guid, day, predicted.m2)], all.x = T)
str(m12)
mod2_reg<-lm(m12$PM25 ~ m12$predicted.m2)
#cleanup and save current stages (workspace)
mod1table$r2006[20] <-summary(mod2_reg)$r.squared
#map the predictions
#aggregate by guid
m2_agg <- m2_2006[, list(LTPM.m2 = mean(predicted.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = guid]
saveRDS(m2_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2006.rds")
#map the predictions
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM2004.m2.png")

#############saving point
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
#clean
keep(m2_2006s,m1_2006,mod1table, sure=TRUE) 
gc()




##############################
#MeanPM calculations
##############################

mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2006.rds")
mpmg$month <- as.numeric(format(mpmg$day, "%m"))
#create biomon
mpmg[, bimon := (month + 1) %/% 2]
#remove uneeded files and save
#merge with mod2 predicted data to create mod2
setkey(m2_2006s,day, guid)
setkey(mpmg,day, guid)
m2mpm <- merge(m2_2006s, mpmg)
names(m2mpm)
m2mpm[,m:=NULL]


#############saving point
saveRDS(m2mpm, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2mpm_2006.rds")
#clean
keep(m2mpm,m1_2006,mod1table,mpmg, sure=TRUE) 
gc()



###############
#Mod3
###############

#run the lmer part regressing stage 2 pred Vs mean pm
m2.smooth = lme(predicted.m2 ~ meanPMmean*as.factor(bimon),random = list(guid= ~1 + meanPMmean),control=lmeControl(opt = "optim"),data= m2mpm )

#correlate to see everything from mod2 and the mpm works
m2mpm$tpred <- predict(m2.smooth)
mod3a_reg<-lm(m2mpm$predicted~m2mpm$tpred)
mod1table$r2006[21] <-summary(mod3a_reg)$r.squared

saveRDS(m2.smooth, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2.smooth_2006.rds")
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")

#get the residuals from the above fit
m2mpm$resid   <- residuals(m2.smooth)

#split the files to the separate bi monthly datsets
T2006_bimon1 <- subset(m2mpm ,m2mpm$bimon == "1")
T2006_bimon2 <- subset(m2mpm ,m2mpm$bimon == "2")
T2006_bimon3 <- subset(m2mpm ,m2mpm$bimon == "3")
T2006_bimon4 <- subset(m2mpm ,m2mpm$bimon == "4")
T2006_bimon5 <- subset(m2mpm ,m2mpm$bimon == "5")
T2006_bimon6 <- subset(m2mpm ,m2mpm$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon1 )
fit2_2 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon2 )
fit2_3 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon3 )
fit2_4 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon4 )
fit2_5 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon5 )
fit2_6 = gam(resid ~ s(long_aod,lat_aod),  data= T2006_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2006_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2006_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2006_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2006_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2006_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2006_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
m2mpm$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2006  = lme(newpred ~ meanPMmean ,random = list(guid= ~1 + meanPMmean ),control=lmeControl(opt = "optim"),data= m2mpm  )

#check correlations
m2mpm$tpred2 <- predict(Final_pred_2006)

mod3b_reg<-lm(m2mpm$predicted~m2mpm$tpred2)
mod1table$r2006[22] <-summary(mod3b_reg)$r.squared

#############saving point
saveRDS(Final_pred_2006 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2006.rds")
saveRDS(fit2_1 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2006.rds")
saveRDS(fit2_2 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2006.rds")
saveRDS(fit2_3 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2006.rds")
saveRDS(fit2_4 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2006.rds")
saveRDS(fit2_5 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2006.rds")
saveRDS(fit2_6 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2006.rds")

saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")

#clean
keep(Final_pred_2006,m1_2006,mpmg,mod1table,fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6, sure=TRUE) 
gc()


###in case of emergency..break glass...
# mod1table<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
# Final_pred_2006<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2006.rds")
# fit2_1<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2006.rds")
# fit2_2<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2006.rds")
# fit2_3<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2006.rds")
# fit2_4<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2006.rds")
# fit2_5<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2006.rds")
# fit2_6<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2006.rds")


###############
#create full mod 3
###############

################
#### PREDICT for all daily grids for study area (for mixed model part)
###############
#mpmg$mixpred<-  predict(Final_pred_2006,mpmg)
mpmg.seT1<-mpmg[1:20000000,]
mpmg.seT1$mixpred<-  predict(Final_pred_2006,mpmg.seT1)
mpmg.seT2<-mpmg[20000001:40000000,]
mpmg.seT2$mixpred<-  predict(Final_pred_2006,mpmg.seT2)
mpmg.seT3<-mpmg[40000001:60000000,]
mpmg.seT3$mixpred<-  predict(Final_pred_2006,mpmg.seT3)
cc<-dim(mpmg)
mpmg.seT4<-mpmg[60000001:cc[1],]
mpmg.seT4$mixpred<-  predict(Final_pred_2006,mpmg.seT4)
mpmg<-rbind(mpmg.seT1,mpmg.seT2,mpmg.seT3,mpmg.seT4)

################
#### PREDICT Gam part
###############

#split back into bimons to include the gam prediction in final prediction  			

mpmg_bimon1 <- mpmg[bimon == 1, ]
mpmg_bimon2 <- mpmg[bimon == 2, ]
mpmg_bimon3 <- mpmg[bimon == 3, ]
mpmg_bimon4 <- mpmg[bimon == 4, ]
mpmg_bimon5 <- mpmg[bimon == 5, ]
mpmg_bimon6 <- mpmg[bimon == 6, ]

if(!exists("m2_agg")){
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2006.rds")
}

m2_agg[,LTPM.m2:=NULL]

#addin unique grid to each bimon           
uniq_gid_bimon1 <- m2_agg
uniq_gid_bimon2 <- m2_agg
uniq_gid_bimon3 <- m2_agg
uniq_gid_bimon4 <- m2_agg
uniq_gid_bimon5 <- m2_agg
uniq_gid_bimon6 <- m2_agg

#get predictions for Bimon residuals

uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)

#clean
keep(uniq_gid_bimon1,uniq_gid_bimon2,uniq_gid_bimon3,uniq_gid_bimon4,uniq_gid_bimon5,uniq_gid_bimon6,mpmg_bimon1,mpmg_bimon2,mpmg_bimon3,mpmg_bimon4,mpmg_bimon5,mpmg_bimon6, sure=TRUE) 
gc()
#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,guid)
setkey(mpmg_bimon1,guid)
mpmg_bimon1 <- merge(mpmg_bimon1, uniq_gid_bimon1, all.x = T)

setkey(uniq_gid_bimon2,guid)
setkey(mpmg_bimon2,guid)
mpmg_bimon2 <- merge(mpmg_bimon2, uniq_gid_bimon2, all.x = T)

setkey(uniq_gid_bimon3,guid)
setkey(mpmg_bimon3,guid)
mpmg_bimon3 <- merge(mpmg_bimon3, uniq_gid_bimon3, all.x = T)

setkey(uniq_gid_bimon4,guid)
setkey(mpmg_bimon4,guid)
mpmg_bimon4 <- merge(mpmg_bimon4, uniq_gid_bimon4, all.x = T)

setkey(uniq_gid_bimon5,guid)
setkey(mpmg_bimon5,guid)
mpmg_bimon5 <- merge(mpmg_bimon5, uniq_gid_bimon5, all.x = T)

setkey(uniq_gid_bimon6,guid)
setkey(mpmg_bimon6,guid)
mpmg_bimon6 <- merge(mpmg_bimon6, uniq_gid_bimon6, all.x = T)

#reattach all parts        
mod3 <- rbind(mpmg_bimon1,mpmg_bimon2,mpmg_bimon3,mpmg_bimon4,mpmg_bimon5,mpmg_bimon6)
# create PM_mod3
mod3$pm_mod3 <-mod3$mixpred+mod3$gpred
summary(mod3$pm_mod3)
describe(mod3$pm_mod3)
#recode negative into zero
mod3 <- subset(mod3,mod3$pm_mod3 >= "0")
hist(mod3$pm_mod3)
saveRDS(mod3,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m32006.pred3.rds")

#clean
keep(mod3, sure=TRUE) 
gc()

#R2 for mod3
#load mod1
mod1table<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006_pred.m1.rds")
mod1[1,2,3,55]
mod1<-mod1[,c(1,2,3,55),with=FALSE]
setnames(mod1,"predicted","predicted.m1")

# m3<- mod3[mod3$guid %in% mod1$guid, ]        
# m3<-data.table(m3)
setkey(mod3,day,guid)
setkey(mod1,day,guid)
mod1 <- merge(mod1,mod3[, list(day,guid,pm_mod3)], all.x = T)  			
mod3d_reg <- lm(PM25~pm_mod3,mod1)
mod1table$r2006[23] <-summary(mod3d_reg)$r.squared
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")


#import mod2
mod2<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2006_pred.m2.rds")
mod2<-mod2[,c(1,2,52),with=FALSE]

# store the best available
mod3best <- mod3[, list(guid, long_aod, lat_aod, day, pm_mod3)]
setkey(mod3best, day, guid)
setkey(mod2, day, guid)
mod3best <- merge(mod3best, mod2, all.x = T)
setkey(mod3best,day,guid)
setkey(mod1,day,guid)
mod3best <- merge(mod3best, mod1, all.x = T)
head(mod3best,1)
mod3best[,bestpred := pm_mod3]
mod3best[!is.na(predicted.m2),bestpred := predicted.m2]
mod3best[!is.na(predicted.m1),bestpred := predicted.m1]

#map the predictions
#aggregate by guid
m3d_agg <- (mod3[, list(LTPM =mean(pm_mod3, na.rm = TRUE), 
                        long_aod = long_aod[1], #use the first long and lat (by guid)
                        lat_aod = lat_aod[1]),by = guid])

#plot
ggplot(m3d_agg, aes(long_aod, lat_aod, color = LTPM)) + 
  geom_point(size = 5, shape = 15) + 
  #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")

write.csv(m3d_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m3d_agg_2006.csv")

last_plot() %+% mod3best[, list(LTPM = mean(bestpred, na.rm = T), long_aod = long_aod[1], #use the first long and lat (by guid)
                                lat_aod = lat_aod[1]) ,by = guid]  

write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(predicted.m1)),
                          npred.m2 = sum(!is.na(predicted.m2)),
                          npred.m3 = sum(!is.na(pm_mod3)),
                          elev =  elev[1], long_aod =  long_aod[1], lat_aod = lat_aod[1]),by=guid], 
          paste0(path.data, "mod3best_2011summary_", Sys.Date(), ".csv"), row.names = F)


























