###############
#LIBS
###############
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(nlme)
library(lme4)


###############
#FUNCTIONS AND TABLES
###############


#function to create CV 90%-10% splits

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}


#create main CV table
mod1table <- data.frame(type=character(40), r2003=numeric(40),r2003=numeric(40),r2005=numeric(40),r2006=numeric(40),r2007=numeric(40),r2008=numeric(40),r2009=numeric(40),r2010=numeric(40),r2011=numeric(40),r2012=numeric(40),mean=numeric(40))

#name columns

mod1table$type<- c("mod1_R2","mod1CV_R2","mod1CV_int","mod1CV_int_SE","mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE","mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial","mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE","mod1CVLPM_Slope","mod1CVLPM_Slope SE","mod1CVLPM_RMSPE","mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial","mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int","mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE","mod3_spatial","mod3_temporal","mod3_RMSPE_spatial","mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope","mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")





#import whole NE_MIA grid
basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")
#import clipped study area
sa <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/guid_study.csv")
# "clip" base grid
basegrid<- basegrid[basegrid$guid %in% sa$guid, ]        




###############
#MOD1
###############

#import mod1 file
m1_2003 <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/mod1_2003.csv") 
m1_2003[, date := as.Date(strptime(DATE, "%d%b%Y"))]
#clean data and exclude bad values
m1_2003 <- m1_2003[aod < 1.4 & NDVI < 1 , ]
m1_2003$logroad<-log(m1_2003$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|date/region))

#full model 1
out.m1_2003 = lmer(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|date/region),data =  m1_2003)
#generate prediction
m1_2003$predicted <- predict(out.m1_2003)
#get overall R2
mod1_reg <- lm(m1_2003$PM25~m1_2003$predicted)

mod1table$r2003[1] <-summary(mod1_reg)$r.squared


###############
#MOD1 CV
###############


#s1
splits_s1 <- splitdf(m1_2003)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,REform=NULL )


#s2
splits_s2 <- splitdf(m1_2003)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,REform=NULL )

#s3
splits_s3 <- splitdf(m1_2003)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,REform=NULL )

#s4
splits_s4 <- splitdf(m1_2003)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,REform=NULL )

#s5
splits_s5 <- splitdf(m1_2003)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,REform=NULL )


#s6
splits_s6 <- splitdf(m1_2003)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,REform=NULL )


#s7
splits_s7 <- splitdf(m1_2003)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,REform=NULL )

#s8
splits_s8 <- splitdf(m1_2003)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,REform=NULL )

#s9
splits_s9 <- splitdf(m1_2003)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,REform=NULL )

#s10
splits_s10 <- splitdf(m1_2003)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,REform=NULL )



####BIND ALL 10% into 1 dataset

mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))

mod1CV_reg <- lm(mod1CV_all$PM25~mod1CV_all$predicted)

mod1table$r2003[2] <-summary(mod1CV_reg)$r.squared #R2
mod1table$r2003[3] <-summary(mod1CV_reg)$coef[1,1] #intercept
mod1table$r2003[4] <-summary(mod1CV_reg)$coef[1,2] #intercept SE
mod1table$r2003[5] <-summary(mod1CV_reg)$coef[2,1] #Slope
mod1table$r2003[6] <-summary(mod1CV_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2003[7]<- sqrt(mean(mod1CV_reg$residual^2))


#spatial
m1CV_agg <- (mod1CV_all[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])	
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2003[8] <- summary(mod1_spatial)$r.squared

#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1CV_all,SiteCode)
dat <- merge(mod1CV_all,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[9] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[10]<- sqrt(mean(dat$spatresid^2))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
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
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm1<-mod1d_all_st$PM25-mod1d_all_st$predicted


#The GAM model
bp.model.ps<-gam(resm1~s(tden,popden)+s(tden,pbl)+s(tden,WDSP)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
#summary(bp.model.ps)
mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2003[11] <-summary(mod1d_reg_st)$r.squared
mod1table$r2003[12] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2003[13] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2003[14] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2003[15] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2003[16]<- sqrt(mean(mod1d_reg_st$residual^2))

#spatial
aggf<- ddply(mod1d_all_st, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))

#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])	
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2003[17] <- summary(mod1LPM_spatial)$r.squared

#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[18] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[19]<- sqrt(mean(dat$spatresid^2))




###############
#MOD2
###############

#import mod2 files
m2_2003 <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/mod2_2003.csv")

m2_2003[, date := as.Date(strptime(DATE, "%d%b%Y"))]
#clean data and exclude bad values
m2_2003 <- m2_2003[aod < 1.4 & NDVI < 1 , ]
m2_2003$logroad<-log(m2_2003$Mjrrdden_1 +.1)
#generate predictions
m2_2003$predicted <- predict(object=out.m1_2003,newdata=m2_2003,allow.new.levels=TRUE,REform=NULL )
m2_2003 <- m2_2003[predicted > 0.00000000000001 , ]

#shorten data sets
m1<-m1_2003[,c(71,6,44,36),with=FALSE]
m2<-m2_2003[,c(4,42,40),with=FALSE]
#merge co located mod1 and mod2 grids
setkey(m1,guid,date)
setkey(m2,guid,date)
mx <- merge(m1, m2)
na.omit(mx)
#calc R2
mod2_reg<-lm(mx$PM25~mx$predicted)
#cleanup and save current stages (workspace)
mod1table$r2003[20] <-summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
# m2_agg <- (m2[, mean(predicted, na.rm = TRUE),by = guid])		
# #add lat-long
# setkey(basegrid,guid)
# setkey(m2_agg,guid)
# m2_agg <- merge(m2_agg,basegrid)
# write.csv(m2_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/LTPM_M2_2003.csv")

# rm(m1)
# rm(m2)
# save.image()

###############
#MOD3
###############

#add guid to lat-long to mpm data and clean mpmg data
#import mean PM per grid cell data (mpm)
mpm <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/final_100kmet2003.csv")
setkey(basegrid,long_aod,lat_aod)
setkey(mpm,long_aod,lat_aod)
mpmg <- merge(mpm,basegrid)
mpmg<- mpmg[mpmg$guid %in% sa$guid, ]   
mpmg[, date := as.Date(strptime(date, "%d%b%Y"))]
mpmg$m <- as.numeric(format(mpmg$date, "%m"))
#recode data
mpmg$bimon[mpmg$m==1] <- 1
mpmg$bimon[mpmg$m==2] <- 1
mpmg$bimon[mpmg$m==3] <- 2
mpmg$bimon[mpmg$m==4] <- 2
mpmg$bimon[mpmg$m==5] <- 3
mpmg$bimon[mpmg$m==6] <- 3
mpmg$bimon[mpmg$m==7] <- 4
mpmg$bimon[mpmg$m==8] <- 4
mpmg$bimon[mpmg$m==9] <- 5
mpmg$bimon[mpmg$m==10] <- 5
mpmg$bimon[mpmg$m==11] <- 6
mpmg$bimon[mpmg$m==12] <- 6


#merge with mod2 predicted data
setkey(m2_2003,date, guid)
setkey(mpmg,date, guid)
GAM_T2003 <- merge(m2_2003, mpmg)
#make short dataset
GAM_T2003x<-GAM_T2003[,c(1,2,40,44,43,42,45,47),with=FALSE]
# Recode using car recode
#remove uneeded files and save
rm(mpm)
rm(GAM_T2003)
save.image()


#run the lmer part regressing stage 2 pred Vs mean pm
#smooth_T2003_yearly = lmer(predicted ~ meanpm100k*as.factor(bimon)+ (1 +meanpm100k|guid),data= GAM_T2003x )
smooth_T2003_yearly = lme(predicted ~ meanpm100k*as.factor(bimon),random = list(guid= ~1 + meanpm100k),control=lmeControl(opt = "optim"),data= GAM_T2003x )

#correlate to see everything from mod2 and the mpm works
GAM_T2003x$tpred <- predict(smooth_T2003_yearly)
mod3a_reg<-lm(GAM_T2003x$predicted~GAM_T2003x$tpred)
mod1table$r2003[21] <-summary(mod3a_reg)$r.squared


#map the predictions
# #aggregate by guid
# m3a_agg <- (GAM_T2003x[, mean(tpred, na.rm = TRUE),by = guid])		
# #add lat-long
# setkey(basegrid,guid)
# setkey(m3a_agg,guid)
# m3a_agg <- merge(m3a_agg,basegrid)
# write.csv(m3a_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/LTPM_M3a_2003.csv")
				
				

#get the residuals from the above fit
GAM_T2003x$resid   <- residuals(smooth_T2003_yearly)

#split the files to the separate bi monthly datsets
T2003_bimon1 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "1")
T2003_bimon2 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "2")
T2003_bimon3 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "3")
T2003_bimon4 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "4")
T2003_bimon5 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "5")
T2003_bimon6 <- subset(GAM_T2003x ,GAM_T2003x$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon1 )
fit2_2 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon2 )
fit2_3 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon3 )
fit2_4 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon4 )
fit2_5 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon5 )
fit2_6 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2003_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2003_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2003_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2003_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2003_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2003_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2003x$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2003  = lme(newpred ~ meanpm100k ,random = list(guid= ~1 + meanpm100k ),control=lmeControl(opt = "optim"),data= GAM_T2003x  )

#check correlations
GAM_T2003x$tpred2 <- predict(Final_pred_2003)

mod3b_reg<-lm(GAM_T2003x$predicted~GAM_T2003x$tpred2)
mod1table$r2003[22] <-summary(mod3b_reg)$r.squared


#map the predictions
#aggregate by guid
# m3b_agg <- (GAM_T2003x[, mean(tpred2, na.rm = TRUE),by = guid])		
# #add lat-long
# setkey(basegrid,guid)
# setkey(m3b_agg,guid)
# m3b_agg <- merge(m3b_agg,basegrid)
# write.csv(m3b_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/LTPM_M3b_2003.csv")
# 				
# 

#save data midpoint
save.image()



###############
#create full mod 3
###############


				
####import all xy points across new-england  
#mpmg$mixpred<-  predict(Final_pred_2003,mpmg,level=0)
mpmg$mixpred<-  predict(Final_pred_2003,mpmg)

#map the predictions
#aggregate by guid
# m3c_agg <- (mpmg[, mean(mixpred, na.rm = TRUE),by = guid])		
# #add lat-long
# setkey(basegrid,guid)
# setkey(m3c_agg,guid)
# m3c_agg <- merge(m3c_agg,basegrid)
# write.csv(m3c_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/LTPM_M3c_2003.csv")


#split back into bimons to include the gam prediction in final prediction				
mpmg_bimon1 <- subset(mpmg ,mpmg$bimon == "1")
mpmg_bimon2 <- subset(mpmg ,mpmg$bimon == "2")
mpmg_bimon3 <- subset(mpmg ,mpmg$bimon == "3")
mpmg_bimon4 <- subset(mpmg ,mpmg$bimon == "4")
mpmg_bimon5 <- subset(mpmg ,mpmg$bimon == "5")
mpmg_bimon6 <- subset(mpmg ,mpmg$bimon == "6")
              
#addin unique grid to each bimon           
uniq_gid_bimon1 <- basegrid
uniq_gid_bimon2 <- basegrid
uniq_gid_bimon3 <- basegrid
uniq_gid_bimon4 <- basegrid
uniq_gid_bimon5 <- basegrid
uniq_gid_bimon6 <- basegrid
        
#get predictions for Bimon residuals
        
bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred
        
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred

bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred
        
bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred
        
bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred
        
bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred
        
        
#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
        
uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
mpmg_bimon1 <- mpmg_bimon1[order(mpmg_bimon1$guid),] 
mpmg_bimon1_merged <- merge(mpmg_bimon1,uniq_gid_bimon1,by="guid")
        
uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
mpmg_bimon2 <- mpmg_bimon2[order(mpmg_bimon2$guid),] 
mpmg_bimon2_merged <- merge(mpmg_bimon2,uniq_gid_bimon2,by="guid")
       
uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
mpmg_bimon3 <- mpmg_bimon3[order(mpmg_bimon3$guid),] 
mpmg_bimon3_merged <- merge(mpmg_bimon3,uniq_gid_bimon3,by="guid")
 
uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
mpmg_bimon4 <- mpmg_bimon4[order(mpmg_bimon4$guid),] 
mpmg_bimon4_merged <- merge(mpmg_bimon4,uniq_gid_bimon4,by="guid")
       
uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
mpmg_bimon5 <- mpmg_bimon5[order(mpmg_bimon5$guid),] 
mpmg_bimon5_merged <- merge(mpmg_bimon5,uniq_gid_bimon5,by="guid")
       
uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
mpmg_bimon6 <- mpmg_bimon6[order(mpmg_bimon6$guid),] 
mpmg_bimon6_merged <- merge(mpmg_bimon6,uniq_gid_bimon6,by="guid")
       
#reattach all parts        
T2003allbimon <- rbind(mpmg_bimon1_merged,mpmg_bimon2_merged,mpmg_bimon3_merged,mpmg_bimon4_merged,mpmg_bimon5_merged,mpmg_bimon6_merged)
        
# create PM_mod3
T2003allbimon$pm_mod3 <-T2003allbimon$mixpred+T2003allbimon$gpred
summary(T2003allbimon$pm_mod3)
describe(T2003allbimon$pm_mod3)
       
# T2003allbimon <- subset(T2003allbimon,T2003allbimon$pm_mod3 >= "0")
hist(T2003allbimon$pm_mod3)
save(T2003allbimon,file="/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/T2003pred3.RData")

#recode negative into zero





#R2 for mod3
m3<- T2003allbimon[T2003allbimon$guid %in% m1_2003$guid, ]        
m3<-data.table(m3)
setkey(m3,date,guid)
setkey(m1_2003,date,guid)
corm3 <- merge(m1_2003,m3)				
mod3d_reg <- lm(corm3$PM25~corm3$pm_mod3)

mod1table$r2003[23] <-summary(mod3d_reg)$r.squared

#map the predictions
#aggregate by guid
m3d_agg <- (T2003allbimon[, mean(pm_mod3, na.rm = TRUE),by = guid])	
# Rename column
setnames(m3d_agg,"V1","LTPM")
#add lat-long
setkey(basegrid,guid)
setkey(m3d_agg,guid)
m3d_agg <- merge(m3d_agg,basegrid)
write.csv(m3d_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/LTPM_M3d_2003.csv")
				

				
				
write.dbf(T2003allbimon,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/poll_T2003.dbf") 
        
				
