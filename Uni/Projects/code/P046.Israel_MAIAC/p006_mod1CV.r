###############
#LIBS
###############
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
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
mod1table <- data.frame(type=character(40), rallyears=numeric(40),
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
#2003
##########################


source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/CV_splits.r")


###############
#DATA
###############

###############
#MOD1
###############
pm10.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all_reg.RDS")  
summary(pm10.m1)

#clean data and exclude bad values
#pm10.m1$logroad<-log(pm10.m1$Mjrrdden_1 +.1)
#pm10.m1$regf<-as.character(pm10.m1$reg_num)

##tera
pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$PM10),]
pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$aod),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$Temp),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$RH),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$WS),]




#base model for stage 1
#m1.formula <- as.formula(PM10~ aod+(1+aod|day)) #0.80
#m1.formula <- as.formula(PM10~ aod+Dust+WS+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))
m1.formula <- as.formula(PM10~ aod+Dust+elev+tden+pden+dist2rail+dist2A1+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))


  
#full model 1
out.pm10.m1 = lmer(m1.formula ,data =  pm10.m1,na.action = na.exclude)
#generate prediction
pm10.m1$predicted <- predict(out.pm10.m1)
#get overall R2
mod1_reg <- lm(pm10.m1$PM10~pm10.m1$predicted)
mod1table$rallyears[1] <-summary(mod1_reg)$r.squared

###############
#MOD1 CV
###############

#s1
splits_s1 <- splitdf(pm10.m1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1,na.action = na.exclude)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,re.form=NULL  )


#s2
splits_s2 <- splitdf(pm10.m1)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2,na.action = na.exclude)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,re.form=NULL )

#s3
splits_s3 <- splitdf(pm10.m1)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3,na.action = na.exclude)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,re.form=NULL )

#s4
splits_s4 <- splitdf(pm10.m1)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4,na.action = na.exclude)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,re.form=NULL )

#s5
splits_s5 <- splitdf(pm10.m1)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5,na.action = na.exclude)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,re.form=NULL )


#s6
splits_s6 <- splitdf(pm10.m1)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6,na.action = na.exclude)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,re.form=NULL )


#s7
splits_s7 <- splitdf(pm10.m1)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7,na.action = na.exclude)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,re.form=NULL )

#s8
splits_s8 <- splitdf(pm10.m1)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8,na.action = na.exclude)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,re.form=NULL )

#s9
splits_s9 <- splitdf(pm10.m1)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9,na.action = na.exclude)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,re.form=NULL )

#s10
splits_s10 <- splitdf(pm10.m1)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10,na.action = na.exclude)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,re.form=NULL )



####BIND ALL 10% into 1 dataset

mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))

# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "mod1d|out_|splits_"))

mod1CV_reg <- lm(mod1CV_all$PM10~mod1CV_all$predicted)
mod1table$rallyears[2] <-summary(mod1CV_reg)$r.squared #R2
mod1table$rallyears[3] <-summary(mod1CV_reg)$coef[1,1] #intercept
mod1table$rallyears[4] <-summary(mod1CV_reg)$coef[1,2] #intercept SE
mod1table$rallyears[5] <-summary(mod1CV_reg)$coef[2,1] #Slope
mod1table$rallyears[6] <-summary(mod1CV_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$rallyears[7]<- sqrt(mean(mod1CV_reg$residual^2))

#spatial
m1CV_agg <- (mod1CV_all[, j=list(mean(PM10, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = stn])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$rallyears[8] <- summary(mod1_spatial)$r.squared

#temporal
setkey(m1CV_agg ,stn)
setkey(mod1CV_all,stn)
dat <- merge(mod1CV_all,m1CV_agg, all.x = T)
dat$delpm <-dat$PM10-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$rallyears[9] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$rallyears[10]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/luf_NE_MIA.rds")

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

mod1table$rallyears[11] <-summary(mod1d_reg_st)$r.squared
mod1table$rallyears[12] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$rallyears[13] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$rallyears[14] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$rallyears[15] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$rallyears[16]<- sqrt(mean(mod1d_reg_st$residual^2))

#spatial
aggf<- ddply(mod1d_all_st, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))

#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$rallyears[17] <- summary(mod1LPM_spatial)$r.squared

#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$rallyears[18] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$rallyears[19]<- sqrt(mean(dat$spatresid^2))


#############save midpoint
saveRDS(mod1table, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p1.rds")

###############
#MOD2
###############

###############
#switch to choose all area or cliped area for paper
m2_2003<-mod2C
#m2_2003<-mod2
###############
m2_2003$logroad<-log(m2_2003$Mjrrdden_1 +.1)

#generate predictions
#m2_2003$predicted <- predict(object=out.pm10.m1,newdata=m2_2003,allow.new.levels=TRUE,re.form=NULL )
m2_2003[, predicted.m2 := predict(object=out.pm10.m1,newdata=m2_2003,allow.new.levels=TRUE,re.form=NULL)]
m2_2003 <- m2_2003[predicted.m2 > 0.00000000000001 , ]
#save mod2 with predictions
saveRDS(m2_2003, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003_pred.m2.rds")

#######
#M2 R2
######
#shorten data sets
names(pm10.m1)
names(m2_2003)
m1<-pm10.m1[,c(1,2,3,7,12),with=FALSE]
m2<-m2_2003[,c(1,2,56),with=FALSE]

#merge co located mod1 and mod2 grids
setkey(pm10.m1,guid,day)
setkey(m2_2003,guid,day)
m.1.2.pred <- merge(pm10.m1, m2_2003[, list(guid, day, predicted.m2)], all.x = T)
mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$predicted.m2)
#cleanup and save current stages (workspace)
mod1table$rallyears[20] <-summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2_2003[, list(LTPM.m2 = mean(predicted.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = guid]
saveRDS(m2_agg, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
#map the predictions
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.m2.png")

saveRDS(mod1table, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")

keep(mod1table , sure=TRUE) 
gc()





