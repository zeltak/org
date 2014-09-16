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

####GAM formula

m4.formula<-as.formula(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl)+s(tden,WDSP)+s(tden,tempc)+ah_gm3+s(tden,visib))

####################################3
#2003
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2003w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2003[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2003[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2003[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2003[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2003[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2003[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2003[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2003[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2003.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2003<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2003,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2003, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")

#The GAM model
#normal
                   
bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2003[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2003[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2003[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2003[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2003[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2003[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2003[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p4.rds")


####################################3
#2004
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2004_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2004w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2004[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2004[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2004[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2004[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2004[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2004[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2004[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2004[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2004[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2004[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2004.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2004<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2004,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2004, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2004_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2004[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2004[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2004[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2004[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2004[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2004[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2004[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2004[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2004[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2004_p4.rds")



####################################3
#2005
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2005_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2005w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2005[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2005[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2005[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2005[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2005[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2005[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2005[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2005[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2005[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2005[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2005.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2005<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2005,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2005, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2005_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2005[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2005[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2005[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2005[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2005[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2005[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2005[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2005[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2005[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2005_p4.rds")



####################################3
#2006
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2006_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2006w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2006[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2006[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2006[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2006[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2006[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2006[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2006[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2006[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2006[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2006[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2006.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2006<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2006,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2006, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2006_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2006[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2006[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2006[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2006[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2006[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2006[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2006[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2006[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2006[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2006_p4.rds")


####################################3
#2007
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2007w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2007[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2007[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2007[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2007[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2007[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2007[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2007[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2007[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2007[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2007[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2007.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2007<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2007,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2007, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2007_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2007[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2007[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2007[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2007[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2007[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2007[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2007[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2007[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2007[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p4.rds")



####################################3
#2008
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2008_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2008w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2008[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2008[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2008[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2008[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2008[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2008[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2008[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2008[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2008[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2008[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2008.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2008<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2008,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2008, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2008_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2008[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2008[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2008[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2008[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2008[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2008[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2008[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2008[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2008[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2008_p4.rds")



####################################3
#2009
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2009_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2009w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2009[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2009[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2009[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2009[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2009[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2009[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2009[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2009[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2009[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2009[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2009.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2009<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2009,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2009, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2009_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2009[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2009[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2009[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2009[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2009[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2009[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2009[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2009[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2009[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2009_p4.rds")



####################################3
#2010
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2010_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2010w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2010[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2010[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2010[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2010[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2010[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2010[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2010[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2010[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2010[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2010[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2010.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2010<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2010,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2010, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2010_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2010[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2010[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2010[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2010[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2010[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2010[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2010[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2010[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2010[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2010_p4.rds")


####################################3
#2011
####################################3

mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2011_p3.rds.rds")
###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2011w_p.m3.rds")



#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2011[23] <-summary(mod3d_reg)$r.squared #R2
mod1table$r2011[24] <-summary(mod3d_reg)$coef[1,1] #intercept
mod1table$r2011[25] <-summary(mod3d_reg)$coef[1,2] #intercept SE
mod1table$r2011[26] <-summary(mod3d_reg)$coef[2,1] #Slope
mod1table$r2011[27] <-summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2011[28]<- sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
mlist<-(mod1$SiteCode)
mod1.cb <- mod1[!SiteCode %in% sample(mlist, 5), ]
mod3d_reg.cb <- lm(PM25~predicted.m3,data=mod1.cb)
summary(mod3d_reg.cb)
mod1table[41, ] <- 0
mod1table$type[41]<-"m3_slope_l5out"
mod1table$r2011[41]<- summary(mod3d_reg.cb)$coef[2,1] #Slope

###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2011[29] <- summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2011[30] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2011[31]<- sqrt(mean(dat$spatresid^2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>ADD LOCAL PM STAGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
luf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN025_LPM_ST_Calc/lpmST2011.rds")
#shorten mod1
l=seq(names(mod1));names(l)=names(mod1);l
mod1_2011<-mod1

###############################
#add 200m LU to CV data
setkey(mod1_2011,SiteCode,day)
setkey(luf,SiteCode,day)
mod1d_all_st <- merge(mod1_2011, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
mod1d_all_st[,guid.y:=NULL]
setnames(mod1d_all_st,"guid.x","guid")
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm3<-mod1d_all_st$PM25-mod1d_all_st$predicted.m3
names(mod1d_all_st)

saveRDS(mod1d_all_st,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2011_st.rds")

#The GAM model
#normal

bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
summary(bp.model.ps)#0.118

mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2011[32] <-summary(mod1d_reg_st)$r.squared
mod1table$r2011[33] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2011[34] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2011[35] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2011[36] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2011[37]<- sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2011[38] <- summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2011[39] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2011[40]<- sqrt(mean(dat$spatresid^2))

####################
saveRDS(mod1table,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2011_p4.rds")









