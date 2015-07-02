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

#mod 3 data
mod3 <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/FN008_model_prep/m3_2003.pred3.rds")
#clean
keep(mod3, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
mod3[,lat_aod.y:=NULL]
mod3[,long_aod.y:=NULL]
setnames(mod3,"lat_aod.x","lat_aod")
setnames(mod3,"long_aod.x","long_aod")
#subset mod3
mod3<-mod3[,c("guid","day","lat_aod","long_aod","predicted.m3"),with=FALSE]

#load mod1
mod1table<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/FN000_RWORKDIR/mod1table2003_p2.rds")
mod1<- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("guid","day","PM25","predicted","SiteCode"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")
#R2.m3
setkey(mod3,day,guid)
setkey(mod1,day,guid)
mod1 <- merge(mod1,mod3[, list(day,guid,predicted.m3)], all.x = T)    		
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
summary(mod3d_reg)$r.squared 
#0.8980116

####GAM formula
m4.formula<-as.formula(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl)+s(tden,WDSP)+s(tden,tempc)+ah_gm3+s(tden,visib))

####################################
#2003
####################################

###############################
mod1<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2003w_p.m3.rds")
head(mod1)


#R2
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
summary(mod3d_reg)$r.squared #R2
summary(mod3d_reg)$coef[1,1] #intercept
summary(mod3d_reg)$coef[1,2] #intercept SE
summary(mod3d_reg)$coef[2,1] #Slope
summary(mod3d_reg)$coef[2,2] #Slope SE
#rmspe
sqrt(mean(mod3d_reg$residual^2))

####################################
#check leave 5 out monitors and see how this affects slope
#####
###############################
#spatial
m1CV_agg <- (mod1[, j=list(mean(PM25, na.rm = TRUE),mean(predicted.m3, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
summary(mod1_spatial)$r.squared

###############################
#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1,SiteCode)
dat <- merge(mod1,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted.m3-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
summary(mod_temporal)$r.squared

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

#The GAM model
#normal
                   
bp.model.ps<-gam(m4.formula ,data = mod1d_all_st)
mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted.m3+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

summary(mod1d_reg_st)$r.squared
summary(mod1d_reg_st)$coef[1,1]
summary(mod1d_reg_st)$coef[1,2]
summary(mod1d_reg_st)$coef[2,1]
summary(mod1d_reg_st)$coef[2,2]
#rmspe
sqrt(mean(mod1d_reg_st$residual^2))

####################
#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(OAPred, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
summary(mod1LPM_spatial)$r.squared

####################
#temporal
setkey(m1CVLPM_agg,SiteCode)
setkey(mod1d_all_st,SiteCode)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$OAPred-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
summary(mod_temporal)$r.squared


