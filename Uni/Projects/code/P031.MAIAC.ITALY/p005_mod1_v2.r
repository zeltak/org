###############
#LIBS
###############
library(lme4)
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

###############
#TABLES
############### 
#create main CV table
mod1table <- data.frame(model=character(40), r2002=numeric(40), r2003=numeric(40),
                        r2004=numeric(40),r2005=numeric(40),
                        r2006=numeric(40),r2007=numeric(40),
                        r2008=numeric(40),r2009=numeric(40),
                        r2010=numeric(40),r2011=numeric(40),
                        r2012=numeric(40), r2013=numeric(40),mean=numeric(40))

#name columns

mod1table$model<- c("allyears","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
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

mod1<-fread("/home/zeltak/Downloads/sas/mass.csv")
names(mod1)
summary(mod1$LATITUDE)
mod1 <- mod1[LATITUDE >= 44.5 , ]
#add date
mod1[, day:=as.Date(strptime(DATE, "%d/%m/%Y"))]
mod1$aod<-mod1$AOD*0.001
describe(mod1$aod)

pm25.m1<- filter(mod1, PM25 >= 0)
pm10.m1<- filter(mod1, PM10 >= 0)








########
##pm10
########

summary(lm(PM25~aod, mod1pm25))
summary(lm(PM10~aod, mod1pm10))

#lme formulas
#base raw
m1t.formula <- as.formula(PM10~ aod+(1+aod|day)) #0.80
#base+regions
m1t.formula <- as.formula(PM10~ aod+(1+aod|day/reg_num))#0.836
# #base+covars
# m1t.formula <- as.formula(PM10~ aod+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+NDVI+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day))#0.8193
# #base+reg+covars
# m1t.formula <- as.formula(PM10~ aod+WS+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num)) # 0.8832 
#base+reg+covars+interactions

m1.formula <- as.formula(PM10~ aod+DUST+ELEVATION+NDVI+PBL+RESTOT+NEAR_EMIP+LENGTH_A1+(1+aod|day))



####################################################
####################################################
###Base formula terra

out.m1 = lmer(m1.formula ,data =  pm10.m1,na.action = na.exclude)
summary(out.m1)
pm10.m1$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=pm10.m1))


out.m1 = lmer(m1t.formula ,data =  pm25.m1,na.action = na.exclude)
pm25.m1$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=pm25.m1))











###############
#MOD1 CV
###############
source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/CV_splits.r")
#s1
splits_s1 <- splitdf(pm10.m1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1t.formula ,data =  mod1d_90_s1,na.action = na.exclude)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,REform=NULL )










### Aqua formulas

m1t.formula <- as.formula(PM10~ aod.aq+(1+aod.aq|day))  #0.804
#base+regions
m1t.formula <- as.formula(PM10~ aod.aq+(1+aod.aq|day/reg_num))#0.8377
#base+covars
m1aq.formula <- as.formula(PM10~ aod.aq+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod.aq|day))#0.8098 
#base+reg+covars
m1aq.formula <- as.formula(PM10~ aod.aq+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod.aq|day/reg_num))#0.8503 


#model Aqua
###Base formula terra
out.m1 = lmer(m1aq.formula ,data =  pm10.m1,na.action = na.exclude)
pm10.m1$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=pm10.m1))














# #PM25
# pmbyc<- pm10.m1 %.% group_by(c) %.% do(function(df){summary(lmer(m1.formula,data=df,na.action = na.exclude))})
# mod1table$r2002[1]<-pmbyc[[1]][8]
# mod1table$r2003[1]<-pmbyc[[2]][8]
# mod1table$r2004[1]<-pmbyc[[3]][8]
# mod1table$r2005[1]<-pmbyc[[4]][8]
# mod1table$r2006[1]<-pmbyc[[5]][8]
# mod1table$r2007[1]<-pmbyc[[6]][8]
# mod1table$r2008[1]<-pmbyc[[7]][8]
# mod1table$r2009[1]<-pmbyc[[8]][8]
# mod1table$r2010[1]<-pmbyc[[9]][8]
# mod1table$r2011[1]<-pmbyc[[10]][8]
# mod1table$r2012[1]<-pmbyc[[11]][8]
# pm10.m1$predicted <- predict(out.m1)
# 




