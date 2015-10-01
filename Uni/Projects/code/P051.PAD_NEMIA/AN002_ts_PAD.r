library (MASS)
library (splines)
library(nlme)
library(ggplot2)
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
library(DataCombine)
library(survival)
### long term analysis

#OLD data
#AP<-fread("/media/NAS/Uni/Projects/P042_Medicare_DVT/3.1.10.4.Work/3.Analysis/AN002_timeseries/all_0008_AP.csv")
AP2<-fread("/media/NAS/Uni/Projects/P051.PAD_NEMIA/2.work/TS_APD_counts.csv")

head(AP2)
names(AP2)
AP[,c("_TYPE_","_FREQ_"):=NULL]



#main-DELTA PM's
APres <- (glmmPQL(count ~ ns(date,df=45)+deltapm+mpmguid+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable


#main-DELTA PM's la0
APres <- (glmmPQL(count ~ ns(date,df=45)+deltapm0+mpmguid+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable


#main-DELTA PM's la2
APres <- (glmmPQL(count ~ ns(date,df=45)+deltapm2+mpmguid+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable

#main-DELTA interactions
APres <- (glmmPQL(count ~ ns(date,df=45)+deltapm+mpmguid+temp_fmayear+Avg_P05300+min_bin_m+min_bin_m*deltapm+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable



#main
APres <- (glmmPQL(count ~ ns(date,df=45)+pmnewmayear+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable


#main _ST+LT
APres <- (glmmPQL(count ~ ns(date,df=45)+ pmnew+pmnewmayear+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable


#main lag01
APres <- (glmmPQL(count ~ ns(date,df=45)+ pmnew_l1+pmnewmayear+temp_f_l1+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable

#main lag02
APres <- (glmmPQL(count ~ ns(date,df=45)+ pmnew_l2+pmnewmayear+temp_f_l2+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable

#interaction
APres <- (glmmPQL(count ~ ns(date,df=45)+ pmnew+pmnewmayear+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65+col_bin_m*pmnewmayear, random = ~ 1 | guid, family = poisson, data = AP2))
summary(APres)$tTable




#short term
#import cases
cases<-fread("/media/NAS/Uni/Projects/P051.PAD_NEMIA/2.work/CXO_APD_counts.csv")
head(cases)
modcc1<-coxph(Surv(Time, case) ~ pmnew+Temp_F+strata(QID),data=cases)
summary(modcc1)




#descriptives

APD<-fread("/media/NAS/Uni/Projects/P051.PAD_NEMIA/2.work/casesAPD.csv")

## disable sci notations
options(scipen = 99)

describe(APD$race, digits=8)
describe(APD$sex, digits=8)
summary(APD$age)
sd(APD$age)

#exposure
head(AP2)
describe(AP2$pmnew)
sd(AP2$pmnew)
IQR(AP2$pmnew)


describe(AP2$mpmguid)
sd(AP2$mpmguid)
IQR(AP2$mpmguid)

AP2$tcels<-(AP2$temp_f_l0-  32) *  5/9 
describe(AP2$tcels)
sd(AP2$tcels,na.rm=T)
IQR(AP2$tcels,na.rm=T)


