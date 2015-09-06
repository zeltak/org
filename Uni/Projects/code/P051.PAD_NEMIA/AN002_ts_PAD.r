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

AP<-fread("/media/NAS/Uni/Projects/P042_Medicare_DVT/3.1.10.4.Work/3.Analysis/AN002_timeseries/all_0008_AP.csv")

names(AP)
AP[,c("_TYPE_","_FREQ_"):=NULL]

#+Avg_pctcol dosent converge
APres <- (glmmPQL(count ~ ns(date,df=45)+pmnewmayear+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = AP))
summary(APres)$tTable

#short term
#import cases
cases<-fread("/media/NAS/Uni/Projects/P051.PAD_NEMIA/2.work/CXO_APD_counts.csv")
head(cases)



modcc1<-coxph(Surv(Time, case) ~ pmnew+Temp_F+strata(QID),data=cases)
