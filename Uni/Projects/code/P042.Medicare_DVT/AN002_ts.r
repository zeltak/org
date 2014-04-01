library (MASS)
library (splines)
library(nlme)
library(ggplot2)
library(data.table)
library(reshape2)
library(Hmisc)




DVT<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P042_Medicare_DVT/3.1.10.4.Work/3.Analysis/AN002_timeseries/all_0008_mon.csv")

names(DVT)
DVT[,c("_TYPE_","_FREQ_"):=NULL]


DVTres <- (glmmPQL(count ~ ns(date,df=45)+pmnewmayear+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65+Avg_pctcol, random = ~ 1 | guid, family = poisson, data = DVT))
str(DVTres)$tTable
class(DVTres)

DVTres2 <- (glmmPQL(count ~ ns(date,df=45)+pmnewmayear*mon20+mon20+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_p_A65+Avg_pctcol, random = ~ 1 | guid, family = poisson, data = DVT))
summary(DVTres2)$tTable


DVTres <- (glmmPQL(count ~ ns(date,df=45)+pmnewmayear*age_bin_m+temp_fmayear+Avg_P05300+Avg_per_mi+Avg_pctcol, random = ~ 1 | guid, family = poisson, data = DVT))
str(DVTres)$tTable

