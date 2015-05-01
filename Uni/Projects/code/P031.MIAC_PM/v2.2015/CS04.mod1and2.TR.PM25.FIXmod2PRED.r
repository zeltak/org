
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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2000.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2000.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2000.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2001.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2001.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2001.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2002.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2002.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2002.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2003.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2003.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2003.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2004.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2004.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2004.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2005.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2005.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2005.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2006.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2006.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2006.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2007.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2007.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2007.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2008.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2008.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2008.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2009.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2009.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2009.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2010.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2010.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2010.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2011.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2011.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2011.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2012.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2012.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2012.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2013.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2013.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2013.rds")

rm(list = ls(all = TRUE))
gc()

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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod1.Tr.2014.rds")

names(m1.all)
m1.all<-filter(m1.all,!is.na(Temp_C))
m1.all<-filter(m1.all,!is.na(NDVI))
summary(m1.all)
#base model for stage 1
#clean data and exclude bad values
m1.all$logroad<-log(m1.all$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+Temp_C+wdsp+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+RH+visib+aod*hpbl+hpbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+Temp_C|day/region))

m1_sc <- lmer(m1.formula,data=m1.all)

#### mod2 

m2.all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.Tr.2014.rds")
#generate predictions
m2.all$logroad<-log(m2.all$Mjrrdden_1 +.1)
m2.all<-filter(m2.all,!is.na(Temp_C))
summary(m2.all)
m2.all[, pred.m2 := predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)]
summary(m2.all$pred.m2)
#delete implossible values
m2.all <- m2.all[pred.m2 > 0.00000000000001 , ]
m2.all <- m2.all[pred.m2 < 200   , ]

saveRDS(m2.all,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod2.AQ.PM25.pred.2014.rds")

rm(list = ls(all = TRUE))
gc()
