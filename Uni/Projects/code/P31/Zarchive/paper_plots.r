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
#DATA
###############
#import whole NE_MIA grid
basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")
#import and clip data
mod1<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003.rds")
mod2<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003.rds")
#clip to just NE and NY/NJ
mod2C <- mod2[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
mod1C <- mod1[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
###############
#MOD1
###############
##############
#switch to choose all area or cliped area for paper
m1_2003<-mod1C
#m1_2003<-mod1
##############
#clean data and exclude bad values
m1_2003$logroad<-log(m1_2003$Mjrrdden_1 +.1)
#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))
#full model 1
out.m1_2003 = lmer(m1.formula ,data =  m1_2003)
#generate prediction
m1_2003$predicted <- predict(out.m1_2003)
#get overall R2
mod1_reg <- lm(m1_2003$PM25~m1_2003$predicted)

#####################
#density plot
#####################


ran2001 <-out.model_T2003$coef$random 
ran2<-unlist(out.model_T2003$coef$random)

summary(ran2001)
ran2001$AOD
names(data1)
summary(data1$AOD)
summary(data1$fixran)

ran2001 #copy paste colums from term to editor and save as text

#then reread the file after i manually edit the above step ^^^^^
data1 <- read.table("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/random_effect/ranef2.txt", header = TRUE)

data1$fixran <- data1$AOD+0.321 #add the fixed effect (9.45) to each random effect (st_faren)
