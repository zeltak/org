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
mod1table <- data.frame(type=character(40), r2003=numeric(40),
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


source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/CV_splits.r")


###############
#DATA
###############

#import whole NE_MIA grid
basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")


#import and clip data
mod1<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003.rds")

mod1C <- mod1[long_aod > -71.62 & long_aod < -70 & lat_aod < 43 & lat_aod > 41 , ]


###############
#MOD1
###############

###############
#switch to choose all area or cliped area for paper
m1_2003<-mod1C
#m1_2003<-mod1
###############


#clean data and exclude bad values
m1_2003$logroad<-log(m1_2003$Mjrrdden_1 +.1)



#base model for stage 1
m1.formula <- as.formula(PM25 ~ (1 |day))
#full model 1
out.m1_2003 = lmer(m1.formula ,data =  m1_2003)
#generate prediction
m1_2003$predicted <- predict(out.m1_2003)
#get overall R2
mod1_reg <- lm(m1_2003$PM25~m1_2003$predicted)

summary(mod1_reg)$r.squared








