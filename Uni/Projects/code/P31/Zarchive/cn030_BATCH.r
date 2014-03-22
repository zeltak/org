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
#FUNCTIONS 
###############
#CV
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/CV_splits.r")
#nearest point per day
source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")
#closesttempknn-which nearest neigbour rdid it take
#closesttempnobs-number of obsv that were avilable


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####for first run only create a ndvi id keytable by sourcing this file
#source("/home/zeltak/org/files/Uni/Projects/code/P31/code_snips/create ndvi_id.r")
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



###########Paths
# set working directory
path.root <- "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/"
setwd(path.root); list.files()
path.keys <- paste(path.root, "FN007_Key_tables/", sep = "")
# clip to bounding coordinates of road network
#clip only NE_NYNJ
#dat <- dat[long_aod > -66.950005 & long_aod < 83.675290 & lat_aod < 36.540739 & lat_aod > 47.459687, ]






###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(type=character(40), r2003=numeric(40),r2003=numeric(40),r2005=numeric(40),r2006=numeric(40),r2007=numeric(40),r2008=numeric(40),r2009=numeric(40),r2010=numeric(40),r2011=numeric(40),r2012=numeric(40),mean=numeric(40))

#name columns

mod1table$type<- c("mod1_R2","mod1CV_R2","mod1CV_int","mod1CV_int_SE","mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE","mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial","mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE","mod1CVLPM_Slope","mod1CVLPM_Slope SE","mod1CVLPM_RMSPE","mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial","mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int","mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE","mod3_spatial","mod3_temporal","mod3_RMSPE_spatial","mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope","mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")

###############
#DATA
###############






#import and clip data
mod1<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2008.rds")
mod2<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2008.rds")
mod2C <- mod2[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
mod1C <- mod1[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]

#import whole NE_MIA grid
basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")
# #import clipped study area
# sa <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/guid_study.csv")
# # "clip" base grid
# basegrid<- basegrid[basegrid$guid %in% sa$guid, ]        

