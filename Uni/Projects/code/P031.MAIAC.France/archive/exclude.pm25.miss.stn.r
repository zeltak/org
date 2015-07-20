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
library(FNN)


basestn<-  fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/pm25.stn.XY.csv")
badstn<- fread("/home/zeltak/ZH_tmp/pm25_stn_nodata.csv")
setnames(basestn, "field_1","stn")
setnames(badstn, "V1","stn")
out <- basestn[!(basestn$stn %in% badstn$stn), ] 
