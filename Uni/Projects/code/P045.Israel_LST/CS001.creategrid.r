###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
library(zoo)
library(DataCombine)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")


###load Aqua
#load aod data
aqua<-read.csv("/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/MAIACAqIsr_2008.csv", header=T)
aqua<-as.data.table(aqua)
aqua$lstid<-paste(aqua$Lon,aqua$Lat,sep="-")
grid <- unique(aqua, by="lstid")

write.csv(grid,"~/ZH_tmp/lstgrid.csv")

