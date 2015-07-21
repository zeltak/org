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
library(FNN)


#x<-  fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/keys/pm25.stn.XY.csv")
basestn<- fread("/home/zeltak/ZH_tmp/pm25_stn.csv")
badstn<- fread("/home/zeltak/ZH_tmp/pm25_stn_nodata.csv")
setnames(basestn, "CODESTATION","stn")
setnames(badstn, "V1","stn")
out <- basestn[!(basestn$stn %in% badstn$stn), ] 
#join XY
setnames(x, "field_1","stn")
setkey(out  ,stn)
setkey(x ,stn )
pmall <- outer_join(out,x)

write.csv(out,"/home/zeltak/ZH_tmp/Valid_pm25_stn.csv")
