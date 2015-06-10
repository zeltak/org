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
library(sas7bdat)


out<-as.data.table(read.sas7bdat ("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/diddier/foritai.sas7bdat",debug=FALSE))
out<-as.data.table(read.sas7bdat ("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/diddier/rates.sas7bdat",debug=FALSE))
zipxy<-fread ("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/diddier/zipXY.csv")
zipxy<-fread ("/media/NAS/Uni/Data/GIS/USA/zipcodes_xy/zipxy.csv")
setkey(out, zipcode)
setkey(zipxy,  zipcode)
zipxyf <- merge(out, zipxy[,list(zipcode, x,y)], all.x = T)
summary(zipxyf)
write.dbf(zipxyf,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/diddier/zipdataXY.dbf")

