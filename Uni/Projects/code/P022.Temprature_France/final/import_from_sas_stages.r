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
library(readr)
library(DataCombine)

m1.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2008.csv")

system.time(m3.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2008.csv"))
names(m1.2008)
names(m3.2008)

m1.2008$lstid<-paste(m1.2008$Longitude,m1.2008$Latitude,sep="-")
m3.2008$lstid<-paste(m3.2008$Longitude,m3.2008$Latitude,sep="-")
m1.2008[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2008[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2008,day,lstid)
setkey(m1.2008,day,lstid)
m1.all <- merge(m1.2008,m3.2008[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))

write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2008.csv")

