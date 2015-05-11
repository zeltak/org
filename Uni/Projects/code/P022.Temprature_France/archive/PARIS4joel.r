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


m3.2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2010.csv")
m3.2010$lstid<-paste(m3.2010$Longitude,m3.2010$Latitude,sep="-")
m3.2010[, day:=as.Date(strptime(date, "%d%b%Y"))]

m3.2010<-m3.2010[day >= as.Date("2010-07-01") & day < as.Date("2010-08-01")]


ugrid <-m3.2010 %>%
    group_by(lstid) %>%
    summarise(Latitude = mean(Latitude, na.rm=TRUE),  Longitude = mean(Longitude, na.rm=TRUE), Final_Pred = mean(Final_Pred, na.rm=TRUE))

ugrid<-na.omit(ugrid)
summary(ugrid)
write.csv (ugrid,"~/ZH_tmp/par.2010.csv")


#one week

m3.2010<-m3.2010[day >= as.Date("2010-07-01") & day < as.Date("2010-07-08")]


ugrid <-m3.2010 %>%
    group_by(lstid) %>%
    summarise(Latitude = mean(Latitude, na.rm=TRUE),  Longitude = mean(Longitude, na.rm=TRUE), Final_Pred = mean(Final_Pred, na.rm=TRUE))

ugrid<-na.omit(ugrid)
summary(ugrid)
write.csv (ugrid,"~/ZH_tmp/par.week.2010.csv")
