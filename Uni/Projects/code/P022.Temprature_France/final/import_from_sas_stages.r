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


m1.2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2000.csv")

system.time(m3.2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2000.csv"))
#names(m1.2000)
#names(m3.2000)

m1.2000$lstid<-paste(m1.2000$Longitude,m1.2000$Latitude,sep="-")
m3.2000$lstid<-paste(m3.2000$Longitude,m3.2000$Latitude,sep="-")
m1.2000[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2000[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2000,day,lstid)
setkey(m1.2000,day,lstid)
m1.all <- merge(m1.2000,m3.2000[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2000.csv")

m1.2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2001.csv")

system.time(m3.2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2001.csv"))
#names(m1.2001)
#names(m3.2001)

m1.2001$lstid<-paste(m1.2001$Longitude,m1.2001$Latitude,sep="-")
m3.2001$lstid<-paste(m3.2001$Longitude,m3.2001$Latitude,sep="-")
m1.2001[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2001[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2001,day,lstid)
setkey(m1.2001,day,lstid)
m1.all <- merge(m1.2001,m3.2001[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2001.csv")

m1.2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2002.csv")

system.time(m3.2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2002.csv"))
#names(m1.2002)
#names(m3.2002)

m1.2002$lstid<-paste(m1.2002$Longitude,m1.2002$Latitude,sep="-")
m3.2002$lstid<-paste(m3.2002$Longitude,m3.2002$Latitude,sep="-")
m1.2002[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2002[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2002,day,lstid)
setkey(m1.2002,day,lstid)
m1.all <- merge(m1.2002,m3.2002[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2002.csv")

m1.2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2003.csv")

system.time(m3.2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2003.csv"))
#names(m1.2003)
#names(m3.2003)

m1.2003$lstid<-paste(m1.2003$Longitude,m1.2003$Latitude,sep="-")
m3.2003$lstid<-paste(m3.2003$Longitude,m3.2003$Latitude,sep="-")
m1.2003[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2003[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2003,day,lstid)
setkey(m1.2003,day,lstid)
m1.all <- merge(m1.2003,m3.2003[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2003.csv")

m1.2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2004.csv")

system.time(m3.2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2004.csv"))
#names(m1.2004)
#names(m3.2004)

m1.2004$lstid<-paste(m1.2004$Longitude,m1.2004$Latitude,sep="-")
m3.2004$lstid<-paste(m3.2004$Longitude,m3.2004$Latitude,sep="-")
m1.2004[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2004[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2004,day,lstid)
setkey(m1.2004,day,lstid)
m1.all <- merge(m1.2004,m3.2004[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2004.csv")

m1.2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2005.csv")

system.time(m3.2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2005.csv"))
#names(m1.2005)
#names(m3.2005)

m1.2005$lstid<-paste(m1.2005$Longitude,m1.2005$Latitude,sep="-")
m3.2005$lstid<-paste(m3.2005$Longitude,m3.2005$Latitude,sep="-")
m1.2005[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2005[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2005,day,lstid)
setkey(m1.2005,day,lstid)
m1.all <- merge(m1.2005,m3.2005[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2005.csv")

m1.2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2006.csv")

system.time(m3.2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2006.csv"))
#names(m1.2006)
#names(m3.2006)

m1.2006$lstid<-paste(m1.2006$Longitude,m1.2006$Latitude,sep="-")
m3.2006$lstid<-paste(m3.2006$Longitude,m3.2006$Latitude,sep="-")
m1.2006[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2006[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2006,day,lstid)
setkey(m1.2006,day,lstid)
m1.all <- merge(m1.2006,m3.2006[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2006.csv")

m1.2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2007.csv")

system.time(m3.2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2007.csv"))
#names(m1.2007)
#names(m3.2007)

m1.2007$lstid<-paste(m1.2007$Longitude,m1.2007$Latitude,sep="-")
m3.2007$lstid<-paste(m3.2007$Longitude,m3.2007$Latitude,sep="-")
m1.2007[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2007[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2007,day,lstid)
setkey(m1.2007,day,lstid)
m1.all <- merge(m1.2007,m3.2007[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2007.csv")

m1.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2008.csv")

system.time(m3.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2008.csv"))
#names(m1.2008)
#names(m3.2008)

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

m1.2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2009.csv")

system.time(m3.2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2009.csv"))
#names(m1.2009)
#names(m3.2009)

m1.2009$lstid<-paste(m1.2009$Longitude,m1.2009$Latitude,sep="-")
m3.2009$lstid<-paste(m3.2009$Longitude,m3.2009$Latitude,sep="-")
m1.2009[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2009[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2009,day,lstid)
setkey(m1.2009,day,lstid)
m1.all <- merge(m1.2009,m3.2009[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2009.csv")

m1.2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2010.csv")

system.time(m3.2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2010.csv"))
#names(m1.2010)
#names(m3.2010)

m1.2010$lstid<-paste(m1.2010$Longitude,m1.2010$Latitude,sep="-")
m3.2010$lstid<-paste(m3.2010$Longitude,m3.2010$Latitude,sep="-")
m1.2010[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2010[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2010,day,lstid)
setkey(m1.2010,day,lstid)
m1.all <- merge(m1.2010,m3.2010[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2010.csv")

m1.2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2011.csv")

system.time(m3.2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2011.csv"))
#names(m1.2011)
#names(m3.2011)

m1.2011$lstid<-paste(m1.2011$Longitude,m1.2011$Latitude,sep="-")
m3.2011$lstid<-paste(m3.2011$Longitude,m3.2011$Latitude,sep="-")
m1.2011[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2011[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2011,day,lstid)
setkey(m1.2011,day,lstid)
m1.all <- merge(m1.2011,m3.2011[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2011.csv")

m1.2012<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2012.csv")

system.time(m3.2012<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2012.csv"))
#names(m1.2012)
#names(m3.2012)

m1.2012$lstid<-paste(m1.2012$Longitude,m1.2012$Latitude,sep="-")
m3.2012$lstid<-paste(m3.2012$Longitude,m3.2012$Latitude,sep="-")
m1.2012[, day:=as.Date(strptime(date, "%d%b%Y"))]
m3.2012[, day:=as.Date(strptime(date, "%d%b%Y"))]

#########################
#prepare for m3.R2
#########################
#load mod1

#R2.m3
setkey(m3.2012,day,lstid)
setkey(m1.2012,day,lstid)
m1.all <- merge(m1.2012,m3.2012[, list(day,lstid,Final_Pred)], all.x = T)
summary(lm(T_Day~Final_Pred,data=m1.all))
write.csv(m1.all, "/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/m3.r2.2012.csv")











