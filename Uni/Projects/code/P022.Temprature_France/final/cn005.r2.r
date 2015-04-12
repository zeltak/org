library(ztable)
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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")




#mod1
mod1res<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/4.results/r2/mod1.csv")
options(ztable.type="html")
#options(ztable.type="viewer")
z=ztable(mod1res)
z


#mod3


#-------------------->> RES TABLE
res <- matrix(nrow=12, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011")

#m3

#y2000
mod3.2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2000.csv")
mod3.2000[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2000$lstid<-paste(mod3.2000$Longitude,mod3.2000$Latitude,sep="-")
#m1
mod1.2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2000.csv")
mod1.2000[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2000$lstid<-paste(mod1.2000$Longitude,mod1.2000$Latitude,sep="-")



#R2.m3
setkey(mod3.2000,day,lstid)
setkey(mod1.2000,day,lstid)
mod1.2000 <- merge(mod1.2000,mod3.2000[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2000))
saveRDS(mod1.2000,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2000.csv")

res[res$type=="2000", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2000))$r.squared)    
res[res$type=="2000", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2000))$coef[1,1])
res[res$type=="2000", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2000))$coef[1,2])
res[res$type=="2000", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2000))$coef[2,1])
res[res$type=="2000", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2000))$coef[2,2])
#RMSPE
res[res$type=="2000", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2000,res,rmse, sure=TRUE) 
gc()



#y2001
mod3.2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2001.csv")
mod3.2001[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2001$lstid<-paste(mod3.2001$Longitude,mod3.2001$Latitude,sep="-")
#m1
mod1.2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2001.csv")
mod1.2001[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2001$lstid<-paste(mod1.2001$Longitude,mod1.2001$Latitude,sep="-")

#R2.m3
setkey(mod3.2001,day,lstid)
setkey(mod1.2001,day,lstid)
mod1.2001 <- merge(mod1.2001,mod3.2001[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2001))
saveRDS(mod1.2001,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2001.csv")

res[res$type=="2001", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2001))$r.squared)    
res[res$type=="2001", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2001))$coef[1,1])
res[res$type=="2001", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2001))$coef[1,2])
res[res$type=="2001", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2001))$coef[2,1])
res[res$type=="2001", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2001))$coef[2,2])
#RMSPE
res[res$type=="2001", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2001,res,rmse, sure=TRUE) 
gc()


#y2002
mod3.2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2002.csv")
mod3.2002[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2002$lstid<-paste(mod3.2002$Longitude,mod3.2002$Latitude,sep="-")
#m1
mod1.2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2002.csv")
mod1.2002[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2002$lstid<-paste(mod1.2002$Longitude,mod1.2002$Latitude,sep="-")

#R2.m3
setkey(mod3.2002,day,lstid)
setkey(mod1.2002,day,lstid)
mod1.2002 <- merge(mod1.2002,mod3.2002[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2002))
saveRDS(mod1.2002,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2002.csv")

res[res$type=="2002", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2002))$r.squared)    
res[res$type=="2002", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2002))$coef[1,1])
res[res$type=="2002", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2002))$coef[1,2])
res[res$type=="2002", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2002))$coef[2,1])
res[res$type=="2002", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2002))$coef[2,2])
#RMSPE
res[res$type=="2002", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2002,res,rmse, sure=TRUE) 
gc()


#y2003
mod3.2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2003.csv")
mod3.2003[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2003$lstid<-paste(mod3.2003$Longitude,mod3.2003$Latitude,sep="-")
#m1
mod1.2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2003.csv")
mod1.2003[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2003$lstid<-paste(mod1.2003$Longitude,mod1.2003$Latitude,sep="-")

#R2.m3
setkey(mod3.2003,day,lstid)
setkey(mod1.2003,day,lstid)
mod1.2003 <- merge(mod1.2003,mod3.2003[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2003))
saveRDS(mod1.2003,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2003.csv")

res[res$type=="2003", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2003))$r.squared)    
res[res$type=="2003", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2003))$coef[1,1])
res[res$type=="2003", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2003))$coef[1,2])
res[res$type=="2003", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2003))$coef[2,1])
res[res$type=="2003", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2003))$coef[2,2])
#RMSPE
res[res$type=="2003", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2003,res,rmse, sure=TRUE) 
gc()


#y2004
mod3.2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2004.csv")
mod3.2004[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2004$lstid<-paste(mod3.2004$Longitude,mod3.2004$Latitude,sep="-")
#m1
mod1.2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2004.csv")
mod1.2004[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2004$lstid<-paste(mod1.2004$Longitude,mod1.2004$Latitude,sep="-")

#R2.m3
setkey(mod3.2004,day,lstid)
setkey(mod1.2004,day,lstid)
mod1.2004 <- merge(mod1.2004,mod3.2004[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2004))
saveRDS(mod1.2004,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2004.csv")

res[res$type=="2004", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2004))$r.squared)    
res[res$type=="2004", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2004))$coef[1,1])
res[res$type=="2004", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2004))$coef[1,2])
res[res$type=="2004", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2004))$coef[2,1])
res[res$type=="2004", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2004))$coef[2,2])
#RMSPE
res[res$type=="2004", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2004,res,rmse, sure=TRUE) 
gc()


#y2005
mod3.2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2005.csv")
mod3.2005[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2005$lstid<-paste(mod3.2005$Longitude,mod3.2005$Latitude,sep="-")
#m1
mod1.2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2005.csv")
mod1.2005[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2005$lstid<-paste(mod1.2005$Longitude,mod1.2005$Latitude,sep="-")

#R2.m3
setkey(mod3.2005,day,lstid)
setkey(mod1.2005,day,lstid)
mod1.2005 <- merge(mod1.2005,mod3.2005[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2005))
saveRDS(mod1.2005,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2005.csv")

res[res$type=="2005", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2005))$r.squared)    
res[res$type=="2005", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2005))$coef[1,1])
res[res$type=="2005", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2005))$coef[1,2])
res[res$type=="2005", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2005))$coef[2,1])
res[res$type=="2005", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2005))$coef[2,2])
#RMSPE
res[res$type=="2005", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2005,res,rmse, sure=TRUE) 
gc()


#y2006
mod3.2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2006.csv")
mod3.2006[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2006$lstid<-paste(mod3.2006$Longitude,mod3.2006$Latitude,sep="-")
#m1
mod1.2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2006.csv")
mod1.2006[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2006$lstid<-paste(mod1.2006$Longitude,mod1.2006$Latitude,sep="-")

#R2.m3
setkey(mod3.2006,day,lstid)
setkey(mod1.2006,day,lstid)
mod1.2006 <- merge(mod1.2006,mod3.2006[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2006))
saveRDS(mod1.2006,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2006.csv")

res[res$type=="2006", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2006))$r.squared)    
res[res$type=="2006", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2006))$coef[1,1])
res[res$type=="2006", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2006))$coef[1,2])
res[res$type=="2006", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2006))$coef[2,1])
res[res$type=="2006", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2006))$coef[2,2])
#RMSPE
res[res$type=="2006", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2006,res,rmse, sure=TRUE) 
gc()


#y2007
mod3.2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2007.csv")
mod3.2007[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2007$lstid<-paste(mod3.2007$Longitude,mod3.2007$Latitude,sep="-")
#m1
mod1.2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2007.csv")
mod1.2007[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2007$lstid<-paste(mod1.2007$Longitude,mod1.2007$Latitude,sep="-")

#R2.m3
setkey(mod3.2007,day,lstid)
setkey(mod1.2007,day,lstid)
mod1.2007 <- merge(mod1.2007,mod3.2007[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2007))
saveRDS(mod1.2007,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2007.csv")

res[res$type=="2007", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2007))$r.squared)    
res[res$type=="2007", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2007))$coef[1,1])
res[res$type=="2007", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2007))$coef[1,2])
res[res$type=="2007", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2007))$coef[2,1])
res[res$type=="2007", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2007))$coef[2,2])
#RMSPE
res[res$type=="2007", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2007,res,rmse, sure=TRUE) 
gc()


#y2008
mod3.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2008.csv")
mod3.2008[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2008$lstid<-paste(mod3.2008$Longitude,mod3.2008$Latitude,sep="-")
#m1
mod1.2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2008.csv")
mod1.2008[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2008$lstid<-paste(mod1.2008$Longitude,mod1.2008$Latitude,sep="-")

#R2.m3
setkey(mod3.2008,day,lstid)
setkey(mod1.2008,day,lstid)
mod1.2008 <- merge(mod1.2008,mod3.2008[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2008))
saveRDS(mod1.2008,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2008.csv")

res[res$type=="2008", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2008))$r.squared)    
res[res$type=="2008", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2008))$coef[1,1])
res[res$type=="2008", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2008))$coef[1,2])
res[res$type=="2008", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2008))$coef[2,1])
res[res$type=="2008", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2008))$coef[2,2])
#RMSPE
res[res$type=="2008", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2008,res,rmse, sure=TRUE) 
gc()


#y2009
mod3.2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2009.csv")
mod3.2009[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2009$lstid<-paste(mod3.2009$Longitude,mod3.2009$Latitude,sep="-")
#m1
mod1.2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2009.csv")
mod1.2009[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2009$lstid<-paste(mod1.2009$Longitude,mod1.2009$Latitude,sep="-")

#R2.m3
setkey(mod3.2009,day,lstid)
setkey(mod1.2009,day,lstid)
mod1.2009 <- merge(mod1.2009,mod3.2009[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2009))
saveRDS(mod1.2009,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2009.csv")

res[res$type=="2009", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2009))$r.squared)    
res[res$type=="2009", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2009))$coef[1,1])
res[res$type=="2009", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2009))$coef[1,2])
res[res$type=="2009", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2009))$coef[2,1])
res[res$type=="2009", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2009))$coef[2,2])
#RMSPE
res[res$type=="2009", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2009,res,rmse, sure=TRUE) 
gc()


#y2010
mod3.2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2010.csv")
mod3.2010[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2010$lstid<-paste(mod3.2010$Longitude,mod3.2010$Latitude,sep="-")
#m1
mod1.2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2010.csv")
mod1.2010[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2010$lstid<-paste(mod1.2010$Longitude,mod1.2010$Latitude,sep="-")

#R2.m3
setkey(mod3.2010,day,lstid)
setkey(mod1.2010,day,lstid)
mod1.2010 <- merge(mod1.2010,mod3.2010[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2010))
saveRDS(mod1.2010,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2010.csv")

res[res$type=="2010", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2010))$r.squared)    
res[res$type=="2010", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2010))$coef[1,1])
res[res$type=="2010", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2010))$coef[1,2])
res[res$type=="2010", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2010))$coef[2,1])
res[res$type=="2010", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2010))$coef[2,2])
#RMSPE
res[res$type=="2010", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2010,res,rmse, sure=TRUE) 
gc()


#y2011
mod3.2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2011.csv")
mod3.2011[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod3.2011$lstid<-paste(mod3.2011$Longitude,mod3.2011$Latitude,sep="-")
#m1
mod1.2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2011.csv")
mod1.2011[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1.2011$lstid<-paste(mod1.2011$Longitude,mod1.2011$Latitude,sep="-")

#R2.m3
setkey(mod3.2011,day,lstid)
setkey(mod1.2011,day,lstid)
mod1.2011 <- merge(mod1.2011,mod3.2011[, list(day,lstid,Final_Pred)], all.x = T)
m3.fit.all<- summary(lm( NTckin~Final_Pred,data=mod1.2011))
saveRDS(mod1.2011,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2011.csv")

res[res$type=="2011", 'm3.R2'] <- print(summary(lm(NTckin~Final_Pred,data=mod1.2011))$r.squared)    
res[res$type=="2011", 'm3.I'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2011))$coef[1,1])
res[res$type=="2011", 'm3.Ise'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2011))$coef[1,2])
res[res$type=="2011", 'm3.slope'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2011))$coef[2,1])
res[res$type=="2011", 'm3.slopese'] <-print(summary(lm(NTckin~Final_Pred,data=mod1.2011))$coef[2,2])
#RMSPE
res[res$type=="2011", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))
#clean
keep(mod1.2011,res,rmse, sure=TRUE) 
gc()

saveRDS(res,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/res.csv")

