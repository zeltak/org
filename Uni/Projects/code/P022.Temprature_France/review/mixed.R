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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#create CV table
res <- data.frame(type=character(10), R2=numeric(10),Bias=numeric(10),RMSPE=numeric(10),s.r2=numeric(10),s.RMSPE=numeric(10),t.r2=numeric(10))
#name columns
res$type <- c("overall" , "mountain climate", "ocianic climate", "degraded ocianic climate", "mediterranean climate", "continental climate", "winter","summer","urban","rural")

#1-mountain climate
#2-ocianic climate
#3-degraded ocieanic climate
#4-medeteranian climate
#5-continnetal climate

##prepare models
#####################
#2000
#####################
#m1
mod2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2000.csv")
mod2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2001.csv")
mod2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2002.csv")
mod2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2003.csv")
mod2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2004.csv")
mod2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2005.csv")
mod2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2006.csv")
mod2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2007.csv")
mod2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2008.csv")
mod2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2009.csv")
mod2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2010.csv")
mod2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2011.csv")

mod1<-rbindlist(list(mod2005))
mod1[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1$lstid<-paste(mod1$Longitude,mod1$Latitude,sep="-")
#day dataset
mod1$DTckin <-NULL
mod1$T_Day<-NULL
mod1<-na.omit(mod1)
#create full grid
m1.all.cv <-mod1 %>%
    group_by(num_insee) %>%
    summarise(X = mean(Longitude , na.rm=TRUE),  Y = mean(Latitude  , na.rm=TRUE))

#import back climate zone

climz<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metstnXY._czone.csv")

#climz<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metnum_inseeXY._czone.csv")
climz$id<-NULL
setkey(mod1,num_insee)
setkey(climz,num_insee)
mod1 <- merge(mod1,climz[,list(num_insee,cid)], all.x = T)
mod1<-mod1[, c := as.numeric(format(day, "%Y")) ]
m1.all<-mod1

m1.formula <- as.formula(tm ~ T_Night+elev_m+pcturb+NDVI +(1+NTckin|date)) 
s1 <- lmer(m1.formula,data =  m1.all)
m1.all$pred.m1 <- predict(object=s1 ,allow.new.levels=TRUE,re.form=NULL )
#overall
summary(lm(tm~pred.m1,data=m1.all))


m1.formula <- as.formula(tm ~ elev_m+pcturb+NDVI +(1|date)) 
s1 <- lmer(m1.formula,data =  m1.all)
m1.all$pred.m1 <- predict(object=s1 ,allow.new.levels=TRUE,re.form=NULL )
#overall
summary(lm(tm~pred.m1,data=m1.all))



