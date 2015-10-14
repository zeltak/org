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

#create CV table
mod1table <- data.frame(type=character(7), R2=numeric(7),Bias=numeric(7),RMSPE=numeric(7))
#name columns
mod1table$type <- c("mountain climate", "ocianic climate", "degraded ocieanic climate", "medeteranian climate", "continnetal climate", "winter","summer")

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

mod1<-rbindlist(list(mod2000,mod2001,mod2002,mod2003,mod2004,mod2005,mod2006,mod2007,mod2008,mod2009,mod2010,mod2011))
mod1[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1$lstid<-paste(mod1$Longitude,mod1$Latitude,sep="-")
#day dataset
mod1$DTckin <-NULL
mod1$T_Day<-NULL
mod1<-na.omit(mod1)
#create full grid
mod2000 <-mod1 %>%
    group_by(num_insee) %>%
    summarise(X = mean(Longitude , na.rm=TRUE),  Y = mean(Latitude  , na.rm=TRUE))
#write
write.csv(mod2000,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metstnXY.csv")
#import back climate zone
climz<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metstnXY._czone.csv")
climz$id<-NULL
setkey(mod1,num_insee)
setkey(climz,num_insee)
mod1 <- merge(mod1,climz[,list(num_insee,cid)], all.x = T)
mod1<-mod1[, c := as.numeric(format(day, "%Y")) ]


#####################
#2000
#####################
#m1
mod2000<-filter(mod1,c==2000)
#overall model
outd = lme(tm ~ NTckin+elev_m+pcturb+NDVI, random = ~1 + NTckin| date,  data =  mod2000) 
######
#1
#####
mod2000_1 <- mod2000[cid %in% c("1"), ]
mod2000_1$predicted <- predict(object=outd,newdata=mod2000_1 )
m1_out<-summary(lm(tm~predicted,data=mod2000_1))

mod1table$R2[1] <-summary(m1_out)$r.squared
mod1table$Bias[1] <-summary(WV_out)$coef[2,1]
#rmspe
mod1table$RMSPE[1]<- sqrt(mean(WV_out$residual^2))












###3 cities
#boston analysis
#use overall fit to predict for only boston stations
bos2005<- mod2000[glong < -70.5 & glong > -71.5 & glat > 42.00 & glat < 43, ]
bosoutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod2000) 
bos2005$predicted <- predict(object=bosoutd,newdata=bos2005 )
bos2005_out<-lm(tempc~predicted,data=bos2005)
mod1table$R2[14] <-summary(bos2005_out)$r.squared
mod1table$Bias[14] <-summary(bos2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[14]<- sqrt(mean(bos2005_out$residual^2))


#NY analysis
#use overall fit to predict for only boston stations
ny2005<- mod2000[glong < -73 & glong > -75 & glat > 40 & glat < 42, ]
nyoutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod2000) 
ny2005$predicted <- predict(object=nyoutd,newdata=ny2005 )
ny2005_out<-lm(tempc~predicted,data=ny2005)
mod1table$R2[15] <-summary(ny2005_out)$r.squared
mod1table$Bias[15] <-summary(ny2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[15]<- sqrt(mean(ny2005_out$residual^2))

#urban
m3pred2005_urb <- mod2000[purban > 22.2222, ]
m3pred2005_rural <- mod2000[purban <=22.2222, ]

ruroutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  m3pred2005_rural) 
m3pred2005_rural$predicted <- predict(object=ruroutd,newdata=m3pred2005_rural )
rur2005_out<-lm(tempc~predicted,data=m3pred2005_rural)
mod1table$R2[16] <-summary(rur2005_out)$r.squared
mod1table$Bias[16] <-summary(rur2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[16]<- sqrt(mean(rur2005_out$residual^2))

urboutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  m3pred2005_urb) 
m3pred2005_urb$predicted <- predict(object=urboutd,newdata=m3pred2005_urb )
urb2005_out<-lm(tempc~predicted,data=m3pred2005_urb)
mod1table$R2[17] <-summary(urb2005_out)$r.squared
mod1table$Bias[17] <-summary(urb2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[17]<- sqrt(mean(urb2005_out$residual^2))

# summary(lm(tempc~predicted,data=m3pred2005_urb))
# 0.9693
# summary(lm(tempc~pred_m3,data=m3pred2005_rural))
# 0.8851 


#Seasons
library(car)

mod2000$month <- as.numeric(format(mod2000$date, "%m"))
#1-winter, 2-spring,3-summer,4-autum
mod2000$season<-recode(mod2000$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
mod2000$seasonSW<-as.character(recode(mod2000$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))


mod2000_winter <- mod2000[seasonSW %in% c("1"), ]
mod2000_summer <- mod2000[seasonSW %in% c("2"), ]

mod2000_winteroutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod2000_winter) 
mod2000_winter$predicted <- predict(object=mod2000_winteroutd,newdata=mod2000_winter )
mod2000_winter_out<-lm(tempc~predicted,data=mod2000_winter)
mod1table$R2[18] <-summary(mod2000_winter_out)$r.squared
mod1table$Bias[18] <-summary(mod2000_winter_out)$coef[2,1]
#rmspe
mod1table$RMSPE[18]<- sqrt(mean(mod2000_winter_out$residual^2))


mod2000_summeroutd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod2000_summer) 
mod2000_summer$predicted <- predict(object=mod2000_summeroutd,newdata=mod2000_summer )
mod2000_summer_out<-lm(tempc~predicted,data=mod2000_summer)
mod1table$R2[19] <-summary(mod2000_summer_out)$r.squared
mod1table$Bias[19] <-summary(mod2000_summer_out)$coef[2,1]
#rmspe
mod1table$RMSPE[19]<- sqrt(mean(mod2000_summer_out$residual^2))


write.csv(mod1table,"/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/4.Results/paper/revresponse.csv")











