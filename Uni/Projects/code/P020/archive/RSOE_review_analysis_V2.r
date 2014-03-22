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


#create CV table
mod1table <- data.frame(type=character(17), R2=numeric(17),Bias=numeric(17),RMSPE=numeric(17))
#name columns
mod1table$type <- c("WV", "VA", "PA", "MD", "NY", "RI", "NJ", "DE", "CT", "MA", "VT", "NH", "ME","Boston","NYC","rural","urban")

#####################
###Full model
#####################

mod1 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2005.dbf"))
summary(mod1)
#day dataset
DELLIST <-  names(mod1) %in% c("DTckin", "humidity")
mod1d <- mod1[!DELLIST]
mod1d<-na.omit(mod1d)
mod1d$predicted<-NA
unique(mod1d$STATE_ABBR)
# #region
# reg<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN007_Key_tables/station_state.csv")
# #add new region
# setkey(mod1d,station)
# setkey(reg,station)
# mod1d <- merge(mod1d, reg, all.x = T)
#overall model
outd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d) 


######
#1WV
#####
mod1d_WV <- mod1d[STATE_ABBR %in% c("WV"), ]
mod1d_WV$predicted <- predict(object=outd,newdata=mod1d_WV )
WV_out<-summary(lm(tempc~predicted,data=mod1d_WV))
mod1d_WV <- mod1d[STATE_ABBR %in% c("WV"), ]
mod1d_WV$predicted <- predict(object=outd,newdata=mod1d_WV )
WV_out<-lm(tempc~predicted,data=mod1d_WV)
mod1table$R2[1] <-summary(WV_out)$r.squared
mod1table$Bias[1] <-summary(WV_out)$coef[2,1]
#rmspe
mod1table$RMSPE[1]<- sqrt(mean(WV_out$residual^2))

######
#V2A
#####
mod1d_VA <- mod1d[STATE_ABBR %in% c("VA"), ]
mod1d_VA$predicted <- predict(object=outd,newdata=mod1d_VA )
VA_out<-summary(lm(tempc~predicted,data=mod1d_VA))
mod1d_VA <- mod1d[STATE_ABBR %in% c("VA"), ]
mod1d_VA$predicted <- predict(object=outd,newdata=mod1d_VA )
VA_out<-lm(tempc~predicted,data=mod1d_VA)
mod1table$R2[2] <-summary(VA_out)$r.squared
mod1table$Bias[2] <-summary(VA_out)$coef[2,1]
#rmspe
mod1table$RMSPE[2]<- sqrt(mean(VA_out$residual^2))

######
#3PA
#####
mod1d_PA <- mod1d[STATE_ABBR %in% c("PA"), ]
mod1d_PA$predicted <- predict(object=outd,newdata=mod1d_PA )
PA_out<-summary(lm(tempc~predicted,data=mod1d_PA))
mod1d_PA <- mod1d[STATE_ABBR %in% c("PA"), ]
mod1d_PA$predicted <- predict(object=outd,newdata=mod1d_PA )
PA_out<-lm(tempc~predicted,data=mod1d_PA)
mod1table$R2[3] <-summary(PA_out)$r.squared
mod1table$Bias[3] <-summary(PA_out)$coef[2,1]
#rmspe
mod1table$RMSPE[3]<- sqrt(mean(PA_out$residual^2))

######
#4MD
#####
mod1d_MD <- mod1d[STATE_ABBR %in% c("MD"), ]
mod1d_MD$predicted <- predict(object=outd,newdata=mod1d_MD )
MD_out<-summary(lm(tempc~predicted,data=mod1d_MD))
mod1d_MD <- mod1d[STATE_ABBR %in% c("MD"), ]
mod1d_MD$predicted <- predict(object=outd,newdata=mod1d_MD )
MD_out<-lm(tempc~predicted,data=mod1d_MD)
mod1table$R2[4] <-summary(MD_out)$r.squared
mod1table$Bias[4] <-summary(MD_out)$coef[2,1]
#rmspe
mod1table$RMSPE[4]<- sqrt(mean(MD_out$residual^2))

######
#5NY
#####
mod1d_NY <- mod1d[STATE_ABBR %in% c("NY"), ]
mod1d_NY$predicted <- predict(object=outd,newdata=mod1d_NY )
NY_out<-summary(lm(tempc~predicted,data=mod1d_NY))
mod1d_NY <- mod1d[STATE_ABBR %in% c("NY"), ]
mod1d_NY$predicted <- predict(object=outd,newdata=mod1d_NY )
NY_out<-lm(tempc~predicted,data=mod1d_NY)
mod1table$R2[5] <-summary(NY_out)$r.squared
mod1table$Bias[5] <-summary(NY_out)$coef[2,1]
#rmspe
mod1table$RMSPE[5]<- sqrt(mean(NY_out$residual^2))


######
#6RI
#####
mod1d_RI <- mod1d[STATE_ABBR %in% c("RI"), ]
mod1d_RI$predicted <- predict(object=outd,newdata=mod1d_RI )
RI_out<-summary(lm(tempc~predicted,data=mod1d_RI))
mod1d_RI <- mod1d[STATE_ABBR %in% c("RI"), ]
mod1d_RI$predicted <- predict(object=outd,newdata=mod1d_RI )
RI_out<-lm(tempc~predicted,data=mod1d_RI)
mod1table$R2[6] <-summary(RI_out)$r.squared
mod1table$Bias[6] <-summary(RI_out)$coef[2,1]
#rmspe
mod1table$RMSPE[6]<- sqrt(mean(RI_out$residual^2))

######
#7NJ
#####
mod1d_NJ <- mod1d[STATE_ABBR %in% c("NJ"), ]
mod1d_NJ$predicted <- predict(object=outd,newdata=mod1d_NJ )
NJ_out<-summary(lm(tempc~predicted,data=mod1d_NJ))
mod1d_NJ <- mod1d[STATE_ABBR %in% c("NJ"), ]
mod1d_NJ$predicted <- predict(object=outd,newdata=mod1d_NJ )
NJ_out<-lm(tempc~predicted,data=mod1d_NJ)
mod1table$R2[7] <-summary(NJ_out)$r.squared
mod1table$Bias[7] <-summary(NJ_out)$coef[2,1]
#rmspe
mod1table$RMSPE[7]<- sqrt(mean(NJ_out$residual^2))

######
#8DE
#####
mod1d_DE <- mod1d[STATE_ABBR %in% c("DE"), ]
mod1d_DE$predicted <- predict(object=outd,newdata=mod1d_DE )
DE_out<-summary(lm(tempc~predicted,data=mod1d_DE))
mod1d_DE <- mod1d[STATE_ABBR %in% c("DE"), ]
mod1d_DE$predicted <- predict(object=outd,newdata=mod1d_DE )
DE_out<-lm(tempc~predicted,data=mod1d_DE)
mod1table$R2[8] <-summary(DE_out)$r.squared
mod1table$Bias[8] <-summary(DE_out)$coef[2,1]
#rmspe
mod1table$RMSPE[8]<- sqrt(mean(DE_out$residual^2))

######
#9CT
#####
mod1d_CT <- mod1d[STATE_ABBR %in% c("CT"), ]
mod1d_CT$predicted <- predict(object=outd,newdata=mod1d_CT )
CT_out<-summary(lm(tempc~predicted,data=mod1d_CT))
mod1d_CT <- mod1d[STATE_ABBR %in% c("CT"), ]
mod1d_CT$predicted <- predict(object=outd,newdata=mod1d_CT )
CT_out<-lm(tempc~predicted,data=mod1d_CT)
mod1table$R2[9] <-summary(CT_out)$r.squared
mod1table$Bias[9] <-summary(CT_out)$coef[2,1]
#rmspe
mod1table$RMSPE[9]<- sqrt(mean(CT_out$residual^2))

######
#10MA
#####
mod1d_MA <- mod1d[STATE_ABBR %in% c("MA"), ]
mod1d_MA$predicted <- predict(object=outd,newdata=mod1d_MA )
MA_out<-summary(lm(tempc~predicted,data=mod1d_MA))
mod1d_MA <- mod1d[STATE_ABBR %in% c("MA"), ]
mod1d_MA$predicted <- predict(object=outd,newdata=mod1d_MA )
MA_out<-lm(tempc~predicted,data=mod1d_MA)
mod1table$R2[10] <-summary(MA_out)$r.squared
mod1table$Bias[10] <-summary(MA_out)$coef[2,1]
#rmspe
mod1table$RMSPE[10]<- sqrt(mean(MA_out$residual^2))



######
#11VT
#####
mod1d_VT <- mod1d[STATE_ABBR %in% c("VT"), ]
mod1d_VT$predicted <- predict(object=outd,newdata=mod1d_VT )
VT_out<-summary(lm(tempc~predicted,data=mod1d_VT))
mod1d_VT <- mod1d[STATE_ABBR %in% c("VT"), ]
mod1d_VT$predicted <- predict(object=outd,newdata=mod1d_VT )
VT_out<-lm(tempc~predicted,data=mod1d_VT)
mod1table$R2[11] <-summary(VT_out)$r.squared
mod1table$Bias[11] <-summary(VT_out)$coef[2,1]
#rmspe
mod1table$RMSPE[11]<- sqrt(mean(VT_out$residual^2))



######
#12NH
#####
mod1d_NH <- mod1d[STATE_ABBR %in% c("NH"), ]
mod1d_NH$predicted <- predict(object=outd,newdata=mod1d_NH )
NH_out<-summary(lm(tempc~predicted,data=mod1d_NH))
mod1d_NH <- mod1d[STATE_ABBR %in% c("NH"), ]
mod1d_NH$predicted <- predict(object=outd,newdata=mod1d_NH )
NH_out<-lm(tempc~predicted,data=mod1d_NH)
mod1table$R2[12] <-summary(NH_out)$r.squared
mod1table$Bias[12] <-summary(NH_out)$coef[2,1]
#rmspe
mod1table$RMSPE[12]<- sqrt(mean(NH_out$residual^2))

######
#13ME
#####
mod1d_ME <- mod1d[STATE_ABBR %in% c("ME"), ]
mod1d_ME$predicted <- predict(object=outd,newdata=mod1d_ME )
ME_out<-summary(lm(tempc~predicted,data=mod1d_ME))
mod1d_ME <- mod1d[STATE_ABBR %in% c("ME"), ]
mod1d_ME$predicted <- predict(object=outd,newdata=mod1d_ME )
ME_out<-lm(tempc~predicted,data=mod1d_ME)
mod1table$R2[13] <-summary(ME_out)$r.squared
mod1table$Bias[13] <-summary(ME_out)$coef[2,1]
#rmspe
mod1table$RMSPE[13]<- sqrt(mean(ME_out$residual^2))






###3 cities
#boston analysis
#use overall fit to predict for only boston stations
bos2005<- mod1d[glong < -70.5 & glong > -71.5 & glat > 42.00 & glat < 43, ]
outd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d) 
bos2005$predicted <- predict(object=outd,newdata=bos2005 )
bos2005_out<-lm(tempc~predicted,data=bos2005)
mod1table$R2[14] <-summary(bos2005_out)$r.squared
mod1table$Bias[14] <-summary(bos2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[14]<- sqrt(mean(bos2005_out$residual^2))


#NY analysis
#use overall fit to predict for only boston stations
ny2005<- mod1d[glong < -73 & glong > -75 & glat > 40 & glat < 42, ]
ny2005$predicted <- predict(object=outd,newdata=ny2005 )
ny2005_out<-lm(tempc~predicted,data=ny2005)
mod1table$R2[15] <-summary(ny2005_out)$r.squared
mod1table$Bias[15] <-summary(ny2005_out)$coef[2,1]
#rmspe
mod1table$RMSPE[15]<- sqrt(mean(ny2005_out$residual^2))

#urban
m3pred2005_urb <- m3pred2005[purban.x > 22.2222, ]
m3pred2005_rural <- m3pred2005[purban.x <=22.2222, ]

summary(lm(tempc~pred_m3,data=m3pred2005_urb))
0.9693
summary(lm(tempc~pred_m3,data=m3pred2005_rural))
0.8851 
#########paper
#########paper
#########paper



#Seasons
library(car)
m3pred2005 <-m3pred2005 [, date:=as.Date(strptime(Date, "%m/%d/%y"))]
m3pred2005$month <- as.numeric(format(m3pred2005$date, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m3pred2005$season<-recode(m3pred2005$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m3pred2005$seasonSW<-as.character(recode(m3pred2005$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))


m3pred2005_winter <- m3pred2005[seasonSW %in% c("1"), ]
m3pred2005_summer <- m3pred2005[seasonSW %in% c("2"), ]
summary(lm(tempc~pred_m3,data=m3pred2005_winter))
summary(lm(tempc~pred_m3,data=m3pred2005_summer))



m3pred2005_s1 <- m3pred2005[season %in% c("1"), ]
m3pred2005_s2 <- m3pred2005[season %in% c("2"), ]
m3pred2005_s3 <- m3pred2005[season %in% c("3"), ]
m3pred2005_s4 <- m3pred2005[season %in% c("4"), ]

summary(lm(tempc~pred_m3,data=m3pred2005_s1))
summary(lm(tempc~pred_m3,data=m3pred2005_s2))
summary(lm(tempc~pred_m3,data=m3pred2005_s3))
summary(lm(tempc~pred_m3,data=m3pred2005_s4))


