library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
#library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\MAIAC0615\\CV_splits.r")
source("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\MAIAC0615\\rmspe.r")



library(foreign)

mod1b<-read.dta("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod1_AQ_2010_PM10_rev.dta")

mod1b <- subset(mod1b,excl_obs==0)
dim(mod1b)
# 62,368 x 114
names(mod1b)


#### clean BAD STN PM10 and check if improved model?
raWDaf <- ddply(mod1b, c("site","Year"), 
                function(x) {
                  mod1b <- lm(pm10 ~ aod, data=x)
                  data.frame(R2 = round(summary(mod1b)$r.squared, 5), 
                             nsamps = length(summary(mod1b)$resid))
                })
dim(raWDaf)
head(raWDaf)
quantile(raWDaf$R2, c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99))
#        5%       10%       20%       30%       40%       50%       60%       70%       80%       90%       95%       99% 
# 0.0083075 0.0260800 0.0486000 0.0668950 0.0855100 0.1060200 0.1279700 0.1538650 0.1870600 0.2266650 0.2719900 0.3740900
quantile(sqrt(raWDaf$R2), c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99))
#         5%        10%        20%        30%        40%        50%        60%        70%        80%        90%        95%        99% 
# 0.09114546 0.16149289 0.22045408 0.25864005 0.29242093 0.32560711 0.35772895 0.39225452 0.43250434 0.47609313 0.52152650 0.61157059 

raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.01]
bad[,badid := paste(site,Year,sep="-")]
mod1b<-as.data.table(mod1b)
mod1b[,badid := paste(site,Year,sep="-")]
#Take out bad stations
mod1c <- mod1b[!(mod1b$badid %in% bad$badid), ] 
dim(mod1c)
# 59,180 x 115


mod1e <- filter(mod1c, UN>=0 & UN<=0.04)
# 53,461 x 118

mod1f <- subset(mod1e, flag_lake==0 & flag_sea==0)

mod1f$aodid<-paste(mod1f$long_aod,mod1f$lat_aod,sep="-")

m1.formula <- as.formula(pm10 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)+as.factor(mm)
                         # spatial 1: population, elevation, isa
                         +ns(restot_s,df=3)+ns(elevation_s,df=3)+ns(isa_s,df=3)
                         # spatial 2: point emissions
                         +ns(near_emip_s,df=3)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s
                         # spatial 3: areal emissions
                         +so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s
                         # spatial 4: land coverage vars_
                         +pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s
                         # spatial 5: streets vars_
                         +near_a1_s+near_a2_s+near_a3_s+length_a1_s+ns(length_a23_s,df=3)+ns(length_oth_s,df=3)+r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s
                         # spatial 5: other proximity variables
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)
                         # random component
                         +(1+aod|day/zona)+(1|aodid))

m1.sc <- lmer(m1.formula,data=mod1f,weights=normwt)
mod1f[,pred.m1 := NULL]
mod1f$pred.m1 <- predict(m1.sc)
print(summary(lm(pm10~pred.m1,data=mod1f))$r.squared)
# R2 = 0.775052


##### Try to add interactions: AOD*PBL + AOD*MM. It did not improve.
##### I add interactions speed_ms_s:aod + speed_ms_s:length_a23_s. Somewhat better. Let's try the CV

m2.formula <- as.formula(pm10 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)+as.factor(mm)
                         # interactions
                         +speed_ms_s:aod + speed_ms_s:length_a23_s
                         # spatial 1: population, elevation, isa
                         +ns(restot_s,df=3)+ns(elevation_s,df=3)+ns(isa_s,df=3)
                         # spatial 2: point emissions
                         +ns(near_emip_s,df=3)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s
                         # spatial 3: areal emissions
                         +so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s
                         # spatial 4: land coverage vars_
                         +pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s
                         # spatial 5: streets vars_
                         +near_a1_s+near_a2_s+near_a3_s+length_a1_s+ns(length_a23_s,df=3)+ns(length_oth_s,df=3)+r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s
                         # spatial 5: other proximity variables
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)
                         # random component
                         +(1+aod|day/zona)+(1|aodid))

m1.sc <- lmer(m2.formula,data=mod1f,weights=normwt)
mod1f[,pred.m1 := NULL]
mod1f$pred.m1 <- predict(m1.sc)
print(summary(lm(pm10~pred.m1,data=mod1f))$r.squared)
# R2 = 0.7754413


# R2 = 0.7754413
print(summary(lm(pm10~pred.m1,data=mod1f))$r.squared)
# RMSPE = 6.632362
print(rmse(residuals(m1.sc)))


#spatial
spatialall<-mod1f %>%
  group_by(site) %>%
  summarise(barpm = mean(pm10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
# Spatial R2 = 0.9396727
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
# Spatial RMSPE = 1.675486
print(rmse(residuals(m1.fit.all.s)))

#temporal
tempoall<-left_join(mod1f,spatialall)
tempoall$delpm <-tempoall$pm10-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
# Temporal R2 = 0.7354203
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


#---------------->>>> CV
#s1
splits_s1 <- splitdf(mod1f)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m2.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(mod1f)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m2.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(mod1f)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m2.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(mod1f)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m2.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(mod1f)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m2.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(mod1f)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m2.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(mod1f)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m2.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(mod1f)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m2.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(mod1f)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m2.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(mod1f)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m2.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
mod1.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
# rm(list = ls(pattern = "train_|test_"))

#table updates
m1.fit.all.cv<-lm(pm10~pred.m1.cv,data=mod1.cv)

# CV-R2 = 0.6114968
print(summary(lm(pm10~pred.m1.cv,data=mod1.cv))$r.squared)
# CV-I = 0.4751922
print(summary(lm(pm10~pred.m1.cv,data=mod1.cv))$coef[1,1])
# CV-Ise = 0.09427541
print(summary(lm(pm10~pred.m1.cv,data=mod1.cv))$coef[1,2])
# CV-slope = 0.9790838
print(summary(lm(pm10~pred.m1.cv,data=mod1.cv))$coef[2,1])
# CV-slope.se = 0.00337531
print(summary(lm(pm10~pred.m1.cv,data=mod1.cv))$coef[2,2])
# CV-RMSPE = 8.756588
print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-mod1.cv %>%
  group_by(site) %>%
  summarise(barpm = mean(pm10, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
# Spatial CV-R2 = 0.48491
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
# Spatial CV-RMSPE = 4.820082
print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(mod1.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$pm10-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
# Temporal CV-R2 = 0.xxx
print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)








































m1.sc <- lmer(m1a.formula,data=mod1f,weights=normwt)
mod1e[,pred.m1 := NULL]
mod1e$pred.m1 <- predict(m1.sc)
print(summary(lm(pm10~pred.m1,data=mod1e))$r.squared)
# R2 = 0.6706366





pmcombined <- fread("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\site_pm25_pm10_common_2010_v11.csv")
head(pmcombined)






mod1f <- mod1f[mod1f$site %in% pmcombined$site]

m1.sc <- lmer(m8.formula,data=mod1f,weights=normwt)
mod1f[,pred.m1 := NULL]
mod1f$pred.m1 <- predict(m1.sc)
print(summary(lm(pm10~pred.m1,data=mod1f))$r.squared)
# R2 = 0.775012
# (R2 = 0.7863178 when I remove Sicily and Sardinia)
# (R2 = 0.8365056 when I consider the 123 monitors in common PM10 and PM2.5)



names(mod1f)

