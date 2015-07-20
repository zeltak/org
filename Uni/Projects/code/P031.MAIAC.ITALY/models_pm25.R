

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

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
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
res$type <- c("pm25")


mod1 <- readRDS("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod1.AQ.2010.PM25.rds")
names(mod1)
dim(mod1)
# 16,523 x 112
summary(mod1)

mod1 <- subset(mod1, temp.c.s!="NA" & speed.ms.s!="NA" & rh.s!="NA" & normwt!="NA")
# 16,469 x 112

mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")


########################################################################################


##### FIRST TRY: FULL MODEL WITH ALL SPLINES

m1.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp.c.s,df=3)+ns(speed.ms.s,df=3)+ns(visib.km.s,df=3)+ns(rh.s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl.s,df=3)+ns(ndvi.s,df=3)+as.factor(dust)
                         # spatial 1: population, elevation, isa
                         +ns(restot.s,df=3)+ns(elevation.s,df=3)+ns(isa.s,df=3)
                         # spatial 2: point emissions
                         +ns(near.emip.s,df=3)+so2.2010p.s+co.2010p.s+nh3.2010p.s+pm25.2010p.s
                         # spatial 3: areal emissions
                         +so2.2010a.s+co.2010a.s+nh3.2010a.s+pm25.2010a.s
                         # spatial 4: land coverage vars.
                         +pct.deciduous.s+pct.evergreen.s+pct.crop.s+pct.pasture.s+pct.shrub.s+pct.high.dev.s+pct.low.dev.s
                         # spatial 5: streets vars.
                         +near.a1.s+near.a2.s+near.a3.s+length.a1.s+ns(length.a23.s,df=3)+ns(length.oth.s,df=3)+r.mean.length.a1.s+r.mean.length.a23.s+r.mean.length.oth.s
                         # spatial 5: other proximity variables
                         +ns(near.sea.s,df=3)+ns(near.airport.s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                         # random component
                         +(1+aod+temp.c.s|day/zona))  

m1.sc <- lmer(m1.formula,data=mod1,weights=normwt)
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1.sc)
# R2 = 0.7620031
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
# RMSPE = 5.791997
print(rmse(residuals(m1.sc)))

#spatial
spatialall<-mod1 %>%
  group_by(site) %>%
  summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
# Spatial R2 = 0.778309
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
# Spatial RMSPE = 2.824186
print(rmse(residuals(m1.fit.all.s)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$pm25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
# Temporal R2 = 0.7688447
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)



########################################################################################


##### I NOW REPEAT THE SAME MODEL BUT ON THE STAGE ONE FILE WITH CLEANING OF OBSERVATIONS BASED ON PM25 AND AOD VALUES
##### I HAVE MANUALLY MADE THIS CLEANING IN STATA BY USING THE OLD DO. FILE AND COME BACK AGAIN HERE IN R.
##### AFTER THIS PASSAGE ALL POINTS IN VAR NAMES HAVE BEEN REPLACED TO _, SO I CALL THE MODEL AGAIN 

##### THE CLEANING IS MADE ACCORDING TO THE FOLLOWING STEPS:
# 1. Either PM2.5 or AOD = missing. Not needed because we are already in stage 1 dataset
# 2. AOD below median and PM25 above 90th pct.
# 3. PM25 below median and AOD above 90th pct.
# 4. AOD too much large (above 1.2)
# 5. Number of observations available for one monitor <30 days



library(foreign)

mod1b<-read.dta("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod1_AQ_2010_PM25_rev.dta")

mod1b <- subset(mod1b,excl_obs==0 & temp_c_s!="NA" & speed_ms_s!="NA" & rh_s!="NA" & normwt!="NA")
dim(mod1b)
# 15,326 x 114
names(mod1b)

m1a.formula <- as.formula(pm25 ~ aod
                          # meteo
                          +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                          # other spatio-temporal  
                          +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)
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
                          +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                          # random component
                          +(1+aod+temp_c_s|day/zona))

m1.sc <- lmer(m1a.formula,data=mod1b,weights=normwt)

mod1b <- as.data.table(mod1b)
mod1b[,pred.m1 := NULL]
mod1b$pred.m1 <- predict(m1.sc)
print(summary(lm(pm25~pred.m1,data=mod1b))$r.squared)
# R2 = 0.7749826


########################################################################################


##### BELOW I CHECK SITE-SPECIFIC CORRELATIONS AND TRY THE SAME MODEL BY REMOVING OBSERVATIONS FROM MONITORS WITH RAW
##### CORRELATIONS BELOW 0.10, WHICH CORRESPONDS TO RAW R2 < 0.01.
##### THE UNDERLYING IDEA IS THAT MONITORS WITH ~0 CORRELATION DO NOT CONTRIBUTE TO CALIBRATION 


#### clean BAD STN pm25 and check if improved model?
raWDaf <- ddply(mod1b, c("site","Year"), 
                function(x) {
                  mod1b <- lm(pm25 ~ aod, data=x)
                  data.frame(R2 = round(summary(mod1b)$r.squared, 5), 
                             nsamps = length(summary(mod1b)$resid))
                })
dim(raWDaf)
head(raWDaf)
quantile(raWDaf$R2, c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99))
#        5%       10%       20%       30%       40%       50%       60%       70%       80%       90%       95%       99% 
# 0.0039660 0.0169710 0.0466580 0.0608080 0.0752640 0.0946250 0.1096740 0.1356240 0.1572080 0.2044530 0.2213385 0.2817805
quantile(sqrt(raWDaf$R2), c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99))
#         5%        10%        20%        30%        40%        50%        60%        70%        80%        90%        95%        99% 
# 0.06296764 0.13023649 0.21599984 0.24659115 0.27434280 0.30760833 0.33117008 0.36827038 0.39649445 0.45216465 0.47046490 0.53081945 

raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2 <= 0.01]
bad[,badid := paste(site,Year,sep="-")]
mod1b<-as.data.table(mod1b)
mod1b[,badid := paste(site,Year,sep="-")]
#Take out bad stations
mod1c <- mod1b[!(mod1b$badid %in% bad$badid), ] 
dim(mod1c)
# 14,314 x 115

m1.sc <- lmer(m1a.formula,data=mod1c,weights=normwt)
mod1c[,pred.m1c := NULL]
mod1c$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1c))$r.squared)
# R2 = 0.777788


########################################################################################


##### HERE BELOW I APPLY A DIFFERENT CLEANING CRITERION: NASA PEOPLE SAY THAT WE SHOULD DISCARD AOD VALUES OUT OF 0-0.04
##### BASED ON THE FIELD "UN". I TRY THIS ON THE MOD1B FILE. LATER I WILL APPLY BOTH METHODS


mod1d <- filter(mod1b, UN>=0 & UN<=0.04)
dim(mod1d)
# 14,335 x 115
m1.sc <- lmer(m1a.formula,data=mod1d,weights=normwt)
mod1d[,pred.m1c := NULL]
mod1d$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1d))$r.squared)
# R2 = 0.7804346


########################################################################################


##### HERE BELOW I APPLY BOTH CRITERIA: RAW R2>=0.01 AND "UN" PARAMETER WITHIN 0-0.04


mod1e <- filter(mod1c, UN>=0 & UN<=0.04)
# 13,364 x 116
m1.sc <- lmer(m1a.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# R2 = 0.7830729



########################################################################################


##### NOTE: ALL THE MODELS SEEM TO FAIL: ONE POSSIBILITY IS THAT pm25 IS TOO MUCH SKEWED, AS APPARENT BELOW:

quantile(mod1e$pm25, c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99,.995,.999,.9999,.99999))
# 10%      20%      30%      40%      50%      60%      70%      80%      90%      95%      99%      99.5%     99.9%    99.99%    99.999% 
# 7.0000   9.0000   11.0000  13.0000  15.0000  17.0000  19.0000  22.0000  27.0000  34.0000  62.0000  73.0000   98.0000  111.6637  126.7283 
hist(mod1e$pm25)
hist(log(mod1e$pm25))


##### SINCE THE pm25 DATA ARE SKEWED, I TRY TO LOG-TRANSPHORM pm25

m2.formula <- as.formula(log(pm25) ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)
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
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                         # random component
                         +(1+aod+temp_c_s|day/zona))  

m1.sc <- lmer(m2.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(log(pm25)~pred.m1c,data=mod1e))$r.squared)
# R2 of log(PM2.5) VS PRED = 0.7110544
print(summary(lm(pm25~exp(pred.m1c),data=mod1e))$r.squared)
# R2 of PM2.5 VS exp(PRED) = 0.7771462


########################################################################################


##### LOG-TRANSPHORMATION DOES NOT IMPROVE, I GO BACK TO ORIGINAL SCALE, DATASET "MOD1E".
##### I TRY NOW TWO ADDITIONAL MODELS, WHICH ADD VARIABLES FOR MONTHS: EITHER FACTORS OR A SPLINE
##### BELOW THE MODEL WITH INDICATORS FOR MONTHS


m3.formula <- as.formula(pm25 ~ aod
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
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                         # random component
                         +(1+aod+temp_c_s|day/zona))  

m1.sc <- lmer(m3.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# R2 = 0.783224


m4.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)+ns(mm,df=4)
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
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                         # random component
                         +(1+aod+temp_c_s|day/zona))  

m1.sc <- lmer(m4.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# R2 = 0.7832174


########################################################################################


##### SECOND TRY: MODELS STRATIFIED BY MACRO-REGIONS (NORTH, CENTRE, SOUTH-ISLANDS) WITHOUT NESTING BY CLIMATIC ZONES.
#####             I DO THIS FROM MOD1E DATASET AND THE MODEL WITHOUT MONTH

m2.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)
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
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)+as.factor(flag_lake)
                         # random component
                         +(1+aod+temp_c_s|day))  

mod2 <- subset(mod1e,cod_reg<=8)
try_sc <- lmer(m2.formula,data=mod2,weights=normwt)
mod2[,pred.north := NULL]
mod2$pred.north <- predict(try_sc)
print(summary(lm(pm25~pred.north,data=mod2))$r.squared)
#R2 overall:  0.7821746 for North (region <=8)


##### For Centre and South I have to remove FLAG_LAKE from the formula

m2a.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns(speed_ms_s,df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns(meanpbl_s,df=3)+ns(ndvi_s,df=3)+as.factor(dust)
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
                         +ns(near_sea_s,df=3)+ns(near_airport_s,df=3)+as.factor(flag_sea)
                         # random component
                         +(1+aod+temp_c_s|day))  


mod3 <- subset(mod1e,cod_reg>8 & cod_reg<=14)
try_sc <- lmer(m2a.formula,data=mod3,weights=normwt)
mod3[,pred.centre := NULL]
mod3$pred.centre <- predict(try_sc)
print(summary(lm(pm25~pred.centre,data=mod3))$r.squared)
#R2 overall:  0.7439555 for Centre (region 9-14)

mod4 <- subset(mod1e,cod_reg>14)
try_sc <- lmer(m2a.formula,data=mod4,weights=normwt)
mod4[,pred.south := NULL]
mod4$pred.south <- predict(try_sc)
print(summary(lm(pm25~pred.south,data=mod4))$r.squared)
#R2 overall:  0.7013802 for South (region >=15)


########################################################################################


##### HERE I DO THE FOLLOWING: 1. REMOVE THE TEMPERATURE FROM THE RANDOM COMPONENT, 2. ADD RANDOM INTERCEPT FOR CELL,
##### REMOVE OBSERVATIONS WITH LAKES OR SEA. I USE MOD1E AS BASE AND PUT MONTH AS FACTOR

mod1e$aodid<-paste(mod1e$long_aod,mod1e$lat_aod,sep="-")
mod1e <- subset(mod1e, flag_sea==0 & flag_lake==0)


m6.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns((1/speed_ms_s),df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns((1/meanpbl_s),df=3)+ns(ndvi_s,df=3)+as.factor(dust)+as.factor(mm)
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

m1.sc <- lmer(m6.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# R2 = 0.8437266: MUCH BETTER!!!! THIS IS DUE TO RANDOM INT. FOR CELL.


########################################################################################


##### THE LATTER SEEMS TO BE THE BEST MODEL (M6.FORMULA) APPLIED ON MOD1E DATASET
##### I COMPUTE SPATIAL, TEMPORAL AND CV-R2



# R2 = 0.8437266
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# RMSPE = 4.32757
print(rmse(residuals(m1.sc)))


#spatial
spatialall<-mod1e %>%
  group_by(site) %>%
  summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1c, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
# Spatial R2 = 0.9670012
print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
# Spatial RMSPE = 0.755719
print(rmse(residuals(m1.fit.all.s)))

#temporal
tempoall<-left_join(mod1e,spatialall)
tempoall$delpm <-tempoall$pm25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1c-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
# Temporal R2 = 0.824819
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)


#---------------->>>> CV
#s1
splits_s1 <- splitdf(mod1e)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m6.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(mod1e)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m6.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(mod1e)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m6.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(mod1e)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m6.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(mod1e)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m6.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(mod1e)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m6.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(mod1e)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m6.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(mod1e)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m6.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(mod1e)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m6.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(mod1e)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m6.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
mod1.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
# rm(list = ls(pattern = "train_|test_"))

#table updates
m1.fit.all.cv<-lm(pm25~pred.m1.cv,data=mod1.cv)

# CV-R2 = 0.7346305 (old, to be redone)
print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$r.squared)
# CV-I = 0.7190314 (old, to be redone)
print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[1,1])
# CV-Ise = 0.09590654 (old, to be redone)
print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[1,2])
# CV-slope = 0.953322 (old, to be redone)
print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[2,1])
# CV-slope.se = 0.004957467 (old, to be redone)
print(summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[2,2])
# CV-RMSPE = 5.602073 (old, to be redone)
print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-mod1.cv %>%
  group_by(site) %>%
  summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
# Spatial CV-R2 = 0.9602181 (old, to be redone)
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
# Spatial CV-RMSPE = 0.8701468 (old, to be redone)
print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(mod1.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$pm25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
# Temporal CV-R2 = 0.6965603 (old, to be redone)
print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)






#### Try to write a function to do CV in one line:

cv.pm25<-function(data,formula)
{
  #s1
  splits_s1 <- splitdf(data)
  test_s1 <- splits_s1$testset
  train_s1 <- splits_s1$trainset
  out_train_s1 <- lmer(formula, data=train_s1, weights=normwt)
  test_s1$pred.m1.cv <- predict(object=out_train_s1, newdata=test_s1, allow.new.levels=TRUE, re.form=NULL)
  test_s1$iter<-"s1"
  #s2
  splits_s2 <- splitdf(data)
  test_s2 <- splits_s2$testset
  train_s2 <- splits_s2$trainset
  out_train_s2 <- lmer(formula,data =  train_s2,weights=normwt)
  test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
  test_s2$iter<-"s2"
  #s3
  splits_s3 <- splitdf(data)
  test_s3 <- splits_s3$testset
  train_s3 <- splits_s3$trainset
  out_train_s3 <- lmer(formula,data =  train_s3,weights=normwt)
  test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
  test_s3$iter<-"s3"
  #s4
  splits_s4 <- splitdf(data)
  test_s4 <- splits_s4$testset
  train_s4 <- splits_s4$trainset
  out_train_s4 <- lmer(formula,data =  train_s4,weights=normwt)
  test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
  test_s4$iter<-"s4"
  #s5
  splits_s5 <- splitdf(data)
  test_s5 <- splits_s5$testset
  train_s5 <- splits_s5$trainset
  out_train_s5 <- lmer(formula,data =  train_s5,weights=normwt)
  test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
  test_s5$iter<-"s5"
  #s6
  splits_s6 <- splitdf(data)
  test_s6 <- splits_s6$testset
  train_s6 <- splits_s6$trainset
  out_train_s6 <- lmer(formula,data =  train_s6,weights=normwt)
  test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
  test_s6$iter<-"s6"
  #s7
  splits_s7 <- splitdf(data)
  test_s7 <- splits_s7$testset
  train_s7 <- splits_s7$trainset
  out_train_s7 <- lmer(formula,data =  train_s7,weights=normwt)
  test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
  test_s7$iter<-"s7"
  #s8
  splits_s8 <- splitdf(data)
  test_s8 <- splits_s8$testset
  train_s8 <- splits_s8$trainset
  out_train_s8 <- lmer(formula,data =  train_s8,weights=normwt)
  test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
  test_s8$iter<-"s8"
  #s9
  splits_s9 <- splitdf(data)
  test_s9 <- splits_s9$testset
  train_s9 <- splits_s9$trainset
  out_train_s9 <- lmer(formula,data =  train_s9,weights=normwt)
  test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
  test_s9$iter<-"s9"
  #s10
  splits_s10 <- splitdf(data)
  test_s10 <- splits_s10$testset
  train_s10 <- splits_s10$trainset
  out_train_s10 <- lmer(formula,data =  train_s10,weights=normwt)
  test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
  test_s10$iter<-"s10"
  
  #BIND 1 dataset
  mod1.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
  
  #table updates
  m1.fit.all.cv<-lm(pm25~pred.m1.cv, data=mod1.cv)
  
  cv.r2.all <- summary(lm(pm25~pred.m1.cv,data=mod1.cv))$r.squared
  cv.slope  <- summary(lm(pm25~pred.m1.cv,data=mod1.cv))$coef[2,1]
  cv.rmspe  <- rmse(residuals(m1.fit.all.cv))
  
  #spatial
  spatialall.cv<-mod1.cv %>%
    group_by(site) %>%
    summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
  m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
  cv.r2.spatial   <- m1.fit.all.cv.s$r.squared
  cv.rmse.spatial <- rmse(residuals(m1.fit.all.cv.s))
  
  #temporal
  tempoall.cv<-left_join(mod1.cv,spatialall.cv)
  tempoall.cv$delpm <-tempoall.cv$pm25-tempoall.cv$barpm
  tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
  mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
  cv.r2.temporal   <- summary(mod_temporal.cv)$r.squared
  
  #results
  results <- cbind(cv.r2.all, cv.slope, cv.rmspe, cv.r2.spatial, cv.rmse.spatial, cv.r2.temporal)
  print(results)
}
  
cv.pm25(mod1e,m6.formula)




# Compared with M6, try here to replace several of the variables with their inverse

m7.formula <- as.formula(pm25 ~ aod
                         # meteo
                         +ns(temp_c_s,df=3)+ns((1/speed_ms_s),df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns((1/meanpbl_s),df=3)+ns(ndvi_s,df=3)+as.factor(dust)+as.factor(mm)
                         # spatial 1: population, elevation, isa
                         +ns(restot_s,df=3)+ns(elevation_s,df=3)+ns(isa_s,df=3)
                         # spatial 2: point emissions
                         +ns((1/near_emip_s),df=3)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s
                         # spatial 3: areal emissions
                         +so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s
                         # spatial 4: land coverage vars_
                         +pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s
                         # spatial 5: streets vars_
                         +I(1/near_a1_s)+I(1/near_a2_s)+I(1/near_a3_s)+length_a1_s+ns(length_a23_s,df=3)+ns(length_oth_s,df=3)+r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s
                         # spatial 5: other proximity variables
                         +ns((1/near_sea_s),df=3)+ns((1/near_airport_s),df=3)
                         # random component
                         +(1+aod|day/zona)+(1|aodid))

m1.sc <- lmer(m7.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# IT WAS R2 = 0.8437266 with M6.FORMULA
# IT IS  R2 = 0.8438091 with M7.FORMULA


# I leave the inverse of the variables and add interaction between AOD and ZONA

m8.formula <- as.formula(pm25 ~ aod*as.factor(zona)
                         # meteo
                         +ns(temp_c_s,df=3)+ns((1/speed_ms_s),df=3)+ns(visib_km_s,df=3)+ns(rh_s,df=3)
                         # other spatio-temporal  
                         +ns((1/meanpbl_s),df=3)+ns(ndvi_s,df=3)+as.factor(dust)+as.factor(mm)
                         # spatial 1: population, elevation, isa
                         +ns(restot_s,df=3)+ns(elevation_s,df=3)+ns(isa_s,df=3)
                         # spatial 2: point emissions
                         +ns((1/near_emip_s),df=3)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s
                         # spatial 3: areal emissions
                         +so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s
                         # spatial 4: land coverage vars_
                         +pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s
                         # spatial 5: streets vars_
                         +I(1/near_a1_s)+I(1/near_a2_s)+I(1/near_a3_s)+length_a1_s+ns(length_a23_s,df=3)+ns(length_oth_s,df=3)+r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s
                         # spatial 5: other proximity variables
                         +ns((1/near_sea_s),df=3)+ns((1/near_airport_s),df=3)
                         # random component
                         +(1+aod|day/zona)+(1|aodid))

m1.sc <- lmer(m8.formula,data=mod1e,weights=normwt)
mod1e[,pred.m1c := NULL]
mod1e$pred.m1c <- predict(m1.sc)
print(summary(lm(pm25~pred.m1c,data=mod1e))$r.squared)
# IT WAS R2 = 0.8437266 with M6.FORMULA
# IT IS  R2 = 0.8438183 with M8.FORMULA


summary(m1.sc)


# Try Support-vector machine (SVM), already with cross-validation

library(e1071)



try.nocv<-svm(pm25 ~ aod + temp_c_s+I(1/speed_ms_s)+visib_km_s+rh_s+I(1/meanpbl_s)+ndvi_s+as.factor(dust)+as.factor(mm)+
           restot_s+elevation_s+isa_s+I(1/near_emip_s)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s+
           so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s+
           pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s+
           I(1/near_a1_s)+I(1/near_a2_s)+I(1/near_a3_s)+length_a1_s+length_a23_s+length_oth_s+
           r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s+
           I(1/near_sea_s)+I(1/near_airport_s)+as.factor(day)+as.factor(zona),data=mod1e,type="nu-regression")


mod1e[,pred.trynocv := NULL]
mod1e$pred.trynocv <- predict(try.nocv)
print(summary(lm(pm25~pred.trynocv,data=mod1e))$r.squared)
# IT WAS R2 = 0.8438183 with M8.FORMULA
# IT IS  R2 = 0.555614  with TRY (SVM)








try<-svm(pm25 ~ aod + temp_c_s+I(1/speed_ms_s)+visib_km_s+rh_s+I(1/meanpbl_s)+ndvi_s+as.factor(dust)+as.factor(mm)+
                restot_s+elevation_s+isa_s+I(1/near_emip_s)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s+
                so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s+
                pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s+
                I(1/near_a1_s)+I(1/near_a2_s)+I(1/near_a3_s)+length_a1_s+length_a23_s+length_oth_s+
                r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s+
                I(1/near_sea_s)+I(1/near_airport_s)+as.factor(day)+as.factor(zona),data=mod1e,type="nu-regression",cross=10)


mod1e[,pred.try := NULL]
mod1e$pred.try <- predict(try)
print(summary(lm(pm25~pred.try,data=mod1e))$r.squared)
# IT WAS R2 = 0.8438183 with M8.FORMULA
# IT IS  R2 = 0.555614  with TRY (SVM) (this is cross-validated though)

# I try now to tune the parameters 

try2 <- tune.svm(pm25 ~ aod + temp_c_s+I(1/speed_ms_s)+visib_km_s+rh_s+I(1/meanpbl_s)+ndvi_s+as.factor(dust)+as.factor(mm)+
                   restot_s+elevation_s+isa_s+I(1/near_emip_s)+so2_2010p_s+co_2010p_s+nh3_2010p_s+pm10_2010p_s+
                   so2_2010a_s+co_2010a_s+nh3_2010a_s+pm10_2010a_s+
                   pct_deciduous_s+pct_evergreen_s+pct_crop_s+pct_pasture_s+pct_shrub_s+pct_high_dev_s+pct_low_dev_s+
                   I(1/near_a1_s)+I(1/near_a2_s)+I(1/near_a3_s)+length_a1_s+length_a23_s+length_oth_s+
                   r_mean_length_a1_s+r_mean_length_a23_s+r_mean_length_oth_s+
                   I(1/near_sea_s)+I(1/near_airport_s)+as.factor(day)+as.factor(zona),data=mod1e,gamma=seq(0.001,0.007,by=0.001),nu=seq(0.3,0.7,by=0.1))







#mod2
mod2 <- readRDS("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod2.AQ.2010.rds")
names(mod2)
mod2 <- subset(mod2, temp.c.s!="NA" & speed.ms.s!="NA" & rh.s!="NA" & normwt!="NA" & flag_sea==0 & flag_lake==0)
# 28,745,327

setnames(mod2,"temp.c.s","temp_c_s")
setnames(mod2,"speed.ms.s","speed_ms_s")
setnames(mod2,"visib.km.s","visib_km_s")
setnames(mod2,"rh.s","rh_s")
setnames(mod2,"meanpbl.s","meanpbl_s")
setnames(mod2,"ndvi.s","ndvi_s")
setnames(mod2,"restot.s","restot_s")
setnames(mod2,"elevation.s","elevation_s")
setnames(mod2,"isa.s","isa_s")
setnames(mod2,"near.emip.s","near_emip_s")
setnames(mod2,"so2.2010p.s","so2_2010p_s")
setnames(mod2,"co.2010p.s","co_2010p_s")
setnames(mod2,"nh3.2010p.s","nh3_2010p_s")
setnames(mod2,"pm10.2010p.s","pm10_2010p_s")
setnames(mod2,"so2.2010a.s","so2_2010a_s")
setnames(mod2,"co.2010a.s","co_2010a_s")
setnames(mod2,"nh3.2010a.s","nh3_2010a_s")
setnames(mod2,"pm10.2010a.s","pm10_2010a_s")
setnames(mod2,"pct.deciduous.s","pct_deciduous_s")
setnames(mod2,"pct.evergreen.s","pct_evergreen_s")
setnames(mod2,"pct.crop.s","pct_crop_s")
setnames(mod2,"pct.pasture.s","pct_pasture_s")
setnames(mod2,"pct.shrub.s","pct_shrub_s")
setnames(mod2,"pct.high.dev.s","pct_high_dev_s")
setnames(mod2,"pct.low.dev.s","pct_low_dev_s")
setnames(mod2,"near.a1.s","near_a1_s")
setnames(mod2,"near.a2.s","near_a2_s")
setnames(mod2,"near.a3.s","near_a3_s")
setnames(mod2,"length.a1.s","length_a1_s")
setnames(mod2,"length.a23.s","length_a23_s")
setnames(mod2,"length.oth.s","length_oth_s")
setnames(mod2,"r.mean.length.a1.s","r_mean_length_a1_s")
setnames(mod2,"r.mean.length.a23.s","r_mean_length_a23_s")
setnames(mod2,"r.mean.length.oth.s","r_mean_length_oth_s")
setnames(mod2,"near.sea.s","near_sea_s")
setnames(mod2,"near.airport.s","near_airport_s")
names(mod2)

mod2[, pred.m2 := predict(object=m1.sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
summary(mod2$pred.m2)



#delete implossible values
mod2 <- mod2[pred.m2 > 0.00000000000001 , ]
mod2 <- mod2[pred.m2 < 200   , ]
out <- mod2 %>% 
  group_by(aodid) %>% 
  summarise(pred.m2mean = mean(pred.m2),y = mean(lat_aod),x = mean(long_aod))
out<- na.omit(out)
write.csv(out,"G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod2.map.2010.csv")

saveRDS(mod2,"G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod2.AQ.2010.PM25.predm2.rds")
gc()





#mod3

setkey(mod2,day, aodid)
mod2<-mod2[!is.na(meanPM25)]
mod2[, bimon := (mm + 1) %/% 2]
gc()

# make sure there are no missing in meanPM25
mod2 <- select(mod2, !is.na(meanPM25))
mod2 <- select(mod2,day,aodid,mm,meanPM25,long_aod,lat_aod,bimon,pred.m2,aod)
keep(mod2,res,rmse,splitdf, sure=TRUE) 
gc()


m2.smooth = lme(pred.m2 ~ meanPM25,random = list(aodid= ~1 + meanPM25),control=lmeControl(opt = "optim"), data= mod2)
#correlate to see everything from mod2 and the mpm works
mod2[, pred.t31 := predict(m2.smooth)]
mod2[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=mod2))$r.squared)


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(mod2 ,mod2$bimon == "1")
Tall_bimon2 <- subset(mod2 ,mod2$bimon == "2")
Tall_bimon3 <- subset(mod2 ,mod2$bimon == "3")
Tall_bimon4 <- subset(mod2 ,mod2$bimon == "4")
Tall_bimon5 <- subset(mod2 ,mod2$bimon == "5")
Tall_bimon6 <- subset(mod2 ,mod2$bimon == "6")
gc()

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(long_aod,lat_aod),  data= Tall_bimon6 )
gc()
#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
mod2$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(mod2,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM25 ,random = list(aodid= ~1 + meanPM25 ),control=lmeControl(opt = "optim"),data= mod2  )
mod2[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=mod2))$r.squared)
gc()

saveRDS(Final_pred_all,"G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\Final_pred.AQ.PM25.2010.rds")





mod3 <- readRDS("G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod3.AQ.2010.rds")

#for PM25
mod3 <- select(mod3,day,aodid,mm,meanPM25,long_aod,lat_aod)
mod3[, bimon := (mm + 1) %/% 2]
setkey(mod3,day, aodid)
mod3<-mod3[!is.na(meanPM25)]

#generate m.3 initial pred
mod3$pred.m3.mix <-  predict(Final_pred_all,mod3)
gc()
#create unique grid
ugrid <-mod3 %>%
  group_by(aodid) %>%
  summarise(long_aod = mean(long_aod, na.rm=TRUE),  lat_aod = mean(lat_aod, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
mod3_bimon1 <- mod3[bimon == 1, ]
mod3_bimon2 <- mod3[bimon == 2, ]
mod3_bimon3 <- mod3[bimon == 3, ]
mod3_bimon4 <- mod3[bimon == 4, ]
mod3_bimon5 <- mod3[bimon == 5, ]
mod3_bimon6 <- mod3[bimon == 6, ]


#addin unique grid to each bimon           
uniq_gid_bimon1 <- ugrid
uniq_gid_bimon2 <- ugrid
uniq_gid_bimon3 <- ugrid
uniq_gid_bimon4 <- ugrid
uniq_gid_bimon5 <- ugrid
uniq_gid_bimon6 <- ugrid

#get predictions for Bimon residuals
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)



#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,aodid)
setkey(mod3_bimon1,aodid)
mod3_bimon1 <- merge(mod3_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(mod3_bimon2,aodid)
mod3_bimon2 <- merge(mod3_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(mod3_bimon3,aodid)
mod3_bimon3 <- merge(mod3_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(mod3_bimon4,aodid)
mod3_bimon4 <- merge(mod3_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(mod3_bimon5,aodid)
mod3_bimon5 <- merge(mod3_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(mod3_bimon6,aodid)
mod3_bimon6 <- merge(mod3_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(mod3_bimon1,mod3_bimon2,mod3_bimon3,mod3_bimon4,mod3_bimon5,mod3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3  <= 0 , pred.m3  := 0.5]

saveRDS(mod3,"G:\\NEW_Europe_HDF_LAT_LONG\\Itai\\Satellite_SAS\\dati_input\\mod3.AQ.2010.PM25.predm3.rds")
keep(mod3,res,rmse,splitdf, sure=TRUE) 
gc()













mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.predm1.rds")
mod1<-mod1[,c("aodid","day","pm25","pred.m1","stn"),with=FALSE]
#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(pm25~pred.m3,data=mod1))
res[res$type=="pm25", 'm3.R2'] <- print(summary(lm(pm25~pred.m3,data=mod1))$r.squared)    
res[res$type=="pm25", 'm3.I'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[1,1])
res[res$type=="pm25", 'm3.Ise'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[1,2])
res[res$type=="pm25", 'm3.slope'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[2,1])
res[res$type=="pm25", 'm3.slopese'] <-print(summary(lm(pm25~pred.m3,data=mod1))$coef[2,2])
#RMSPE
res[res$type=="pm25", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


#spatial
###to check
spatialall<-mod1 %>%
  group_by(stn) %>%
  summarise(barpm = mean(pm25, na.rm=TRUE), barpred = mean(pred.m3, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="pm25", 'm3.R2.space'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="pm25", 'm3.rmspe.space'] <- print(rmse(residuals(m1.fit.all.spat)))

#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$pm25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m3-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="pm25", 'm3.R2.time'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

saveRDS(res,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/results.AQ.2003.rds")
gc()

### cretae final predictions 
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.PM25.predm2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#----------------> store the best available
mod3best <- mod3[, list(aodid, long_aod, lat_aod, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
mod1$pred.m3<-NULL 
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T, allow.cartesian=T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
summary(mod3best$bestpred)
mod3best[bestpred <= 0 , bestpred  := 0.5]

#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/bestpred.AQ.2003.PM25.rds")
mod3best<-filter(mod3best,!is.na(bestpred))
#save for GIS
write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          long_aod =  long_aod[1], lat_aod = lat_aod[1]),by=aodid], "/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/bestpred.AQ.2003.LTPM.csv", row.names = F)

#export res to csv

write.csv(res,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/results.AQ.2003.csv")

#bestprmap
m3d_agg <- (mod3best[, list(LTPM =mean(bestpred, na.rm = TRUE), 
                            utmx = long_aod[1], #use the first long and lat (by aodid)
                            utmy = lat_aod[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1











