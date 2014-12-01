###############
#LIBS
###############
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

######## import mod1
pm10.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM10all_reg.RDS")
pm25.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM25all_reg.RDS")

#note terra is '0' and aqua is '1'

system.time(pm25.m1[, MaskCloud := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
system.time(pm25.m1[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(pm25.m1[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
system.time(pm25.m1[, MaskGlint := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}))])
system.time(pm25.m1[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])

#note terra is '0' and aqua is '1'

system.time(pm10.m1[, MaskCloud := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
system.time(pm10.m1[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(pm10.m1[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
system.time(pm10.m1[, MaskGlint := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}))])
system.time(pm10.m1[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])


#new seasons
#1-cloudy,#2-noncloud
pm25.m1$cloudIL<-recode(pm25.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")
pm10.m1$cloudIL<-recode(pm10.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")

pm25.m1<-pm25.m1[aod != 'NA']
pm10.m1<-pm10.m1[aod != 'NA']

#clean clean clean
l=seq(names(pm25.m1));names(l)=names(pm25.m1);l
pm25.m1<-pm25.m1[, c(17:33,40,67,68,70:79) := NULL]
pm10.m1<-pm10.m1[, c(17:33,40,67,68,70:79) := NULL]

### subset to aqua and apply alexei cleaning methods
pm25.m1<-pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & c >= 2003] 
pm10.m1<-pm10.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & c >= 2003] 




### base yearly model
x<-  lmer(m1.formula,data=pm25.m1)
pm25.m1$predicted <- predict(x)
glance(lm(PM25~predicted,data=pm25.m1))#0.549


################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(pm25.m1, c( "c","stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
pm25.m1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
pm25.m1 <- pm25.m1[!(pm25.m1$badid %in% bad$badid), ] 

x<-  lmer(m1.formula,data=pm25.m1)
pm25.m1$predicted <- predict(x)
glance(lm(PM25~predicted,data=pm25.m1))#0.70

#get rid of missing
summary(pm25.m1)
x2 <- pm25.m1[is.na(Temp)]
write.csv(x2,"/home/zeltak/ZH_tmp/t2.csv")

#lme mixed model
m1.formula <- as.formula(PM25~ aod+Temp+WS+NO2+Dust+elev+tden+pden+dist2rail+dist2A1+Dist2road+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+(1+aod|day/reg_num))

m1.formula <- as.formula(PM25~ aod+Temp+
                        WS+NO2+Dust+elev+tden+pden+dist2rail+dist2A1+Dist2road+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+
                        (1+aod|day))


m1.formula <- as.formula( log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,2) + ns(speed_ms,2) +
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_restot,2) + log_ndvi + as.factor(elevation_10_cl3) + log_aod:as.factor(elevation_10_cl3) + 
                            as.factor(isa_cl3) + log_aod:as.factor(isa_cl3)+
                            log_near_a1 + log_near_a2 + log_near_a3 + near_airport_1000 + near_port_1000 +
                            length_a1_1000 + ns(length_a23_1000,2) + r_sum_length_a1_1000 + r_sum_length_a23_1000 + 
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + nox_2010p_100 + nh3_2005p + r_sum_nox_2010p_100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                            rh + ns(visib_km,2) + ns(temp_c,2) +
                            (1+aod|day/nome_zona))

