
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


#
gc()

#import and clip data
mod1<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2005.rds")
mod2<-readRDS ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2005.rds")

#clip to just NE and NY/NJ
mod2C <- mod2[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]
mod1C <- mod1[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]




###############
#MOD1
###############

###############
#switch to choose all area or cliped area for paper
m1_2005<-mod1C
#m1_2005<-mod1
###############


#clean data and exclude bad values
m1_2005$logroad<-log(m1_2005$Mjrrdden_1 +.1)

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))

#full model 1
out.m1_2005 = lmer(m1.formula ,data =  m1_2005)



###############
#MOD2
###############

###############
#switch to choose all area or cliped area for paper
m2_2005<-mod2C
#m2_2005<-mod2
###############
m2_2005$logroad<-log(m2_2005$Mjrrdden_1 +.1)
#generate predictions
#m2_2005$predicted <- predict(object=out.m1_2005,newdata=m2_2005,allow.new.levels=TRUE,REform=NULL )
m2_2005[, predicted.m2 := predict(object=out.m1_2005,newdata=m2_2005,allow.new.levels=TRUE,REform=NULL)]
m2_2005 <- m2_2005[predicted.m2 > 0.00000000000001 , ]
#save mod2 with predictions
saveRDS(m2_2005, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2005_pred.m2.rds")


#map the predictions
#aggregate by guid
m2_agg <- m2_2005[, list(LTPM.m2 = mean(predicted.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = guid]
saveRDS(m2_agg, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2005.rds")
#map the predictions
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM2005.m2.png")


cbind(sapply(ls(), function(x){
  object.size(get(x))
}))



