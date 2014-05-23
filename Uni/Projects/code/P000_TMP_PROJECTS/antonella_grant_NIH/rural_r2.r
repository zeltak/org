mod1CV_all <- readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1CV_all_2003_pred.m1.rds")

mod1d<-mod1CV_all

#urban
m3pred2005_urb <- mod1d[pcturb_1km  > 22.2222, ]
m3pred2005_rural <- mod1d[pcturb_1km  <=50, ]

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))

#full model 1
out.m1_2003 = lmer(m1.formula ,data =  m3pred2005_rural)
#generate prediction
m3pred2005_rural$predicted <- predict(out.m1_2003)
#get overall R2
mod1_reg <- lm(m3pred2005_rural$PM25~m3pred2005_rural$predicted)

summary(mod1_reg)$r.squared
summary(mod1_reg)$coef[2,1]
#rmspe
sqrt(mean(mod1_reg$residual^2))
