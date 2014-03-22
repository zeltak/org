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


##############################
#MeanPM calculations
##############################
mod1table<-readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table.rds")
mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2010.rds")
m2_2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2010_pred.m2.rds")
m2_2010s<-m2_2010[,c(1,2,5,7,8,9,56),with=FALSE]

mpmg$month <- as.numeric(format(mpmg$day, "%m"))
#create biomon
mpmg[, bimon := (month + 1) %/% 2]
#remove uneeded files and save
#merge with mod2 predicted data to create mod2
setkey(m2_2010s,day, guid)
setkey(mpmg,day, guid)
m2mpm <- merge(m2_2010s, mpmg)
names(m2mpm)
m2mpm[,m:=NULL]


#############saving point
saveRDS(m2mpm, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2mpm_2010.rds")
#clean
keep(m2mpm,mod1table,mpmg, sure=TRUE) 
gc()



###############
#Mod3
###############

#run the lmer part regressing stage 2 pred Vs mean pm
m2.smooth = lme(predicted.m2 ~ meanPMmean*as.factor(bimon),random = list(guid= ~1 + meanPMmean),control=lmeControl(opt = "optim"),data= m2mpm )

#correlate to see everything from mod2 and the mpm works
m2mpm$tpred <- predict(m2.smooth)
mod3a_reg<-lm(m2mpm$predicted~m2mpm$tpred)
mod1table$r2010[21] <-summary(mod3a_reg)$r.squared

saveRDS(m2.smooth, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2.smooth_2010.rds")
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")

#get the residuals from the above fit
m2mpm$resid   <- residuals(m2.smooth)

#split the files to the separate bi monthly datsets
T2010_bimon1 <- subset(m2mpm ,m2mpm$bimon == "1")
T2010_bimon2 <- subset(m2mpm ,m2mpm$bimon == "2")
T2010_bimon3 <- subset(m2mpm ,m2mpm$bimon == "3")
T2010_bimon4 <- subset(m2mpm ,m2mpm$bimon == "4")
T2010_bimon5 <- subset(m2mpm ,m2mpm$bimon == "5")
T2010_bimon6 <- subset(m2mpm ,m2mpm$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon1 )
fit2_2 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon2 )
fit2_3 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon3 )
fit2_4 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon4 )
fit2_5 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon5 )
fit2_6 = gam(resid ~ s(long_aod,lat_aod),  data= T2010_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2010_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2010_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2010_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2010_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2010_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2010_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
m2mpm$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2010  = lme(newpred ~ meanPMmean ,random = list(guid= ~1 + meanPMmean ),control=lmeControl(opt = "optim"),data= m2mpm  )

#check correlations
m2mpm$tpred2 <- predict(Final_pred_2010)

mod3b_reg<-lm(m2mpm$predicted~m2mpm$tpred2)
mod1table$r2010[22] <-summary(mod3b_reg)$r.squared

#############saving point
saveRDS(Final_pred_2010 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2010.rds")
saveRDS(fit2_1 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2010.rds")
saveRDS(fit2_2 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2010.rds")
saveRDS(fit2_3 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2010.rds")
saveRDS(fit2_4 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2010.rds")
saveRDS(fit2_5 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2010.rds")
saveRDS(fit2_6 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2010.rds")

saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")

#clean
keep(Final_pred_2010,m1_2010,mpmg,mod1table,fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6, sure=TRUE) 
gc()


###in case of emergency..break glass...
# mod1table<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
# Final_pred_2010<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2010.rds")
# fit2_1<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2010.rds")
# fit2_2<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2010.rds")
# fit2_3<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2010.rds")
# fit2_4<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2010.rds")
# fit2_5<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2010.rds")
# fit2_6<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2010.rds")


###############
#create full mod 3
###############

################
#### PREDICT for all daily grids for study area (for mixed model part)
###############
#mpmg$mixpred<-  predict(Final_pred_2010,mpmg)
mpmg.seT1<-mpmg[1:20000000,]
mpmg.seT1$mixpred<-  predict(Final_pred_2010,mpmg.seT1)
mpmg.seT2<-mpmg[20000001:40000000,]
mpmg.seT2$mixpred<-  predict(Final_pred_2010,mpmg.seT2)
mpmg.seT3<-mpmg[40000001:60000000,]
mpmg.seT3$mixpred<-  predict(Final_pred_2010,mpmg.seT3)
cc<-dim(mpmg)
mpmg.seT4<-mpmg[60000001:cc[1],]
mpmg.seT4$mixpred<-  predict(Final_pred_2010,mpmg.seT4)
mpmg<-rbind(mpmg.seT1,mpmg.seT2,mpmg.seT3,mpmg.seT4)

################
#### PREDICT Gam part
###############

#split back into bimons to include the gam prediction in final prediction    		

mpmg_bimon1 <- mpmg[bimon == 1, ]
mpmg_bimon2 <- mpmg[bimon == 2, ]
mpmg_bimon3 <- mpmg[bimon == 3, ]
mpmg_bimon4 <- mpmg[bimon == 4, ]
mpmg_bimon5 <- mpmg[bimon == 5, ]
mpmg_bimon6 <- mpmg[bimon == 6, ]

if(!exists("m2_agg")){
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2010.rds")
}

m2_agg[,LTPM.m2:=NULL]

#addin unique grid to each bimon           
uniq_gid_bimon1 <- m2_agg
uniq_gid_bimon2 <- m2_agg
uniq_gid_bimon3 <- m2_agg
uniq_gid_bimon4 <- m2_agg
uniq_gid_bimon5 <- m2_agg
uniq_gid_bimon6 <- m2_agg

#get predictions for Bimon residuals

uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)

#clean
keep(uniq_gid_bimon1,uniq_gid_bimon2,uniq_gid_bimon3,uniq_gid_bimon4,uniq_gid_bimon5,uniq_gid_bimon6,mpmg_bimon1,mpmg_bimon2,mpmg_bimon3,mpmg_bimon4,mpmg_bimon5,mpmg_bimon6, sure=TRUE) 
gc()
#merge things back togheter
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges
setkey(uniq_gid_bimon1,guid)
setkey(mpmg_bimon1,guid)
mpmg_bimon1 <- merge(mpmg_bimon1, uniq_gid_bimon1, all.x = T)

setkey(uniq_gid_bimon2,guid)
setkey(mpmg_bimon2,guid)
mpmg_bimon2 <- merge(mpmg_bimon2, uniq_gid_bimon2, all.x = T)

setkey(uniq_gid_bimon3,guid)
setkey(mpmg_bimon3,guid)
mpmg_bimon3 <- merge(mpmg_bimon3, uniq_gid_bimon3, all.x = T)

setkey(uniq_gid_bimon4,guid)
setkey(mpmg_bimon4,guid)
mpmg_bimon4 <- merge(mpmg_bimon4, uniq_gid_bimon4, all.x = T)

setkey(uniq_gid_bimon5,guid)
setkey(mpmg_bimon5,guid)
mpmg_bimon5 <- merge(mpmg_bimon5, uniq_gid_bimon5, all.x = T)

setkey(uniq_gid_bimon6,guid)
setkey(mpmg_bimon6,guid)
mpmg_bimon6 <- merge(mpmg_bimon6, uniq_gid_bimon6, all.x = T)

#reattach all parts        
mod3 <- rbind(mpmg_bimon1,mpmg_bimon2,mpmg_bimon3,mpmg_bimon4,mpmg_bimon5,mpmg_bimon6)
# create PM_mod3
mod3$pm_mod3 <-mod3$mixpred+mod3$gpred
summary(mod3$pm_mod3)
describe(mod3$pm_mod3)
#recode negative into zero
mod3 <- subset(mod3,mod3$pm_mod3 >= "0")
hist(mod3$pm_mod3)
saveRDS(mod3,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m32010.pred3.rds")

#clean
keep(mod3, sure=TRUE) 
gc()

#R2 for mod3
#load mod1
mod1table<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table_Temp.rds")
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2010_pred.m1.rds")
mod1<-mod1[,c(1,2,3,7,55),with=FALSE]
setnames(mod1,"predicted","predicted.m1")

# m3<- mod3[mod3$guid %in% mod1$guid, ]        
# m3<-data.table(m3)
setkey(mod3,day,guid)
setkey(mod1,day,guid)
mod1 <- merge(mod1,mod3[, list(day,guid,pm_mod3)], all.x = T)  			
mod3d_reg <- lm(PM25~pm_mod3,mod1)
mod1table$r2010[23] <-summary(mod3d_reg)$r.squared
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2010_p3.rds.rds")


#import mod2
mod2<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2010_pred.m2.rds")
mod2<-mod2[,c(1,2,56),with=FALSE]

# store the best available
mod3best <- mod3[, list(guid, long_aod, lat_aod, day, pm_mod3)]
setkey(mod3best, day, guid)
setkey(mod2, day, guid)
mod3best <- merge(mod3best, mod2[,list(guid, day, predicted.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2011_pred.m1.rds")
mod1<-mod1[,c("guid","day","PM25","predicted"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")

setkey(mod1,day,guid)
mod3best <- merge(mod3best, mod1, allow.cartesian=TRUE,all.x = T)
head(mod3best,1)
mod3best[,bestpred := pm_mod3]
mod3best[!is.na(predicted.m2),bestpred := predicted.m2]
mod3best[!is.na(predicted.m1),bestpred := predicted.m1]
#save
saveRDS(mod3best,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2010.rds")


#map the predictions
#aggregate by guid
m3d_agg <- (mod3[, list(LTPM =mean(pm_mod3, na.rm = TRUE), 
                        long_aod = long_aod[1], #use the first long and lat (by guid)
                        lat_aod = lat_aod[1]),by = guid])

#plot
ggplot(m3d_agg, aes(long_aod, lat_aod, color = LTPM)) + 
  geom_point(size = 5, shape = 15) + 
  #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")

write.csv(m3d_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m3d_agg_2010.csv")

# last_plot() %+% mod3best[, list(LTPM = mean(bestpred, na.rm = T), long_aod = long_aod[1], #use the first long and lat (by guid)
#                                 lat_aod = lat_aod[1]) ,by = guid]  
# 
# write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
#                           predvariance = var(bestpred, na.rm = T),
#                           predmin = min(bestpred, na.rm = T),
#                           predmax = max(bestpred, na.rm = T),
#                           npred = sum(!is.na(bestpred)),
#                           npred.m1 = sum(!is.na(predicted.m1)),
#                           npred.m2 = sum(!is.na(predicted.m2)),
#                           npred.m3 = sum(!is.na(pm_mod3)),
#                           elev =  elev[1], long_aod =  long_aod[1], lat_aod = lat_aod[1]),by=guid], 
#           paste0(path.data, "mod3best_2011summary_", Sys.Date(), ".csv"), row.names = F)
# 
























