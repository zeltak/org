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
#import grid+mpm
mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")
mpmg <-mpmg[!is.na(bestmpm) , ]
mpmg$month <- as.numeric(format(mpmg$day, "%m"))
#create biomon
mpmg[, bimon := (month + 1) %/% 2]

#import mop2
m2_2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003_pred.m2.rds")
#get names order
#l=seq(names(m2_2003));names(l)=names(m2_2003);l
m2_2003s<-m2_2003[,c(1,2,5,7,8,9,56),with=FALSE]
#remove uneeded files and save
#merge with mod2 predicted data to create mod2
setkey(m2_2003s,day, guid)
setkey(mpmg,day, guid)
m2mpm <- merge(m2_2003s, mpmg)
names(m2mpm)
m2mpm[,m:=NULL]
m2mpm[,lat_aod.y:=NULL]
m2mpm[,long_aod.y:=NULL]
setnames(m2mpm,"lat_aod.x","lat_aod")
setnames(m2mpm,"long_aod.x","long_aod")

#############saving point
saveRDS(m2mpm, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2mpm_2003.rds")
#clean
keep(m2mpm,mpmg, sure=TRUE) 
gc()



###############
#Mod3
###############
mod1table<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")
#run the lmer part regressing stage 2 pred Vs mean pm
m2.smooth = lme(predicted.m2 ~ bestmpm*as.factor(bimon),random = list(guid= ~1 + bestmpm),control=lmeControl(opt = "optim"),data= m2mpm )

#correlate to see everything from mod2 and the mpm works
m2mpm$tpred <- predict(m2.smooth)
mod3a_reg<-lm(m2mpm$predicted~m2mpm$tpred)
mod1table$r2003[21] <-summary(mod3a_reg)$r.squared

saveRDS(m2.smooth, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/m2.smooth_2003.rds")
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")

#get the residuals from the above fit
m2mpm$resid   <- residuals(m2.smooth)

#split the files to the separate bi monthly datsets
T2003_bimon1 <- subset(m2mpm ,m2mpm$bimon == "1")
T2003_bimon2 <- subset(m2mpm ,m2mpm$bimon == "2")
T2003_bimon3 <- subset(m2mpm ,m2mpm$bimon == "3")
T2003_bimon4 <- subset(m2mpm ,m2mpm$bimon == "4")
T2003_bimon5 <- subset(m2mpm ,m2mpm$bimon == "5")
T2003_bimon6 <- subset(m2mpm ,m2mpm$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon1 )
fit2_2 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon2 )
fit2_3 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon3 )
fit2_4 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon4 )
fit2_5 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon5 )
fit2_6 = gam(resid ~ s(long_aod,lat_aod),  data= T2003_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2003_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2003_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2003_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2003_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2003_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2003_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
m2mpm$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2003  = lme(newpred ~ bestmpm ,random = list(guid= ~1 + bestmpm ),control=lmeControl(opt = "optim"),data= m2mpm  )

#check correlations
m2mpm$tpred2 <- predict(Final_pred_2003)

mod3b_reg<-lm(m2mpm$predicted~m2mpm$tpred2)
mod1table$r2003[22] <-summary(mod3b_reg)$r.squared
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")

#############saving point
saveRDS(Final_pred_2003 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2003.rds")
saveRDS(fit2_1 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2003.rds")
saveRDS(fit2_2 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2003.rds")
saveRDS(fit2_3 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2003.rds")
saveRDS(fit2_4 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2003.rds")
saveRDS(fit2_5 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2003.rds")
saveRDS(fit2_6 , "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2003.rds")


#clean
keep(Final_pred_2003,mpmg,mod1table,fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6, sure=TRUE) 
gc()


###in case of emergency..break glass...
# Final_pred_2003<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2003.rds")
# fit2_1<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2003.rds")
# fit2_2<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2003.rds")
# fit2_3<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2003.rds")
# fit2_4<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2003.rds")
# fit2_5<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2003.rds")
# fit2_6<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2003.rds")


###############
#create full mod 3
###############

################
#### PREDICT for all daily grids for study area (for mixed model part)
###############
#mpmg$mixpred<-  predict(Final_pred_2003,mpmg)
mpmg.seT1<-mpmg[1:20000000,]
mpmg.seT1$mixpred<-  predict(Final_pred_2003,mpmg.seT1)
mpmg.seT2<-mpmg[20000001:40000000,]
mpmg.seT2$mixpred<-  predict(Final_pred_2003,mpmg.seT2)
mpmg.seT3<-mpmg[40000001:60000000,]
mpmg.seT3$mixpred<-  predict(Final_pred_2003,mpmg.seT3)
cc<-dim(mpmg)
mpmg.seT4<-mpmg[60000001:cc[1],]
mpmg.seT4$mixpred<-  predict(Final_pred_2003,mpmg.seT4)
mpmg<-rbind(mpmg.seT1,mpmg.seT2,mpmg.seT3,mpmg.seT4)

saveRDS(mpmg, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mpmg_Temp2003.rds")
rm(mpmg.seT1)
rm(mpmg.seT2)
rm(mpmg.seT3)
rm(mpmg.seT4)
gc()

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
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
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
# create predicted.m3
mod3$predicted.m3 <-mod3$mixpred+mod3$gpred
#summary(mod3$predicted.m3)
#describe(mod3$predicted.m3)
#recode negative into zero
mod3 <- mod3[predicted.m3 >= 0]
hist(mod3$predicted.m3)
saveRDS(mod3,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m3_2003.pred3.rds")
#clean
keep(mod3, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
mod3[,lat_aod.y:=NULL]
mod3[,long_aod.y:=NULL]
setnames(mod3,"lat_aod.x","lat_aod")
setnames(mod3,"long_aod.x","long_aod")
#subset mod3
mod3<-mod3[,c("guid","day","lat_aod","long_aod","predicted.m3"),with=FALSE]

#load mod1
mod1table<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("guid","day","PM25","predicted"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")

#R2.m3
setkey(mod3,day,guid)
setkey(mod1,day,guid)
mod1 <- merge(mod1,mod3[, list(day,guid,predicted.m3)], all.x = T)  			
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2003[23] <-summary(mod3d_reg)$r.squared
saveRDS(mod1table, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p3.rds.rds")

#########################
#import mod2
mod2<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003_pred.m2.rds")
mod2<-mod2[,c("guid","day","predicted.m2"),with=FALSE]

#########################
# store the best available
mod3best <- mod3[, list(guid, long_aod, lat_aod, day, predicted.m3)]
setkey(mod3best, day, guid)
setkey(mod2, day, guid)
mod3best <- merge(mod3best, mod2[,list(guid, day, predicted.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("guid","day","PM25","predicted"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")
setkey(mod1,day,guid)
mod3best <- merge(mod3best, mod1, allow.cartesian=TRUE,all.x = T)
mod3best[,bestpred := predicted.m3]
mod3best[!is.na(predicted.m2),bestpred := predicted.m2]
mod3best[!is.na(predicted.m1),bestpred := predicted.m1]
#save
saveRDS(mod3best,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2003.rds")


#map the predictions
#aggregate by guid
m3d_agg <- (mod3[, list(LTPM =mean(predicted.m3, na.rm = TRUE), 
                        long_aod = long_aod[1], #use the first long and lat (by guid)
                        lat_aod = lat_aod[1]),by = guid])

# plot
# ggplot(m3d_agg, aes(long_aod, lat_aod, color = LTPM)) + 
#   geom_point(size = 3, shape = 15) + 
#   #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
#   xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
#   scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(15)) + #c("purple", "blue", "white", "red", "orange")) + 
#   theme_bw() + 
#   ggtitle("Long term predictions")



write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(predicted.m1)),
                          npred.m2 = sum(!is.na(predicted.m2)),
                          npred.m3 = sum(!is.na(predicted.m3)),
                          long_aod =  long_aod[1], lat_aod = lat_aod[1]),by=guid], "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/pestpred2003LPM.csv", row.names = F)


























