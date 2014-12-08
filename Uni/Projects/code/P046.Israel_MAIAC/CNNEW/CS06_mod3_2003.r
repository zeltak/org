###############
#LIBS
###############
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
library(splines)


sink("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/sink_mod3_2003.txt", type = c("output", "message"))


##############################
#MeanPM calculations
##############################
# #import grid+mpm
# mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/bestmpm2003.rds")
# mpmg <-mpmg[!is.na(bestmpm) , ]
# mpmg$month <- as.numeric(format(mpmg$day, "%m"))
# #create biomon
# mpmg[, bimon := (month + 1) %/% 2]
# #get names order
# #l=seq(names(m2.2003));names(l)=names(m2.2003);l
# # m2.2003s<-m2.2003[,c(1,2,5,7,8,9,56),with=FALSE]
# #remove uneeded files and save
# #merge with mod2 predicted data to create mod2
# setkey(m2.2003s,day, aodid)
# setkey(mpmg,day, aodid)
# m2.2003 <- merge(m2.2003s, mpmg)
# names(m2.2003)
# m2.2003[,m:=NULL]
# m2.2003[,y_aod_ITM.y:=NULL]
# m2.2003[,x_aod_ITM.y:=NULL]
# setnames(m2.2003,"y_aod_ITM.x","y_aod_ITM")
# setnames(m2.2003,"x_aod_ITM.x","x_aod_ITM")

#import mod2
m2.2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")
m2.2003[, bimon := (m + 1) %/% 2]

###############
#Mod3
###############
mod1table<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")

#run the lmer part regressing stage 2 pred Vs mean pm
m2.smooth = lme(pred.m2 ~ meanPM25mean,random = list(aodid= ~1 + meanPM25mean),control=lmeControl(opt = "optim"), data= m2.2003 )
#correlate to see everything from mod2 and the mpm works
m2.2003[, pred.t31 := predict(m2.smooth)]
m2.2003[, resid  := residuals(m2.smooth)]

mod1table$r2003[21] <-summary(lm(m2.2003$pred.m2~m2.2003$pred.t31))$r.squared

#split the files to the separate bi monthly datsets
T2003_bimon1 <- subset(m2.2003 ,m2.2003$bimon == "1")
T2003_bimon2 <- subset(m2.2003 ,m2.2003$bimon == "2")
T2003_bimon3 <- subset(m2.2003 ,m2.2003$bimon == "3")
T2003_bimon4 <- subset(m2.2003 ,m2.2003$bimon == "4")
T2003_bimon5 <- subset(m2.2003 ,m2.2003$bimon == "5")
T2003_bimon6 <- subset(m2.2003 ,m2.2003$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon1 )
fit2_2 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon2 )
fit2_3 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon3 )
fit2_4 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon4 )
fit2_5 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon5 )
fit2_6 = gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2003_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2003_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2=(T2003_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3=(T2003_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4=(T2003_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5=(T2003_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6=(T2003_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2003$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2003 <- lme(pred.t32 ~ meanPM25mean ,random = list(aodid= ~1 + meanPM25mean ),control=lmeControl(opt = "optim"),data= m2.2003  )
m2.2003[, pred.t32 := predict(Final_pred_2003)]
#check correlations
mod3b_reg<-lm(m2.2003$pred.m2 ~m2.2003$pred.t32)
mod1table$r2003[22] <-summary(mod3b_reg)$r.squared
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")

#############saving point
saveRDS(Final_pred_2003 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2003.rds")
saveRDS(fit2_1 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2003.rds")
saveRDS(fit2_2 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2003.rds")
saveRDS(fit2_3 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2003.rds")
saveRDS(fit2_4 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2003.rds")
saveRDS(fit2_5 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2003.rds")
saveRDS(fit2_6 , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2003.rds")


#clean
keep(Final_pred_2003,mpmg,mod1table, sure=TRUE) 
gc()

###############
#create full mod 3
###############

################
#### PREDICT for all daily grids for study area (for mixed model part)
###############

mpmg.seT1<-mpmg[1:20000000,]
mpmg.seT1$mixpred<-  predict(Final_pred_2003,mpmg.seT1)
mpmg.seT2<-mpmg[20000001:40000000,]
mpmg.seT2$mixpred<-  predict(Final_pred_2003,mpmg.seT2)
mpmg.seT3<-mpmg[40000001:60000000,]
mpmg.seT3$mixpred<-  predict(Final_pred_2003,mpmg.seT3)
mpmg.seT4<-mpmg[60000001:80000000,]
mpmg.seT4$mixpred<-  predict(Final_pred_2003,mpmg.seT4)
gc()
cc<-dim(mpmg)
mpmg.seT5<-mpmg[80000001:cc[1],]
mpmg.seT5$mixpred<-  predict(Final_pred_2003,mpmg.seT5)
mpmg<-rbind(mpmg.seT1,mpmg.seT2,mpmg.seT3,mpmg.seT4,mpmg.seT5)

saveRDS(mpmg, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mpmg_Temp2003.rds")
rm(mpmg.seT1)
rm(mpmg.seT2)
rm(mpmg.seT3)
rm(mpmg.seT4)
gc()

###in case of emergency..break glass...
# Final_pred_2003<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Final_pred_2003.rds")
fit2_1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_1_2003.rds")
fit2_2<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_2_2003.rds")
fit2_3<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_3_2003.rds")
fit2_4<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_4_2003.rds")
fit2_5<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_5_2003.rds")
fit2_6<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/fit2_6_2003.rds")



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
setkey(uniq_gid_bimon1,aodid)
setkey(mpmg_bimon1,aodid)
mpmg_bimon1 <- merge(mpmg_bimon1, uniq_gid_bimon1, all.x = T)

setkey(uniq_gid_bimon2,aodid)
setkey(mpmg_bimon2,aodid)
mpmg_bimon2 <- merge(mpmg_bimon2, uniq_gid_bimon2, all.x = T)

setkey(uniq_gid_bimon3,aodid)
setkey(mpmg_bimon3,aodid)
mpmg_bimon3 <- merge(mpmg_bimon3, uniq_gid_bimon3, all.x = T)

setkey(uniq_gid_bimon4,aodid)
setkey(mpmg_bimon4,aodid)
mpmg_bimon4 <- merge(mpmg_bimon4, uniq_gid_bimon4, all.x = T)

setkey(uniq_gid_bimon5,aodid)
setkey(mpmg_bimon5,aodid)
mpmg_bimon5 <- merge(mpmg_bimon5, uniq_gid_bimon5, all.x = T)

setkey(uniq_gid_bimon6,aodid)
setkey(mpmg_bimon6,aodid)
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
mod3[,y_aod_ITM.y:=NULL]
mod3[,x_aod_ITM.y:=NULL]
setnames(mod3,"y_aod_ITM.x","y_aod_ITM")
setnames(mod3,"x_aod_ITM.x","x_aod_ITM")
#subset mod3
mod3<-mod3[,c("aodid","day","y_aod_ITM","x_aod_ITM","predicted.m3"),with=FALSE]

#load mod1
mod1table<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("aodid","day","PM25","predicted","SiteCode"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")

#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,predicted.m3)], all.x = T)  			
mod3d_reg <- lm(PM25~predicted.m3,data=mod1)
mod1table$r2003[23] <-summary(mod3d_reg)$r.squared
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p3.rds.rds")
saveRDS(mod1, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2003w_p.m3.rds")


#########################
#import mod2
mod2<- readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003_pred.m2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#########################
# store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, predicted.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("aodid","day","PM25","predicted"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, allow.cartesian=TRUE,all.x = T)
mod3best[,bestpred := predicted.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(predicted.m1),bestpred := predicted.m1]
#save
saveRDS(mod3best,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2003.rds")


#map the predictions
#aggregate by aodid
m3d_agg <- (mod3[, list(LTPM =mean(predicted.m3, na.rm = TRUE), 
                        x_aod_ITM = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y_aod_ITM = y_aod_ITM[1]),by = aodid])

# plot
# ggplot(m3d_agg, aes(x_aod_ITM, y_aod_ITM, color = LTPM)) + 
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
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(predicted.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/pestpred2003LPM.csv", row.names = F)

























