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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod1C.AQ.PM10.rds")
m1.all$aodid<-paste(m1.all$long_aod,m1.all$lat_aod,sep="-")
badaod<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/bad_AOD_IL.csv")
badaod<-select(badaod,aodid)
m1.all <- m1.all[!m1.all$aodid %in% badaod$aodid]

#BEST SO FAR
m1.formula <- as.formula(PM10~ aod
                      +(1+aod|day)) 

m1_sc <- lmer(m1.formula,data=m1.all,weights=normwt)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM10~pred.m1,data=m1.all))$r.squared)

# load mod2 if not already loaded
if(!exists("m2.all")){
m2.all <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod2.AQ.rds")
m2.all <- m2.all[!m2.all$aodid %in% badaod$aodid]
}

#2003
m2.all.2003<-m2.all[c==2003]
#generate predictions
m2.all.2003[, pred.m2 := predict(object=m1_sc,newdata=m2.all.2003,allow.new.levels=TRUE,re.form=NULL)]
#delete implossible values
m2.all.2003 <- m2.all.2003[pred.m2 > 0.00000000000001 , ]
m2.all.2003 <- m2.all.2003[pred.m2 < 200   , ]
#predperyear
m3d_agg <- (m2.all.2003[, list(LTPM =mean(pred.m2, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.2003.csv")



#2012
m2.all.2012<-m2.all[c==2012]
#generate predictions
m2.all.2012[, pred.m2 := predict(object=m1_sc,newdata=m2.all.2012,allow.new.levels=TRUE,re.form=NULL)]
#delete implossible values
m2.all.2012 <- m2.all.2012[pred.m2 > 0.00000000000001 , ]
m2.all.2012 <- m2.all.2012[pred.m2 < 200   , ]
#predperyear
m3d_agg <- (m2.all.2012[, list(LTPM =mean(pred.m2, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans2012.csv")




#2003

#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.all.2003 )
#correlate to see everything from mod2 and the mpm works
m2.all.2003[, pred.t31 := predict(m2.smooth)]
m2.all.2003[, resid  := residuals(m2.smooth)]
print(summary(lm(pred.m2~pred.t31,data=m2.all.2003))$r.squared)


#map
m3d_agg <- (m2.all.2003[, list(LTPM =mean(pred.t31, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
m3d_agg<-filter(m3d_agg,!is.na(LTPM))
write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.csv")

#-------------->prepare for mod3
m2.all.2003[, bimon := (m + 1) %/% 2]


#split the files to the separate bi monthly datsets
Tall_bimon1 <- subset(m2.all.2003 ,m2.all.2003$bimon == "1")
Tall_bimon2 <- subset(m2.all.2003 ,m2.all.2003$bimon == "2")
Tall_bimon3 <- subset(m2.all.2003 ,m2.all.2003$bimon == "3")
Tall_bimon4 <- subset(m2.all.2003 ,m2.all.2003$bimon == "4")
Tall_bimon5 <- subset(m2.all.2003 ,m2.all.2003$bimon == "5")
Tall_bimon6 <- subset(m2.all.2003 ,m2.all.2003$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= Tall_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (Tall_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (Tall_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (Tall_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (Tall_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (Tall_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (Tall_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.all.2003$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.all.2003,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_all <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.all.2003  )
m2.all.2003[, pred.t33 := predict(Final_pred_all)]
#check correlations
res[res$type=="PM25", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.all.2003))$r.squared) 
# 
#map
m3d_agg <- (m2.all.2003[, list(LTPM =mean(pred.t33, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1
m3d_agg<-filter(m3d_agg,!is.na(LTPM))
write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.csv")





#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2003 <-data.m3[c ==2003]
#for PM25
data.m3.2003 <- select(data.m3.2003,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2003[, bimon := (m + 1) %/% 2]
setkey(data.m3.2003,day, aodid)
data.m3.2003<-data.m3.2003[!is.na(meanPM)]
#generate m.3 initial pred
data.m3.2003$pred.m3.mix <-  predict(Final_pred_all,data.m3.2003)

#create unique grid
ugrid <-data.m3.2003 %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2003_bimon1 <- data.m3.2003[bimon == 1, ]
data.m3.2003_bimon2 <- data.m3.2003[bimon == 2, ]
data.m3.2003_bimon3 <- data.m3.2003[bimon == 3, ]
data.m3.2003_bimon4 <- data.m3.2003[bimon == 4, ]
data.m3.2003_bimon5 <- data.m3.2003[bimon == 5, ]
data.m3.2003_bimon6 <- data.m3.2003[bimon == 6, ]


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
setkey(data.m3.2003_bimon1,aodid)
data.m3.2003_bimon1 <- merge(data.m3.2003_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2003_bimon2,aodid)
data.m3.2003_bimon2 <- merge(data.m3.2003_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2003_bimon3,aodid)
data.m3.2003_bimon3 <- merge(data.m3.2003_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2003_bimon4,aodid)
data.m3.2003_bimon4 <- merge(data.m3.2003_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2003_bimon5,aodid)
data.m3.2003_bimon5 <- merge(data.m3.2003_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2003_bimon6,aodid)
data.m3.2003_bimon6 <- merge(data.m3.2003_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2003_bimon1,data.m3.2003_bimon2,data.m3.2003_bimon3,data.m3.2003_bimon4,data.m3.2003_bimon5,data.m3.2003_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
#mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/mod3.AQ.PM25.2003.pred3.rds")

mod3<-filter(mod3,!is.na(pred.m3))
#predperyear
m3d_agg <- (mod3[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
                        x = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(x, y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

write.csv(m3d_agg,"~/ZH_tmp/R.tmp/qtrans.csv")
