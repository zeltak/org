#LIBS
library(lme4); library(reshape); library(foreign) ; library(ggplot2); library(plyr); library(data.table); library(reshape2); library(Hmisc); library(mgcv);
library(gdata); library(car); library(dplyr); library(ggmap); library(broom);library(splines);


#sink("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/sink_mod3_2007.txt", type = c("output", "message"))

#import mod2
m2.2007<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.pred2.rds")
m2.2007[, bimon := (m + 1) %/% 2]
setkey(m2.2007,day, aodid)
m2.2007<-m2.2007[!is.na(meanPM)]

###############
#Mod3
###############
res<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resALL.rds")

#run the lmer part regressing stage 2 pred Vs mean pm
#in israel check per month, also check 30km band and other methods for meanpm
m2.smooth = lme(pred.m2 ~ meanPM,random = list(aodid= ~1 + meanPM),control=lmeControl(opt = "optim"), data= m2.2007 )
#xm2.smooth = lmer(pred.m2 ~ meanPM+(1+ meanPM|aodid), data= m2.2007 )
#correlate to see everything from mod2 and the mpm works
m2.2007[, pred.t31 := predict(m2.smooth)]
m2.2007[, resid  := residuals(m2.smooth)]
res[res$year=="2007", 'm3.t31'] <- print(summary(lm(pred.m2~pred.t31,data=m2.2007))$r.squared)


#split the files to the separate bi monthly datsets
T2007_bimon1 <- subset(m2.2007 ,m2.2007$bimon == "1")
T2007_bimon2 <- subset(m2.2007 ,m2.2007$bimon == "2")
T2007_bimon3 <- subset(m2.2007 ,m2.2007$bimon == "3")
T2007_bimon4 <- subset(m2.2007 ,m2.2007$bimon == "4")
T2007_bimon5 <- subset(m2.2007 ,m2.2007$bimon == "5")
T2007_bimon6 <- subset(m2.2007 ,m2.2007$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#whats the default band (distance) that the spline goes out and uses
fit2_1 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon1 )
fit2_2 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon2 )
fit2_3 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon3 )
fit2_4 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon4 )
fit2_5 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon5 )
fit2_6 <- gam(resid ~ s(x_aod_ITM,y_aod_ITM),  data= T2007_bimon6 )

#get the predicted-fitted 
Xpred_1 <- (T2007_bimon1$pred.t31 - fit2_1$fitted)
Xpred_2 <- (T2007_bimon2$pred.t31 - fit2_2$fitted)
Xpred_3 <- (T2007_bimon3$pred.t31 - fit2_3$fitted)
Xpred_4 <- (T2007_bimon4$pred.t31 - fit2_4$fitted)
Xpred_5 <- (T2007_bimon5$pred.t31 - fit2_5$fitted)
Xpred_6 <- (T2007_bimon6$pred.t31 - fit2_6$fitted)

#remerge to 1 file
m2.2007$pred.t32 <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
#this is important so that its sorted as in the first gamm
setkey(m2.2007,day, aodid)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2007 <- lme(pred.t32 ~ meanPM ,random = list(aodid= ~1 + meanPM ),control=lmeControl(opt = "optim"),data= m2.2007  )
m2.2007[, pred.t33 := predict(Final_pred_2007)]
#check correlations
res[res$year=="2007", 'm3.t33'] <- print(summary(lm(pred.m2 ~ pred.t33,data=m2.2007))$r.squared)

#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2007.rds")
data.m3 <- data.m3[,c(1,2,5,29:32,51,52),with=FALSE]
data.m3[, bimon := (m + 1) %/% 2]
setkey(data.m3,day, aodid)
summary(data.m3)
data.m3<-data.m3[!is.na(meanPM)]
data.m3<-data.m3[!is.na(meanPM10)]

data.m3$pred.m3.mix <-  predict(Final_pred_2007,data.m3)




#create unique grid
ugrid <-data.m3 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction    		
data.m3_bimon1 <- data.m3[bimon == 1, ]
data.m3_bimon2 <- data.m3[bimon == 2, ]
data.m3_bimon3 <- data.m3[bimon == 3, ]
data.m3_bimon4 <- data.m3[bimon == 4, ]
data.m3_bimon5 <- data.m3[bimon == 5, ]
data.m3_bimon6 <- data.m3[bimon == 6, ]


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
setkey(data.m3_bimon1,aodid)
data.m3_bimon1 <- merge(data.m3_bimon1, uniq_gid_bimon1, all.x = T)

setkey(uniq_gid_bimon2,aodid)
setkey(data.m3_bimon2,aodid)
data.m3_bimon2 <- merge(data.m3_bimon2, uniq_gid_bimon2, all.x = T)

setkey(uniq_gid_bimon3,aodid)
setkey(data.m3_bimon3,aodid)
data.m3_bimon3 <- merge(data.m3_bimon3, uniq_gid_bimon3, all.x = T)

setkey(uniq_gid_bimon4,aodid)
setkey(data.m3_bimon4,aodid)
data.m3_bimon4 <- merge(data.m3_bimon4, uniq_gid_bimon4, all.x = T)

setkey(uniq_gid_bimon5,aodid)
setkey(data.m3_bimon5,aodid)
data.m3_bimon5 <- merge(data.m3_bimon5, uniq_gid_bimon5, all.x = T)

setkey(uniq_gid_bimon6,aodid)
setkey(data.m3_bimon6,aodid)
data.m3_bimon6 <- merge(data.m3_bimon6, uniq_gid_bimon6, all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3_bimon1,data.m3_bimon2,data.m3_bimon3,data.m3_bimon4,data.m3_bimon5,data.m3_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
#hist(mod3$pred.m3)
#describe(mod3$pred.m3)
#recode negative into zero
mod3 <- mod3[pred.m3 >= 0]
saveRDS(mod3,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.2007.pred.rds")
#clean
keep(mod3,res, sure=TRUE) 
gc()


#########################
#prepare for m3.R2
#########################
mod3[,y_aod_ITM.y:=NULL]
mod3[,x_aod_ITM.y:=NULL]
setnames(mod3,"y_aod_ITM.x","y_aod_ITM")
setnames(mod3,"x_aod_ITM.x","x_aod_ITM")
#subset mod3
#mod3<-mod3[,c("aodid","day","y_aod_ITM","x_aod_ITM","pred.m3"),with=FALSE]

#load mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.pred.rds")
mod1[,aodid:= paste(mod1$long_aod,mod1$lat_aod,sep="-")]
mod1<-mod1[,c("aodid","day","PM25","pred.m1","stn"),with=FALSE]


#R2.m3
setkey(mod3,day,aodid)
setkey(mod1,day,aodid)
mod1 <- merge(mod1,mod3[, list(day,aodid,pred.m3)], all.x = T)  			
res[res$year=="2007", 'm3.t33'] <- print(summary(lm(PM25~pred.m3,data=mod1))$r.squared)    


saveRDS(mod1table, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p3.rds.rds")
saveRDS(mod1, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1_2007w_p.m3.rds")


#########################
#import mod2
mod2<- readRDS( "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.pred2.rds")
mod2<-mod2[,c("aodid","day","pred.m2"),with=FALSE]

#########################
# store the best available
mod3best <- mod3[, list(aodid, x_aod_ITM, y_aod_ITM, day, pred.m3)]
setkey(mod3best, day, aodid)
setkey(mod2, day, aodid)
mod3best <- merge(mod3best, mod2[,list(aodid, day, pred.m2)], all.x = T)
#reload mod1
mod1<- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.pred.rds")
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")
mod1<-mod1[,c("aodid","day","PM25","pred.m1"),with=FALSE]
setkey(mod1,day,aodid)
mod3best <- merge(mod3best, mod1, all.x = T)
mod3best[,bestpred := pred.m3]
mod3best[!is.na(pred.m2),bestpred := pred.m2]
mod3best[!is.na(pred.m1),bestpred := pred.m1]
#save
saveRDS(mod3best,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3best_2007.rds")


#map the predictions
#aggregate by aodid
m3d_agg <- (mod3[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
                        x_aod_ITM = x_aod_ITM[1], #use the first long and lat (by aodid)
                        y_aod_ITM = y_aod_ITM[1]),by = aodid])

# plot
ggplot(m3d_agg, aes(x_aod_ITM, y_aod_ITM, color = LTPM)) + 
  geom_point(size = 3, shape = 15) + 
  #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(15)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")



write.csv(mod3best[, list(LTPM = mean(bestpred, na.rm = T), 
                          predvariance = var(bestpred, na.rm = T),
                          predmin = min(bestpred, na.rm = T),
                          predmax = max(bestpred, na.rm = T),
                          npred = sum(!is.na(bestpred)),
                          npred.m1 = sum(!is.na(pred.m1)),
                          npred.m2 = sum(!is.na(pred.m2)),
                          npred.m3 = sum(!is.na(pred.m3)),
                          x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/pestpred2007LPM.csv", row.names = F)

























