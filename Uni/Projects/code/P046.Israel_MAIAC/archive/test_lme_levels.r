#------------------------>>>
#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIRx/Xmod3.AQ.rds")
data.m3.2005 <-data.m3[c ==2005]
#for PM25
data.m3.2005 <- select(data.m3.2005,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2005[, bimon := (m + 1) %/% 2]
setkey(data.m3.2005,day, aodid)
data.m3.2005<-data.m3.2005[!is.na(meanPM)]
#generate m.3 initial pred level=1
data.m3.2005$pred.m3.mix <-  predict(Final_pred_all,data.m3.2005)
#split full
data.m3.2005.F<-filter(data.m3.2005,!is.na(pred.m3.mix))
#split missing
data.m3.2005.NA<-filter(data.m3.2005,is.na(pred.m3.mix))
data.m3.2005.NA$pred.m3.mix<-NULL
data.m3.2005.NA$pred.m3.mix<-  predict(Final_pred_all,data.m3.2005,level=0)
summary(data.m3.2005.NA)
#rejoin
data.m3.2005.E<-rbindlist(list(data.m3.2005.F,data.m3.2005.NA))


#bestprmap
m3d_agg <- (data.m3.2005.E[, list(LTPM =mean(pred.m3.mix, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1


m3d_agg <- (xt[, list(LTPM =mean(pred.m3.mix, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1



#create unique grid
ugrid <-data.m3.2005.E %>%
    group_by(aodid) %>%
    summarise(x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE)) 


#### PREDICT Gam part
#split back into bimons to include the gam prediction in final prediction        
data.m3.2005.E_bimon1 <- data.m3.2005.E[bimon == 1, ]
data.m3.2005.E_bimon2 <- data.m3.2005.E[bimon == 2, ]
data.m3.2005.E_bimon3 <- data.m3.2005.E[bimon == 3, ]
data.m3.2005.E_bimon4 <- data.m3.2005.E[bimon == 4, ]
data.m3.2005.E_bimon5 <- data.m3.2005.E[bimon == 5, ]
data.m3.2005.E_bimon6 <- data.m3.2005.E[bimon == 6, ]


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
setkey(data.m3.2005.E_bimon1,aodid)
data.m3.2005.E_bimon1 <- merge(data.m3.2005.E_bimon1, uniq_gid_bimon1[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon2,aodid)
setkey(data.m3.2005.E_bimon2,aodid)
data.m3.2005.E_bimon2 <- merge(data.m3.2005.E_bimon2, uniq_gid_bimon2[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon3,aodid)
setkey(data.m3.2005.E_bimon3,aodid)
data.m3.2005.E_bimon3 <- merge(data.m3.2005.E_bimon3, uniq_gid_bimon3[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon4,aodid)
setkey(data.m3.2005.E_bimon4,aodid)
data.m3.2005.E_bimon4 <- merge(data.m3.2005.E_bimon4, uniq_gid_bimon4[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon5,aodid)
setkey(data.m3.2005.E_bimon5,aodid)
data.m3.2005.E_bimon5 <- merge(data.m3.2005.E_bimon5, uniq_gid_bimon5[,list(aodid,gpred)], all.x = T)
setkey(uniq_gid_bimon6,aodid)
setkey(data.m3.2005.E_bimon6,aodid)
data.m3.2005.E_bimon6 <- merge(data.m3.2005.E_bimon6, uniq_gid_bimon6[,list(aodid,gpred)], all.x = T)

#reattach all parts        
mod3 <- rbind(data.m3.2005.E_bimon1,data.m3.2005.E_bimon2,data.m3.2005.E_bimon3,data.m3.2005.E_bimon4,data.m3.2005.E_bimon5,data.m3.2005.E_bimon6)
# create pred.m3
mod3$pred.m3 <-mod3$pred.m3.mix+mod3$gpred
hist(mod3$pred.m3)

m3d_agg <- (mod3[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

write.csv(m3d_agg,"~/ZH_tmp/lvljoin.csv")
