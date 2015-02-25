#-------------->prepare for mod3
m2.all[, bimon := (m + 1) %/% 2]
setkey(m2.all,day, aodid)
m2.all<-m2.all[!is.na(meanPM)]


#2004
#take out 2004
m2.all.2004<-m2.all[c ==2004]


#save for GIS

x<-na.omit(mod3)
write.csv(x[, list(LTPM = mean(pred.m3, na.rm = T), 
                                                   x_aod_ITM =  x_aod_ITM[1], y_aod_ITM = y_aod_ITM[1]),by=aodid], "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/tstm3.csv", row.names = F)

m2.all.2004[, sqrtpred.m2 := sqrt(pred.m2)]
m2.all.2004[, sqrtmpm := sqrt(meanPM)]


m2.smooth.x <- bam(pred.m2 ~ meanPM + te(x_aod_ITM, y_aod_ITM, by = bimon), 
                              data = m2.all.2004[, list(pred.m2,meanPM, x_aod_ITM, y_aod_ITM, bimon)])

# m2.smooth.x <- bam(sqrtpred.m2 ~ sqrtmpm + te(x_aod_ITM, y_aod_ITM, by = bimon), 
#                               data = m2.all.2004[, list(sqrtpred.m2,sqrtmpm, x_aod_ITM, y_aod_ITM, bimon)])


#import mod3 
data.m3 <- readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod3.AQ.rds")
data.m3.2004 <-data.m3[c ==2004]
#for PM25
data.m3.2004 <- select(data.m3.2004,day,aodid,m,meanPM,x_aod_ITM,y_aod_ITM)
data.m3.2004[, bimon := (m + 1) %/% 2]
setkey(data.m3.2004,day, aodid)
data.m3.2004<-data.m3.2004[!is.na(meanPM)]


#correlate to see everything from mod2 and the mpm works
data.m3.2004$pred.m3 <- predict(m2.smooth.x, newdata = data.m3.2004)

#########################
#prepare for m3.R2
#########################
#load mod1
m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1C.PM25.AQ.rds")
m1.all[,aodid:= paste(m1.all$long_aod.x,m1.all$lat_aod.x,sep="-")]
m1.all<-m1.all[,c("aodid","day","PM25","stn","c"),with=FALSE]
#R2.m3
setkey(data.m3.2004,day,aodid)
setkey(m1.all,day,aodid)
m1.all <- merge(m1.all,data.m3.2004[, list(day,aodid,pred.m3)], all.x = T)
m3.fit.all<- summary(lm(PM25~pred.m3,data=m1.all))
print(summary(lm(PM25~pred.m3,data=m1.all))$r.squared)    
print(rmse(residuals(m3.fit.all)))


# mod3[, pm_mod3sqrt := predict(m2.gamsmooth.sqrt, newdata = mod3, cluster = cl)^2]
# 
# print(summary(lm(pred.m2~pred.t31,data=m2.all.2004))$r.squared)


m3d_agg <- (data.m3.2004[, list(LTPM =mean(pred.m3, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1


write.csv(m3d_agg,"/home/zeltak/ZH_tmp/bam2004.csv")
