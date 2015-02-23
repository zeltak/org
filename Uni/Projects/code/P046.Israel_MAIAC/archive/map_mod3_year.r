
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
source("/media/NAS/Uni/org/files/Uni/Projects")

mod3<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod3.AQ.PM25.2003.pred3.rds")


#bestprmap
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

res<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resm1.AQ.PM25.rds")

write.csv(res,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/resm1.AQ.PM25.csv")
