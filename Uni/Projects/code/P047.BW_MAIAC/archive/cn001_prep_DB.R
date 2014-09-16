#Prediction
library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
#library(rgdal)


#######################################
# for 1x1 grid
#######################################

loc<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_diab37.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(loc));names(l)=names(loc);
l
str(loc$FIPS)
head(loc,n=3)
locxy<-loc[,c("lat","long","uniqueid_y"),with=FALSE]
write.csv(locxy,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy.csv")



#######################################
# for 200x200 grid
#######################################

lpmxy<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/LPM_GRID_IDS.csv")
str(lpmxy)
ss <- lpmxy[long_aod > -73.566 & long_aod < -69.9 & lat_aod < 43 & lat_aod > 41.1, ]

#aggregate by guid
lpmxyCagg <- (lpmxyC[, list(LTPM =mean(predicted.m3, na.rm = TRUE), 
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




write.csv(lpmxyC ,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/lpmxy.csv")


