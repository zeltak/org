
###load Aqua
#load aod data
aqua<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")
#load PA grid (points in "palestine authority")
ilgreen <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL.green_grid_north.csv")
aqua <- aqua[aqua$aodid %in% ilgreen$aodid, ] 

#create single aod point per aodid per day (this addresses cartesean error below)
xplot <-aqua %>%
    group_by(aodid) %>%
    summarise_each(funs(mean),long_aod,lat_aod,aod,x_aod_ITM, y_aod_ITM )

write.csv(xplot,"~/ZH_tmp/aodplor.csv")


#predperyear
m3d_agg <- (xplot[, list(LTPM =mean(aod, na.rm = TRUE), 
                        utmx = x_aod_ITM[1], #use the first long and lat (by aodid)
                        utmy = y_aod_ITM[1]),by = aodid])  
P1 <- ggplot(m3d_agg, aes(utmx, utmy, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction-2013", colours = cbPalette) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
#PM25<-PM25[PM25 > 0.000000000001 & PM25 < 900 ]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")


#other 
write.csv (x <-PM25 %>%
    group_by(stn) %>%
    summarise(pm=mean(PM25),x = mean(x_stn_ITM, na.rm=TRUE),  y = mean(y_stn_ITM, na.rm=TRUE)) , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/PM25.allyears_mean.csv")
 


#calculate meanPM per grid per day to each station (excluding first station)
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
#PM10<-PM10[PM10 > 0.000000000001 & PM10 < 20200 ]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")


#other 
write.csv (x <-PM10 %>%
    group_by(stn) %>%
    summarise(pm=mean(PM10),x = mean(x_stn_ITM, na.rm=TRUE),  y = mean(y_stn_ITM, na.rm=TRUE)) , "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/PM10.allyears_mean.csv")
 

