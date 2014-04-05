mpmg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN015_MPM/mpm2006.rds")
head(mpmg)
mpmg$count<-1
c_agg <- (mpmg[, list(obs =sum(count, na.rm = TRUE),by = guid)])  
c_agg  <- as.data.tabl(mpmg[, sum(count, na.rm = TRUE),by = guid])
c_agg<-as.data.table(c_agg)

basegrid <-  fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.csv")
setkey(basegrid  ,guid)
setkey(c_agg ,guid)
try <- merge(c_agg,basegrid, all.x = T)
try2 <- try[V1<360 , ]
ggplot(try2, aes(long_aod, lat_aod, color = V1)) + 
  geom_point(size = 5, shape = 15) + 
  #geom_text(aes(label = naod), color = "black", size = 6, subset = .(distcoy < 1500)) + #similar numbers of points
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")