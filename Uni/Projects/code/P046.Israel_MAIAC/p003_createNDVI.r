#Add ndvi
#create grid
ndvi <-fread("/media/NAS/Uni/Data/Israel/NDVI/NDVI_Israel.csv")
ndvi$ndvi<-ndvi$NDVI*0.0001
ndvi[,c("NDVI"):=NULL]
setnames(ndvi,"Long","long_ndvi")
setnames(ndvi,"Lat","lat_ndvi")
ndvi[, ndviid := paste(long_ndvi,lat_ndvi,sep="")]
setnames(ndvi,"Month","m")
setnames(ndvi,"Year","c")
grid <- unique(ndvi, by="ndviid")
gridX <- grid[,ndvi :=NULL]
gridX <- grid[,c :=NULL]
gridX <- grid[,m :=NULL]
#export grid
write.csv(gridX, "/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/ndvi_ID.csv")

saveRDS(ndvi,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN006_NDVI_yearly/ndvi.rds")

