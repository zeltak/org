# read and check
m1<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.rds")
m1<-as.data.table(m1)
summary(m1)
describe(m1$day)
describe(m1$stn)

ndvi.na<-filter(m1,is.na(ndvi))
ndvi.na<-ndvi.na[,c("day","stn","long_pm25","lat_pm25"),with=FALSE]  
setkey(ndvi.na,day)
unique(ndvi.na$day)
unique(ndvi.na$stn)

pbl.na<-filter(m1,is.na(PBL))
pbl.na<-pbl.na[,c("day","stn","long_pm25","lat_pm25"),with=FALSE]  

library(ggmap)
map <- get_map(location = 'France',zoom=5)
mapPoints <- ggmap(map) +
   geom_point(data = ndvi,aes(x = long_ndvi, y = lat_ndvi,size=25,colour="red"),  alpha = .5)
mapPoints
