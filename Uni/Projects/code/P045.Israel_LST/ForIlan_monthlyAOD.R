#For Ilan Levy
library(data.table)
library(dplyr)
library(ggplot2)

pm<-readRDS('/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod2.AQ.rds')
setnames(pm,"long_aod.x","Long")
setnames(pm,"lat_aod.x","Lat")

aod<-pm[,c("aodid","aod","c","m","Long","Lat"),with=FALSE]
setnames(aod,"c","year")
aod.y<-aod%>%group_by(aodid,year,m)%>%summarise(aod=mean(aod),N=n(),Lat=mean(Lat),Long=mean(Long))
write.csv(aod.y,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/aod_m.csv')


