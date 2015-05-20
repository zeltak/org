grid <- unique(allbestpred, by="aodid")
names(grid)
grid<-grid[,c(5,6,29),with=FALSE]
grid[,long_aod:=as.numeric(long_aod)]
grid[,lat_aod:=as.numeric(lat_aod)]

write.csv(grid,"~/ZH_tmp/fggg.grid.csv")



library(R.matlab)
08.16
data <- readMat("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v01.2012.mat")
#headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','QA'}; %In R we have a better code for extracting the QA flag

head(data)
d2<-rbind(data$headers,data$TableAq)
head(d2)
