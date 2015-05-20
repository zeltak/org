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
install.packages("devtools")
devtools::install_github("hadley/readr")
library(readr)

out1 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v01.2003.csv")
out2 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h00v02.2007.csv")
out3 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v02.2007.csv")
out4 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v01.2007.csv")
out5 <- read_csv("/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.h01v01.2008.csv")
out<-rbind(out1,out2,out3,out4,out5)
rm(out1,out2,out3,out4,out5)
gc()
outdt<-as.data.table(out)
#create aodid and unique grid
setnames(outdt,"Lat","lat_aod")
setnames(outdt,"Lon","long_aod")
#create aodid
outdt$aodid<-paste(outdt$long_aod,outdt$lat_aod,sep="-")
grid <- unique(outdt, by="aodid")
write_csv(grid,"/media/NAS/Uni/Data/MV3/Out/fullgrid.csv")
