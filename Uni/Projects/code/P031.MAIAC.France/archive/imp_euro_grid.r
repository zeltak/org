library(lme4)
 library(reshape)
 library(foreign) 
 library(ggplot2)
 library(plyr)
 library(data.table)
 library(Hmisc)
 library(mgcv)
 library(gdata)
 library(car)
 library(dplyr)
 library(readr)
 library(ggmap)
 library(broom)
 library(splines)
 library(DataCombine)

g1 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h00v01.csv")
g2 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h00v02.csv")
g3 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h00v03.csv")
g4 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h01v00.csv")
g5 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h01v01.csv")
g6 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h01v02.csv")
g7 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h01v03.csv")
g8 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h02v00.csv")
g9 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h02v01.csv")
g11 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h02v02.csv")
g12 <- read_csv("/media/NAS/Uni/Data/MV3/latlong_csv/LatLong_h02v03.csv")

G<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g11,g12)
G$aodid<-paste(G$Long,G$Lat,sep="-")

write.csv(G,"/media/NAS/Uni/Data/MV3/latlong_csv/Euro_grid.csv")