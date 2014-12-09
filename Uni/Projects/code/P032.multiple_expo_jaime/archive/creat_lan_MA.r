
###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)



lan00 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/world/LAN/2000.dbf"))
lan00MA<-filter(lan00, x > -74 , x < -69 , y < 44 , y > 41)
lan00MA$year<-2000
write.csv(lan00MA,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/1.Raw_data/lan_MA/lanMA00.csv")



lan02 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/world/LAN/2002.dbf"))
lan02MA<-filter(lan02, x > -74 , x < -69 , y < 44 , y > 41)
lan02MA$year<-2002
write.csv(lan02MA,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/1.Raw_data/lan_MA/lanMA02.csv")



lan05 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/world/LAN/2005_2006.dbf"))
lan05MA<-filter(lan05, x > -74 , x < -69 , y < 44 , y > 41)
lan05MA$year<-2006
write.csv(lan05MA,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/1.Raw_data/lan_MA/lanMA06.csv")

