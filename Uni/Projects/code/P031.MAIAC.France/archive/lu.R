###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
library(zoo)
library(DataCombine)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")


#land use data
m1<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/merge_data.csv")
m2<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/merge_data2.csv")
setkey(m1,aodid)
setkey(m2,aodid)
m3<-merge(m1,m2)
#summary(m3)
fin.lu<-select(m3,aodid, Longitude,Latitude , pop06,pcturb,elev_m,distA1,wflag,tden )
setnames(m4,"aodid","LUaodid")
#write.csv(m4,"/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/Final_LU.csv")
#load aid grid 
fgrid <- fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/france.grid.csv")
#use geomerge to find closest point per day
met.m <- makepointsmatrix(m4, "Longitude", "Latitude", "LUaodid")
setkey(fgrid, aodid)
lu.m <- makepointsmatrix(fgrid[fgrid[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#create fake day
m4$day<-as.Date("2000-01-01")
fgrid$day<-as.Date("2000-01-01")
#we need to leave at least one numeric variable to make the join work... here we use pcturb
key.lu <- nearestbyday(lu.m ,met.m ,fgrid, m4[, list(day,LUaodid,pcturb)], 
                            "aodid", "LUaodid","closestID", "pcturb",knearest = 1, maxdistance = NA)

key.lu<-select(key1,aodid,closestID)
setnames(key.lu,"closestID","LUaodid")




########### import NDVI
ndvidh17v04<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h17v04.csv")
ndvidh18v03<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h18v03.csv")
ndvidh18v04<-fread("/media/NAS/Uni/Data/Europe/france/ndvi_france/out/NDVI_h18v04.csv")
ndvi<-rbindlist(list(ndvidh17v04,ndvidh18v04,ndvidh18v03))
#head(ndvi)







########### import pbl
pbl1<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._0_250.csv")
pbl2<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._0_250.csv")
pbl3<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._251_500.csv")
pbl4<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._501_750.csv")
pbl5<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1001_1250.csv")
pbl6<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1251_1500.csv")
pbl7<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._1751_2000.csv")
pbl8<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._2001_2250.csv")
pbl9<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/pbl_france._2501_2750.csv")
pbl11<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._2751_3000.csv")
pbl12<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3251_3500.csv")
pbl13<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3501_3750.csv")
pbl14<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._3751_4000.csv")
pbl15<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4001_4250.csv")
pbl16<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4251_4500.csv")
pbl17<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4501_4750.csv")
pbl18<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._4751_5000.csv")
pbl19<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._5001_5250.csv")
pbl20<-fread("/media/NAS/Uni/Data/Europe/france/pbl/final_csv/PBL_France._5251_5479.csv")

pbl<-rbindlist(list(pbl1,pbl2,pbl3,pbl4,pbl5,pbl6,pbl7,pbl8,pbl9,pbl11,pbl12,pbl13,pbl14,pbl15,pbl16,pbl17,pbl18,pbl19,pbl20))

#createPBL ID 
pbl$pblid<-paste(pbl$X,pbl$Y,sep="-")
grid <- unique(pbl, by="pblid")
setnames(grid,"X","long_pbl")
setnames(grid,"Y","lat_pbl")
save.csv(grid,"")

saveRDS(pbl,"/media/NAS/Uni/Data/Europe/france/pbl/final_csv/ALLPBL.rds")
write.csv(grid,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/gird/pblgird.csv")


