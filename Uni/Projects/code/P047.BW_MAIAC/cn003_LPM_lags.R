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

cases<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy0308_guid_lpmid.csv")

############### devide to sets
#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = cases[, unique(lpmid)], day = days_2003))
setkey(test3.se,lpmid)
setkey(cases,lpmid)
#make sure to allow cartesian
test4.se<- merge(test3.se,cases,allow.cartesian=TRUE)


##########################################################################3
#met
met <- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
#xtract year met
met2003<- met[c==2003]


###PBL
pbl <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/HPBL/P2003.dbf"))
str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se, day, pblid)
test4.se.pb <- merge(test4.se, pbl, all.x = T)
test4.se.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(test4.se.pb, day, stn)
test4.se.pb.met <- merge(test4.se.pb, met2003 , all.x = T)
test4.se.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]

#read 2003 RDS
m3<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")
#here's the formula used to calculate lpm
m4.formula<-as.formula(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl)+s(tden,WDSP)+s(tden,tempc)+ah_gm3+s(tden,visib))
bp.model.ps<-gam(m4.formula ,data = m3)
summary(bp.model.ps)#0.118

make sure var names and units are the same as in the NE pm model 
# summary(m3)
# summary(test4.se.pb.met)

#keep only needed
l=seq(names(test4.se.pb.met));names(l)=names(test4.se.pb.met);
l
f1<-test4.se.pb.met[,c(1,4,11,14:19,33,34,36,39,40),with=FALSE]
f2<-f1[1:30000000,]


lpm <- predict(bp.model.ps,f2)
