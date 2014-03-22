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
library(sqldf)

lpmgrid<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")

#subset boston
boslpm <- lpmgrid[x_alb_LPMgrid > 2000000 & x_alb_LPMgrid < 2050000 & y_alb_LPMgrid  < 2444000 & y_alb_LPMgrid  > 2401000   , ]
#to check on gis
#write.csv(boslpm,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/boslulpm.csv")




#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(lpmid = boslpm[, unique(lpmid)], day = days_2003))


setkey(test3.se,lpmid)
setkey(boslpm,lpmid)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.se<- merge(test3.se,boslpm,all.x=TRUE,allow.cartesian=TRUE)
test4.se$m <- as.numeric(format(test4.se$day, "%m"))


##########################################################################3
#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
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


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to LPM
setkey(ndvi, m, ndviid)
setkey(test4.se, m, ndviid)
test4.se.ndv <- merge(test4.se, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
test4.se.ndv [, c("long_ndvi.y", "lat_ndvi.y") := NULL]
test4.se.ndv [, c("long_ndvi.x", "lat_ndvi.x") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2003.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(test4.se.ndv)
#fix pbl levels
test4.se.ndv[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(test4.se.ndv, day, pblid)
test4.se.ndv.pb <- merge(test4.se.ndv, pbl, all.x = T)
test4.se.ndv.pb [, c("Long_pbl.x", "Lat_pbl.x") := NULL]
test4.se.ndv.pb [, c("Long_pbl.y", "Lat_pbl.y") := NULL]


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(test4.se.ndv.pb, day, stn)
test4.se.ndv.pb.met <- merge(test4.se.ndv.pb, met2003 , all.x = T)
test4.se.ndv.pb.met[, c("date", "lat_met.y","long_met.y","c","TEMP") := NULL]
saveRDS(test4.se.ndv.pb.met,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/BostonlpmST2003.rds")
keep("test4.se.ndv.pb.met",sure=TRUE)



###################
############# GET Bos pred 2003
mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")

bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl)+pbl*WDSP+s(tden,WDSP),data=mod1d_all_st)

test4.se.ndv.pb.met$lpm <-predict(bp.model.ps,test4.se.ndv.pb.met)


m4d_agg <- (test4.se.ndv.pb.met[, list(LTLPM =mean(lpm, na.rm = TRUE), 
                        x_alb_LPMgrid= x_alb_LPMgrid[1], #use the first long and lat (by guid)
                        y_alb_LPMgrid = y_alb_LPMgrid[1]),by = lpmid])


write.csv(m4d_agg ,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/bos_m4d_agg.csv")





