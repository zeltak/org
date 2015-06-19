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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)

#add newly avilable LU data
#airport
dair<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/Qgis/dist_airports.csv")
setnames(dair,"InputID","aodid")
setnames(dair,"Distance","dair")
#dports
dport<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/Qgis/dist_ports.csv")
setnames(dport,"InputID","aodid")
setnames(dport,"Distance","dport")
lua<-left_join(dair,dport, by = c("aodid" = "aodid") )
#distances
dist<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/lu/distances.csv")
lua<-left_join(lua,dist, by = c("aodid" = "aodid") )
dim(lua)
lua$TargetID.y<-NULL
lua$TargetID.x<-NULL
#add emissions
ems<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/LU/fin.emission.rds")
names(ems)
setnames(ems,"NOx (as NO2)","NO2")
setnames(ems,"SOx (as SO2)","SO2")
setnames(ems,"PCDD/ PCDF (dioxins/ furans)","PCD")
#create single aod point per aodid per day 
ems <-ems %>%
    group_by(emsid) %>%
    summarise(NO2=mean(NO2),SO2=mean(SO2),PM10=mean(PM10),PM2.5=mean(PM2.5) )
lua<-left_join(lua,ems, by = c("emsid" = "emsid") )
#lucor
lucor<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/lu/lucor.csv")
head(lucor)
lucor<-select(lucor,aodid,baggrimean,bopenmean,burban_mea,bforest_me,reg,pmreg,cid,c2id,pmzsimpid,cmidsimp)
lucor$baggrimean<-lucor$baggrimean*100
lucor$bopenmean<-lucor$bopenmean*100
lucor$burban_mea<-lucor$burban_mea*100
lucor$bforest_me<-lucor$bforest_me*100
head(lucor)
#renames
setnames(lucor,"baggrimean","p.agric")
setnames(lucor,"bopenmean","p.opem")
setnames(lucor,"burban_mea","p.urban")
setnames(lucor,"bforest_me","p.forest")
dim(lucor)
lua<-left_join(lua,lucor, by = c("aodid" = "aodid") )
#delete uneeded
lua$reg.x<-NULL
lua$V1<-NULL

lua[,dair.s:= scale(dair)]
lua[,dport.s:= scale(dport)]
lua[,dtrain.s:= scale(dist_train)]
lua[,daroad.s:= scale(dist.aroad)]
lua[,dcoast.s:= scale(dist.coast)]
lua[,dwb.s:= scale(dist.wb)]
lua[,NO2.s:= scale(NO2 )]
lua[,SO2.s:= scale(SO2)]
lua[,PM25ems.s:= scale(PM2.5)]
lua[,PM10ems.s:= scale(PM10)]
lua[,p.agric.s:= scale(p.agric)]
lua[,p.open.s:= scale(p.opem)]
lua[,p.urban.s:= scale(p.urban)]
lua[,p.forest.s:= scale(p.forest)]

lua[,dair.l:= log(dair+ 0.0001234)]
lua[,dport.l:= log(dport+ 0.0001234)]
lua[,dtrain.l:= log(dist_train+ 0.0001234)]
lua[,daroad.l:= log(dist.aroad+ 0.0001234)]
lua[,dcoast.l:= log(dist.coast+ 0.0001234)]
lua[,dwb.l:= log(dist.wb+ 0.0001234)]
lua[,NO2.l:= log(NO2 + 0.0001234)]
lua[,SO2.l:= log(SO2+ 0.0001234)]
lua[,PM25ems.l:= log(PM2.5+ 0.0001234)]
lua[,PM10ems.l:= log(PM10+ 0.0001234)]
lua[,p.agric.l:= log(p.agric+ 0.0001234)]
lua[,p.open.l:= log(p.opem+ 0.0001234)]
lua[,p.urban.l:= log(p.urban+ 0.0001234)]
lua[,p.forest.l:= log(p.forest+ 0.0001234)]

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2004.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2004.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2005.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2006.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2007.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2008.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2009.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2010.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2011.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2012.c.rds")

keep(lua, sure=TRUE) 
gc()

mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.rds")
#take out stations in non contigious france
mod1<-filter(mod1,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
mod1<-filter(mod1,wflag != 1)
mod1<-filter(mod1,UN >0  & UN  <0.04)
#recreate aodid
mod1$aodid<-paste(mod1$long_aod,mod1$lat_aod,sep="-")

mod1<-left_join(mod1,lua, by = c("aodid" = "aodid") )
head(mod1)

mod1[,distA1.s:= scale(distA1)]

setnames(mod1,"distA1.s","da1.s")
setnames(mod1,"MeanPbl.s","pbl.s")
setnames(mod1,"tempa.s","temp.s")
setnames(mod1,"WSa.s","winds.s")
setnames(mod1,"RHa.s","rh.s")
setnames(mod1,"Raina.s","rain.s")
mod1[,tden.l:= log(tden+ 0.0001234)]
mod1[,elev.l:= log(elev_m+ 0.0001234)]
mod1[,pden.l:= log(pop06+ 0.0001234)]
mod1[,da1.l:= log(distA1+ 0.0001234)]
mod1[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod1[,pbl.l:= log(PBL+ 0.0001234)]
mod1[,winds.l:= log(wsavg + 0.0001234)]
mod1[,rh.l:= log(rhavg + 0.0001234)]
mod1[,rain.l:= log(rainday + 0.0001234)]
mod1[,aod.l:= log(aod+ 0.0001234)]
mod1[,temp.l:= log(tempavg+10)]

summary(mod1)
mod1<-filter(mod1,!is.na(wsavg))
mod1<-filter(mod1,!is.na(rhavg))
mod1<-filter(mod1,!is.na(normwt))
mod1<-filter(mod1,!is.na(temp.l))

saveRDS(mod1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.c1.rds")

################# clean BAD STN PM25 and check if improved model?
  raWDaf <- ddply(mod1, c("stn","c"), 
        function(x) {
          mod1 <- lm(pm25 ~ aod, data=x)
          data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                     nsamps = length(summary(mod1)$resid))
  })
  raWDaf
  raWDaf<-as.data.table(raWDaf)
  bad<- raWDaf[R2 <= 0.02]
  bad[,badid := paste(stn,c,sep="-")]
  #################BAD STN
  mod1[,badid := paste(stn,c,sep="-")]
  ####Take out bad stations
  mod1c <- mod1[!(mod1$badid %in% bad$badid), ] 

saveRDS(mod1c,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.c2.rds")

mod2 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.rds")
dim(mod2)

#delete water flags
mod2<-filter(mod2,wflag != 1)
mod2<-filter(mod2,UN >0  & UN  <0.04)
dim(mod2)
gc()

mod2<-left_join(mod2,lua, by = c("aodid" = "aodid") )
dim(mod2)

mod2[,distA1.s:= scale(distA1)]
setnames(mod2,"distA1.s","da1.s")
setnames(mod2,"MeanPbl.s","pbl.s")
setnames(mod2,"tempa.s","temp.s")
setnames(mod2,"WSa.s","winds.s")
setnames(mod2,"RHa.s","rh.s")
setnames(mod2,"Raina.s","rain.s")
mod2[,tden.l:= log(tden+ 0.0001234)]
mod2[,elev.l:= log(elev_m+ 0.0001234)]
mod2[,pden.l:= log(pop06+ 0.0001234)]
mod2[,da1.l:= log(distA1+ 0.0001234)]
mod2[,ndvi.l:= log(ndvi+1+ 0.0001234)]
mod2[,pbl.l:= log(PBL+ 0.0001234)]
mod2[,winds.l:= log(wsavg + 0.0001234)]
mod2[,rh.l:= log(rhavg + 0.0001234)]
mod2[,rain.l:= log(rainday + 0.0001234)]
mod2[,aod.l:= log(aod+ 0.0001234)]
mod2[,temp.l:= log(tempavg+10)]

summary(mod2)
mod2<-filter(mod2,!is.na(wsavg))
mod2<-filter(mod2,!is.na(rhavg))
mod2<-filter(mod2,!is.na(normwt))
mod2<-filter(mod2,!is.na(temp.l))

saveRDS(mod2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2013.c.rds")

keep(lua, sure=TRUE) 
gc()
