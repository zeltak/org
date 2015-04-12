###############
#LIBS
###############
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
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

#import new XY and guid for v2 run 2000-2014
nguid<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU_v2.csv")
nguid<-select(nguid,Lat,Lon,GUID,guidOLD)

#LU
lu<- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
poplu<- fread ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/pop_lu.csv")
stack <- fread("/media/NAS/Uni/Data/USA/EPA/NEI05_stacks/midatlneweng_nei05stacks.csv" )
lc <- fread("/media/NAS/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/1km_landcover.csv" )
ems <- fread("/media/NAS/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/LU_emis_EPA.csv" )

setnames(lu,"guid", "guidOLD")
setnames(stack,"guid_", "guidOLD")
setnames(lc,"guid", "guidOLD")
setnames(poplu ,"guid", "guidOLD")
setnames(ems,"guid", "guidOLD")

setkey(nguid, guidOLD)
setkey(lu, guidOLD)
setkey(stack, guidOLD)
setkey(lc, guidOLD)
setkey(ems, guidOLD)
setkey(poplu, guidOLD)

lux <- merge(nguid,lu ,all.x = T)
lux <- merge(lux,stack ,all.x = T)
lux <- merge(lux,lc ,all.x = T)
lux <- merge(lux,ems ,all.x = T)
lux <- merge(lux,poplu ,all.x = T)

names(lux)

luxf<-select(lux,GUID,Lat ,Lon ,dist_PE ,   pm25stge30_15k    ,  pm25stlt30_3k  , pm10stge30_15k    ,  pm10stlt30_3k    ,   noxstge30_15k ,      noxstlt30_3k,        so2stge30_15k ,      so2stlt30_3k       , pcthd_1km ,  pctpa_1km    ,       pctsh_1km         ,  pctgr_1km    ,       elev_m          ,    Mjrrdden_1km ,   
pctmd_1km    ,       pctld_1km           ,pctop_1km     ,      pctdf_1km  ,         pctmf_1km   ,pctev_1km    ,       pctcr_1km   ,                 
             State_Abbreviation,  NOXsum,   PM10sum    ,         SO2sum    ,          nei05nonpntcntypm25, pcturb_1km  ,        SumOfEMISS ,         pop_sqkm      )

summary(luxf)
names(luxf)

#base model for stage 1
#PM25 aod,tempc,WDSP,NDVI,ah_gm3,visib,pbl
saveRDS(luxf,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU_v2_ALL.rds")


luxf<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU_v2_ALL.rds")


