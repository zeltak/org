library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
library(FNN)
library(ggplot2)
library(bit64)




fullbw<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_diab37.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(fullbw));names(l)=names(fullbw);
l
str(fullbw$FIPS)



#subset data
fullbw.s<-fullbw[,c("byob","birthw","lbw","sex","plur","bdob","kess","tden","age_centered","age_centered_sq","FIPS",
"cig_preg", "cig_pre", "med_income", "p_ospace", "gender","pcturban","adtmean","dist_A1", "dist_pemis","prev_400","diab",  "hyper" ,"lungd", "diab_other", "prevpret","edu_group","MRN","ges_calc","uniqueid_y","sinetime","costime","clinega"),with=FALSE]


setnames(fullbw.s, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))

gestlong.pm.lags<-readRDS("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup.rds")
gestlong.pm.lags.clin<-readRDS("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup_clin.rds")




# merge in other covariates
setkey(fullbw.s,id)
setkey(gestlong.pm.lags ,id)
bwfull.pm <- merge(fullbw.s, gestlong.pm.lags)
head(bwfull.pm)
setkey(bwfull.pm, id)
setkey(gestlong.pm.lags.clin ,id)
bwfull.pmc <- merge(bwfull.pm, gestlong.pm.lags.clin)
head(bwfull.pmc)



#import ndvi id
ndvi.id<-fread("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/P007_key_tables/caseid_ndviid.csv")
setnames(ndvi.id, "uniqueid_y","id")

#merge id
setkey(bwfull.pmc, id)
setkey(ndvi.id ,id)
bwfull.pmc <- merge(bwfull.pmc, ndvi.id[,list(id,ndviid)])

#fix dates
bwfull.pmc[, birthdate := as.Date(strptime(birthdate, format = "%m/%d/%y"))]
bwfull.pmc$m <- as.numeric(format(bwfull.pmc$birthdate, "%m"))



###NDVI
ndvi03 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
ndvi04 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2004.dbf"))
ndvi05 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2005.dbf"))
ndvi06 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2006.dbf"))
ndvi07 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2007.dbf"))
ndvi08 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2008.dbf"))

ndvi <- rbind(ndvi03,ndvi04,ndvi05,ndvi06,ndvi07,ndvi08)
rm(ndvi03,ndvi04,ndvi05,ndvi06,ndvi07,ndvi08)
gc()



#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(bwfull.pmc, m, ndviid)
bwfull.pmc.ndv <- merge(bwfull.pmc, ndvi[,list(ndviid,NDVI,m)])
summary(bwfull.pmc.ndv$NDVI)


saveRDS(bwfull.pmc.ndv,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/Archive/tmpsav.rds")


#### add temperature data
######## import pollution sets

tmp2003<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2003.csv")
tmp2004<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2004.csv")
tmp2005<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2005.csv")
tmp2006<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2006.csv")
tmp2007<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2007.csv")
tmp2008<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2008.csv")

allbestpred <- rbind(tmp2003,tmp2004,tmp2005,tmp2006,tmp2007,tmp2008)
rm(tmp2003,tmp2004,tmp2005,tmp2006,tmp2007,tmp2008)
gc()
allbestpred <- allbestpred[glong > -74 & glong < -69 & glat < 44 & glat > 41, ]
#common dates
allbestpred[, day := as.Date(strptime(date, format = "%d%b%Y"))]
allbestpred[,date:=NULL]
tempdb<-allbestpred
tempdb$x<- tempdb$glong*10000
tempdb$y<- tempdb$glat*10000
tempdb[, guid := paste(x,y,sep="")]
setnames(tempdb, "guid","tempid")



#import Ta id
#import ndvi id
temp.id<-fread("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/P007_key_tables/caseid_tempid.csv")
setnames(temp.id, "uniqueid_y","id")
temp.id$x<- temp.id$glong*10000
temp.id$y<- temp.id$glat*10000
temp.id[, guid := paste(x,y,sep="")]

#merge id
setkey(bwfull.pmc.ndv, id)
setkey(temp.id ,id)
bwfull.pmc.ndv <- merge(bwfull.pmc.ndv, temp.id[,list(id,guid)])
setnames(bwfull.pmc.ndv, "guid.y","tempid")






###############################
#STEP 3
###############################


###########
#create temp lags


#subset data
bwfull.g<-bwfull.pmc.ndv[,c("birthyear","birthw","birthdate","ges_calc","id","clinega","tempid"),with=FALSE]
#convert ges_calc to numeric
bwfull.g[,ges_calc:=as.numeric(ges_calc)]
#get rid of impossible gestational ages other wise the expanding part later wont work
bwfull.s<- bwfull.g[ges_calc > 12 & ges_calc < 48,  ]
 
#create unique location
# lengthen out to each day of pregnancy
# new variable for start of gestation using the best gestational age (in weeks)
bwfull.s[, pregstart := birthdate - 7*ges_calc]

#subset to current expo year range (all pregnancies that start after first day of exposure)
bwfull.s <- bwfull.s[pregstart >= as.Date("2003-01-01") , ]


# create every single day of pregnancy for each pregnancy
gestlong <- bwfull.s[,list(day = seq(.SD$pregstart, .SD$birthdate -1, by = "day")),by=id]

 
setkey(bwfull.s,id)
setkey(gestlong,id)
gestlong <- merge(gestlong, bwfull.s, by = "id")





####merge ges_calc dataset

setkey(gestlong,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gestlong.pm <- merge(gestlong,allbestpred,all.x=TRUE)
summary(gestlong.pm$predpm25)

####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).

gestlong.pm.lags <- gestlong.pm[, list(pmpreg = mean(predpm25), 
                                   pm3rdT = mean(tail(.SD[,predpm25], 90)),
                                   pmlast30 = mean(tail(.SD[,predpm25], 30)),
                                   pm1stT = mean(head(.SD[,predpm25], 90)),
                                   pmweek12to24 = mean(.SD[84:168,predpm25]),
                                   pm2ndT = mean(.SD[91:175,predpm25]),
                                   pmf20w = mean(.SD[1:140,predpm25]),
                                   guid = guid[1]),by=id]


saveRDS(gestlong.pm.lags,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup.rds")
summary(gestlong.pm.lags)
write.csv(gestlong.pm.lags,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup.csv")



####merge ges_clinical dataset

setkey(gestlong.clin,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gestlong.pm.clin <- merge(gestlong.clin,allbestpred,all.x=TRUE)
summary(gestlong.pm.clin$predpm25)

####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).

gestlong.pm.lags.clin <- gestlong.pm.clin[, list(pmpreg.clin = mean(predpm25), 
                                   pm3rdT.clin = mean(tail(.SD[,predpm25], 90)),
                                   pmlast30.clin = mean(tail(.SD[,predpm25], 30)),
                                   pm1stT.clin = mean(head(.SD[,predpm25], 90)),
                                   pmweek12to24.clin = mean(.SD[84:168,predpm25]),
                                   pm2ndT.clin = mean(.SD[91:175,predpm25]),
                                   pmf20w.clin = mean(.SD[1:140,predpm25]),
                                   guid.clin = guid[1]),by=id]


saveRDS(gestlong.pm.lags.clin,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup_clin.rds")
summary(gestlong.pm.lags.clin)
write.csv(gestlong.pm.lags.clin,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup_clin.csv")











