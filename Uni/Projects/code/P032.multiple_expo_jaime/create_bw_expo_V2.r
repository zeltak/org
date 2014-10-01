library(data.table)
library(dplyr)
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
"cig_preg", "cig_pre", "med_income", "p_ospace", "gender","pcturban","adtmean","dist_A1", "dist_pemis","prev_400","diab",  "hyper" ,"lungd", "diab_other", "prevpret","edu_group","MRN","ges_calc","uniqueid_y","sinetime","costime"),with=FALSE]


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




###############################
#STEP 3
###############################


###########
#create temp lags


#subset data
bwfull.g<-fullbw[,c("byob","birthw","bdob","ges_calc","uniqueid_y","clinega"),with=FALSE]
#convert ges_calc to numeric
bwfull.g[,ges_calc:=as.numeric(ges_calc)]
#get rid of impossible gestational ages other wise the expanding part later wont work
bwfull.s<- bwfull.g[ges_calc > 12 & ges_calc < 48,  ]
 
#create unique location
# lengthen out to each day of pregnancy
setnames(bwfull.s, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))
bwfull.s[, birthdate := as.Date(strptime(birthdate, format = "%m/%d/%y"))]
# new variable for start of gestation using the best gestational age (in weeks)
bwfull.s[, pregstart := birthdate - 7*ges_calc]


#subset to current expo year range (all pregnancies that start after first day of exposure)
bwfull.s <- bwfull.s[pregstart >= as.Date("2003-01-01") , ]
# create every single day of pregnancy for each pregnancy
gestlong <- bwfull.s[,list(day = seq(.SD$pregstart, .SD$birthdate -1, by = "day")),by=id]
 
setkey(bwfull.s,id)
setkey(gestlong,id)
gestlong <- merge(gestlong, bwfull.s, by = "id")


#import Ta id
#import ndvi id
temp.id<-fread("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/P007_key_tables/caseid_tempid.csv")
setnames(temp.id, "uniqueid_y","id")
temp.id$x<- temp.id$glong*10000
temp.id$y<- temp.id$glat*10000
temp.id[, tempid := paste(x,y,sep="")]

#merge id
setkey(gestlong, id)
setkey(temp.id ,id)
gestlong.tid <- merge(gestlong, temp.id[,list(id,tempid)])


####merge ges_calc dataset
setkey(gestlong.tid,tempid,day)
setkey(tempdb ,tempid, day)
gestlong.tempc <- merge(gestlong.tid,tempdb,all.x=TRUE)
head(gestlong.tempc)
gestlong.tempc[, c("x", "y","glong","glat") := NULL]


####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).

gestlong.tempc.lags <- gestlong.tempc[, list(temppreg = mean(fintemp), 
                                   temp3rdT = mean(tail(.SD[,fintemp], 90)),
                                   templast30 = mean(tail(.SD[,fintemp], 30)),
                                   temp1stT = mean(head(.SD[,fintemp], 90)),
                                   tempweek12to24 = mean(.SD[84:168,fintemp]),
                                   temp2ndT = mean(.SD[91:175,fintemp]),
                                   tempf20w = mean(.SD[1:140,fintemp]),
                                   tempid = tempid[1]),by=id]


saveRDS(gestlong.tempc.lags,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/FN001_datastes/temp_lags.rds")
summary(gestlong.tempc.lags)


##add to main data temp data

setkey(bwfull.pmc, id)
setkey(gestlong.tempc.lags, id)
bwfull.ptv<-merge(bwfull.pmc,gestlong.tempc.lags)
names(bwfull.ptv)
#discard clinical for now
bwfull.ptv[, 41:48:= NULL]
#clean again for uneeded data
summary(bwfull.ptv)
bwfull.ptv<-na.omit(bwfull.ptv)


saveRDS(bwfull.ptv,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/Archive/tmpsav.rds")
#bwfull.ptv<-readRDS("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/Archive/tmpsav.rds")


################ LAN
lanid <- fread("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/P007_key_tables/caseid_LANid.csv")
setnames(lanid, "uniqueid_y","id")

#merge id
setkey(bwfull.ptv, id)
setkey(lanid ,id)
bwfull.ptvl<- merge(bwfull.ptv, lanid[,list(id,geonameID)])

rang1<- filter(bwfull.ptvl, birthyear==2003|birthyear==2004|birthyear==2005|birthyear==2006)
rang2<- filter(bwfull.ptvl, birthyear==2007|birthyear==2008)
rang1$yearb<-2002
rang2$yearb<-2006
bwfull.ptvl2<-rbindlist(list(rang1, rang2))


lan02 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/world/LAN/2002.dbf"))
lan02MA<-filter(lan02, x > -74 , x < -69 , y < 44 , y > 41)
lan02MA$yearb<-2002
write.csv(lan02MA,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/1.Raw_data/lan_MA/lanMA02.csv")

lan06 <-  as.data.table(read.dbf("/media/NAS/Uni/Data/world/LAN/2005_2006.dbf"))
lan06MA<-filter(lan06, x > -74 , x < -69 , y < 44 , y > 41)
lan06MA$yearb<-2006
write.csv(lan06MA,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/1.Raw_data/lan_MA/lanMA06.csv")

Lan<-rbindlist(list(lan02MA, lan06MA))

#merge id
setkey(bwfull.ptvl2, geonameID, yearb)
setkey(Lan ,geonameID, yearb)
bwfull.lan<- merge(bwfull.ptvl2, Lan[,list(yearb,geonameID,lan=MEAN)])




#import ndvi id
ndvi.id<-fread("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/P007_key_tables/caseid_ndviid.csv")
setnames(ndvi.id, "uniqueid_y","id")

#merge id
setkey(bwfull.lan, id)
setkey(ndvi.id ,id)
bwfull.lan.n <- merge(bwfull.lan, ndvi.id[,list(id,ndviid)])

#fix dates
bwfull.lan.n$m <- as.numeric(format(bwfull.lan.n$birthdate, "%m"))
bwfull.lan.n$yr <- as.numeric(format(bwfull.lan.n$birthdate, "%Y"))


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
ndvi <- ndvi[, c("xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]
ndvi$yr <- as.numeric(format(ndvi$date, "%Y"))


#join NDVI to data (using dplyr)
z<-left_join(bwfull.lan.n, ndvi, by=c("ndviid", "yr", "m"))
z<-na.omit(z)
saveRDS(z,"/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/FN001_datastes/bw_expo_final.rds")
