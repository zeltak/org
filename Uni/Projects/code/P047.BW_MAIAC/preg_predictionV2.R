#Prediction
library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
library(FNN)
#library(rgdal)

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")


#######################################
# prediction for enrollment locations
#######################################

loc<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_diab37.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(loc));names(l)=names(loc);
l
str(loc$FIPS)
head(loc,n=3)
locxy<-loc[,c("lat","long","uniqueid_y"),with=FALSE]
write.csv(locxy,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy.csv")


#subset data
gestpred<-loc[,c("byob","birthw","lbw","sex","plur","bdob","kess","tden","age_centered","age_centered_sq","FIPS",
"cig_preg", "cig_pre", "med_income", "p_ospace", "gender","pcturban","adtmean","dist_A1", "dist_pemis","prev_400","diab",  "hyper" ,"lungd", "diab_other", "prevpret",
"edu_group","MRN","ges_calc","uniqueid_y","sinetime","costime"                
                                  ),with=FALSE]





#create unique location
# lengthen out to each day of pregnancy
setnames(gestpred, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))
gestpred[, birthdate := as.Date(strptime(birthdate, format = "%m/%d/%Y"))]
# new variable for start of gestation using the best gestational age (in weeks)
gestpred[, pregstart := birthdate - 7*ges_calc]

#subset to current expo year range
gestpred <- gestpred[birthyear >= 2003 , ]

# lengthen this out so that each day is one row
# from pregstart to day before you were born (birthdate - 1)
gestlong <- ddply(gestpred, .(id), function(x){
  data.frame(day = seq(x$pregstart, (x$birthdate - 1), by = "day"), id = x$id)
})
gestlong <- merge(gestlong, gestpred, by = "id")
gestlong <- data.table(gestlong)

##Descriptives
# check that everyone has a number of rows that makes sense for their gestational age
#describe(gestlong[, .N/7 - gestage_comb,by="id"][,V1])
# making predictions on 
#nrow(gestlong) # site-day combinations
# from what time period
#range(gestlong$day)



######## import pollution sets


path.data<-"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/"

# assemble all predictions from the yearly bestpred here:
#loadRDS
allbestpredlist <- list()
for(i in 2003:2008){
  allbestpredlist[[paste0("year_", i)]] <- readRDS(paste0(path.data, "mod3best_", i, ".rds"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)
dim(allbestpred)

allbestpred <-allbestpred [,c(1,2,9),with=FALSE]





# find the closest aodid
gestlong.m <- makepointsmatrix(gestlong, xvar="longutm", yvar="latutm", idvar= "id")
allbestpred.m <- makepointsmatrix(allbestpred, xvar="x_aod_utm", yvar="y_aod_utm", idvar= "aodid")
# use the nearestbyday() function
###########
nearestbestpred <- nearestbyday(gestlong.m, allbestpred.m, 
                          gestlong, allbestpred, 
                          "id", "aodid", "closestbestpred", "bestpred", knearest = 1, maxdistance = 1000)#was 9 and 1100
nearestbestpred[, c("closestbestpredknn", "closestbestprednobs", "closestbestpredmean") := NULL]
nearestbestpred[, id := as.numeric(id)]
nearestbestpred[, day := as.Date(day)]

setkey(gestlong,id,day)
setkey(nearestbestpred,id,day)
gestlong <- merge(gestlong, nearestbestpred, all.x = T)
head(gestlong, 2)
describe(gestlong$bestpred)
# are there negative predictions
describe(gestlong$bestpred<0)
# how many unique AODIDs?
gestlong[, length(unique(closestbestpred))]
# compute summaries
setkey(gestlong,id,day)

####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
gestlongsummary <- gestlong[, list(pmpreg = mean(bestpred), 
                                   pmlast90 = mean(tail(.SD[,bestpred], 90)),
                                   pmlast30 = mean(tail(.SD[,bestpred], 30)),
                                   pm1stT = mean(head(.SD[,bestpred], 90)),
                                   #pmweek12to24 = mean(.SD[84:168,bestpred]),# just an example, make sure key is set above
                                   aodid = closestbestpred[1]),by=id]

# remove variables from previous runs through
gestpred[, names(gestlongsummary)[!names(gestlongsummary) %in% "folio"] := NULL]
setkey(gestpred,folio)
setkey(gestlong,folio)
gestpred <- merge(gestpred, gestlongsummary)
head(gestpred,2)
describe(gestpred$pmpreg)
# histogram
ggplot(gestpred, aes(pmpreg)) + geom_histogram()

# show them with utm
ggplot(gestpred, aes(longutm,latutm,color = pmpreg)) + geom_point() + coord_equal()

# show this with ggmap
library(ggmap)
MxC_Map_df <- get_map(location = 'mexico city', maptype = "hybrid", zoom = 9)
str(MxC_Map_df)
P4 <- ggmap(MxC_Map_df, darken = c(0.5, "white"))
P4 + 
  geom_point(data = gestpred,
             aes(-longdd, latdd, color = pmpreg, size = pmpreg)) + 
  theme_bw(10) + 
  ggtitle("Predictions over pregnancy")

rm(MxC_Map_df); rm(P4)
file.remove("ggmapTemp.png")

# merge in the closest monitor value
# find the closest aodid
#gestlong.m <- makepointsmatrix(gestlong, xvar="longutm", yvar="latutm", idvar= "folio")
pm.m <- makepointsmatrix(pm, xvar="pm_x", yvar="pm_y", idvar= "mon")
# use the nearestbyday() function
###########
nearestmon <- nearestbyday(gestlong.m, pm.m, 
                                gestlong, pm[, list(day,mon,daymean)], 
                                "folio", "mon", "closestmon", "daymean", knearest = 12)#was 9 and 1100
nearestmon[, c("closestmonknn", "closestmonnobs", "closestmonmean") := NULL]
nearestmon[, folio := as.numeric(folio)]
setkey(gestlong,folio,day)
setkey(nearestmon,folio,day)
gestlong <- merge(gestlong, nearestmon, all.x = T)

gestlongsummary <- gestlong[, list(monpreg = mean(daymean), 
                                   monlast90 = mean(tail(.SD[,daymean], 90)),
                                   monlast30 = mean(tail(.SD[,daymean], 30)),
                                   mon1stT = mean(head(.SD[,daymean], 90))),by=folio]

# remove variables from previous runs through
# then merge
setkey(gestpred,folio)
setkey(gestlong,folio)
gestpred <- merge(gestpred, gestlongsummary)

# merge in other covariates
if(!exists("gestpred.premerge")){gestpred.premerge <- copy(gestpred)}
gestpred <- merge(gestpred, participants[etapa == "00", list(folio, peso_h, talla_h, fecha_naci_M)])

# some pre-processing
# construct seasonality terms
gestpred[, jday := as.numeric(format(birthdate, "%j"))]
gestpred[, costime := cos(2*pi*jday/365.25)]
gestpred[, sintime := sin(2*pi*jday/365.25)]
gestpred[, female := sex - 1]
# simple regression
summary(lm(Fenton_Z_score ~ pmpreg + sintime + costime, data=gestpred))
summary(lm(gestage_comb ~ pmpreg, data=gestpred))
summary(lm(peso_h ~ pmpreg + sex, data=gestpred))
summary(lm(peso_h ~ monpreg + gestage_comb + female + costime + sintime, data=gestpred[gestage_comb >= 37,]))

# add random intercept for aodid
summary(lmer(Fenton_Z_score ~ pmpreg + (1|aodid), data=gestpred))
summary(lmer(peso_h ~ pmpreg + gestage_comb + female + costime + sintime + (1|aodid), data=gestpred[gestage_comb >= 37,]))
summary(lmer(peso_h ~ pmlast90 + gestage_comb + female + costime + sintime + (1|aodid), data=gestpred[gestage_comb >= 37,]))

ggplot(gestpred, aes(pmpreg, Fenton_Z_score)) + geom_point() + geom_smooth()
ggplot(gestpred, aes(pmpreg, gestage_comb)) + geom_point() + geom_smooth()
ggplot(gestpred, aes(pmpreg, peso_h)) + geom_point() + geom_smooth()

# how does the closest monitor compare with the prediction?
ggplot(gestpred, aes(pmpreg, monpreg)) + geom_point() + geom_smooth() + 
  geom_abline(linetype = "dotted", color = "darkred") + theme_bw(13)
summary(lm(monpreg~pmpreg, gestpred))

# bring in land use terms
gestpred <- merge(gestpred,aodidlur[,list(aodid,elev,rden,rden_OSM)], all.x = T, by="aodid")
describe(gestpred[, list(elev,rden)])

# does road density predict birthweight?
summary(lm(peso_h ~ rden + gestage_comb + female + costime + sintime, data=gestpred[gestage_comb >= 37,]))
summary(lm(peso_h ~ rden + pmpreg + gestage_comb + female + costime + sintime, data=gestpred[gestage_comb >= 37,]))
summary(lm(peso_h ~ rden + monpreg + gestage_comb + female + costime + sintime, data=gestpred[gestage_comb >= 37,]))


#########################
# OLD CODE BELOW
#########################
pred2011 <- roughpred2011[, list(LTPM = mean(bestpred), sdbestpred = sd(bestpred)), by = foliomm]

describe(pred2011)
# how many gridcells represented?
(roughpred2011[, length(unique(closestbestpred))])

ggplot(pred2011, aes(LTPM)) + geom_histogram(binwidth = 0.25) + 
  xlab("Annual average PM2.5 at enrollment locations in 2011") + 
  theme_bw(14)

if(saveplots){
  ggsave(last_plot(), file = paste0(getwd(), "/figures/", "bestpredhist_atloc2011_", Sys.Date(), ".png"), height = 6.5, width = 10)
}

head(roughpred2011,2)
ggplot(roughpred2011[, list(foliomm, day, bestpred)]) + 
  geom_line(aes(day, bestpred, group = foliomm), alpha = 0.1) + 
  ggtitle("predicted exposure in enrollment locations in 2011") + 
  ylab("PM2.5 concentration (ug/m3)") + 
  theme_bw() + 
  theme(axis.title.x = element_blank())

if(saveplots){
  ggsave(last_plot(), file = paste0(getwd(), "/figures/", "bestpred_atloc2011_", Sys.Date(), ".png"), height = 6.5, width = 10)
}


######################
# old code

path.root <- "S:/LUR_MEX/airquality/SIMAT_original/"
setwd(path.root); list.files()

load("LUR_MEX_2_cleaning_out.Rdata")
dat.gis <- dat.gis[daymean != 0]
dat.full <- dat.gis[pol == "CO"]
dat.full[, sum(!is.na(daymean)), by = "mon"]
dat.full[, daymeanlog := log(daymean)]
dim(dat.full)

# data that Maritsa sent, but doesn't have delivery date
#C4_gestdata <- read.dta('S:/LUR_MEX/GIS/basemap/participants/C4_gestdata.dta')
#part <- C4_gestdata

# from the last data break
raw.mom.import <- stata.get('E:/mm_MOTHERS_ENERO_2012.dta')
raw.kid.import <- stata.get('C:/Users/ACJUST/Downloads/mm_CHILDREN_ENERO_2012.dta')

# from the geocoding conducted in Fall 2013
MMgeocode <- data.table(stata.get('S:/LUR_MEX/GIS/basemap/participants/MMRR_GPS_LEON.dta'))
names(MMgeocode)
# since all of the other fields are character, force fecha.5 (date) the same way
MMgeocode[, fecha.5 := format(fecha.5, format = "%m/%d/%Y")]
MMlist <- list()
for(i in 1:5){
  tempMMdt <- MMgeocode[, c("id", "folio", "ren.48", grep(paste0(i, "$"), names(MMgeocode), value = T)), with = F]
  tempMMdt[, addressnum := i]
  MMlist[[i]] <- tempMMdt
}
mmgeodt <- rbindlist(MMlist)
rm(MMlist, i)
setnames(mmgeodt, c("latitud.1", "longitud.1", "fecha.1"), c("latchar", "longchar", "fecha"))
mmgeodt[nchar(latchar) == 0, latchar := NA_character_]
mmgeodt[nchar(longchar) == 0, longchar := NA_character_]
# how many geocodes per ID
describe(mmgeodt[!is.na(latchar), .N, by=folio])
# clean and convert geocodes 
# inconsistent use of '' vs "" and , versus .
mmgeodt[, latchar := sub('"$', "''", latchar), ] 
mmgeodt[, longchar := sub('"$', "''", longchar), ] 
mmgeodt[, latchar := sub(",", "\\.", latchar), ] 
mmgeodt[, longchar := sub(",", "\\.", longchar), ] 
mmgeodt[, c("longd", "longm", "longs") := data.frame(do.call(rbind, strsplit(longchar, "°|'|''")), stringsAsFactors = F)]
mmgeodt[, c("latd", "latm", "lats") := data.frame(do.call(rbind, strsplit(latchar, "°|'|''")), stringsAsFactors = F)]
mmgeodt[, longdd := as.numeric(longd) + (as.numeric(longm) + as.numeric(longs)/60)/60]
mmgeodt[, latdd := as.numeric(latd) + (as.numeric(latm) + as.numeric(lats)/60)/60]
# which were not numeric
mmgeodt[(is.na(longdd) & !is.na(longchar)) | (is.na(latdd) & !is.na(latchar)), ]
sapply(mmgeodt, class)
mmgeodt[!is.na(longdd + latdd), c("longutm", "latutm") := data.frame(project(as.matrix(mmgeodt[!is.na(longdd + latdd), list(longdd = -1 * longdd, latdd)]), "+proj=utm +zone=14 ellps=WGS84"), stringsAsFactors = F)]
# drop missing addresses and old fields
mmgeodt <- mmgeodt[!is.na(longdd + latdd), ]
mmgeodt[, c("longd", "longm", "longs", "latd", "latm", "lats") := NULL]
# clean dates
mmgeodt[, datestart := as.Date(strptime(x=fecha, format = "%m/%d/%Y"))]
mmgeodt[is.na(datestart), fecha]
describe(mmgeodt[,datestart])
mmgeodt[which.min(datestart)]
describe(mmgeodt$datestart)
# summary
describe(mmgeodt[,list(numgeocodes = .N, numgeocodeswithdate = nrow(.SD[!is.na(datestart)])), by=folio, .SDcols = "datestart"])
# output for Leon
#write.table(mmgeodt, file = "S:/LUR_MEX/GIS/basemap/participants/MM_geocodes_Oct2013.csv", row.names = F)
rm(MMgeocode)

raw.kid.import$fecha.nacbay
# pull out an example record
raw.kid.import[raw.kid.import$folio == 1017, c("folio", "etapa", "fecha.etapa", "fecha.nacbay")]
# date of birth is the visit date from the 00 record

raw <- raw.mom.import
raw <- raw[raw$etapa == "00", c("folio", "fecha.ult.mens", "fecha.visita")]
names(raw)[2:3] <- c("lmp", "delivery")
raw$rownum <- 1:nrow(raw)

enrollgis <- read.table("S:/LUR_MEX/GIS/basemap/participants/Export_enrollment_2012_05_01.txt", header = T, sep = ",", as.is = T)
# non-unique locations
enrollgis[enrollgis$foliomm %in% enrollgis$foliomm[duplicated(enrollgis$foliomm)], ]
# how many?
sum(duplicated(enrollgis$foliomm))

# if a mother has more than one location
# here we are using the first
# this occurs for 14 moms (maybe one is work and one is home - according to Emily)
raw.gis <- merge(raw, enrollgis, by.x = "folio", by.y = "foliomm", all.x = T)
raw.gis <- data.table(raw.gis[,c("folio", "lmp", "delivery", "ID", "latitude", 
	"longitude", "POINT_X", "POINT_Y", "dem_val")])
raw.gis$utmkmx <- raw.gis$POINT_X / 1000
raw.gis$utmkmy <- raw.gis$POINT_Y / 1000

# summarize lmp to delivery in weeks
# (we will subtract two for gestage)
describe(as.numeric(raw$delivery - raw$lmp)/7)

setkey(raw.gis, folio)
raw.gis <- unique(raw.gis)
rawlong <- ddply(raw.gis, .(folio), function(df) {
	days <- seq(df$lmp + 14, df$delivery, by = "days")
	ndays <- length(days)
	dflong <- data.frame(days, 
		folio = rep(df$folio, ndays),
		lmp = rep(df$lmp, ndays),
		delivery = rep(df$delivery, ndays),
		utmkmx= rep(df$utmkmx, ndays), 
		utmkmy= rep(df$utmkmy, ndays),
		dem_val= rep(df$dem_val, ndays))
	dflong
})
rawlong$dayint <- as.numeric(rawlong$days - as.Date("1970-01-01"))
rawlong$daysincelmp <- as.numeric(rawlong$days - rawlong$lmp)
raw.long <- data.table(rawlong, key = "dayint")
# trimester definitions relative to lmp
raw.long[, trimester := 0]
raw.long[daysincelmp >= 14 & daysincelmp <= 107, trimester := 1]
raw.long[daysincelmp >= 108 &daysincelmp <= 201, trimester := 2]
raw.long[daysincelmp >= 202, trimester := 3]

#merge in allmonmean
setkey(dat.gis, dayint)
raw.long <- merge(raw.long, unique(dat.gis[pol == "CO", list(dayint, allmonmean, allmonnum)]), by = "dayint", all.x = T)

# how does the mean across the monitors in the city vary
describe(unique(dat.gis[pol == "CO", list(dayint, allmonmean, allmonnum)]))
# and just during pregnancy days
describe(unique(dat.gis[pol == "CO" & 
	dayint >= 13576 & dayint <= 15175, 
	list(dayint, allmonmean, allmonnum)]))

#fit a simple model
M0 <- gam(daymeanlog ~ log(allmonmean), data = dat.full)
raw.long[, pred.M0 := exp(predict(M0, newdata = raw.long))]

# variation in average across pregnancy
pred.pregnancy <- raw.long[, list(pregmean = mean(pred.M0)), by = list(folio)]
describe(pred.pregnancy)

ggplot(pred.pregnancy, aes(pregmean)) + geom_histogram() +
	xlab("Mean of daily CO (ppm) over pregnancy\ntemporal prediction from mean of all monitors for each day") + 
	theme_bw(16)
ggsave("Histogram_pregnancy_predictions_temporal_CO.pdf", height = 8, width = 10.5, dpi = 600)


# variation between trimester averages by tri
pred.tri <- raw.long[, list(trimean = mean(pred.M0)), by = list(folio,trimester)]
dlply(pred.tri, .(trimester), describe)

ggplot(pred.tri, aes(trimean)) + geom_histogram() +
	facet_wrap(~ trimester, ncol = 1) + theme_bw(16)
ggsave("Histogram_trimester_predictions_temporal_CO.pdf", height = 8, width = 10.5, dpi = 600)

# correlations 
tri.wide <- merge(
	pred.tri[trimester == 1, list(folio, trimean.1 = trimean)], 
	pred.tri[trimester == 2, list(folio, trimean.2 = trimean)], by = "folio", all.x = T)
tri.wide <- merge(tri.wide, 
	pred.tri[trimester == 3, list(folio, trimean.3 = trimean)], by = "folio", all.x = T)
cor(as.data.frame(tri.wide)[complete.cases(tri.wide),-1], method = "spearman")

## will need more structures to make a long format
## when each participant can have more than one location per day

#fake <- structure(list(folio = c(1L, 1L, 1L, 2L), start = structure(c(13757, 
#15369, 15369, 13757), class = "Date"), end = structure(c(15368, 
#15407, 15407, 15407), class = "Date"), hrs = c(24L, 8L, 16L, 
#24L), lat = structure(c(1L, 4L, 3L, 2L), .Label = c("culat", 
#"dummylat", "homelat", "worklat"), class = "factor"), long = structure(c(1L, 
#4L, 3L, 2L), .Label = c("culong", "dummylong", "homelong", "worklong"
#), class = "factor"), rownum = 1:4), .Names = c("folio", "start", 
#"end", "hrs", "lat", "long", "rownum"), row.names = c(NA, -4L
#), class = "data.frame")
#
#test <- ddply(fake, .(rownum), function(df) {
#	days <- seq(df$start, df$end, by = "days")
#	ndays <- length(days)
#	dflong <- data.frame(days, 
#		folio = rep(df$folio, ndays), 
#		hrs = rep(df$hrs, ndays),
#		lat = rep(df$lat, ndays), 
#		long = rep(df$long, ndays))
#	dflong
#})


#
