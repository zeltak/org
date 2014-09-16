library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
library(FNN)


# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")


#######################################
# prediction for enrollment locations
#######################################

loc<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_diab37.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(loc));names(l)=names(loc);
l
str(loc$FIPS)
loc[,guid:=NULL]
# head(loc,n=3)
# locxy<-loc[,c("lat","long","uniqueid_y"),with=FALSE]
# write.csv(locxy,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy.csv")
# 

xyguid<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy0308_guid_lpmid.csv")
l=seq(names(xyguid));names(l)=names(xyguid);
l
xyguid<-xyguid[,c(4,7),with=FALSE]


setkey(xyguid,uniqueid_y)
setkey(loc,uniqueid_y)
#make sure to allow cartesian
loc.g <- merge(loc,xyguid)
setnames(loc.g ,"guid.y","guid")


#subset data
gestpred<-loc.g[,c("byob","birthw","lbw","sex","plur","bdob","kess","tden","age_centered","age_centered_sq","FIPS",
"cig_preg", "cig_pre", "med_income", "p_ospace", "gender","pcturban","adtmean","dist_A1", "dist_pemis","prev_400","diab",  "hyper" ,"lungd", "diab_other", "prevpret",
"edu_group","MRN","ges_calc","uniqueid_y","sinetime","costime","guid"                
                                  ),with=FALSE]

#create unique location
# lengthen out to each day of pregnancy
setnames(gestpred, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))
gestpred[, birthdate := as.Date(strptime(birthdate, format = "%m/%d/%y"))]
# new variable for start of gestation using the best gestational age (in weeks)
gestpred[, pregstart := birthdate - 7*ges_calc]

#subset to current expo year range (all pregnancies that start after first day of exposure)
gestpred <- gestpred[pregstart >= as.Date("2003-01-01") , ]

# trying a data.table way to do this
gestlong <- gestpred[,list(day = seq(.SD$pregstart, .SD$birthdate - 1, by = "day")),by=id]
setkey(gestlong,id)
xgestlong <- merge(gestlong, gestpred, by = "id")




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


setkey(xgestlong,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gxgestlong <- merge(xgestlong,allbestpred,all.x=TRUE)
summary(gxgestlong$bestpred)


# find the closest aodid
# gestlong.m <- makepointsmatrix(gestlong, xvar="longutm", yvar="latutm", idvar= "id")
# allbestpred.m <- makepointsmatrix(allbestpred, xvar="x_aod_utm", yvar="y_aod_utm", idvar= "aodid")
# # use the nearestbyday() function
# ###########
# nearestbestpred <- nearestbyday(gestlong.m, allbestpred.m, 
#                           gestlong, allbestpred, 
#                           "id", "aodid", "closestbestpred", "bestpred", knearest = 1, maxdistance = 1000)#was 9 and 1100
# nearestbestpred[, c("closestbestpredknn", "closestbestprednobs", "closestbestpredmean") := NULL]
# nearestbestpred[, id := as.numeric(id)]
# nearestbestpred[, day := as.Date(day)]
# 
# setkey(gestlong,id,day)
# setkey(nearestbestpred,id,day)
# gestlong <- merge(gestlong, nearestbestpred, all.x = T)
# head(gestlong, 2)
# describe(gestlong$bestpred)
# # are there negative predictions
# describe(gestlong$bestpred<0)
# # how many unique AODIDs?
# gestlong[, length(unique(closestbestpred))]
# # compute summaries
# setkey(gestlong,id,day)










####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
gestlongsummary <- gxgestlong[, list(pmpreg = mean(bestpred), 
                                   pmlast90 = mean(tail(.SD[,bestpred], 90)),
                                   pmlast30 = mean(tail(.SD[,bestpred], 30)),
                                   pm1stT = mean(head(.SD[,bestpred], 90)),
                                   pmweek12to24 = mean(.SD[84:168,bestpred]),# just an example, make sure key is set above
                                   guid = guid[1]),by=id]

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

