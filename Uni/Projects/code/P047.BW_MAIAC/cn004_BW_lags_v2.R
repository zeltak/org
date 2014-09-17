library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
library(FNN)
library(ggplot2)


######################################
# prediction for enrollment locations
#######################################
bwfull <- read.dbf("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN003_BW_data/bwall.dbf")
bwfull<-as.data.table(bwfull)
l=seq(names(bwfull));names(l)=names(bwfull);
l
str(bwfull$FIPS)


xyguid<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy0308_guid_lpmid.csv")
# l=seq(names(xyguid));names(l)=names(xyguid);
# l
xyguid<-xyguid[,c(4,7),with=FALSE]

setkey(xyguid,uniqueid_y)
setkey(bwfull,uniqueid_y)
#make sure to allow cartesian
bwfull.g <- merge(bwfull,xyguid)


#subset data (short dataset)
bwfull.s<-bwfull.g[,c("bdob","byob","birthw","ges_calc","uniqueid_y","guid"),with=FALSE]

#create unique location
# lengthen out to each day of pregnancy
setnames(bwfull.s, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))
bwfull.s[, birthdate := as.Date(strptime(birthdate, format = "%Y-%m-%d"))]
# new variable for start of gestation using the best gestational age (in weeks)
bwfull.s[, pregstart := birthdate - 7*ges_calc]

#subset to current expo year range (all pregnancies that start after first day of exposure)
bwfull.s <- bwfull.s[pregstart >= as.Date("2003-01-01") , ]

# create every single day of pregnancy for each pregnancy
gestlong <- bwfull.s[,list(day = seq(.SD$pregstart, .SD$birthdate, by = "day")),by=id]

setkey(gestlong,id)
gestlong <- merge(gestlong, bwfull.s, by = "id")



######## import pollution sets

p2003<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2003 <- p2003[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]
p2004<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2004 <- p2004[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]
p2005<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2005 <- p2005[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]
p2006<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2006 <- p2006[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]
p2007<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2007 <- p2007[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]
p2008<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv")
p2008 <- p2008[long_aod > -74 & long_aod < -69 & lat_aod < 44 & lat_aod > 41, ]


allbestpred <- rbind(p2003,p2004,p2005,p2006,p2007,p2008)
allbestpred <-allbestpred [,c(1,2,7),with=FALSE]
allbestpred$guid<-as.numeric(allbestpred$guid)


setkey(gestlong,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gestlong.pm <- merge(gestlong,allbestpred,all.x=TRUE)
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
                                   pm3rdT = mean(tail(.SD[,bestpred], 90)),
                                   pmlast30 = mean(tail(.SD[,bestpred], 30)),
                                   pm1stT = mean(head(.SD[,bestpred], 90)),
                                   pmweek12to24 = mean(.SD[84:168,bestpred]),
                                   pm2ndT = mean(.SD[91:175,bestpred]),
                                   pmf20w = mean(.SD[1:140,bestpred]),
                                   guid = guid[1]),by=id]

#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).


saveRDS(gestlongsummary,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1k.rds")
summary(gestlongsummary)

# remove variables from previous runs through
setkey(gxgestlong,id)
setkey(gestlongsummary,id)
bw.o1 <- merge(gxgestlong, gestlongsummary)
describe(bw.o1$pmpreg)
# histogram
ggplot(bw.o1, aes(pmpreg)) + geom_histogram()

# show this with ggmap
# library(ggmap)
# MxC_Map_df <- get_map(location = 'massachusetts', maptype = "hybrid", zoom = 9)
# str(MxC_Map_df)
# P4 <- ggmap(MxC_Map_df, darken = c(0.5, "white"))
# P4 + 
#   geom_point(data = bw.o1 ,
#              aes(-longdd, latdd, color = pmpreg, size = pmpreg)) + 
#   theme_bw(10) + 
#   ggtitle("Predictions over pregnancy")




# merge in other covariates
bwfull.s <- merge(bwfull.s, participants[etapa == "00", list(folio, peso_h, talla_h, fecha_naci_M)])

# some pre-processing
# construct seasonality terms
bwfull.s[, jday := as.numeric(format(birthdate, "%j"))]
bwfull.s[, costime := cos(2*pi*jday/365.25)]
bwfull.s[, sintime := sin(2*pi*jday/365.25)]
bwfull.s[, female := sex - 1]
# simple regression
summary(lm(Fenton_Z_score ~ pmpreg + sintime + costime, data=bwfull.s))
summary(lm(peso_h ~ monpreg + gestage_comb + female + costime + sintime, data=gestpred[gestage_comb >= 37,]))

# add random intercept for aodid
summary(lmer(Fenton_Z_score ~ pmpreg + (1|aodid), data=gestpred))
summary(lmer(peso_h ~ pmpreg + gestage_comb + female + costime + sintime + (1|aodid), data=gestpred[gestage_comb >= 37,]))
summary(lmer(peso_h ~ pmlast90 + gestage_comb + female + costime + sintime + (1|aodid), data=gestpred[gestage_comb >= 37,]))
ggplot(gestpred, aes(pmpreg, Fenton_Z_score)) + geom_point() + geom_smooth()
ggplot(gestpred, aes(pmpreg, gestage_comb)) + geom_point() + geom_smooth()
ggplot(gestpred, aes(pmpreg, peso_h)) + geom_point() + geom_smooth()

# bring in land use terms
gestpred <- merge(gestpred,aodidlur[,list(aodid,elev,rden,rden_OSM)], all.x = T, by="aodid")
describe(gestpred[, list(elev,rden)])

