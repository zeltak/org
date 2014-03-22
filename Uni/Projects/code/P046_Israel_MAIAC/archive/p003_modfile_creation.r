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


#PM
pm10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData10.csv")
pm10$date<-paste(pm10$Day,pm10$Month,pm10$Year,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
str(pm10)
pm10[, c := as.numeric(format(day, "%Y")) ]
summary(pm10)
pm10[,c("Year","Month","Day","V10","date"):=NULL]
setnames(pm10,"StationID","x_stn_ITM")
setnames(pm10,"X","y_stn_ITM")
setnames(pm10,"Y", "stn")

summary(pm10$PM10)
describe(pm10$PM10)
describe(pm10$day)
test<-as.data.table(pm10[PM10>=300])
dim(test)
write.csv(test,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/Archive/highvaluePM10/highvalpm10.csv")
describe(test$day)

#clean waco values
pm10 <- pm10[PM10 < 120]
#xtract year PM
pm2003<- pm10[c==2003]

dat2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2003.rds")
summary(dat2003)
test<-as.data.table(dat2003[aod>=2])
dim(test)
describe(test$aod)
write.csv(test,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/Archive/highvaluePM10/highvalaod2003.csv")



dat2003[, describe(aod)]
ggplot(dat2003[, list(aod)], aes(aod)) + geom_histogram(binwidth = 0.005) + theme_bw(16)
# remove values that are too high to tease apart anthropogenic contribution
dat2003<-dat2003[aod <= 3]


# this confirms there are aodIDs that are completely missing aod for a given year
dat2003[, yr := format(day, "%Y")]
# dat2003[,list(nobsmissing = 5925 - length(unique(aodid))),by="yr"]



xdat2003<-dat2003[aod >= 2]

# plot 2003 and check for missing
ggplot(xdat2003[yr == 2003,list(nobs = .N, meanaod = mean(aod, na.rm = T),
long_aod = x_aod_ITM[1], lat_aod = y_aod_ITM[1]),by=aodid], aes(long_aod, lat_aod, color = nobs)) + geom_point(size = 3.5, shape = 15)



map <- get_map(location = 'Israel', maptype = "hybrid", zoom = 7)
str(map)
P12 <- ggmap(map, fullpage = TRUE, darken = c(0.5, "white"))

P12 + geom_point(size = 3.5, shape = 15, aes(long_aod, lat_aod, color = nobs), 
                 data = dat2003[yr == 2003, 
                                 list(nobs = .N, meanaod = mean(aod, na.rm = T),
                                      long_aod = x_aod_ITM[1], lat_aod = y_aod_ITM[1]),
                                 by=aodid])

dat2003[yr == 2003, 
         list(nobs = .N, meanaod = mean(aod, na.rm = T),
              long_aod = x_aod_ITM[1], lat_aod = y_aod_ITM[1]),
         by=aodid][, range(long_aod)]















###################
#start with mod1
###################
#delete missing values
pm2003<-na.omit(pm2003)

# import monitor data and spatial merge with nearestbyday()
source("/media/NAS/Uni/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2003, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod matrix
aod.m <- makepointsmatrix(dat2003[dat2003[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm2003, dat2003 [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm2003,stn,day)
setkey(closestaod,stn,day)
mod1 <- merge(pm2003, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]


x1<-mod1[aod >= 2]



#base model for stage 1
#m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))
m1.formula <- as.formula(PM10 ~ aod+ (1+aod|day))


#full model 1
out.m1_2003 = lmer(m1.formula ,data =  mod1)
#out.m1_2003 = lm(PM10 ~ aod ,data =  mod1)
#generate prediction
mod1$predicted <- predict(out.m1_2003)
#get overall R2
summary(lm(PM10~predicted,data=mod1))





