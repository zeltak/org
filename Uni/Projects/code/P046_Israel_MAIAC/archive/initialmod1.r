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
pm25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PMData25.csv")
pm25$date<-paste(pm25$Day,pm25$Month,pm25$Year,sep="/")
pm25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
str(pm25)
pm25[, c := as.numeric(format(day, "%Y")) ]
summary(pm25)
pm25[,c("Year","Month","Day","V10","date"):=NULL]
setnames(pm25,"StationID","x_stn_ITM")
setnames(pm25,"X","y_stn_ITM")
setnames(pm25,"Y", "stn")
str(pm25)
describe(pm25$PM25)
#clean waco values
pm25 <- pm25[PM25 < 120]

#xtract year PM
pm2003<- pm25[c==2003]




dat2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2003.rds")
summary(dat2003)
summary(pm2003)
pm2003<-na.omit(pm2003)

###################
#start with mod1
###################
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
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2003,stn,day)
setkey(closestaod,stn,day)
mod1 <- merge(pm2003, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



#base model for stage 1
#m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+pm25sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm25stge30_15k   + pm25stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))
m1.formula <- as.formula(PM25 ~ aod+ (1+aod|day))



#full model 1
out.m1_2003 = lmer(m1.formula ,data =  mod1)
#out.m1_2003 = lm(pm25 ~ aod ,data =  mod1)
#generate prediction
mod1$predicted <- predict(out.m1_2003)
#get overall R2
summary(lm(PM25~predicted,data=mod1))




