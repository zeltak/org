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

if(!exists("m2_agg")){
  m2_agg<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
}

#################################33
lu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
poplu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/pop_lu.csv")
stack <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/EPA/NEI05_stacks/midatlneweng_nei05stacks.csv" )
setnames(stack,"guid_", "guid")
lc <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/1km_landcover.csv" )
ems <- fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/midatlantic_newengland/LU_emis_EPA.csv" )
setkey(lu, guid)
setkey(stack, guid)
setkey(lc, guid)
setkey(ems, guid)
setkey(poplu, guid)
lux <- merge(lu,stack ,all.x = T)
lux <- merge(lux,lc ,all.x = T)
lux <- merge(lux,ems ,all.x = T)
lux <- merge(lux,poplu ,all.x = T)

#subset to study area
lux <- lux [lux $guid %in% m2_agg$guid, ] 
l=seq(names(lux));names(l)=names(lux);l
wlu<-lux[,c(1,3,4,8,21,43),with=FALSE]

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




################################
##2003
################################
#xtract year met
met2003<- met[c==2003]


#create full TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(guid = wlu[, unique(guid)], day = days_2003))
setkey(test3.se,guid)
setkey(wlu,guid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(test4.se, day, stn)
test4.se.met <- merge(test4.se, met2003 , all.x = T)
summary(test4.se.met)
test4.se.met [, c("TEMP", "dewp","date","lat_met","long_met","wvp_mb","ah_gm3","c") := NULL]

## import AOD
w2003<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003.rds")
#l=seq(names(w2003));names(l)=names(w2003);l
xw2003<-w2003[,c(1,2,9),with=FALSE]



#join AOD
setkey(test4.se.met,guid,day)
setkey(xw2003,guid,day)
#we allow cartesian since there is some site codes sharing a lpmid and thus need to expand the base lpmid-date file
#to check correctnes issue:
#length(test2[,unique(SiteCode)])*365
test4.wt<- merge(test4.se.met,xw2003,all.x=TRUE)
test4.wt$obs<-1
test4.wt[is.na(aod), obs := 0]
test4.wt [, c("stn","lat_aod","long_aod","pblid","WDSP","visib","aod") := NULL]
test4.wt[, m := as.numeric(format(day, "%m")) ]
test4.wt <- na.omit(test4.wt)


w1<- glm(obs ~ elev_m+slp+tempc+as.factor(m),family=binomial,data=test4.wt)

summary(w1)
test4.wt$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
test4.wt$wt <- 1/test4.wt$prob
test4.wt$normwt <- test4.wt$wt/mean(test4.wt$wt)
summary(test4.wt$normwt)

ftest4.wt<-test4.wt[,c(1,2,9),with=FALSE]
saveRDS(ftest4.wt,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/weight_2003.rds")
