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

##import AOD
aod<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014.RDS")


#################################33

#import LU
lu1<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/LU1.csv")
#add Land cover to LU
p_os<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_os.csv")
p_dev<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devHG.csv")
p_dos<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_devOS.csv")
p_farm<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_farming.csv")
p_for<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_forest.csv")
p_ind<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/p_industry.csv")

lu1 <- merge(lu1, p_os[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_os:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dev[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dev:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_dos[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_dos:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_farm[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_farm:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_for[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_for:=MEAN*100]
lu1[,MEAN:=NULL]
lu1 <- merge(lu1, p_ind[, list(aodid,MEAN)], all.x= T, by = c("aodid"))
lu1[,p_ind:=MEAN*100]
lu1[,MEAN:=NULL]
#delete "palestine"
wlu<-lu1[!is.na(p_for)]

#Temp
Temp <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/temporal MOEP/Temp_D.csv")
Temp$date<-paste(Temp$Day,Temp$Month,Temp$Year,sep="/")
Temp[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
Temp[, c := as.numeric(format(day, "%Y")) ]
Temp[,c("Year","Month","Day","date"):=NULL]
Temp <- Temp[X != 'NaN']
#remove missing
Temp[,length(na.omit(Temp)),by=list(stn,c)]
Temp[, Temp_miss := length(na.omit(Temp)),by=list(stn,c)]
Temp<-Temp[Temp_miss > 364]





################################
##2003
################################
#xtract year met
met2003<- met[c==2003]


#create full LU TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2003))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)



source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
pm.m <- makepointsmatrix(Temp, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
mod3.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(mod3.m ,pm.m , 
                            test4.se, Temp [, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 3, maxdistance = NA)

#check data completness
x1<-closestaodse[, .N, by=c("aodid")]
summary(x1)









# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")
#create PM matrix
wlu.m <- makepointsmatrix(wlu, "x_stn_ITM", "y_stn_ITM", "stn")
#create aod terra matrix
t.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
########### join Terra to PM10
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10, terra [, list(day, aodid, aod,UN,WV,QA)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

########### 
# meanaod <- nearestbyday(pm.m, aod.m, 
#                            PM10, terra [, list(day, aodid, aod,UN,WV,QA)], 
#                            "stn", "aodid", "closestaod", "aod", knearest = 9, maxdistance = 2200, nearestmean = T)




setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod[,list(stn,day,aod,UN,WV,QA)], all.x = T)





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
setkey(test4.se.met,aodid,day)
setkey(xw2003,aodid,day)
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
