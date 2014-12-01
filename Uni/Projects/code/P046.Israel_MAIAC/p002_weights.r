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
aod[, c := as.numeric(format(day, "%Y")) ]
aod<-aod[, c(7:24) := NULL]

#create full LU TS
days<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
fullaod <- data.table(expand.grid(aodid = aod[, unique(aodid)], day = days))
setkey(fullaod,aodid)
setkey(aod,aodid)
#aodf<- merge(fullaod,aod,all.x=TRUE)
aodf<-left_join(fullaod,aod)
aodf<-aodf[, c(3:4,6:11) := NULL]
aodf[, c := as.numeric(format(day, "%Y")) ]


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

#import PBL
pbl <-  fread("/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/fianlpblXY_2002.csv")
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Data/Europe/PBL_Europe/dailymeanpbl/"

for(i in 2002:2013){
  allbestpredlist[[paste0("year_", i)]] <- fread(paste0(path.data, "fianlpblXY_", i, ".csv"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

pbl <-  allbestpred[ longitude > 32 & longitude < 37 & latitude < 34 & latitude > 29, ]
pbl <- pbl [, day:=as.Date(strptime(date, "%m/%d/%Y"))]

#full grid 28382 with PA)



################################
##2003
################################

## subset AOD
aod2003<-aodf[c==2003]

#xtract year met
met2003<- Temp[c==2003]
#create full LU TS
days_2003<-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2003))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2003, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2003[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2003 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2003 ,aodid,day)
setkey(aod2003,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2003, ot2003)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2003.rds")



################################
##2004
################################

## subset AOD
aod2004<-aodf[c==2004]

#xtract year met
met2004<- Temp[c==2004]
#create full LU TS
days_2004<-seq.Date(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2004))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2004, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2004[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2004 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2004 ,aodid,day)
setkey(aod2004,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2004, ot2004)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2004.rds")






################################
##2005
################################

## subset AOD
aod2005<-aodf[c==2005]

#xtract year met
met2005<- Temp[c==2005]
#create full LU TS
days_2005<-seq.Date(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2005))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2005, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2005[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2005 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2005 ,aodid,day)
setkey(aod2005,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2005, ot2005)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2005.rds")




################################
##2006
################################

## subset AOD
aod2006<-aodf[c==2006]

#xtract year met
met2006<- Temp[c==2006]
#create full LU TS
days_2006<-seq.Date(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2006))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2006, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2006[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2006 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2006 ,aodid,day)
setkey(aod2006,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2006, ot2006)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2006.rds")



################################
##2007
################################

## subset AOD
aod2007<-aodf[c==2007]

#xtract year met
met2007<- Temp[c==2007]
#create full LU TS
days_2007<-seq.Date(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2007))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2007, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2007[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2007 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2007 ,aodid,day)
setkey(aod2007,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2007, ot2007)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2007.rds")



################################
##2008
################################

## subset AOD
aod2008<-aodf[c==2008]

#xtract year met
met2008<- Temp[c==2008]
#create full LU TS
days_2008<-seq.Date(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2008))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2008, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2008[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2008 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2008 ,aodid,day)
setkey(aod2008,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2008, ot2008)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2008.rds")



################################
##2009
################################

## subset AOD
aod2009<-aodf[c==2009]

#xtract year met
met2009<- Temp[c==2009]
#create full LU TS
days_2009<-seq.Date(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2009))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2009, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2009[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2009 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2009 ,aodid,day)
setkey(aod2009,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2009, ot2009)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2009.rds")



################################
##2010
################################

## subset AOD
aod2010<-aodf[c==2010]

#xtract year met
met2010<- Temp[c==2010]
#create full LU TS
days_2010<-seq.Date(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2010))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2010, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2010[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2010 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2010 ,aodid,day)
setkey(aod2010,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2010, ot2010)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2010.rds")



################################
##2011
################################

## subset AOD
aod2011<-aodf[c==2011]

#xtract year met
met2011<- Temp[c==2011]
#create full LU TS
days_2011<-seq.Date(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2011))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2011, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2011[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2011 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2011 ,aodid,day)
setkey(aod2011,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2011, ot2011)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2011.rds")



################################
##2012
################################

## subset AOD
aod2012<-aodf[c==2012]

#xtract year met
met2012<- Temp[c==2012]
#create full LU TS
days_2012<-seq.Date(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2012))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2012, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2012[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2012 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2012 ,aodid,day)
setkey(aod2012,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2012, ot2012)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2012.rds")



################################
##2013
################################

## subset AOD
aod2013<-aodf[c==2013]

#xtract year met
met2013<- Temp[c==2013]
#create full LU TS
days_2013<-seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 1)
#create date range
test3.se <- data.table(expand.grid(aodid = wlu[, unique(aodid)], day = days_2013))
setkey(test3.se,aodid)
setkey(wlu,aodid)
test4.se<- merge(test3.se,wlu,all.x=TRUE)

source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday_MPM.r")
#create PM matrix
met.m <- makepointsmatrix(met2013, "X", "Y", "stn")
#create aod matrix
setkey(test4.se, aodid)
lu.m <- makepointsmatrix(test4.se[test4.se[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

closestaodse<- nearestbyday(lu.m ,met.m , 
                            test4.se, met2013[, list(day,Temp,stn)], 
                            "aodid", "stn", "meanT", "Temp", knearest = 1, maxdistance = NA)


setkey(test4.se,aodid,day)
setkey(closestaodse,aodid,day)
ot2013 <- merge(test4.se[,list(stn,day,aodid,elev,x_aod_ITM, y_aod_ITM,pblid)], closestaodse[,list(day,Temp,aodid)], all.x = T)

#join AOD
setkey(ot2013 ,aodid,day)
setkey(aod2013,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2013, ot2013)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]

#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=1]
x3[is.na(aod), obs:= 0]
x3[, m := as.numeric(format(day, "%m")) ]


#model
w1<- glm(obs ~ elev+MeanPbl+Temp+as.factor(m),family=binomial,data=x3)
x3$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
x3$wt <- 1/x3$prob
x3$normwt <- x3$wt/mean(x3$wt)
x4<-x3[,c(2,3,15,18:19),with=FALSE]
saveRDS(x4,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN009_weights/weight_2013.rds")





