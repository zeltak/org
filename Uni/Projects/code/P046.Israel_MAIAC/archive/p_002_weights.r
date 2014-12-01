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
system.time(aod[, MaskCloud := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])

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



################################
##2003
################################
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

## subset AOD
aod2003<-aod[c==2003]

#join AOD
setkey(ot2003 ,aodid,day)
setkey(aod2003,aodid,day)

#note there will be missing in PA areas (gaza and east bank)
x<-left_join(aod2003, ot2003)
x1<-as.data.table(x)
x2<- x1[!is.na(pblid)]
x2<-x2[, c(8:27) := NULL]


#Join PBL
setkey(pbl , day, pblid)
setkey(x2, day, pblid)
x3<-left_join(x2, pbl)

#create for model
x3<-x3[,obs:=0]
x3<- x3[ aod  > 0 , obs  := 1]


w1<- glm(obs ~ elev_m+slp+tempc+as.factor(m),family=binomial,data=test4.wt)

summary(w1)
test4.wt$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option
test4.wt$wt <- 1/test4.wt$prob
test4.wt$normwt <- test4.wt$wt/mean(test4.wt$wt)
summary(test4.wt$normwt)

ftest4.wt<-test4.wt[,c(1,2,9),with=FALSE]
saveRDS(ftest4.wt,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/weight_2003.rds")
