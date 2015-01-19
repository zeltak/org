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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(doRNG)
library(doParallel)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.TR.rds")

#------------>  take out stn with co located PM10/25 with very high ratios
{
#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 900 ]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
#calculate meanPM per grid per day to each station (excluding first station)
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 < 20200 ]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
setkey(PM10,stn,day)
setkey(PM25,stn,day)
PM.j=merge(PM10,PM25,by=c("stn","day"))
#leave only stations with both PM2.5 and PM 10 measurements
PM.j=na.omit(PM.j)
PM.j$ratio=PM.j[,PM25]/PM.j[,PM10]
PM.j[,badstn := paste(stn,day,sep="-")]
#################BAD STN
m1.all[,badstn := paste(stn,day,sep="-")]
PM.j<- PM.j[ratio > 0.95]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badstn %in% PM.j$badstn), ] 
}


#cal prev/post day
#read PM data
{
setkey(PM25,stn,day)
PM25<-as.data.frame(PM25)

Data1 <- slide(PM25, Var = "PM25", GroupVar = "stn",
               slideBy = 1)
Data2 <- slide(Data1, Var = "PM25", GroupVar = "stn",
               slideBy = -1)

data2<-as.data.table(Data2)
setkey(data2,day,stn)
setnames(data2,"PM251","PM25_pre")
setnames(data2,"PM25-1","PM25_post")
setkey(m1.all,day,stn)
m1.all <- merge(m1.all, data2[,list(stn,day, PM25_pre, PM25_post)], all.x = T)
summary(m1.all)
}

#rescale and clean
{
m1.all[,elev.s:= scale(elev)]
m1.all[,tden.s:= scale(tden)]
m1.all[,pden.s:= scale(pden)]
m1.all[,dist2A1.s:= scale(dist2A1)]
m1.all[,dist2water.s:= scale(dist2water)]
m1.all[,dist2rail.s:= scale(dist2rail)]
m1.all[,Dist2road.s:= scale(Dist2road)]
m1.all[,ndvi.s:= scale(ndvi)]
m1.all[,MeanPbl.s:= scale(MeanPbl)]
m1.all[,p_ind.s:= scale(p_ind)]
m1.all[,p_for.s:= scale(p_for)]
m1.all[,p_farm.s:= scale(p_farm)]
m1.all[,p_dos.s:= scale(p_dos)]
m1.all[,p_dev.s:= scale(p_dev)]
m1.all[,p_os.s:= scale(p_os)]
m1.all[,tempa.s:= scale(Temp.im)]
m1.all[,WDa.s:= scale(WD.im)]
m1.all[,WSa.s:= scale(WS.im)]
m1.all[,RHa.s:= scale(RH.im)]
m1.all[,Raina.s:= scale(Rain.im)]
m1.all[,NOa.s:= scale(NO.im)]
m1.all[,O3a.s:= scale(O3.im)]
m1.all[,SO2a.s:= scale(SO2.im)]
################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.all, c("stn","m"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,m,sep="-")]
#################BAD STN
m1.all[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badid %in% bad$badid), ] 

summary(m1.all)
#clear missings
m1.all<- m1.all[!is.na(m1.mpm)]
m1.all<- m1.all[!is.na(PM25_pre)]
m1.all<- m1.all[!is.na(PM25_post)]
}



m1.formula <- as.formula(PM25~ aod
                         +PM25_pre+PM25_post+m1.mpm
                         +tempa.s
                         +elev.s+tden.s+pden.s+ndvi.s #spatial
                        +p_os.s
                        +(1+aod|day)) #+(1|stn) !!! stn screws up mod3 




# cross-validation and model building
# repeated leave x monitors out CV
mons <- unique(m1.all); length(mons)
xout <- 2 # number of monitors to hold out
# how many combinations if we pull out xout mons
ncol(combn(mons, xout))
n.iter <- 30
# we will compute mean of the other monitors using all monitoring data
setkey(m1.all, stn)

# list to store scheme
cvscheme <- list()
cvout <- list()
# set seed for reproducibility
set.seed(20150112)

# cross-validation in parallel
registerDoParallel(14)
# use a proper reproducible backend RNG
registerDoRNG(1234)
system.time({
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
  #system.time(for(i in 1:n.iter){
  #mons.test <- mons[sample(length(mons), xout)]
  mons.test <- combn(mons, xout)[,i]
  cvscheme[[i]] <- mons.test
  test <- m1.all[stn %in% mons.test, ]
  train<- m1.all[!stn %in% mons.test, ]

#   # calculate loo mean among sites in train (drop 1 monitor at a time) - excluding PER
#   pm[!neveruse, allmeandrop1 := sapply(.SD$daymean, function(z) mean(.SD[mon!=z,daymean])), by=day]
#   train[, allmeandrop1 := NULL]
#   train <- merge(train, pm[, list(day, mon, allmeandrop1)], by = c("day", "mon"), all.x = T)
#   # but in test - allmeandrop1 should use the mean of all monitors in train
#   pm[!neveruse, allmeandrop1 := mean(daymean, na.rm = T), by=day]
#   test[, allmeandrop1 := NULL]
#   test <- merge(test, pm[, list(day, mon, allmeandrop1)], by = c("day", "mon"), all.x = T)
  
  # fit the model
  print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  #run the actual model
  trainmod <-  lmer(m1.formula, data =  train,weights=normwt)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i  
  # export these results
  test[, list(day, stn, PM25, predcv, itercv)]
}# end of cross-validation loop
})
summary(lm(PM25 ~ predcv, data = iter.out))
# compute root mean squared error
iter.out[, sqrt(mean((PM25 - predcv)^2))]
