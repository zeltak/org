
#########Aqua.2003
#reload mod3
aqm2.2003<- readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
head(aqm2.2003)
aqm2.2003$meanPM25<- NULL
aqm2.2003$meanPM10<- NULL
#make sure
aqm2.2003$meanPM10<- NULL
aqm2.2003$long_aod<- NULL
aqm2.2003$lat_aod<- NULL
gc()



#Fix LAT/LONG
setkey(aqm2.2003,aodid)
setkey(fgrid,aodid)
aqm2.2003 <- merge(aqm2.2003,fgrid[,list(aodid,long_aod,lat_aod)],all.x = T)


#PM
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)


#-------> meanPM25  for mod 2+3
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
setkey(aqm2.2003, aodid)
aod.m <- makepointsmatrix(aqm2.2003[aqm2.2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2003, PM25 [, list(day,pm25,stn)], 
                            "aodid", "stn", "closest","pm25",knearest = 7, maxdistance = 60000, nearestmean = T)
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2003,aodid,day)
aqm2.2003 <- merge(aqm2.2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2003,"closestmean","meanPM25")
gc()
#-------> meanPM10  for mod 2+3
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
setkey(aqm2.2003, aodid)
aod.m <- makepointsmatrix(aqm2.2003[aqm2.2003[,unique(aodid)], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")

pmj1<- nearestbyday(aod.m  ,pm.m , 
                            aqm2.2003, PM10 [, list(day,pm10,stn)], 
                            "aodid", "stn", "closest","pm10",knearest = 7, maxdistance = 60000, nearestmean = T)
gc()
#join to DB
setkey(pmj1,aodid,day)
setkey(aqm2.2003,aodid,day)
aqm2.2003 <- merge(aqm2.2003,pmj1[,list(day,aodid,closestmean)],all.x = T)
setnames(aqm2.2003,"closestmean","meanPM10")
summary(aqm2.2003$meanPM10)
#cleanup
keep(fgrid,aqm2.2003,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()


#import regions
reg<-read.dbf("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/Qgis/france_grid_region.dbf")
reg<-as.data.table(select(reg,aodid,reg,region))
#create aodid
#aqm2.2003$aodid<-paste(aqm2.2003$long_aod,aqm2.2003$lat_aod,sep="-")
setkey(aqm2.2003,aodid)
setkey(reg,aodid)
aqm2.2003 <- merge(aqm2.2003,reg,all.x = T)

#---------> save mod3
#clean
aqm2.2003[,c("ndviid"):=NULL]
saveRDS(aqm2.2003,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")

#########-------------------weights ############
aqm2.2003<-aqm2.2003[,obs:=1]
aqm2.2003[is.na(aod), obs:= 0]
ws.2003<-select(aqm2.2003,obs,elev_m,PBL,m,tempavg,aodid,day)
ws.2003<-filter(ws.2003,!(is.na(tempavg)))
rm(aqm2.2003)
gc()

#splits
ws.2003.s1<-ws.2003[1:50000000,]
w1.s1<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2003.s1)
ws.2003.s1$prob <- predict(w1.s1,type = c("response"))  
ws.2003.s1$wt <- 1/ws.2003.s1$prob
ws.2003.s1$normwt <- ws.2003.s1$wt/mean(ws.2003.s1$wt)
ws.2003.s1[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s1)
gc()


#splits
ws.2003.s2<-ws.2003[50000001:100000000,]
w1.s2<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2003.s2)
ws.2003.s2$prob <- predict(w1.s2,type = c("response"))  
ws.2003.s2$wt <- 1/ws.2003.s2$prob
ws.2003.s2$normwt <- ws.2003.s2$wt/mean(ws.2003.s2$wt)
ws.2003.s2[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s2)
gc()

#splits
ws.2003.s3<-ws.2003[100000001:150000000,]
w1.s3<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2003.s3)
ws.2003.s3$prob <- predict(w1.s3,type = c("response"))  
ws.2003.s3$wt <- 1/ws.2003.s3$prob
ws.2003.s3$normwt <- ws.2003.s3$wt/mean(ws.2003.s3$wt)
ws.2003.s3[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s3)
gc()


#splits
x<-dim(ws.2003)
ws.2003.s4<-ws.2003[150000001:x[1],]
w1.s4<- glm(obs ~ elev_m+PBL+as.factor(m)+tempavg,family=binomial,data=ws.2003.s4)
ws.2003.s4$prob <- predict(w1.s4,type = c("response"))  
ws.2003.s4$wt <- 1/ws.2003.s4$prob
ws.2003.s4$normwt <- ws.2003.s4$wt/mean(ws.2003.s4$wt)
ws.2003.s4[, c("prob", "wt","obs","elev_m", "PBL" , "m","tempavg"  ) := NULL]
rm(w1.s4)
gc()

wf<-rbindlist(list(ws.2003.s1,ws.2003.s2,ws.2003.s3,ws.2003.s4))

#reread m3
aqm2.2003<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod3.AQ.2003.rds")
setkey(aqm2.2003,aodid,day)
setkey(wf,aodid,day)
aqm2.2003 <- merge(aqm2.2003,wf,all.x = T)

###################################mod2
aqm2.2003.m2 <- aqm2.2003[!is.na(aod)]
#rm m3
rm(aqm2.2003)
gc()


#calculate low retrival day
f<- aqm2.2003.m2 %>%
    group_by(aodid) %>%
    summarise(numadata = n())
#describe(f)
setkey(f,aodid)
setkey(aqm2.2003.m2,aodid)
aqm2.2003.m2<-merge(aqm2.2003.m2,f)
aqm2.2003.m2$flag1000<-0
aqm2.2003.m2<-aqm2.2003.m2[numadata < 1000, flag1000 :=1]
aqm2.2003.m2$flag500<-0
aqm2.2003.m2<-aqm2.2003.m2[numadata < 500, flag500 :=1]
gc()
#prepare mod2 scale
aqm2.2003.m2[,tden.s:= scale(tden)]
aqm2.2003.m2[,elev.s:= scale(elev_m)]
aqm2.2003.m2[,pden.s:= scale(pop06)]
aqm2.2003.m2[,dist2A1.s:= scale(distA1)]
aqm2.2003.m2[,ndvi.s:= scale(ndvi)]
aqm2.2003.m2[,MeanPbl.s:= scale(PBL)]
aqm2.2003.m2[,p_urb.s:= scale(pcturb )]
aqm2.2003.m2[,tempa.s:= scale(tempavg)]
aqm2.2003.m2[,WSa.s:= scale(wsavg)]
aqm2.2003.m2[,RHa.s:= scale(rhavg)]
aqm2.2003.m2[,Raina.s:= scale(rainday)]
#save mod2
saveRDS(aqm2.2003.m2,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
#aqm2.2003.m2<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.rds")
gc()




#--------->mod1
#PM25
#to fix missing days issues resulting in cartesean error
aqm2.2003days <- sort(unique(aqm2.2003.m2$day))

#PM import again
PM25<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm25.rds")
PM25<-filter(PM25,c==2003)
PM10<-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/pm10.rds")
PM10<-filter(PM10,c==2003)

########### join aod to PM25
#create PM matrix
pm.m <- makepointsmatrix(PM25, "long_pm25", "lat_pm25", "stn")
#create aod terra matrix
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(as.character(aodid))], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM25[day %in% aqm2.2003days,], aqm2.2003.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1 <- merge(PM25, closestaod, all.x = T)
PM25.m1<-PM25.m1[!is.na(aod)]

#save mod 1
saveRDS(PM25.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.rds")


########### join aod to PM10
#create PM matrix
pm.m <- makepointsmatrix(PM10, "long_pm10", "lat_pm10", "stn")
#create aod terra matrix
setkey(aqm2.2003.m2,aodid)
aod.m <- makepointsmatrix(aqm2.2003.m2[aqm2.2003.m2[,unique(as.character(aodid))], list(long_aod, lat_aod, aodid), mult = "first"], "long_aod", "lat_aod", "aodid")
#run function
closestaod <- nearestbyday(pm.m, aod.m, 
                           PM10[day %in% aqm2.2003days,], aqm2.2003.m2, 
                           "stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)


#closestaod[,i.stn :=NULL]
closestaod[,closestknn :=NULL]

setkey(PM10,stn,day)
setkey(closestaod,stn,day)
PM10.m1 <- merge(PM10, closestaod, all.x = T)
PM10.m1<-PM10.m1[!is.na(aod)]

#save mod 1
saveRDS(PM10.m1,"/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM10.rds")


keep(fgrid,nearestbyday,nearestbydayM1,makepointsmatrix, sure=TRUE) 
gc()
