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
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.AQ.rds")
#take out station with wildly diff PM from surrounding stations
neveruse <- c("TMM","ASH","KV9","BIL","REM")
m1.all <- m1.all[!stn %in% neveruse]

# take out stn with co located PM10/25 with very high ratios
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
PM10<-PM10[PM10 > 0.000000000001 & PM10 <  2000 ]
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
bad<- raWDaf[R2 <= 0.1]
bad[,badid := paste(stn,m,sep="-")]
#################BAD STN
m1.all[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badid %in% bad$badid), ] 

#added met region
xreg<-fread("/media/NAS/Uni/ztmp/treg.csv")
setkey(xreg,stn)
setkey(m1.all,stn)
m1.all <- merge(m1.all,xreg[,list(stn,reg5=metreg_1)],all.x = T)

# #residuals analysis
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
#full fit
m1_sc <- lmer(m1.formula,data=m1.all)
m1.all$res <- resid(m1_sc)
# resx<-select(m1.all,res,x_aod_ITM,y_aod_ITM)
# m1.all<-filter(m1.all,res <10 & res > -10)

#monitor residuls
resmon<- m1.all %>%
    group_by(stn) %>%
    summarise(monresid = mean(res, na.rm=TRUE),x = mean(x_aod_ITM, na.rm=TRUE),y = mean(y_aod_ITM, na.rm=TRUE))
write.csv(resmon,"~/ZH_tmp/resmon.csv")

#remove aod points with many missing



summary(m1.all)
m1.all<- m1.all[!is.na(pbldag)]

saveRDS(m1.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1C.PM25.AQ.rds")

