###PM25
##################################
#PM25
##################################

#PM25
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
#num. of obsv per year per stn
PM25[,length(na.omit(PM25)),by=list(stn,c)]
#PM25_m means avialble obs per year
PM25[, PM25_n := length(na.omit(PM25)),by=list(stn,c)]
#clear non PM25 days
PM25<-PM25[!is.na(PM25)]
#clear non continous stations
PM25 <- PM25[PM25_n > 5  , ]
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
PM25<-na.omit(PM25)

xyll<- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/stnpm25.csv")
setkey(PM25,stn)
setkey(xyll,stn)
PM25<-merge(PM25,xyll[,list(stn,long,lat)])





#aod
allbestpred<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/AOD_AQ_0014_3k.RDS")
aod5kmw<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/IL3kclipgrid_within5k.csv")
allbestpred.se <-allbestpred[allbestpred$aodid %in% aod5kmw$aodid, ] 


##############################################################################3
#collocate pm station and aod grid
slon=PM25[,10] ; slat=PM25[,11]

###lat-lon to earth distance in meters### script taken from "http://www.biostat.umn.edu/~sudiptob/Software/distonearth.R"
geodetic.distance <- function(point1, point2) 
{ 
  R <- 6371 
  p1rad <- point1 * pi/180 
  p2rad <- point2 * pi/180 
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d) 
  R*d 
} 

allbestpred.se<-allbestpred.se[1:10,]

##########################

for (I in 1:dim(allbestpred.se)[1]) {
  
  lon=allbestpred.se[I,6];  lat=allbestpred.se[I,5]   
  i = which.min(abs(slon-lon) + abs(slat-lat))
  point1=c(slon[i] ,slat[i] ) 
  point2=c(lon,lat)
  y=geodetic.distance(point1,point2)
  allbestpred.se$stn=as.factor(PM25[i,1]) 
  allbestpred.se$dist=y
  
}
# 1242811
AODPM=allbestpred.se[which(allbestpred.se$dist<sqrt(4.5)),]

colnames(AODPM)[9:10]=c("stn","dist")

#########################################################################3


# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
#pm.m <- makepointsmatrix(PM25, "x_stn_ITM", "y_stn_ITM", "stn")
pm.m <- makepointsmatrix(PM25, "long", "lat", "stn")


#create aod terra matrix
itm<-read.csv("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/Archive/pwITM.csv")
itm<-as.data.table(itm)
setkey(allbestpred , aodid)
setkey(itm, aodid)
allbestpred <- merge(allbestpred, itm[,list(aodid,x_aod_ITM, y_aod_ITM)], all.x = T)
summary(allbestpred)
allbestpred<-allbestpred[x_aod_ITM.x != 'NA']
allbestpred$aodid<-as.character(allbestpred$aodid)
setnames(allbestpred,"x_aod_ITM.x","x_aod_ITM")
setnames(allbestpred,"y_aod_ITM.x","y_aod_ITM")
#aod.m <- makepointsmatrix(allbestpred, "x_aod_ITM", "y_aod_ITM", "aodid")
aod.m <- makepointsmatrix(allbestpred, "long_aod", "lat_aod", "aodid")


########### join aqua to PM25
closestaod <- nearestbyday(pm.m, aod.m, 
                              PM25, allbestpred [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 2000, maxdistance = 4500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(PM25,stn,day)
setkey(closestaod,stn,day)
PM25.m1.3k <- merge(PM25, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# PM25.m1 <- PM25.m1[aod != "NA"]
summary(PM25.m1.3k)
PM25.m1.3k<-na.omit(PM25.m1.3k)

#################BAD STN

rawdf <- ddply(PM25.m1.3k, c("stn", "c"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.01]
bad<-bad %>%
  arrange(stn,c)
badu<-unique(bad$stn)
badu<-as.data.table(badu)
setnames(badu,"badu","stn")
#aggregate station xy
stnxy<-pm25.m1 %>%
  group_by(stn) %>%
  dplyr::summarise(x = mean(x_stn_ITM, na.rm=TRUE),y = mean(y_stn_ITM, na.rm=TRUE),UN = mean(UN, na.rm=TRUE),MA = mean(MaskAdjacency, na.rm=TRUE) )
# join
x<- left_join(badu, stnxy)
write.csv(x,"/home/zeltak/ZH_tmp/x3k.csv")

#################BAD STN





