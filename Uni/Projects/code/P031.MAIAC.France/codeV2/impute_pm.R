library(data.table)
library(ggplot2)
library(zoo)
library(dplyr)
library(sp)
library(magrittr)
library(raster)
library(rgdal)
#clear workspace
rm(list = ls())


#impute pm2.5 from pm10 - FRANCE

#read pm data and clean from duplicate obs by method per day and stn
#pm10
pm10<-fread("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\pm10\\new\\PM10.all.csv")
#pm10 <- pm10[Long > -12 & Lat > 7, ]
#pm2.5
pm25<-fread("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\pm25\\new\\PM25.all.csv")
#pm25<-pm25[Long > -12 & Lat > 7, ]

both<-merge(pm10,pm25,by=c("day","stn"))
old=NULL
#loop imputation per year
for (J in unique(pm10$year)) {
#find collocated pm10 and pm2.5 stations
   both.1<-both[year.x==J] 
stn<-both.1[,c("stn","Lat.x","Long.x"),with=FALSE]
rid<-unique(stn)

#each collocated stationd receives a unique id
rid$id<-seq(1,dim(rid)[1],1) 

both.1<-merge(both.1,rid,by=c("stn","Lat.x","Long.x"))
#create unique list of pm10 station for uear J
pm10.j<-pm10[year==J] 
pm.stn<-pm10.j[,c("stn","Lat","Long"),with=FALSE]

pm.rid<-unique(pm.stn)
pm.rid$Lat<-as.numeric(pm.rid$Lat)
pm.rid$Long<-as.numeric(pm.rid$Long)
rid$Long<-as.numeric(rid$Long)
rid$Lat<-as.numeric(rid$Lat)
rid<-rid[,c(1,4:6),with=FALSE]
#check distances between collocated pm stations and all pm10 stations
#unique list of collocated stations for year J
#each row in "dist" represents a collocated station, each column a pm10 station...
#and the value presented is the geographical distance between them.
x=rid
y=pm.rid
coordinates(x) = ~ Long + Lat
coordinates(y) = ~ Long + Lat
proj4string(x) = CRS("+init=epsg:4326")
proj4string(y) = CRS("+init=epsg:4326")

# Distances
dist = spDists(x, y)

dist_min = apply(dist, 1, function(x) sort(x)[2])
dist_min_id = apply(dist, 1, function(x) which(x == sort(x)[2]))
rid$stn_pm10 = pm.rid$stn[dist_min_id]
rid$dist = t(dist_min)
#leave only stations with distances below Xkm to collocated station
#X=10
min.n<-rid[dist<=10]
#merge id into pm data 
setkey(min.n,stn_pm10)
setnames(pm10.j,"stn","stn_pm10")
setkey(pm10.j,stn_pm10)
pm10.m<-merge(pm10.j,min.n,by="stn_pm10") 
 
#run linear regression per rid per year
OLD<-NULL
for (Y in 1:dim(dist)[1]){
both.1.rid<-both.1[id==Y]
  reg<-lm(PM25~PM10,data=both.1.rid)
  
#subset pm10.m by rid and predict
  pm10.m.rid<-pm10.m[id==Y]
  pm10.m.rid$PM25imp<-predict(reg,pm10.m.rid)
  pm10.m.rid$rsq<-summary(reg)$r.squared
  pm10.m.rid$N<-summary(reg)$df[2]+2
#add to previous rid results to create one table of all predicted impPM25
  pm10.m.imp<-rbind(OLD,pm10.m.rid)
  OLD<-pm10.m.imp
}
regres<-pm10.m.imp%>%group_by(id,stn)%>%summarise(rsq=mean(rsq))
print(min(regres$rsq))
#add true pm25 monitored at that year as a separate column
pm10.m.imp<-pm10.m.imp[,c("stn_pm10","day","Lat.x","Long.x","year","PM25imp","rsq"),with=FALSE]
setnames(pm10.m.imp,"stn_pm10","stn")
pm10.m.imp$day<-as.Date(pm10.m.imp$day)
pm25$day<-as.Date(pm25$day)
#pm25$year<-year(pm25$day)
pm25.j<-pm25[year==J]
pm25.j<-pm25.j[,c("stn","day","PM25","Lat","Long"),with=FALSE]
setkey(pm25.j,stn,day)
setkey(pm10.m.imp,stn,day)
pm10.imp<-merge(pm10.m.imp,pm25.j,all=TRUE,allow.cartesian=TRUE)
pm10.imp<-pm10.imp[!duplicated(pm10.imp), ]

#create combined pm column: flag=0 if monitored, 1 if imputed
pm10.imp$flag<-0
pm10.imp$flag[is.na(pm10.imp$PM25)]<- 1
pm10.imp$Lat[is.na(pm10.imp$PM25)] <- pm10.imp$Lat.x[is.na(pm10.imp$PM25)]
pm10.imp$Long[is.na(pm10.imp$PM25)] <- pm10.imp$Long.x[is.na(pm10.imp$PM25)]
pm10.imp$PM25[is.na(pm10.imp$PM25)] <- pm10.imp$PM25imp[is.na(pm10.imp$PM25)]

#exclude observations with negative PM2.5 imputed values
pm10.imp<-pm10.imp[PM25>0]
#exclude imputing obs when r square between pm2.5 an pm10 is below 0.6
pm10.imp.r<-pm10.imp[rsq>0.65]

#check results
pm25.j<-pm25[year==J]
tab1<-summary(pm25.j$PM25)
tab2<-summary(pm10.imp.r$PM25) #combined PM25
tab<-rbind(tab1,tab2)
row.names(tab)<-c(paste("mon.",I,sep=""),paste("imp.",I,sep=""))
imp.n<-na.omit(pm10.imp.r)
cor<-round(cor(imp.n$PM25,imp.n$PM25imp),3) #for observations with only both monitored and imputed PM25
tab<-cbind(tab,cor)
fin<-rbind(old,tab)
old<-fin

pm10.imp.r<-pm10.imp.r[,c("stn","day","PM25","Lat","Long","flag"),with=FALSE]

#save
#write.csv(pm10.imp.r,paste("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\",J,".csv",sep=""))
}

#View final table of summary stat and cor between monitored and imputed PM2.5
View(fin)

#merge all imputed PM2.5 files to final imputed pm dataset for model
setwd("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed")
files <- list.files( pattern="*.csv", full.names=TRUE)
files<-files[1:14]
imp.all = rbindlist(lapply( files, fread ))
imp.all<-imp.all[,-1,with=FALSE]

write.csv(imp.all,"C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\imp25.all.csv")

