library(data.table)
library(ggplot2)
library(zoo)
library(dplyr)
library(sp)
#clear workspace
rm(list = ls())


#impute pm2.5 from pm10 - FRANCE

#read pm data
pm10<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM10.all.csv")
pm10 <- pm10[Long > -12 & Lat > 7, ]
pm25<-fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/PM25.all.csv")
pm25<-pm25[Long > -12 & Lat > 7, ]



both<-merge(pm10,pm25,by=c("day","stn"))






#loop imputation per year
for (J in unique(pm10$year)) {
#find collocated pm10 and pm2.5 stations
   both.1<-both[year==J] 
stn<-both.1[,c("stn","Lat.x","Long.x"),with=FALSE]
rid<-unique(stn)
#each collocated stationd receives a unique id
rid$id<-seq(1,dim(rid)[1],1) 
both.1<-merge(both.1,rid,by=c("stn","Lat.x","Long.x"))
#create unique list of pm10 station for uear J
pm10.j<-pm10[year==J] 
pm.stn<-pm10[,c("stn","Lat","Long"),with=FALSE]
pm.rid<-unique(pm.stn)

#check distances between collocated pm stations and all pm10 stations
#unique list of collocated stations for year J
x<-as.matrix(rid[,c("Lat.x","Long.x"),with=FALSE])
#unique list of pm10 stations for year J
y<-as.matrix(pm.rid[,c("Lat","Long"),with=FALSE])
#each row in "out" represents a collocated station, each column a pm10 station...
#and the value presented is the geographical distance between them.
out<-as.data.table(spDists(x, y, longlat = TRUE))
colnames(out)<-(pm.rid$stn)
#calculate the closest distance (min) to a collocated station
min<-as.data.table(apply(out,2,min))
id<-as.data.table(apply(out,2,which.min))
min<-as.data.table(cbind(id,min))
min$stn<-pm.rid$stn
colnames(min)<-c("id","dist","stn")

#leave only stations with distances below Xkm to collocated station
#X=10
min.n<-min[dist<=10]
min.n<-min.n[,-2,with=FALSE]

#merge id into pm data 
setkey(min.n,stn)
setkey(pm10.j,stn)
pm10.m<-merge(pm10.j,min.n) 
 
#run linear regression per rid per year
old<-NULL
for (Y in 1:dim(out)[1]){
both.1.rid<-both.1[id==Y]
  reg<-lm(PM25~PM10,data=both.1.rid)  
#subset pm10.m by rid and predict
  pm10.m.rid<-pm10.m[id==Y]
  pm10.m.rid$PM25imp<-predict(reg,pm10.m.rid)
#add to previous rid results to create one table of all predicted impPM25
  pm10.m.imp<-rbind(old,pm10.m.rid)
  old<-pm10.m.imp
}
#add true pm25 monitored at that year as a separate column
pm10.m.imp$day<-as.Date(pm10.m.imp$day)
pm25$day<-as.Date(pm25$day)
pm25$year<-year(pm25$day)
pm25.j<-pm25[year==J]
pm25.j<-pm25.j[,c("stn","day","PM25","Lat","Long"),with=FALSE]
setkey(pm25.j,stn,day)
setkey(pm10.m.imp,stn,day)
pm10.imp<-merge(pm10.m.imp,pm25.j,all=TRUE,allow.cartesian=TRUE)
pm10.imp<-pm10.imp[!duplicated(pm10.imp), ]

#create combined pm column: flag=0 if monitored, 1 if imputed
pm10.imp$flag<-0
pm10.imp$flag[is.na(pm10.imp$PM25)]<- 1
pm10.imp$PM25[is.na(pm10.imp$PM25)] <- pm10.imp$PM25imp[is.na(pm10.imp$PM25)]
pm10.imp$Lat.y[is.na(pm10.imp$Lat.y)] <- pm10.imp$Lat.x[is.na(pm10.imp$Lat.y)]
pm10.imp$Long.y[is.na(pm10.imp$Long.y)] <- pm10.imp$Long.x[is.na(pm10.imp$Long.y)]
setnames(pm10.imp,"Lat.y","lat_stn")
setnames(pm10.imp,"Long.y","long_stn")

#exclude observations with negative PM2.5 imputed values
pm10.imp<-pm10.imp[PM25>0]
#save
write.csv(pm10.imp,paste("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\",J,".csv",sep=""))

}
#clear workspace
rm(list = ls())

#check results
pm25<-fread("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\pm25\\new\\PM25.all.csv")
pm25$day<-as.Date(pm25$day)
pm25$year<-year(pm25$day)
setkey(pm25,year)

old<-NULL
for (I in unique(pm25$year)) {
pm25.j<-pm25[year==I]
imp<-fread(paste("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\",I,".csv",sep=""))

tab1<-summary(pm25.j$PM25)
tab2<-summary(imp$PM25) #combined PM25
tab<-rbind(tab1,tab2)
row.names(tab)<-c(paste("mon.",I,sep=""),paste("imp.",I,sep=""))
imp.n<-na.omit(imp)
cor<-round(cor(imp.n$PM25,imp.n$PM25imp),3) #for observations with only both monitored and imputed PM25
tab<-cbind(tab,cor)

fin<-rbind(old,tab)
old<-fin
}
View(fin)
write.csv(fin,"C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\cor_table_pm25.csv")
