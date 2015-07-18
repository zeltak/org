library(readr)
library(data.table)
library(celestial)
library(zoo)
library(dplyr)
#2013 data
data<-read_csv("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/FR_AQDATA_2013.CSV")
data<-as.data.frame(data)
colnames(data)=c("CODESTATION","PM10.25","Date","PMconc","QA")
#extract the date characters from V3
data$Year<- substr(data$Date, 1, 4)
data$Month<- substr(data$Date, 6, 7)
data$Day<- substr(data$Date, 9, 10)
data$Hour<- substr(data$Date, 12, 16)
data=transform(data, Date = as.Date(paste(Year,Month,Day,1,sep="/")))
Y=2013
data$PMconc[data$PMconc==-999]<-NA

#pm10=code 24, pm25=code 39
pm10=data[which(data$PM10.25==24),]
pm25=data[which(data$PM10.25==39),]
rm(data)
gc()
pm10<-filter(pm10,!(is.na(PMconc)))
pm25<-filter(pm25,!(is.na(PMconc)))


#aGG
pm10.2013 <-pm10 %>%
    group_by(CODESTATION,Date) %>%
    summarise(PM10=mean(PMconc),count=n())


pm25.2013 <-pm25 %>%
    group_by(CODESTATION,Date) %>%
    summarise(PM25=mean(PMconc),count=n())

#count number of missing data
#No. of days with over 5 hours of missing data in 2013
miss10.5<-pm10.2013[pm10.2013$count<19,]
pm10.miss.5 <-miss10.5 %>%
    group_by(CODESTATION) %>%
    summarise(count=n())
#No. of days with over 10 hours of missing data in 2013
miss10.10<-pm10.2013[pm10.2013$count<14,]
pm10.miss.10 <-miss10.10 %>%
    group_by(CODESTATION) %>%
    summarise(count=n())
#No. of days with over 20 hours of missing data in 2013
miss10.20<-pm10.2013[pm10.2013$count<4,]
pm10.miss.20 <-miss10.20 %>%
    group_by(CODESTATION) %>%
    summarise(count=n())
#create table of missing data for 2013
tab<-merge(pm10.miss.5,pm10.miss.10,by="CODESTATION",all.x=TRUE)
table<-merge(tab,pm10.miss.20,by="CODESTATION",all.x=TRUE)
setnames(table,c("count.x","count.y","count"),c("miss>5hr","miss>10hr","miss>20hr"))

#read xy data
stnXY=fread("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/FR_AQMETA_STATIONS_2013.csv") #2013
setnames(stnXY,"CODE STATION","CODESTATION")
setnames(stnXY,"TYPE STATION","TYPESTATION")

xy=subset(stnXY,select=c("CODESTATION","lat","long","ALTITUDE" ,"TYPESTATION"))
xy$lat<-gsub("\t", "",xy$lat)
xy$long<-gsub("\t", "",xy$long)
xy$ALTITUDE<-gsub("\t", "",xy$ALTITUDE)
xy$TYPESTATION<-gsub("\t", "",xy$TYPESTATION)
xy$CODESTATION<-gsub("\t", "",xy$CODESTATION)
head(xy)

pm10.2013$CODESTATION=sprintf("%05d",pm10.2013$CODESTATION)
PM10.XY=merge(pm10.2013,xy,by=c("CODESTATION"),all.x=TRUE)
write.csv(PM10.XY,paste("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM10_ 2013 .csv"))


pm25.2013$CODESTATION=sprintf("%05d",pm25.2013$CODESTATION)
PM25.XY=merge(pm25.2013,xy,by=c("CODESTATION"),all.x=TRUE)
write.csv(PM25.XY,paste("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/ PM25_ 2013 .csv"))
