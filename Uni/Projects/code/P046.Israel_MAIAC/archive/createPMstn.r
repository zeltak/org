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
setnames(PM25,"Y","y_stn_ITM")vv

#aggregate and write to csv
x <-PM25 %>%
    group_by(stn) %>%
    summarise(lat_aod = mean(y_stn_ITM, na.rm=TRUE),  long_aod = mean(x_stn_ITM, na.rm=TRUE)) 
 


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
y <-PM10 %>%
    group_by(stn) %>%
    summarise(lat_aod = mean(y_stn_ITM, na.rm=TRUE),  long_aod = mean(x_stn_ITM, na.rm=TRUE))


z<-rbindlist(list(x,y))
setkey(z,stn)
q<-unique(z,by="stn")
str(q)
write.csv(q,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/PMstn.csv")








