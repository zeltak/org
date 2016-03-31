#impute pm2.5 from pm10 - FRANCE

#read pm10 data by year
setwd("/media/NAS/Uni/Projects/P031_MAIAC_France/1.RAW/PM/")
#filenames <- list.files( pattern="18.csv", full.names=TRUE)
 
#loop imputation on year
for (I in 1:length(filenames)) {
  J=as.numeric(I)
  #read pm10 data by year
  setwd("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\pm10\\new")
  filenames <- list.files( pattern="18.csv", full.names=TRUE)
  pm10<-fread(filenames[J])
   pm10<-na.omit(pm10) 
   setnames(pm10,"dailyPM","PM10") 
   #read pm25 data by year
    setwd("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\pm25\\new")
    filenames1 <- list.files( pattern="18.csv", full.names=TRUE)
        pm25<-fread(filenames1[J])
        pm25<-na.omit(pm25)
both<-merge(pm10,pm25,by=c("day","stn"))
#run linear regression
reg<-lm(PM25~PM10,data=both)  
#extract coefficients
a<-reg$coefficients[1]
b<-reg$coefficients[2]
rm(both)

both.all<-merge(pm10,pm25,by=c("day","stn"),all.x=TRUE,allow.cartesian=TRUE)
both.all<-both.all[!is.na(PM10)]
setkey(both.all,PM10)

while (is.na(both.all$PM25))
{both.all$PM25<-a+b*both.all$PM10}

setnames(both.all,"lat.x","lat")
setnames(both.all,"long.x","long")
both.all<-both.all[,c(1,2,4:6,8),with=FALSE]
y=substr(both.all$day[1],1,4)

#save
write.csv(both.all,paste("C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\pm25monitors\\Daily\\imputed\\",y,".csv"))
}