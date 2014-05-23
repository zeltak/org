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
library(bit64)

#import clean blocks
clean.guid<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN007_Key_tables/clean_guids_xy.csv")
clean.guid[, guid := paste( glong,glat,sep="")]
#these are grids with missing days
fxn <- fintemp[is.na(fintemp)]
#take out missing grids
clean.guid.c <- clean.guid[!(clean.guid$guid %in% fxn$guid), ] 
#export only full Time series blocks
write.csv(clean.guid.c,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/greg_w_temperature/cleangrid.csv")



census<-fread("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/greg_w_temperature/census_guid.csv")



#import yearly me
tc2000<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2000.csv")
tc2000[,guid:=NULL]
tc2000[, guid := paste( glong,glat,sep="")]
tc2000.c <- tc2000[tc2000$guid %in% census$guid, ] 
tc2000.c <- tc2000.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2000.c,day,guid)
rm(tc2000)


x1<-tc2000.c[, .N, by=c("guid")]


tc2001<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2001.csv")
tc2001[,guid:=NULL]
tc2001[, guid := paste( glong,glat,sep="")]
tc2001.c <- tc2001[tc2001$guid %in% census$guid, ] 
tc2001.c <- tc2001.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2001.c,day,guid)
rm(tc2001)

tc2002<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2002.csv")
tc2002[,guid:=NULL]
tc2002[, guid := paste( glong,glat,sep="")]
tc2002.c <- tc2002[tc2002$guid %in% census$guid, ] 
tc2002.c <- tc2002.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2002.c,day,guid)
rm(tc2002)

tc2003<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2003.csv")
tc2003[,guid:=NULL]
tc2003[, guid := paste( glong,glat,sep="")]
tc2003.c <- tc2003[tc2003$guid %in% census$guid, ] 
tc2003.c <- tc2003.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2003.c,day,guid)
rm(tc2003)


tc2004<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2004.csv")
tc2004[,guid:=NULL]
tc2004[, guid := paste( glong,glat,sep="")]
tc2004.c <- tc2004[tc2004$guid %in% census$guid, ] 
tc2004.c <- tc2004.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2004.c,day,guid)
rm(tc2004)


tc2005<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2005.csv")
tc2005[,guid:=NULL]
tc2005[, guid := paste( glong,glat,sep="")]
tc2005.c <- tc2005[tc2005$guid %in% census$guid, ] 
tc2005.c <- tc2005.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2005.c,day,guid)
rm(tc2005)


tc2006<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2006.csv")
tc2006[,guid:=NULL]
tc2006[, guid := paste( glong,glat,sep="")]
tc2006.c <- tc2006[tc2006$guid %in% census$guid, ] 
tc2006.c <- tc2006.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2006.c,day,guid)
rm(tc2006)


tc2007<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2007.csv")
tc2007[,guid:=NULL]
tc2007[, guid := paste( glong,glat,sep="")]
tc2007.c <- tc2007[tc2007$guid %in% census$guid, ] 
tc2007.c <- tc2007.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2007.c,day,guid)
rm(tc2007)


tc2008<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2008.csv")
tc2008[,guid:=NULL]
tc2008[, guid := paste( glong,glat,sep="")]
tc2008.c <- tc2008[tc2008$guid %in% census$guid, ] 
tc2008.c <- tc2008.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2008.c,day,guid)
rm(tc2008)


tc2009<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2009.csv")
tc2009[,guid:=NULL]
tc2009[, guid := paste( glong,glat,sep="")]
tc2009.c <- tc2009[tc2009$guid %in% census$guid, ] 
tc2009.c <- tc2009.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2009.c,day,guid)
rm(tc2009)


tc2010<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2010.csv")
tc2010[,guid:=NULL]
tc2010[, guid := paste( glong,glat,sep="")]
tc2010.c <- tc2010[tc2010$guid %in% census$guid, ] 
tc2010.c <- tc2010.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2010.c,day,guid)
rm(tc2010)


tc2011<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Fintmpc_2011.csv")
tc2011[,guid:=NULL]
tc2011[, guid := paste( glong,glat,sep="")]
tc2011.c <- tc2011[tc2011$guid %in% census$guid, ] 
tc2011.c <- tc2011.c [, day:=as.Date(strptime(date,"%d%b%Y"))]
setkey(tc2011.c,day,guid)
rm(tc2011)


fintemp<- rbindlist(list(tc2000.c, tc2001.c,tc2002.c,tc2003.c,tc2004.c,tc2005.c,tc2006.c,tc2007.c,tc2008.c,tc2009.c,tc2010.c,tc2011.c))
fintemp[, day:=as.Date(strptime(date, "%d%b%Y"))]
fintemp[, y := as.numeric(format(day, "%y")) ]
describe(fintemp$fintemp)

hist(fintemp$fintemp)
fintemp[,y:=NULL]
fintemp[,glong:=NULL]
fintemp[,glat:=NULL]

census[,glong:=NULL]
census[,glat:=NULL]

summary(fintemp)

#for greg
write.csv(census,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/greg_w_temperature/final_product/census_gridid.csv")
write.csv(fintemp,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/greg_w_temperature/final_product/temp_gridid.csv")

