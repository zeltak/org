library(bit64)
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(readr)
#My study spans Jan 1, 2000 through Dec 31, 2009
#13:42

mod2000<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2000.rds")
mod2001<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2001.rds")
mod2002<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2002.rds")
mod2003<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2003.rds")
mod2004<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2004.rds")
mod2005<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2005.rds")
mod2006<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2006.rds")
mod2007<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2007.rds")
mod2008<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2008.rds")
mod2009<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2009.rds")
mod2010<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2010.rds")
mod2011<-read_rds("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/MAtempc2011.rds")

mod2010<-dplyr::rename(mod2010,long_lst=glong,lat_lst=glat)
mod2011<-dplyr::rename(mod2011,long_lst=glong,lat_lst=glat)
names(mod2010)
mod2010<-mod2010[,c(4,5,1,2,3)]
mod2011<-mod2011[,c(4,5,1,2,3)]
fintemp<- rbind(mod2000, mod2001,mod2002,mod2003,mod2004,mod2005,mod2006,mod2007,mod2008,mod2009,mod2010,mod2011)
#fintemp[, y := as.numeric(format(day, "%y")) ]
summary(fintemp$fintemp)

tst<-fintemp %>%
    group_by(guid) %>%
    dplyr::summarise(data = n() )
tst2<-filter(tst,data >= 4383)


## clean data to keep only good grids
#take out missing grids and export to join cases to cloest guid
cfintemp <- fintemp[fintemp$guid %in% tst2$guid, ] 

write.csv (x <-cfintemp  %>%
    group_by(guid) %>%
    dplyr::summarise(long_lst = mean(long_lst, na.rm=TRUE),  lat_lst = mean(lat_lst, na.rm=TRUE)) , "/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/magrid.csv")

#import back cases_guid layer
case.guid<-read_csv("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/cases_guid0011.csv")

#merge temp reading

temp.cases <- cfintemp[cfintemp$guid %in% case.guid$guid, ] 
#check
tst.tc<-temp.cases %>%
    group_by(guid) %>%
    dplyr::summarise(data = n())


#for veronica
write.csv(temp.cases,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/final/temp.cases0011.csv")
case.guid.clean<-select(case.guid,X,Y,KIDUID, Latitude, Longitude ,maiac_x ,maiac_y ,guid)
write.csv(case.guid.clean,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/final/guid.cases011.csv")
