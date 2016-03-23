library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
library(FNN)
library(ggplot2)
library(dplyr)
library(bit64)
library(readr)

#bring in temperature


#mod2003<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2003.csv")
#x<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/Temperature_NE/tempnd_2004.csv")
#y<-fread("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/new/new2004.csv")
mod2003<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2003.rds")
mod2003$fintemp<-as.numeric(mod2003$fintemp)
mod2003<-filter(mod2003,!is.na(fintemp))
mod2004<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2004.rds")
#mod2004$fintemp<-as.numeric(mod2004$fintemp)
mod2005<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2005.rds")
mod2006<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2006.rds")
mod2007<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2007.rds")
mod2008<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2008.rds")
mod2009<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2009.rds")
mod2010<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2010.rds")
mod2011<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2011.rds")



mod2003<-dplyr::rename(mod2003,long_lst=glong,lat_lst=glat)
mod2004<-dplyr::rename(mod2004,long_lst=glong,lat_lst=glat)
mod2005<-dplyr::rename(mod2005,long_lst=glong,lat_lst=glat)
mod2006<-dplyr::rename(mod2006,long_lst=glong,lat_lst=glat)
mod2007<-dplyr::rename(mod2007,long_lst=glong,lat_lst=glat)
mod2008<-dplyr::rename(mod2008,long_lst=glong,lat_lst=glat)
mod2009<-dplyr::rename(mod2009,long_lst=glong,lat_lst=glat)
mod2010<-dplyr::rename(mod2010,long_lst=glong,lat_lst=glat)
mod2011<-dplyr::rename(mod2011,long_lst=glong,lat_lst=glat)


mod2004<-mod2004[,c(2,3,4,5,1)]
mod2010<-mod2010[,c(3,4,5,1,2)]
mod2011<-mod2011[,c(3,4,5,1,2)]


fintemp<- rbind(mod2003,mod2004,mod2005,mod2006,mod2007,mod2008,mod2009,mod2010,mod2011)
#fintemp[, y := as.numeric(format(day, "%y")) ]
summary(fintemp$fintemp)
fintemp<-filter(fintemp,fintemp > -35)
fintemp<-filter(fintemp,fintemp < 50)
#merge temp reading
fintemp<- dplyr::rename(fintemp,lstid=guid)
#check
tst.tc<-fintemp %>%
    group_by(lstid) %>%
    dplyr::summarise(data = n())
tst.tc <- filter (tst.tc, data > 3285)

maplst <- fintemp[fintemp$lstid %in% tst.tc$lstid, ] 

write.csv (x <-maplst %>%
    group_by(lstid) %>%
    summarise(lat_lst = mean(lat_lst, na.rm=TRUE),  long_lst = mean(long_lst, na.rm=TRUE)) , "/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN007_Key_tables/clean_lst_gird.csv")

saveRDS(maplst,"/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/FINAL2016CLEANED/fincleaned.rds")

maplst$lstid<-paste(maplst$long_lst,maplst$lat_lst,sep="-")


#for the people that dont have a .X (full number) add a 0.5 day to them
#send a year before delivery 
#send temperture 

#imports
bxy<-fread("/media/NAS/Uni/Projects/P056_hburris/RAW/Burris_final_geo.csv")
library(sas7bdat)
#Read PM data
bfull<-fread("/media/NAS/Uni/Projects/P056_hburris/work/qgis/cases_both_guids.csv")
head(bfull)
bwfull<-select(bfull,ID,gestation,birth_weig,X,Y,dob,guid,GUID_1) %>% dplyr::rename (aodid=GUID_1,lstid=guid)



#temp exposure
tmp.cases<-fread("/media/NAS/Uni/Projects/P056_hburris/work/keytables/cases_tempguid.csv")
tmp.cases<-select(tmp.cases,X,Y,ID,lstid, lat_lst ,long_lst)
saveRDS(tmp.cases,"/media/NAS/Uni/Projects/P056_hburris/work/keytables/cases_tempguid.rds")


tmp.cases$lstid<-paste(tmp.cases$long_lst,tmp.cases$lat_lst,sep="-")
#leave only related guids
tmpmatrix <- maplst[maplst$lstid %in% tmp.cases$lstid, ] 

saveRDS(tmpmatrix,"/media/NAS/Uni/Projects/P056_hburris/work/data/tmpmatrix.csv")


######## import pollution sets

p2003<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2003")
p2004<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2004")
p2005<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2005")
p2006<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2006")
p2007<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2007")
p2008<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2008")
p2009<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2009")
p2010<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2010")
p2011<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2011")
p2012<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2012")
p2013<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/pm25pred.final.2013")
allbestpred <- rbind(p2003,p2004,p2005,p2006,p2007,p2008,p2009,p2010,p2011,p2012,p2013)

rm(p2003,p2004,p2005,p2006,p2007,p2008,p2009,p2010,p2011,p2012,p2013)
gc()

summary(allbestpred$pm25_final)
# #checked and all values are valid
# tst.tc<-allbestpred %>%
#     group_by(GUID) %>%
#     dplyr::summarise(data = n())
# tst.tc <- filter (tst.tc, data > 4010)
# maplst <- allbestpred[allbestpred$GUID %in% tst.tc$GUID, ] 

write.csv (x <-allbestpred %>%
    group_by(GUID) %>%
    summarise(lat_aod = mean(Lat, na.rm=TRUE),  long_aod = mean(Long, na.rm=TRUE)) , "/media/NAS/Uni/Projects/P056_hburris/work/keytables/clean_aod_gird.csv")



#pm exposure
aod.cases<-fread("/media/NAS/Uni/Projects/P056_hburris/work/keytables/cases_aodguid.csv")
aod.cases<-select(aod.cases,X,Y,ID,GUID, lat_aod ,long_aod)
saveRDS(aod.cases,"/media/NAS/Uni/Projects/P056_hburris/work/keytables/cases_aodguid.rds")

#leave only related guids
pmmatrix <- allbestpred[allbestpred$GUID %in% aod.cases$GUID, ] 
saveRDS(pmmatrix,"/media/NAS/Uni/Projects/P056_hburris/work/data/pmmatrix.rds")

