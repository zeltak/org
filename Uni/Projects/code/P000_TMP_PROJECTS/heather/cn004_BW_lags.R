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


mod2003<-read_csv("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2003.csv")
mod2004<-read_csv("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2004.rds")

mod2005<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2005.rds")
mod2006<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2006.rds")
mod2007<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2007.rds")
mod2008<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2008.rds")
mod2009<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2009.rds")
mod2010<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2010.rds")
mod2011<-readRDS("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN010_bestpred_csv/Fintmpc_2011.rds")

ugrid <-mod2004 %>%
    group_by(aodid) %>%
    summarise(lat_aod = mean(lat_aod, na.rm=TRUE),  long_aod = mean(long_aod, na.rm=TRUE),x_aod_ITM = mean(x_aod_ITM, na.rm=TRUE),  y_aod_ITM = mean(y_aod_ITM, na.rm=TRUE))



mod2010<-dplyr::rename(mod2010,long_lst=glong,lat_lst=glat)
mod2011<-dplyr::rename(mod2011,long_lst=glong,lat_lst=glat)
names(mod2010)
mod2010<-mod2010[,c(4,5,1,2,3)]
mod2011<-mod2011[,c(4,5,1,2,3)]
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
tst.tc <- filter (tst.tc, data > 4380)
maplst <- fintemp[fintemp$lstid %in% tst.tc$lstid, ] 

write.csv(maplst,"/media/NAS/Uni/Projects/P056_hburris/work/qgis/full_temp_grid.csv")

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



#for expo
write.csv(temp.cases,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/final/add.temp.cases0011.csv")
case.guid.clean<-select(case.guid,X,Y,rowid, lat1, long1 ,long_lst ,lat_lst ,guid)
write.csv(case.guid.clean,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Veronica_temprature/final/add.guid.cases011.csv")




######## import pollution sets

p2003<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv",select=c(1,2,3))
p2004<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM04.csv",select=c(1,2,3))
p2005<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM05.csv",select=c(1,2,3))
p2006<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM06.csv",select=c(1,2,3))
p2007<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM07.csv",select=c(1,2,3))
p2008<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM08.csv",select=c(1,2,3))
allbestpred <- rbind(p2003,p2004,p2005,p2006,p2007,p2008)
rm(p2003,p2004,p2005,p2006,p2007,p2008)
gc()
allbestpred$guid<-as.numeric(allbestpred$guid)
#common dates
allbestpred[, day := as.Date(strptime(day, format = "%Y-%m-%d"))]

