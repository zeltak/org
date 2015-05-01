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
library(ggmap)
library(broom)
library(splines)
library(DataCombine)



t2000<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2000.csv")

t2001<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2001.csv")

t2002<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2002.csv")

t2003<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2003.csv")

t2004<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2004.csv")

t2005<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2005.csv")

t2006<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2006.csv")

t2007<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2007.csv")

t2008<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2008.csv")

t2009<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2009.csv")

t2010<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2010.csv")

t2011<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2011.csv")

t2012<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2012.csv")

t2013<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2013.csv")

t2014<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.2014.csv")


tall <- rbindlist(list(t2003,t2004,t2005,t2006,t2007,t2008,t2009,t2010,t2011), fill=TRUE)

tall <-tall  %>%
    group_by(GUID) %>%
    summarise_each(funs(mean),Long,Lat,LTPM )



write.csv(tall,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.ALL.csv")
