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



tall <- rbindlist(list(t2003,t2004,t2005,t2006,t2007,t2008,t2009,t2010,t2011), fill=TRUE)

tall <-tall  %>%
    group_by(GUID) %>%
    summarise_each(funs(mean),Long,Lat,LTPM )



write.csv(tall,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/m3rds/Xmod3.TR.PM25.LTPM.ALL.csv")
