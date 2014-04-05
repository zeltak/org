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


y2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p4.rds")
y2004<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2004_p4.rds")
y2005<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2005_p4.rds")
y2006<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2006_p4.rds")
y2007<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p4.rds")
y2008<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2008_p4.rds")
y2009<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2009_p4.rds")
y2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2010_p4.rds")
y2011<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2011_p4.rds")


xall<-cbind(y2003[,1],y2003[,2],y2004[,3],y2005[,4],y2006[,5],y2007[,6],y2008[,7],y2009[,8],y2010[,9],y2011[,10])

write.csv(xall,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/4.Results/mod3/mod3_4.csv")





