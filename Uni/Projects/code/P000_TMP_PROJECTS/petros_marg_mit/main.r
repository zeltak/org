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
library(doBy)

mitp <-fread("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/petros_Marguerite_MIT/mitpoints_guid.csv")


######## import pollution sets
pm2009<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2009.rds")
pm2009<-pm2009[,c(1,2,3,4,9),with=FALSE]
pm2009<-as.data.table(pm2009)
#see if there is a complete time series
pm2009[, .N, by=c("guid")]


pm2009$count<-1
sum2009<- as.data.table(summaryBy(count~guid, data=pm2009, FUN=sum))
#sum2009.full<-sum2009[count.sum > 360]
describe(sum2009)

bg<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")
setkey(sum2009,guid)
setkey(bg,guid)
mrg <- merge(sum2009, bg[,list(guid,long_aod,lat_aod)], all.y = F)
write.csv(mrg,"/media/NAS/Uni/ztmp/sum2009.csv")



#subset to nas 
pm2009.nasse <- pm2009 [pm2009 $guid %in% mitp[,unique(guid)], ] 
pm2009.nasse[, m := as.numeric(format(day, "%m")) ]
#only relevant months needed
pm2009.nassez <- pm2009.nasse[m %in% c(9, 8, 7,10), ]




nasPM<- rbindlist(list(pm2003.nasse,pm2003.nasse,pm2004.nasse,pm2005.nasse,pm2006.nasse,pm2007.nasse,pm2008.nasse,pm2009.nasse,pm2010.nasse,pm2011.nasse))
