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
summary(mitp)

######## import pollution sets
pm2009<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2009NM.rds")
pm2009<-pm2009[,c(1,2,3,4,9),with=FALSE]
pm2009<-as.data.table(pm2009)
#see if there is a complete time series
pm2009[, .N, by=c("guid")]

# bg<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/basegrid.se.csv")
# setkey(sum2009,guid)
# setkey(bg,guid)
# mrg <- merge(sum2009, bg[,list(guid,long_aod,lat_aod)], all.y = F)
# write.csv(mrg,"/media/NAS/Uni/ztmp/sum2009.csv")


#subset to nas 
pm2009.nasse <- pm2009 [pm2009 $guid %in% mitp[,unique(guid)], ] 
pm2009.nasse[, m := as.numeric(format(day, "%m")) ]
#only relevant months needed
pm2009.nassez <- pm2009.nasse[m %in% c(9, 8, 7,10), ]





setkey(mitp,guid)
setkey(pm2009.nassez,guid)
mitPM <- merge(pm2009.nassez, mitp, all.x = T,allow.cartesian=T)

# mitPM[, mid := paste(x_mit,y_mit,sep="")]
# mitPM[, .N, by=c("mid")]

mitPM<-mitPM[,c(1,2,5,8,9),with=FALSE]
setnames(mitPM,"bestpred","PM25_pred")

write.csv(mitPM,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/petros_Marguerite_MIT/final_pred.csv")

#map
mitPM[, mid := paste(x_mit,y_mit,sep="")]
mitPM_agg <- (mitPM[, list(LTPM =mean(PM25_pred, na.rm = TRUE), 
                           x_mit = x_mit[1], #use the first long and lat (by guid)
                           y_mit = y_mit[1]),by = mid])

write.csv(mitPM_agg,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/petros_Marguerite_MIT/pred_map.csv")


