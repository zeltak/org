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
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")



###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(type=character(43), r2007=numeric(43),
                        r2004=numeric(43),r2005=numeric(43),
                        r2006=numeric(43),r2007=numeric(43),
                        r2008=numeric(43),r2009=numeric(43),
                        r2010=numeric(43),r2011=numeric(43),
                        r2012=numeric(43),mean=numeric(43))

#name columns

mod1table$type<- c("mod1_R2","mod_RMSPE","mod1_spatial","mod1_temporal","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
                   "mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE",
                   "mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial",
                   "mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE",
                   "mod1CVLPM_Slope","mod1CVLPM_Slope_SE","mod1CVLPM_RMSPE",
                   "mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial",
                   "mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int",
                   "mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE",
                   "mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
                   "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                   "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")





### import data
m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.rds")

### subset to aqua and apply alexei cleaning methods
m1.2007<-m1.2007[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2007, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2007[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2007 <- m1.2007[!(m1.2007$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2007)
m1.2007$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2007))#0.74
#get rid of missing
m1.2007 <- na.omit(m1.2007)

m1.2007[,elev.s:= scale(elev)]
m1.2007[,tden.s:= scale(tden)]
m1.2007[,pden.s:= scale(pden)]
m1.2007[,dist2A1.s:= scale(dist2A1)]
m1.2007[,dist2water.s:= scale(dist2water)]
m1.2007[,dist2rail.s:= scale(dist2rail)]
m1.2007[,Dist2road.s:= scale(Dist2road)]
m1.2007[,ndvi.s:= scale(ndvi)]
m1.2007[,MeanPbl.s:= scale(MeanPbl)]
m1.2007[,p_ind.s:= scale(p_ind)]
m1.2007[,p_for.s:= scale(p_for)]
m1.2007[,p_farm.s:= scale(p_farm)]
m1.2007[,p_dos.s:= scale(p_dos)]
m1.2007[,p_dev.s:= scale(p_dev)]
m1.2007[,p_os.s:= scale(p_os)]


#lme mixed model
#m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+(1+aod|day/reg_num))

#full fit
m1.fit.2007 <-  lmer(m1.formula,data=m1.2007,weights=normwt)
m1.2007$predicted <- predict(m1.fit.2007)
mod1table$r2007[1]<-summary(lm(PM25~predicted,data=m1.2007))$r.squared #0.75


###############
#MOD2
###############
m2.2007<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2007.rds")

m2.2007[,elev.s:= scale(elev)]
m2.2007[,tden.s:= scale(tden)]
m2.2007[,pden.s:= scale(pden)]
m2.2007[,dist2A1.s:= scale(dist2A1)]
m2.2007[,dist2water.s:= scale(dist2water)]
m2.2007[,dist2rail.s:= scale(dist2rail)]
m2.2007[,Dist2road.s:= scale(Dist2road)]
m2.2007[,ndvi.s:= scale(ndvi)]
m2.2007[,MeanPbl.s:= scale(MeanPbl)]
m2.2007[,p_ind.s:= scale(p_ind)]
m2.2007[,p_for.s:= scale(p_for)]
m2.2007[,p_farm.s:= scale(p_farm)]
m2.2007[,p_dos.s:= scale(p_dos)]
m2.2007[,p_dev.s:= scale(p_dev)]
m2.2007[,p_os.s:= scale(p_os)]

#generate predictions
m2.2007[, pred.m2 := predict(object=m1.fit.2007,newdata=m2.2007,allow.new.levels=TRUE,re.form=NULL)]

#delete implossible values
m2.2007 <- m2.2007[pred.m2 > 0.00000000000001 , ]
m2.2007 <- m2.2007[pred.m2 < 500   , ]

#test
describe(m2.2007$pred.m2)
hist(m2.2007$pred.m2)

#######
#M2 R2
######

m1.2007[,aodid:= paste(m1.2007$long_aod,m1.2007$lat_aod,sep="-")]

#merge co located mod1 and mod2 grids
setkey(m1.2007,aodid,day)
setkey(m2.2007,aodid,day)
m.1.2.pred <- merge(m1.2007, m2.2007[, list(aodid, day, pred.m2)], all.x = T)
mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$pred.m2)
#cleanup and save current stages (workspace)
summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2007[, list(LTPM.m2 = mean(pred.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2007.rds")
#map the predictions
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.m2.png")
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p2.rds")
keep(mod1table , sure=TRUE) 
gc()
write.csv(m2_agg,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/ltpm.m2.2007.csv")
