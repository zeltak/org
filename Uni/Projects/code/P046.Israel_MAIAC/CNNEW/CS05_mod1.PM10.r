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



source("/media/NAS/Uni/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/CV_splits.r")


######## import mod1 files
pm10.m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2003.rds")
pm10.m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2004.rds")
pm10.m1.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2005.rds")
pm10.m1.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2006.rds")
pm10.m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2007.rds")
pm10.m1.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2008.rds")
pm10.m1.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2009.rds")
pm10.m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2010.rds")
pm10.m1.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2011.rds")
pm10.m1.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.PM10.AQ.2012.rds")

#bind to 1 year
pm10.m1<- rbindlist(list(pm10.m1.2003,pm10.m1.2004,pm10.m1.2005,pm10.m1.2006,pm10.m1.2007,pm10.m1.2008,pm10.m1.2009,pm10.m1.2010,pm10.m1.2011,pm10.m1.2012))


#new seasons
#1-cloudy,#2-noncloud
# pm10.m1$cloudIL<-recode(pm10.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")
# pm10.m1$cloudIL<-recode(pm10.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")

### subset to aqua and apply alexei cleaning methods
pm10.m1<-pm10.m1[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

### base yearly model
m1.formula <- as.formula(PM10~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=pm10.m1)
pm10.m1$predicted <- predict(x)
glance(lm(PM10~predicted,data=pm10.m1)) #0.82


################# clean BAD STN pm10 and check if improved model?
rawdf <- ddply(pm10.m1, c( "c","stn"), 
      function(x) {
        mod1 <- lm(PM10 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
pm10.m1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
pm10.m1 <- pm10.m1[!(pm10.m1$badid %in% bad$badid), ] 

x<-  lmer(m1.formula,data=pm10.m1)
pm10.m1$predicted <- predict(x)
glance(lm(PM10~predicted,data=pm10.m1))#0.69
#get rid of missing
pm10.m1 <- na.omit(pm10.m1) #0.85


pm10.m1[,elev.s:= scale(elev)]
pm10.m1[,tden.s:= scale(tden)]
pm10.m1[,pden.s:= scale(pden)]
pm10.m1[,dist2A1.s:= scale(dist2A1)]
pm10.m1[,dist2water.s:= scale(dist2water)]
pm10.m1[,dist2rail.s:= scale(dist2rail)]
pm10.m1[,Dist2road.s:= scale(Dist2road)]
pm10.m1[,ndvi.s:= scale(ndvi)]
pm10.m1[,MeanPbl.s:= scale(MeanPbl)]
pm10.m1[,p_ind.s:= scale(p_ind)]
pm10.m1[,p_for.s:= scale(p_for)]
pm10.m1[,p_farm.s:= scale(p_farm)]
pm10.m1[,p_dos.s:= scale(p_dos)]
pm10.m1[,p_dev.s:= scale(p_dev)]
pm10.m1[,p_os.s:= scale(p_os)]





#lme mixed model

#m1.formula <- as.formula(PM10~ aod+(1+aod|day))
m1.formula <- as.formula(PM10~ aod+
                        Temp+WD+RH+WS+Dust+Rain+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+dist2rail.s+dist2A1.s+Dist2road.s+dist2water.s+ndvi.s+season #spatial
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                          +as.factor(reg_num)+aod:as.factor(reg_num)+
                         +(1+aod|day/reg_num))

# full mod1
x<-  lmer(m1.formula,data=pm10.m1,weights=normwt)
pm10.m1$predicted <- predict(x)
glance(lm(PM10~predicted,data=pm10.m1))#0.90




# m1.formula <- as.formula( log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,2) + ns(speed_ms,2) +
#                             flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
#                             dust + ns(log_restot,2) + log_ndvi + as.factor(elevation_10_cl3) + log_aod:as.factor(elevation_10_cl3) + 
#                             as.factor(isa_cl3) + log_aod:as.factor(isa_cl3)+
#                             log_near_a1 + log_near_a2 + log_near_a3 + near_airport_1000 + near_port_1000 +
#                             length_a1_1000 + ns(length_a23_1000,2) + r_sum_length_a1_1000 + r_sum_length_a23_1000 + 
#                             pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
#                             log(near_emip) + nox_2010p_100 + nh3_2005p + r_sum_nox_2010p_100 + r_sum_nh3_2010p +
#                             log_so2_2010a + log_nox_2010a + log_nh3_2010a +
#                             rh + ns(visib_km,2) + ns(temp_c,2) +
#                             (1+aod|day/nome_zona))



#s1
splits_s1 <- splitdf(pm10.m1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1,weights=normwt)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,re.form=NULL )


#s2
splits_s2 <- splitdf(pm10.m1)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2,weights=normwt)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,re.form=NULL )

#s3
splits_s3 <- splitdf(pm10.m1)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3,weights=normwt)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,re.form=NULL )

#s4
splits_s4 <- splitdf(pm10.m1)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4,weights=normwt)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,re.form=NULL )

#s5
splits_s5 <- splitdf(pm10.m1)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5,weights=normwt)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,re.form=NULL )


#s6
splits_s6 <- splitdf(pm10.m1)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6,weights=normwt)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,re.form=NULL )


#s7
splits_s7 <- splitdf(pm10.m1)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7,weights=normwt)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,re.form=NULL )

#s8
splits_s8 <- splitdf(pm10.m1)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8,weights=normwt)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,re.form=NULL )

#s9
splits_s9 <- splitdf(pm10.m1)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9,weights=normwt)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,re.form=NULL )

#s10
splits_s10 <- splitdf(pm10.m1)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10,weights=normwt)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,re.form=NULL )



####BIND ALL 10% into 1 dataset

mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))

# cleanup (remove from WS) objects from CV
#rm(list = ls(pattern = "mod1d|out_|splits_"))

mod1CV_reg <- lm(mod1CV_all$PM10~mod1CV_all$predicted)
summary(mod1CV_reg)$r.squared 
