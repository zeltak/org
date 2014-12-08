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
mod1table <- data.frame(type=character(43), r2003=numeric(43),
                        r2004=numeric(43),r2005=numeric(43),
                        r2006=numeric(43),r2003=numeric(43),
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
m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")

### subset to aqua and apply alexei cleaning methods
m1.2003<-m1.2003[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2003, c( "stn"), 
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
m1.2003[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2003 <- m1.2003[!(m1.2003$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2003)
m1.2003$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2003))

#get rid of missing
m1.2003 <- na.omit(m1.2003)



m1.2003[,elev.s:= scale(elev)]
m1.2003[,tden.s:= scale(tden)]
m1.2003[,pden.s:= scale(pden)]
m1.2003[,dist2A1.s:= scale(dist2A1)]
m1.2003[,dist2water.s:= scale(dist2water)]
m1.2003[,dist2rail.s:= scale(dist2rail)]
m1.2003[,Dist2road.s:= scale(Dist2road)]
m1.2003[,ndvi.s:= scale(ndvi)]
m1.2003[,MeanPbl.s:= scale(MeanPbl)]
m1.2003[,p_ind.s:= scale(p_ind)]
m1.2003[,p_for.s:= scale(p_for)]
m1.2003[,p_farm.s:= scale(p_farm)]
m1.2003[,p_dos.s:= scale(p_dos)]
m1.2003[,p_dev.s:= scale(p_dev)]
m1.2003[,p_os.s:= scale(p_os)]
m1.2003[,Temp.s:= scale(Temp)]
m1.2003[,WD.s:= scale(WD)]
m1.2003[,WS.s:= scale(WS)]
m1.2003[,RH.s:= scale(RH)]
m1.2003[,Rain.s:= scale(Rain)]

m1.2003[,zid := 1] #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
m1.2003[,elev.s :=  elev.s*-1]
m1.2003[,p_os.s :=  p_os.s*-1]
m1.2003[,WS.s :=  WS.s*-1]
m1.2003[,p_for.s :=  p_for.s*-1]
m1.2003[,p_farm.s :=  p_farm.s*-1]
m1.2003[,ndvi.s :=  ndvi.s*-1]
m1.2003[,dist2A1.s :=  dist2A1.s*-1]
m1.2003[,dist2water.s :=  dist2water.s*-1]
m1.2003[,dist2rail.s :=  dist2rail.s*-1]
m1.2003[,Dist2road.s :=  Dist2road.s*-1]

#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables
attach(m1.2003)
s.mat <- cbind(elev.s,tden.s,pden.s,dist2A1.s,dist2water.s,dist2rail.s,Dist2road.s,p_ind.s,p_for.s,p_farm.s,p_dos.s,p_dev.s,p_os.s)
t.mat <- cbind(ndvi.s,MeanPbl.s,Temp.s,WD.s,WS.s,RH.s)
detach(m1.2003)


#function to create the matrix (st.mat) of interaction between the space variables and time variables

  st.mat <-NULL
  count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
}
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

m1.2003 <- data.frame(m1.2003,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)




#lme mixed model
#m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+
                        Temp.s+WD.s+RH.s+WS.s+Dust+Rain+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+dist2rail.s+dist2A1.s+Dist2road.s+dist2water.s+ndvi.s+season #spatial
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         +(1+aod|day/reg_num))




#    model pm25 = aod Temp_F_x t_avgs s_avgs  st_avgs  / s outpred=pdataA_&year;
#     random int aod Temp_F_x / sub = date s ;
#   random int aod  / sub = date(reg_id) s;
#     random x1--x18 / sub = zid type=toep(1) s;




#lme mixed model
#m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+t.avgs+s.avgs+st.avgs+(1+aod|day/reg_num)+ (1+zid|X1)+ (1+zid|X2)+ (1+zid|X3)+ (1+zid|X4) )
m1.formula <- as.formula(PM25~ aod+t.avgs+s.avgs+st.avgs+(1+aod|day/reg_num) )

m1.fit.2003 <-  lmer(m1.formula,data=m1.2003,weights=normwt)



m1.2003$predicted <- predict(m1.fit.2003)
summary(lm(PM25~predicted,data=m1.2003))$r.squared 


#spatial
spatial2003<-m1.2003 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(predicted, na.rm=TRUE)) 
mod1table$r2003[3]<-  summary(lm(barpm ~ barpred, data=spatial2003))$r.squared
spatial2003[,spatresid:= barpm-barpred]
mod1table$r2003[2]<- sqrt(mean(spatial2003$spatresid^2))                   
                           
#temporal
tempo2003<-left_join(m1.2003,spatial2003)
tempo2003$delpm <-tempo2003$PM25-tempo2003$barpm
tempo2003$delpred <-tempo2003$predicted-tempo2003$barpred
mod1table$r2003[4] <- summary(lm(delpm ~ delpred, data=tempo2003))$r.squared


######## CV
#s1
splits_s1 <- splitdf(m1.2003)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1,weights=normwt)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,re.form=NULL )


#s2
splits_s2 <- splitdf(m1.2003)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2,weights=normwt)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,re.form=NULL )

#s3
splits_s3 <- splitdf(m1.2003)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3,weights=normwt)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,re.form=NULL )

#s4
splits_s4 <- splitdf(m1.2003)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4,weights=normwt)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,re.form=NULL )

#s5
splits_s5 <- splitdf(m1.2003)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5,weights=normwt)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,re.form=NULL )


#s6
splits_s6 <- splitdf(m1.2003)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6,weights=normwt)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,re.form=NULL )


#s7
splits_s7 <- splitdf(m1.2003)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7,weights=normwt)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,re.form=NULL )

#s8
splits_s8 <- splitdf(m1.2003)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8,weights=normwt)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,re.form=NULL )

#s9
splits_s9 <- splitdf(m1.2003)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9,weights=normwt)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,re.form=NULL )

#s10
splits_s10 <- splitdf(m1.2003)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10,weights=normwt)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,re.form=NULL )
####BIND ALL 10% into 1 dataset
mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))
# save CV data sets
saveRDS(mod1CV_all,"/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1CVset.2003.rds")

# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "mod1d|out_|splits_"))
mod1CV_reg <- lm(mod1CV_all$PM25~mod1CV_all$predicted)
mod1table$r2003[5] <-summary(mod1CV_reg)$r.squared #R2
mod1table$r2003[6] <-summary(mod1CV_reg)$coef[1,1] #intercept
mod1table$r2003[7] <-summary(mod1CV_reg)$coef[1,2] #intercept SE
mod1table$r2003[8] <-summary(mod1CV_reg)$coef[2,1] #Slope
mod1table$r2003[9] <-summary(mod1CV_reg)$coef[2,2] #Slope SE
mod1table$r2003[10]<- sqrt(mean(mod1CV_reg$residual^2))#rmspe

#spatial
spatialCV2003<-mod1CV_all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(predicted, na.rm=TRUE)) 
mod1table$r2003[8]<-  summary(lm(barpm ~ barpred, data=spatialCV2003))$r.squared
spatialCV2003[,spatresid:= barpm-barpred]
mod1table$r2003[10]<- sqrt(mean(spatialCV2003$spatresid^2))                   
                           
#temporal
tempoCV2003<-left_join(mod1CV_all,spatialCV2003)
tempoCV2003[,delpm := PM25-barpm]
tempoCV2003[,delpred := PM25-barpred]
mod1table$r2003[9] <- summary(lm(delpm ~ delpred, data=tempoCV2003))$r.squared
#ADD LOCAL PM STAGE
luf<-readRDS("/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN004_LU_full_dataset/luf_NE_MIA.rds")

#add 50m LU to CV data
setkey(mod1CV_all,stn)
setkey(luf,stn)
mod1d_all_st <- merge(mod1CV_all, luf, all.x = T)
mod1d_all_st<-na.omit(mod1d_all_st)
#summary(mod1d_all_st)
#create residual mp3 variable
mod1d_all_st$resm1<-mod1d_all_st$PM25-mod1d_all_st$predicted


#The GAM model
bp.model.ps<-gam(resm1~s(tden,popden)+s(tden,pbl)+s(tden,WDSP)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
#summary(bp.model.ps)
mod1d_all_st$Predlocm <-predict(bp.model.ps)
mod1d_all_st$OAPred <- mod1d_all_st$predicted+mod1d_all_st$Predlocm

####################reg
mod1d_reg_st <- lm(mod1d_all_st$PM25~mod1d_all_st$OAPred)

mod1table$r2003[11] <-summary(mod1d_reg_st)$r.squared
mod1table$r2003[12] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2003[13] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2003[14] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2003[15] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2003[16]<- sqrt(mean(mod1d_reg_st$residual^2))

#spatial
aggf<- ddply(mod1d_all_st, c("stn"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))

#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = stn])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2003[17] <- summary(mod1LPM_spatial)$r.squared

#temporal
setkey(m1CVLPM_agg,stn)
setkey(mod1d_all_st,stn)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[18] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[19]<- sqrt(mean(dat$spatresid^2))


#############save midpoint
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p1.rds")




###############
#MOD2
###############
m2.2003<-readRDS("/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod2.AQ.2003.rds")

m2.2003[,elev.s:= scale(elev)]
m2.2003[,tden.s:= scale(tden)]
m2.2003[,pden.s:= scale(pden)]
m2.2003[,dist2A1.s:= scale(dist2A1)]
m2.2003[,dist2water.s:= scale(dist2water)]
m2.2003[,dist2rail.s:= scale(dist2rail)]
m2.2003[,Dist2road.s:= scale(Dist2road)]
m2.2003[,ndvi.s:= scale(ndvi)]
m2.2003[,MeanPbl.s:= scale(MeanPbl)]
m2.2003[,p_ind.s:= scale(p_ind)]
m2.2003[,p_for.s:= scale(p_for)]
m2.2003[,p_farm.s:= scale(p_farm)]
m2.2003[,p_dos.s:= scale(p_dos)]
m2.2003[,p_dev.s:= scale(p_dev)]
m2.2003[,p_os.s:= scale(p_os)]

#generate predictions
m2.2003[, predicted.m2 := predict(object=m1.fit.2003,newdata=m2.2003,allow.new.levels=TRUE,re.form=NULL)]
describe(m2.2003$predicted.m2)
#delete implossible values
m2.2003 <- m2.2003[predicted.m2 > 0.00000000000001 , ]
m2.2003 <- m2.2003[predicted.m2 < 500   , ]

#test
# describe(m2.2003$predicted.m2)
# hist(m2.2003$predicted.m2)

#######
#M2 R2
######

m1.2003[,aodid:= paste(m1.2003$long_aod,m1.2003$lat_aod,sep="-")]

#merge co located mod1 and mod2 grids
setkey(m1.2003,aodid,day)
setkey(m2.2003,aodid,day)
m.1.2.pred <- merge(m1.2003, m2.2003[, list(aodid, day, predicted.m2)], all.x = T)
mod2_reg<-lm(m.1.2.pred$predicted~m.1.2.pred$predicted.m2)
#cleanup and save current stages (workspace)
mod1table$r2003[20] <-summary(mod2_reg)$r.squared

#map the predictions
#aggregate by guid
m2_agg <- m2.2003[, list(LTPM.m2 = mean(predicted.m2, na.rm = TRUE), lat_aod = lat_aod[1], long_aod = long_aod[1]), by = aodid]
#saveRDS(m2_agg, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/m2_agg_2003.rds")
#map the predictions
ggplot(m2_agg, aes(long_aod,lat_aod, color = LTPM.m2)) + 
  geom_point(size = 3, shape = 15) +  xlab("longitude") + ylab("latitude") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(5)) + theme_bw() + ggtitle("Long term predictions")
ggsave(file="/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM.m2.png")
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2003_p2.rds")
keep(mod1table , sure=TRUE) 
gc()

