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
m1.formula <- as.formula(PM25~ aod+
                        Temp+WD+RH+WS+Dust+Rain+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+dist2rail.s+dist2A1.s+Dist2road.s+dist2water.s+ndvi.s+season #spatial
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         +(1+aod|day/reg_num))

#full fit
m1.fit.2007 <-  lmer(m1.formula,data=m1.2007,weights=normwt)
m1.2007$predicted <- predict(m1.fit.2007)
mod1table$r2007[1]<-summary(lm(PM25~predicted,data=m1.2007))$r.squared #0.80


#spatial
spatial2007<-m1.2007 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(predicted, na.rm=TRUE)) 
mod1table$r2007[3]<-  summary(lm(barpm ~ barpred, data=spatial2007))$r.squared
spatial2007[,spatresid:= barpm-barpred]
mod1table$r2007[2]<- sqrt(mean(spatial2007$spatresid^2))                   
                           
#temporal
tempo2007<-left_join(m1.2007,spatial2007)
tempo2007[,delpm := PM25-barpm]
tempo2007[,delpred := PM25-barpred]
mod1table$r2007[4] <- summary(lm(delpm ~ delpred, data=tempo2007))$r.squared


#new method allan
mod1[, sqrt(mean((daymean - predicted)^2))]

# easy way to run on subsets
# first we define the function we want on each subset
runandsummarizemod1 <- function(df){
  out.m1.plyr = lmer(m1.formula, data = df)
  df$predicted <- predict(out.m1.plyr)
  data.frame(nobs = nrow(df), rsq = round(summary(lm(daymean~predicted, data = df))$r.squared, 3))  
}
# then we call ddply
mod1subsets <- ddply(mod1, .(yr), runandsummarizemod1)
mod1subsets
ggplot(mod1subsets, aes(yr, rsq)) + geom_point() + geom_line()






# Cross-validation
# for mod1

stns <- unique(m1.2007$stn)

# cross-validation and model building
# repeated leave x stnitors out CV
n.iter <- 20
xout <- 5 # number of stnitors to hold out
# how many combinations if we pull out xout stns
ncol(combn(stns, 2))
# list to store scheme
cvscheme <- list()
cvout <- list()
system.time(for(i in 1:n.iter){
  stns.test <- stns[sample(length(stns), xout)]
  cvscheme[[i]] <- stns.test
  test <- m1.2007[stn %in% stns.test, ]
  train<- m1.2007[!stn %in% stns.test, ]
  print(paste("iteration #", i, "testing set is stnitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  trainmod <-  lmer(m1.formula, data =  train)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i  
  # export these results
  cvout[[i]] <- test[, list(day, stn, PM25, predcv, itercv)]
}# end of cross-validation loop
)
alltest <- rbindlist(cvout)
head(alltest, 2)
summary(lm(PM25 ~ predcv, data = alltest))
# compute root mean squared error
alltest[, sqrt(mean((PM25 - predcv)^2))]

alltest[which.max(PM25)]

ggplot(alltest, aes(predcv, PM25)) + 
  geom_abline(linetype = "dashed") + 
  geom_point(aes(color = factor(itercv))) + geom_smooth() + 
  facet_wrap(~stn) + coord_equal() + 
  theme_bw(12)

#save(iter.out, cv.rsq, stns.testset, file = "LUR_cross-validation_splits.Rdata") 


# end of file


# drop new years and newyears eve
mod1 <- mod1[!dayofyr %in% c(1,365,366),]
# drop christmas
mod1 <- mod1[!day  %in% as.Date(paste0(2004:2014, "-12-25")), ]

















#spatial
spatialCV2007<-mod1CV_all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(predicted, na.rm=TRUE)) 
mod1table$r2007[8]<-  summary(lm(barpm ~ barpred, data=spatialCV2007))$r.squared
spatialCV2007[,spatresid:= barpm-barpred]
mod1table$r2007[10]<- sqrt(mean(spatialCV2007$spatresid^2))                   
                           
#temporal
tempoCV2007<-left_join(mod1CV_all,spatialCV2007)
tempoCV2007[,delpm := PM25-barpm]
tempoCV2007[,delpred := PM25-barpred]
mod1table$r2007[9] <- summary(lm(delpm ~ delpred, data=tempoCV2007))$r.squared
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

mod1table$r2007[11] <-summary(mod1d_reg_st)$r.squared
mod1table$r2007[12] <-summary(mod1d_reg_st)$coef[1,1]
mod1table$r2007[13] <-summary(mod1d_reg_st)$coef[1,2]
mod1table$r2007[14] <-summary(mod1d_reg_st)$coef[2,1]
mod1table$r2007[15] <-summary(mod1d_reg_st)$coef[2,2]
#rmspe
mod1table$r2007[16]<- sqrt(mean(mod1d_reg_st$residual^2))

#spatial
aggf<- ddply(mod1d_all_st, c("stn"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))

#spatial
m1CVLPM_agg <- (mod1d_all_st[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = stn])  
# Rename column
setnames(m1CVLPM_agg,"V1","barpm")
setnames(m1CVLPM_agg,"V2","barpred")
mod1LPM_spatial <- lm(barpm ~ barpred, data=m1CVLPM_agg)
mod1table$r2007[17] <- summary(mod1LPM_spatial)$r.squared

#temporal
setkey(m1CVLPM_agg,stn)
setkey(mod1d_all_st,stn)
dat <- merge(mod1d_all_st,m1CVLPM_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2007[18] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2007[19]<- sqrt(mean(dat$spatresid^2))


#############save midpoint
saveRDS(mod1table, "/media/NAS/Uni/Projects/P046.Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1table2007_p1.rds")




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
describe(m2.2007$pred.m2)
#delete implossible values
m2.2007 <- m2.2007[pred.m2 > 0.00000000000001 , ]
m2.2007 <- m2.2007[pred.m2 < 500   , ]



#test
# describe(m2.2007$pred.m2)
# hist(m2.2007$pred.m2)

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
mod1table$r2007[20] <-summary(mod2_reg)$r.squared

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

