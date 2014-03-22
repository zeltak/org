library(foreign) 
library(mgcv)
library(reshape)

FALL <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN080_files_4CV_RMSE/t2001.csv", header=T)  



names(FALL)
#run "kriging" analysis
fi1_gam <- gam(PM25 ~ s(Lat_PM,Long_PM, by=m), data=FALL)
fit1pred <- predict(fi1_gam)
cor(fit1pred, FALL$PM25)^2
FALL$predicted <-fit1pred



#create barpm and barpred
attach(FALL)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf <- merge(agg1,agg2,by="SiteCode")
detach(FALL)

aggf <- rename(aggf,c(PM25="barpm",predicted="barpred")  )  #rename variables with the reshape package


aggf <- aggf[order(aggf$SiteCode),]  #sort by sitecode

FALL <- FALL[order(FALL$SiteCode),] #sort by sitecode

FALLm <- merge(FALL,aggf,by="SiteCode") #merge by sitecode


FALLm$delpm <-FALLm$PM25-FALLm$barpm
FALLm$delpred <-FALLm$predicted-FALLm$barpred


######################################################################
#get R2 for the spatial and temporal:

# Temporal R2           Delta PM~ Delta Predicted
# Delta PM: is the difference between the actual PM2.5 in place i at time t and the annual mean PM2.5 at that location
# Delta predicted: is the difference between the predicted PM2.5 in place i at time t and the annual predicted mean PM2.5 at that location
# Spatial R2   was calculated by regressing the annual mean PM2.5 against the mean predicted pm at place i.

######################################################################

#temporal
 mod_temporal <- lm(delpm ~ delpred, data=FALLm)
 summary(mod_temporal)

#spatial
mod_spatial <- lm(barpm ~ barpred, data=FALLm)
summary(mod_spatial)

#final R2 for krigiing is 0.53






####RUN THE ALL_MIXED_SPATIAL_VS_TEMPORAL_2005 SCRIPT (d4F) TO GET THE cv FILES NEEDED

library(reshape)
T2005all <- rename(FALLm, c(latitude="LATITUDE" ,longitude="LONGITUDE", m="M" ))

T2005pred <- predict(fi1_gam,T2005all)
cor(T2005pred, T2005all$mfvalue)


