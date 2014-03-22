library(nlme)
library(foreign) 
library(psych)
library(mgcv)
library(reshape)

F_T2000_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2000.dbf")

F_T2001_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2001.dbf")

F_T2002_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2002.dbf")

F_T2003_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2003.dbf")

F_T2004_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2004.dbf")

F_T2005_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2005.dbf")

F_T2006_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2006.dbf")

F_T2007_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2007.dbf")

F_T2008_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2008.dbf")





All0008<-rbind(F_T2000_All,F_T2001_All,F_T2002_All,F_T2003_All,F_T2004_All,F_T2005_All,F_T2006_All,F_T2007_All,F_T2008_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
summary(lu)


library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode")) 
lu<-rename(lu,c(guid_="guid"))


# sorts
All0008 <- All0008[order(All0008$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T0008_merged <- merge(All0008,lu,by="SiteCode")

#delete missing cases in Predictions
T0008_merged = T0008_merged[complete.cases(T0008_merged$pm_mod3),]
summary(T0008_merged)


#create residual mp3 variable
T0008_merged$resm3<-T0008_merged$PM25-T0008_merged$pm_mod3



summary(T0008_merged)

#The GAM model
bp.model.ps<-gam(resm3~s(tden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(popden,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T0008_merged)

summary(bp.model.ps)
#plot(bp.model.ps)

points200<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN070_LPM_stage_200x200_base/lu200.dbf") 

summary(points200)



library(reshape)
points200<-rename(points200,c(popdens="popden")) 
points200<-rename(points200,c(urban="pcturban"))


points200$lpm <-predict(bp.model.ps,points200)
hist(points200$lpm)

write.csv(points200,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN070_LPM_stage_200x200_base/lu200LPM.csv") 

head(points200)