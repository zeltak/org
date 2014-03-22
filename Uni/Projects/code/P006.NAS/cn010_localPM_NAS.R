library(foreign) 
library(stats)
library(mgcv)
library(splines)
library(nlme)
library(car)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

# IMPORTS

#import all pm stations for all years and the stage 3 predictions


F_T2000_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2000.dbf")

F_T2001_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2001.dbf")

F_T2002_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2002.dbf")

F_T2003_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2003.dbf")

F_T2004_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2004.dbf")

F_T2005_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2005.dbf")

F_T2006_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2006.dbf")

F_T2007_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2007.dbf")

F_T2008_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2008.dbf")





All0008<-rbind(F_T2000_All,F_T2001_All,F_T2002_All,F_T2003_All,F_T2004_All,F_T2005_All,F_T2006_All,F_T2007_All,F_T2008_All)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE

#import the 50x50 LU terms for the pred model monitoring stations

lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 


names(lu)



# sorts
All0008 <- All0008[order(All0008$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2000_merged <- merge(All0008,lu,by="SiteCode")
T2000_merged <- subset(T2000_merged,T2000_merged$pcturban >-0.0000000001)
T2000_merged <- subset(T2000_merged,T2000_merged$pm_mod3 >-0.0000000001)
summary(T2000_merged)

T2000_merged$resm3<-T2000_merged$PM25-T2000_merged$pm_mod3

summary(T2000_merged$pm_mod3)


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2000_merged)

summary(bp.model.ps)

#
#import the cases datastes

g3 <- read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.4.Work/2.Gather_data/FN009_localPM/lu_50.dbf") 
summary(g3)







#make sure var names and units are the same as in the NE pm model 
library(reshape)

g3<-rename(g3,c(poden="popden")) 

summary(T2000_merged)
summary(g3)




resid_pred <- predict(bp.model.ps,g3)

g3$localpm <- resid_pred

summary(g3$localpm)
hist(g3$localpm)

write.csv (g3, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.4.Work/2.Gather_data/FN010_localPM_predictions/nas_lpm.csv")


