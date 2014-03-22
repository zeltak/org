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
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_200x200.dbf")
summary(lu)


library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode")) 
lu<-rename(lu,c(popdens="popden"))



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

hist(T0008_merged$dist_A1)
hist(T0008_merged$dist_pemis)


  
#quantile(T0008_merged$dist_A1,probs=c(0.05,0.1,0.5,0.9,0.95))
#quantile(points200$dist_A1,probs=c(0.05,0.1,0.5,0.9,0.95))

T0008_merged$dist_A1o<-1/(0.01+T0008_merged$dist_A1)
T0008_merged$dist_pemiso<-1/(0.01+T0008_merged$dist_pemis)

#T0008_merged$dist_A1o<-ifelse (T0008_merged$dist_A1o >= 0.3, T0008_merged$dist_A1o, 0.3)


library(Hmisc)
describe(T0008_merged$dist_A1o)
describe(T0008_merged$dist_pemiso)


attach(T0008_merged)
T0008_merged$dist_A1q[dist_A1o >= 0 & dist_A1o< 0.286 ] <- 1
T0008_merged$dist_A1q[dist_A1o >= 0.286 & dist_A1o < 0.60]  <- 2
T0008_merged$dist_A1q[dist_A1o >= 0.6 & dist_A1o < 1.58]  <- 3
T0008_merged$dist_A1q[dist_A1o >= 1.58] <-4
T0008_merged$dist_pemisq[dist_pemiso >= 0 & dist_pemiso< 0.28 ] <- 1
T0008_merged$dist_pemisq[dist_pemiso >= 0.28 & dist_pemiso < 0.93]  <- 2
T0008_merged$dist_pemisq[dist_pemiso >= 0.93 & dist_pemiso < 1.77]  <- 3
T0008_merged$dist_pemisq[dist_pemiso >= 1.77] <-4
detach(T0008_merged) 

T0008_merged$dist_A1qn<-as.numeric(T0008_merged$dist_A1q)
T0008_merged$dist_pemisqn<-as.numeric(T0008_merged$dist_pemisq)
summary(T0008_merged$dist_A1qn)

#The GAM model
#bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+as.factor(dist_A1qn)+as.factor(dist_pemisqn),na.action=na.omit,data=T0008_merged)

#bp.model.ps<-gam(resm3~s(pcturban,fx=FALSE,k=4,bs='cr')+s(tden,fx=FALSE,k=4,bs='cr')+s(popden,fx=FALSE,k=4,bs='cr')+s(elev),na.action=na.omit,data=T0008_merged)

#bp.model.ps<-gam(resm3~s(pcturban)+s(tden)+s(popden)+s(elev),na.action=na.omit,data=T0008_merged)

#bp.model.ps<-lm(resm3~tden+popden+pcturban+dist_A1qn+dist_pemisqn,na.action=na.omit,data=T0008_merged)

bp.model.ps<-lm(resm3~tden+popden+pcturban+elev,na.action=na.omit,data=T0008_merged)

plot(bp.model.ps)

summary(bp.model.ps)
#plot(bp.model.ps)

points200<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN070_LPM_stage_200x200_base/lu200.dbf") 
summary(points200)
library(reshape)
points200<-rename(points200,c(popdens="popden")) 
points200<-rename(points200,c(urban="pcturban"))



points200$dist_A1o<-1/(0.01+points200$dist_A1)
points200$dist_pemiso<-1/(0.01+points200$dist_pemis)


attach(points200)
points200$dist_A1q[dist_A1o >= 0 & dist_A1o< 0.286 ] <- 1
points200$dist_A1q[dist_A1o >= 0.286 & dist_A1o < 0.60]  <- 2
points200$dist_A1q[dist_A1o >= 0.6 & dist_A1o < 1.58]  <- 3
points200$dist_A1q[dist_A1o >= 1.58] <-4
points200$dist_pemisq[dist_pemiso >= 0 & dist_pemiso< 0.28 ] <- 1
points200$dist_pemisq[dist_pemiso >= 0.28 & dist_pemiso < 0.93]  <- 2
points200$dist_pemisq[dist_pemiso >= 0.93 & dist_pemiso < 1.77]  <- 3
points200$dist_pemisq[dist_pemiso >= 1.77] <-4
detach(points200)
head(points200)


points200$dist_A1qn<-as.numeric(points200$dist_A1q)
points200$dist_pemisqn<-as.numeric(points200$dist_pemisq)

#predict
points200$lpm <-predict(bp.model.ps,points200)
hist(points200$lpm)

summary(points200)


write.csv(points200,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN070_LPM_stage_200x200_base/lu200LPM.csv") 


#write.dbf(points200,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN070_LPM_stage_200x200_base/lu200LPM.dbf") 
