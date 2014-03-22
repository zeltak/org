library(nlme)
library(foreign) 
library(psych)
library(mgcv)
library(reshape)

#create CV table

cvtable <- data.frame(type=character(17), r2000=numeric(17),r2001=numeric(17),r2002=numeric(17),r2003=numeric(17),r2004=numeric(17),r2005=numeric(17),r2006=numeric(17),r2007=numeric(17),r2008=numeric(17),mean=numeric(17))

cvtable$type <- c("it_1", "it_2","it_3","it_4","it_5","it_6","it_7","it_8","it_9","it_10","R2_preloc","R2_spat_pre","R2_tem_pre","","R2_FIN","R2_spatial","R2_temporal")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2000
# IMPORTS

F_T2000_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2000.dbf") 
names(F_T2000_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2000[11] <- cor(F_T2000_All$PM25, F_T2000_All$Pred,use = "complete")*cor(F_T2000_All$PM25, F_T2000_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2000_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2000_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] #sort by SiteCode
t2000m <- merge(F_T2000_All,aggf,by="SiteCode") #merge by SiteCode



t2000m$delpm <- t2000m$PM25-t2000m$barpm
t2000m$delPred <-t2000m$Pred-t2000m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2000[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2000m)
summary(mod_temporal)
cvtable$r2000[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2000_merged <- merge(F_T2000_All,lu,by="SiteCode")
names(T2000_merged)

#create residual mp3 variable
T2000_merged$resm3<-T2000_merged$PM25-T2000_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2000_merged)

summary(bp.model.ps)



T2000_merged$predlocm <-predict(bp.model.ps)

T2000_merged$OApred <- T2000_merged$Pred+T2000_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2000[15] <- cor(T2000_merged$PM25, T2000_merged$OApred,use = "complete")*cor(T2000_merged$PM25, T2000_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2000_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2000_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2000_merged <- T2000_merged[order(T2000_merged$SiteCode),] #sort by SiteCode
t2000m <- merge(T2000_merged,aggf,by="SiteCode") #merge by SiteCode


t2000m$delpm <- t2000m$PM25-t2000m$barpm
t2000m$delpred <-t2000m$OApred-t2000m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2000m)
summary(mod_spatial)
cvtable$r2000[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2000m)
summary(mod_temporal)
cvtable$r2000[17] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2001
# IMPORTS

F_T2001_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2001.dbf") 
names(F_T2001_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2001[11] <- cor(F_T2001_All$PM25, F_T2001_All$Pred,use = "complete")*cor(F_T2001_All$PM25, F_T2001_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2001_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2001_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] #sort by SiteCode
t2001m <- merge(F_T2001_All,aggf,by="SiteCode") #merge by SiteCode



t2001m$delpm <- t2001m$PM25-t2001m$barpm
t2001m$delPred <-t2001m$Pred-t2001m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2001[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2001m)
summary(mod_temporal)
cvtable$r2001[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2001_merged <- merge(F_T2001_All,lu,by="SiteCode")
names(T2001_merged)

#create residual mp3 variable
T2001_merged$resm3<-T2001_merged$PM25-T2001_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2001_merged)

summary(bp.model.ps)



T2001_merged$predlocm <-predict(bp.model.ps)

T2001_merged$OApred <- T2001_merged$Pred+T2001_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2001[15] <- cor(T2001_merged$PM25, T2001_merged$OApred,use = "complete")*cor(T2001_merged$PM25, T2001_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2001_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2001_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2001_merged <- T2001_merged[order(T2001_merged$SiteCode),] #sort by SiteCode
t2001m <- merge(T2001_merged,aggf,by="SiteCode") #merge by SiteCode


t2001m$delpm <- t2001m$PM25-t2001m$barpm
t2001m$delpred <-t2001m$OApred-t2001m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2001m)
summary(mod_spatial)
cvtable$r2001[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2001m)
summary(mod_temporal)
cvtable$r2001[17] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2002
# IMPORTS

F_T2002_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2002.dbf") 
names(F_T2002_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2002[11] <- cor(F_T2002_All$PM25, F_T2002_All$Pred,use = "complete")*cor(F_T2002_All$PM25, F_T2002_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2002_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2002_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] #sort by SiteCode
t2002m <- merge(F_T2002_All,aggf,by="SiteCode") #merge by SiteCode



t2002m$delpm <- t2002m$PM25-t2002m$barpm
t2002m$delPred <-t2002m$Pred-t2002m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2002[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2002m)
summary(mod_temporal)
cvtable$r2002[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2002_merged <- merge(F_T2002_All,lu,by="SiteCode")
names(T2002_merged)

#create residual mp3 variable
T2002_merged$resm3<-T2002_merged$PM25-T2002_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2002_merged)

summary(bp.model.ps)



T2002_merged$predlocm <-predict(bp.model.ps)

T2002_merged$OApred <- T2002_merged$Pred+T2002_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2002[15] <- cor(T2002_merged$PM25, T2002_merged$OApred,use = "complete")*cor(T2002_merged$PM25, T2002_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2002_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2002_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2002_merged <- T2002_merged[order(T2002_merged$SiteCode),] #sort by SiteCode
t2002m <- merge(T2002_merged,aggf,by="SiteCode") #merge by SiteCode


t2002m$delpm <- t2002m$PM25-t2002m$barpm
t2002m$delpred <-t2002m$OApred-t2002m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2002m)
summary(mod_spatial)
cvtable$r2002[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2002m)
summary(mod_temporal)
cvtable$r2002[17] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2003
# IMPORTS

F_T2003_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2003.dbf") 
names(F_T2003_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2003[11] <- cor(F_T2003_All$PM25, F_T2003_All$Pred,use = "complete")*cor(F_T2003_All$PM25, F_T2003_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2003_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2003_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] #sort by SiteCode
t2003m <- merge(F_T2003_All,aggf,by="SiteCode") #merge by SiteCode



t2003m$delpm <- t2003m$PM25-t2003m$barpm
t2003m$delPred <-t2003m$Pred-t2003m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2003[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2003m)
summary(mod_temporal)
cvtable$r2003[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2003_merged <- merge(F_T2003_All,lu,by="SiteCode")
names(T2003_merged)

#create residual mp3 variable
T2003_merged$resm3<-T2003_merged$PM25-T2003_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2003_merged)

summary(bp.model.ps)



T2003_merged$predlocm <-predict(bp.model.ps)

T2003_merged$OApred <- T2003_merged$Pred+T2003_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2003[15] <- cor(T2003_merged$PM25, T2003_merged$OApred,use = "complete")*cor(T2003_merged$PM25, T2003_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2003_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2003_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2003_merged <- T2003_merged[order(T2003_merged$SiteCode),] #sort by SiteCode
t2003m <- merge(T2003_merged,aggf,by="SiteCode") #merge by SiteCode


t2003m$delpm <- t2003m$PM25-t2003m$barpm
t2003m$delpred <-t2003m$OApred-t2003m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2003m)
summary(mod_spatial)
cvtable$r2003[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2003m)
summary(mod_temporal)
cvtable$r2003[17] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2004
# IMPORTS

F_T2004_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2004.dbf") 
names(F_T2004_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2004[11] <- cor(F_T2004_All$PM25, F_T2004_All$Pred,use = "complete")*cor(F_T2004_All$PM25, F_T2004_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2004_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2004_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] #sort by SiteCode
t2004m <- merge(F_T2004_All,aggf,by="SiteCode") #merge by SiteCode



t2004m$delpm <- t2004m$PM25-t2004m$barpm
t2004m$delPred <-t2004m$Pred-t2004m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2004[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2004m)
summary(mod_temporal)
cvtable$r2004[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2004_merged <- merge(F_T2004_All,lu,by="SiteCode")
names(T2004_merged)

#create residual mp3 variable
T2004_merged$resm3<-T2004_merged$PM25-T2004_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2004_merged)

summary(bp.model.ps)



T2004_merged$predlocm <-predict(bp.model.ps)

T2004_merged$OApred <- T2004_merged$Pred+T2004_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2004[15] <- cor(T2004_merged$PM25, T2004_merged$OApred,use = "complete")*cor(T2004_merged$PM25, T2004_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2004_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2004_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2004_merged <- T2004_merged[order(T2004_merged$SiteCode),] #sort by SiteCode
t2004m <- merge(T2004_merged,aggf,by="SiteCode") #merge by SiteCode


t2004m$delpm <- t2004m$PM25-t2004m$barpm
t2004m$delpred <-t2004m$OApred-t2004m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2004m)
summary(mod_spatial)
cvtable$r2004[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2004m)
summary(mod_temporal)
cvtable$r2004[17] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2005
# IMPORTS

F_T2005_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2005.dbf") 
names(F_T2005_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2005[11] <- cor(F_T2005_All$PM25, F_T2005_All$Pred,use = "complete")*cor(F_T2005_All$PM25, F_T2005_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2005_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2005_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] #sort by SiteCode
t2005m <- merge(F_T2005_All,aggf,by="SiteCode") #merge by SiteCode



t2005m$delpm <- t2005m$PM25-t2005m$barpm
t2005m$delPred <-t2005m$Pred-t2005m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2005[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2005m)
summary(mod_temporal)
cvtable$r2005[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2005_merged <- merge(F_T2005_All,lu,by="SiteCode")
names(T2005_merged)

#create residual mp3 variable
T2005_merged$resm3<-T2005_merged$PM25-T2005_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2005_merged)

summary(bp.model.ps)



T2005_merged$predlocm <-predict(bp.model.ps)

T2005_merged$OApred <- T2005_merged$Pred+T2005_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2005[15] <- cor(T2005_merged$PM25, T2005_merged$OApred,use = "complete")*cor(T2005_merged$PM25, T2005_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2005_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2005_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2005_merged <- T2005_merged[order(T2005_merged$SiteCode),] #sort by SiteCode
t2005m <- merge(T2005_merged,aggf,by="SiteCode") #merge by SiteCode


t2005m$delpm <- t2005m$PM25-t2005m$barpm
t2005m$delpred <-t2005m$OApred-t2005m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2005m)
summary(mod_spatial)
cvtable$r2005[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2005m)
summary(mod_temporal)
cvtable$r2005[17] <-summary(mod_temporal)$r.squared
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2006
# IMPORTS

F_T2006_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2006.dbf") 
names(F_T2006_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2006[11] <- cor(F_T2006_All$PM25, F_T2006_All$Pred,use = "complete")*cor(F_T2006_All$PM25, F_T2006_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2006_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2006_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] #sort by SiteCode
t2006m <- merge(F_T2006_All,aggf,by="SiteCode") #merge by SiteCode



t2006m$delpm <- t2006m$PM25-t2006m$barpm
t2006m$delPred <-t2006m$Pred-t2006m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2006[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2006m)
summary(mod_temporal)
cvtable$r2006[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 
 
names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2006_merged <- merge(F_T2006_All,lu,by="SiteCode")
names(T2006_merged)

#create residual mp3 variable
T2006_merged$resm3<-T2006_merged$PM25-T2006_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2006_merged)

summary(bp.model.ps)



T2006_merged$predlocm <-predict(bp.model.ps)

T2006_merged$OApred <- T2006_merged$Pred+T2006_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2006[15] <- cor(T2006_merged$PM25, T2006_merged$OApred,use = "complete")*cor(T2006_merged$PM25, T2006_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2006_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2006_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2006_merged <- T2006_merged[order(T2006_merged$SiteCode),] #sort by SiteCode
t2006m <- merge(T2006_merged,aggf,by="SiteCode") #merge by SiteCode


t2006m$delpm <- t2006m$PM25-t2006m$barpm
t2006m$delpred <-t2006m$OApred-t2006m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2006m)
summary(mod_spatial)
cvtable$r2006[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2006m)
summary(mod_temporal)
cvtable$r2006[17] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2007
# IMPORTS

F_T2007_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2007.dbf") 
names(F_T2007_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2007[11] <- cor(F_T2007_All$PM25, F_T2007_All$Pred,use = "complete")*cor(F_T2007_All$PM25, F_T2007_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2007_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2007_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] #sort by SiteCode
t2007m <- merge(F_T2007_All,aggf,by="SiteCode") #merge by SiteCode



t2007m$delpm <- t2007m$PM25-t2007m$barpm
t2007m$delPred <-t2007m$Pred-t2007m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2007[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2007m)
summary(mod_temporal)
cvtable$r2007[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2007_merged <- merge(F_T2007_All,lu,by="SiteCode")
names(T2007_merged)

#create residual mp3 variable
T2007_merged$resm3<-T2007_merged$PM25-T2007_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2007_merged)

summary(bp.model.ps)



T2007_merged$predlocm <-predict(bp.model.ps)

T2007_merged$OApred <- T2007_merged$Pred+T2007_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2007[15] <- cor(T2007_merged$PM25, T2007_merged$OApred,use = "complete")*cor(T2007_merged$PM25, T2007_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2007_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2007_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2007_merged <- T2007_merged[order(T2007_merged$SiteCode),] #sort by SiteCode
t2007m <- merge(T2007_merged,aggf,by="SiteCode") #merge by SiteCode


t2007m$delpm <- t2007m$PM25-t2007m$barpm
t2007m$delpred <-t2007m$OApred-t2007m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2007m)
summary(mod_spatial)
cvtable$r2007[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2007m)
summary(mod_temporal)
cvtable$r2007[17] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2008
# IMPORTS

F_T2008_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2008.dbf") 
names(F_T2008_All)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2008[11] <- cor(F_T2008_All$PM25, F_T2008_All$Pred,use = "complete")*cor(F_T2008_All$PM25, F_T2008_All$Pred,use = "complete")



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barPred
attach(F_T2008_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(Pred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2008_All)

names(aggf) <- c("SiteCode", "barpm", "barPred")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] #sort by SiteCode
t2008m <- merge(F_T2008_All,aggf,by="SiteCode") #merge by SiteCode



t2008m$delpm <- t2008m$PM25-t2008m$barpm
t2008m$delPred <-t2008m$Pred-t2008m$barPred

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2008[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2008m)
summary(mod_temporal)
cvtable$r2008[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf") 

names(lu)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))





# sorts
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 





# merge two dataframes by ID
T2008_merged <- merge(F_T2008_All,lu,by="SiteCode")
names(T2008_merged)

#create residual mp3 variable
T2008_merged$resm3<-T2008_merged$PM25-T2008_merged$Pred




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=8,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2008_merged)

summary(bp.model.ps)



T2008_merged$predlocm <-predict(bp.model.ps)

T2008_merged$OApred <- T2008_merged$Pred+T2008_merged$predlocm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2008[15] <- cor(T2008_merged$PM25, T2008_merged$OApred,use = "complete")*cor(T2008_merged$PM25, T2008_merged$OApred,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpred
attach(T2008_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApred ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2008_merged)


names(aggf) <- c("SiteCode", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2008_merged <- T2008_merged[order(T2008_merged$SiteCode),] #sort by SiteCode
t2008m <- merge(T2008_merged,aggf,by="SiteCode") #merge by SiteCode


t2008m$delpm <- t2008m$PM25-t2008m$barpm
t2008m$delpred <-t2008m$OApred-t2008m$barpred



#get R2 for the spatial and temporal:



#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2008m)
summary(mod_spatial)
cvtable$r2008[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2008m)
summary(mod_temporal)
cvtable$r2008[17] <-summary(mod_temporal)$r.squared



cvtable$mean<- rowMeans(cvtable[,2:10])


write.csv(cvtable,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.5.Results/mod1/Final_mod1_table.csv") 

