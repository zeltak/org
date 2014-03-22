library(nlme)
library(foreign) 
library(psych)
library(mgcv)
library(reshape)

#create CV table
cvtable <- data.frame(type=character(14), r2000=numeric(14),r2001=numeric(14),r2002=numeric(14),r2003=numeric(14),r2004=numeric(14),r2005=numeric(14),r2006=numeric(14),r2007=numeric(14),r2008=numeric(14),mean=numeric(14))

#name columns
cvtable$type <- c("R2_preloc","pre_inter","pre_inter_SE","pre_slope","pre_slope_SE","R2_spat_pre","R2_tem_pre","R2_FIN","FIN_inter","FIN_inter_SE","FIN_slope","FIN_slope_SE","R2_spatial","R2_temporal")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2000

# IMPORTS
F_T2000_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2000_val_val.csv",header=T) 

names(F_T2000_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2000_All)
summary(mod_all)
cvtable$r2000[1] <-summary(mod_all)$r.squared
cvtable$r2000[2] <-summary(mod_all)$coef[1,1]
cvtable$r2000[3] <-summary(mod_all)$coef[1,2]
cvtable$r2000[4] <-summary(mod_all)$coef[2,1]
cvtable$r2000[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2000_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2000_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] #sort by SiteCode
t2000m <- merge(F_T2000_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2000m)
t2000m$delpm <- t2000m$PM25-t2000m$barpm
t2000m$delPred <-t2000m$pm_mod3-t2000m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2000[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2000m)
summary(mod_temporal)
cvtable$r2000[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2000_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2000_merged <- merge(F_T2000_All,lu,by="SiteCode")
summary(T2000_merged)


T2000_merged$resm3<-T2000_merged$PM25-T2000_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2000_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2000_merged$predlocm <-pred_locm
T2000_merged$OApred <- T2000_merged$pm_mod3+T2000_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2000_merged)
summary(mod_all_lpm)
cvtable$r2000[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2000[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2000[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2000[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2000[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2000[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2000m)
summary(mod_temporal)
cvtable$r2000[14] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2001

# IMPORTS
F_T2001_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2001_val.csv",header=T) 

names(F_T2001_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2001_All)
summary(mod_all)
cvtable$r2001[1] <-summary(mod_all)$r.squared
cvtable$r2001[2] <-summary(mod_all)$coef[1,1]
cvtable$r2001[3] <-summary(mod_all)$coef[1,2]
cvtable$r2001[4] <-summary(mod_all)$coef[2,1]
cvtable$r2001[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2001_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2001_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] #sort by SiteCode
t2001m <- merge(F_T2001_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2001m)
t2001m$delpm <- t2001m$PM25-t2001m$barpm
t2001m$delPred <-t2001m$pm_mod3-t2001m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2001[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2001m)
summary(mod_temporal)
cvtable$r2001[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2001_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2001_merged <- merge(F_T2001_All,lu,by="SiteCode")
summary(T2001_merged)


T2001_merged$resm3<-T2001_merged$PM25-T2001_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2001_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2001_merged$predlocm <-pred_locm
T2001_merged$OApred <- T2001_merged$pm_mod3+T2001_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2001_merged)
summary(mod_all_lpm)
cvtable$r2001[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2001[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2001[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2001[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2001[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2001[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2001m)
summary(mod_temporal)
cvtable$r2001[14] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2002

# IMPORTS
F_T2002_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2002_val.csv",header=T) 

names(F_T2002_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2002_All)
summary(mod_all)
cvtable$r2002[1] <-summary(mod_all)$r.squared
cvtable$r2002[2] <-summary(mod_all)$coef[1,1]
cvtable$r2002[3] <-summary(mod_all)$coef[1,2]
cvtable$r2002[4] <-summary(mod_all)$coef[2,1]
cvtable$r2002[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2002_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2002_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] #sort by SiteCode
t2002m <- merge(F_T2002_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2002m)
t2002m$delpm <- t2002m$PM25-t2002m$barpm
t2002m$delPred <-t2002m$pm_mod3-t2002m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2002[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2002m)
summary(mod_temporal)
cvtable$r2002[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2002_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2002_merged <- merge(F_T2002_All,lu,by="SiteCode")
summary(T2002_merged)


T2002_merged$resm3<-T2002_merged$PM25-T2002_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2002_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2002_merged$predlocm <-pred_locm
T2002_merged$OApred <- T2002_merged$pm_mod3+T2002_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2002_merged)
summary(mod_all_lpm)
cvtable$r2002[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2002[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2002[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2002[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2002[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2002[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2002m)
summary(mod_temporal)
cvtable$r2002[14] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2003

# IMPORTS
F_T2003_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2003_val.csv",header=T) 

names(F_T2003_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2003_All)
summary(mod_all)
cvtable$r2003[1] <-summary(mod_all)$r.squared
cvtable$r2003[2] <-summary(mod_all)$coef[1,1]
cvtable$r2003[3] <-summary(mod_all)$coef[1,2]
cvtable$r2003[4] <-summary(mod_all)$coef[2,1]
cvtable$r2003[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2003_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2003_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] #sort by SiteCode
t2003m <- merge(F_T2003_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2003m)
t2003m$delpm <- t2003m$PM25-t2003m$barpm
t2003m$delPred <-t2003m$pm_mod3-t2003m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2003[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2003m)
summary(mod_temporal)
cvtable$r2003[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2003_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2003_merged <- merge(F_T2003_All,lu,by="SiteCode")
summary(T2003_merged)


T2003_merged$resm3<-T2003_merged$PM25-T2003_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2003_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2003_merged$predlocm <-pred_locm
T2003_merged$OApred <- T2003_merged$pm_mod3+T2003_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2003_merged)
summary(mod_all_lpm)
cvtable$r2003[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2003[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2003[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2003[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2003[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2003[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2003m)
summary(mod_temporal)
cvtable$r2003[14] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2004

# IMPORTS
F_T2004_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2004_val.csv",header=T) 

names(F_T2004_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2004_All)
summary(mod_all)
cvtable$r2004[1] <-summary(mod_all)$r.squared
cvtable$r2004[2] <-summary(mod_all)$coef[1,1]
cvtable$r2004[3] <-summary(mod_all)$coef[1,2]
cvtable$r2004[4] <-summary(mod_all)$coef[2,1]
cvtable$r2004[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2004_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2004_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] #sort by SiteCode
t2004m <- merge(F_T2004_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2004m)
t2004m$delpm <- t2004m$PM25-t2004m$barpm
t2004m$delPred <-t2004m$pm_mod3-t2004m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2004[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2004m)
summary(mod_temporal)
cvtable$r2004[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2004_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2004_merged <- merge(F_T2004_All,lu,by="SiteCode")
summary(T2004_merged)


T2004_merged$resm3<-T2004_merged$PM25-T2004_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2004_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2004_merged$predlocm <-pred_locm
T2004_merged$OApred <- T2004_merged$pm_mod3+T2004_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2004_merged)
summary(mod_all_lpm)
cvtable$r2004[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2004[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2004[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2004[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2004[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2004[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2004m)
summary(mod_temporal)
cvtable$r2004[14] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2005

# IMPORTS
F_T2005_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2005_val.csv",header=T) 

names(F_T2005_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2005_All)
summary(mod_all)
cvtable$r2005[1] <-summary(mod_all)$r.squared
cvtable$r2005[2] <-summary(mod_all)$coef[1,1]
cvtable$r2005[3] <-summary(mod_all)$coef[1,2]
cvtable$r2005[4] <-summary(mod_all)$coef[2,1]
cvtable$r2005[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2005_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2005_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] #sort by SiteCode
t2005m <- merge(F_T2005_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2005m)
t2005m$delpm <- t2005m$PM25-t2005m$barpm
t2005m$delPred <-t2005m$pm_mod3-t2005m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2005[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2005m)
summary(mod_temporal)
cvtable$r2005[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2005_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2005_merged <- merge(F_T2005_All,lu,by="SiteCode")
summary(T2005_merged)


T2005_merged$resm3<-T2005_merged$PM25-T2005_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2005_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2005_merged$predlocm <-pred_locm
T2005_merged$OApred <- T2005_merged$pm_mod3+T2005_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2005_merged)
summary(mod_all_lpm)
cvtable$r2005[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2005[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2005[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2005[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2005[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2005[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2005m)
summary(mod_temporal)
cvtable$r2005[14] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2006

# IMPORTS
F_T2006_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2006_val.csv",header=T) 

names(F_T2006_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2006_All)
summary(mod_all)
cvtable$r2006[1] <-summary(mod_all)$r.squared
cvtable$r2006[2] <-summary(mod_all)$coef[1,1]
cvtable$r2006[3] <-summary(mod_all)$coef[1,2]
cvtable$r2006[4] <-summary(mod_all)$coef[2,1]
cvtable$r2006[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2006_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2006_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] #sort by SiteCode
t2006m <- merge(F_T2006_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2006m)
t2006m$delpm <- t2006m$PM25-t2006m$barpm
t2006m$delPred <-t2006m$pm_mod3-t2006m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2006[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2006m)
summary(mod_temporal)
cvtable$r2006[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2006_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2006_merged <- merge(F_T2006_All,lu,by="SiteCode")
summary(T2006_merged)


T2006_merged$resm3<-T2006_merged$PM25-T2006_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2006_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2006_merged$predlocm <-pred_locm
T2006_merged$OApred <- T2006_merged$pm_mod3+T2006_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2006_merged)
summary(mod_all_lpm)
cvtable$r2006[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2006[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2006[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2006[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2006[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2006[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2006m)
summary(mod_temporal)
cvtable$r2006[14] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2007

# IMPORTS
F_T2007_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2007_val.csv",header=T) 

names(F_T2007_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2007_All)
summary(mod_all)
cvtable$r2007[1] <-summary(mod_all)$r.squared
cvtable$r2007[2] <-summary(mod_all)$coef[1,1]
cvtable$r2007[3] <-summary(mod_all)$coef[1,2]
cvtable$r2007[4] <-summary(mod_all)$coef[2,1]
cvtable$r2007[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2007_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2007_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] #sort by SiteCode
t2007m <- merge(F_T2007_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2007m)
t2007m$delpm <- t2007m$PM25-t2007m$barpm
t2007m$delPred <-t2007m$pm_mod3-t2007m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2007[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2007m)
summary(mod_temporal)
cvtable$r2007[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2007_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2007_merged <- merge(F_T2007_All,lu,by="SiteCode")
summary(T2007_merged)


T2007_merged$resm3<-T2007_merged$PM25-T2007_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2007_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2007_merged$predlocm <-pred_locm
T2007_merged$OApred <- T2007_merged$pm_mod3+T2007_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2007_merged)
summary(mod_all_lpm)
cvtable$r2007[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2007[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2007[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2007[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2007[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2007[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2007m)
summary(mod_temporal)
cvtable$r2007[14] <-summary(mod_temporal)$r.squared







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2008

# IMPORTS
F_T2008_All <-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN006_mod3pred__localPM/m3si_2008_val.csv",header=T) 

names(F_T2008_All)


#calculate R2 pre local pm and spatial vs temporal
mod_all <- lm(PM25 ~ pm_mod3, data=F_T2008_All)
summary(mod_all)
cvtable$r2008[1] <-summary(mod_all)$r.squared
cvtable$r2008[2] <-summary(mod_all)$coef[1,1]
cvtable$r2008[3] <-summary(mod_all)$coef[1,2]
cvtable$r2008[4] <-summary(mod_all)$coef[2,1]
cvtable$r2008[5] <-summary(mod_all)$coef[2,2]
#calculate R2 pre local pm and spatial vs temporal

#calculate R2 pre local pm and spatial vs temporal

#create barpm and barPred
attach(F_T2008_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2008_All)
names(aggf) <- c("SiteCode", "barpm", "barPred")
#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] #sort by SiteCode
t2008m <- merge(F_T2008_All,aggf,by="SiteCode") #merge by SiteCode
summary(t2008m)
t2008m$delpm <- t2008m$PM25-t2008m$barpm
t2008m$delPred <-t2008m$pm_mod3-t2008m$barPred


#get R2 for the spatial and temporal:

#spatial
mod_spatial <- lm(barpm ~ barPred, data=aggf)
summary(mod_spatial)
cvtable$r2008[6] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delPred, data=t2008m)
summary(mod_temporal)
cvtable$r2008[7] <-summary(mod_temporal)$r.squared



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2008_All)

library(reshape)
lu<-rename(lu,c(SITECODE="SiteCode"))



# sorts
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2008_merged <- merge(F_T2008_All,lu,by="SiteCode")
summary(T2008_merged)


T2008_merged$resm3<-T2008_merged$PM25-T2008_merged$pm_mod3




#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2008_merged)
summary(bp.model.ps)






pred_locm <-predict(bp.model.ps)
T2008_merged$predlocm <-pred_locm
T2008_merged$OApred <- T2008_merged$pm_mod3+T2008_merged$predlocm




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2

#calculate R2 pre local pm and spatial vs temporal
mod_all_lpm <- lm(PM25 ~ OApred, data=T2008_merged)
summary(mod_all_lpm)
cvtable$r2008[8] <-summary(mod_all_lpm)$r.squared
cvtable$r2008[9] <-summary(mod_all_lpm)$coef[1,1]
cvtable$r2008[10] <-summary(mod_all_lpm)$coef[1,2]
cvtable$r2008[11] <-summary(mod_all_lpm)$coef[2,1]
cvtable$r2008[12] <-summary(mod_all_lpm)$coef[2,2]




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
cvtable$r2008[13] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2008m)
summary(mod_temporal)
cvtable$r2008[14] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#FINALIZE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

cvtable$mean<- rowMeans(cvtable[,2:10])


write.csv(cvtable,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.5.Results/mod3cv/all0008.csv") 




