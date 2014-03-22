library(nlme)
library(foreign) 
library(psych)
library(mgcv)
library(reshape)

#create CV table

cvtable <- data.frame(type=character(17), r2000=numeric(17),r2001=numeric(17),r2002=numeric(17),r2003=numeric(17),r2004=numeric(17),r2005=numeric(17),r2006=numeric(17),r2007=numeric(17),r2008=numeric(17),r2009=numeric(17),r2010=numeric(17),r2011=numeric(17),mean=numeric(17))

cvtable$type <- c("it_1", "it_2","it_3","it_4","it_5","it_6","it_7","it_8","it_9","it_10","R2_preloc","R2_spat_pre","R2_tem_pre","","R2_FIN","R2_spatial","R2_temporal")





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2000
# IMPORTS

F_T2000_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2000.dbf") 
names(F_T2000_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2000[11] <- cor(F_T2000_All$PM25, F_T2000_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2000_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2000_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] #sort by SiteCode
t2000m <- merge(F_T2000_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2000m)

t2000m$delpm <- t2000m$PM25-t2000m$barpm
t2000m$delpm_mod3 <-t2000m$pm_mod3-t2000m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2000[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2000m)
summary(mod_temporal)
cvtable$r2000[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2000_All)



# sorts
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2000_merged <- merge(F_T2000_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2000_merged = T2000_merged[complete.cases(T2000_merged$pm_mod3),]
names(T2000_merged)


#create residual mp3 variable
T2000_merged$resm3<-T2000_merged$PM25-T2000_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2000_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2000_merged$pm_mod3locm <-pm_mod3_locm
T2000_merged$OApm_mod3 <- T2000_merged$pm_mod3+T2000_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2000[15] <- cor(T2000_merged$PM25, T2000_merged$OApm_mod3,use = "complete")*cor(T2000_merged$PM25, T2000_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2000_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2000_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2000_merged <- T2000_merged[order(T2000_merged$SiteCode),] #sort by SiteCode
t2000m <- merge(T2000_merged,aggf,by="SiteCode") #merge by SiteCode


t2000m$delpm <- t2000m$PM25-t2000m$barpm
t2000m$delpm_mod3 <-t2000m$OApm_mod3-t2000m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2000[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2000m)
summary(mod_temporal)
cvtable$r2000[17] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2001
# IMPORTS

F_T2001_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2001.dbf") 
names(F_T2001_All)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2001[11] <- cor(F_T2001_All$PM25, F_T2001_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2001_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2001_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] #sort by SiteCode
t2001m <- merge(F_T2001_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2001m)

t2001m$delpm <- t2001m$PM25-t2001m$barpm
t2001m$delpm_mod3 <-t2001m$pm_mod3-t2001m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2001[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2001m)
summary(mod_temporal)
cvtable$r2001[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2001_All)



# sorts
F_T2001_All <- F_T2001_All[order(F_T2001_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2001_merged <- merge(F_T2001_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2001_merged = T2001_merged[complete.cases(T2001_merged$pm_mod3),]
names(T2001_merged)


#create residual mp3 variable
T2001_merged$resm3<-T2001_merged$PM25-T2001_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2001_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2001_merged$pm_mod3locm <-pm_mod3_locm
T2001_merged$OApm_mod3 <- T2001_merged$pm_mod3+T2001_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2001[15] <- cor(T2001_merged$PM25, T2001_merged$OApm_mod3,use = "complete")*cor(T2001_merged$PM25, T2001_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2001_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2001_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2001_merged <- T2001_merged[order(T2001_merged$SiteCode),] #sort by SiteCode
t2001m <- merge(T2001_merged,aggf,by="SiteCode") #merge by SiteCode


t2001m$delpm <- t2001m$PM25-t2001m$barpm
t2001m$delpm_mod3 <-t2001m$OApm_mod3-t2001m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2001[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2001m)
summary(mod_temporal)
cvtable$r2001[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2002
# IMPORTS

F_T2002_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2002.dbf") 
names(F_T2002_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2002[11] <- cor(F_T2002_All$PM25, F_T2002_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2002_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2002_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] #sort by SiteCode
t2002m <- merge(F_T2002_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2002m)

t2002m$delpm <- t2002m$PM25-t2002m$barpm
t2002m$delpm_mod3 <-t2002m$pm_mod3-t2002m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2002[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2002m)
summary(mod_temporal)
cvtable$r2002[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2002_All)



# sorts
F_T2002_All <- F_T2002_All[order(F_T2002_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2002_merged <- merge(F_T2002_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2002_merged = T2002_merged[complete.cases(T2002_merged$pm_mod3),]
names(T2002_merged)


#create residual mp3 variable
T2002_merged$resm3<-T2002_merged$PM25-T2002_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2002_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2002_merged$pm_mod3locm <-pm_mod3_locm
T2002_merged$OApm_mod3 <- T2002_merged$pm_mod3+T2002_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2002[15] <- cor(T2002_merged$PM25, T2002_merged$OApm_mod3,use = "complete")*cor(T2002_merged$PM25, T2002_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2002_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2002_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2002_merged <- T2002_merged[order(T2002_merged$SiteCode),] #sort by SiteCode
t2002m <- merge(T2002_merged,aggf,by="SiteCode") #merge by SiteCode


t2002m$delpm <- t2002m$PM25-t2002m$barpm
t2002m$delpm_mod3 <-t2002m$OApm_mod3-t2002m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2002[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2002m)
summary(mod_temporal)
cvtable$r2002[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2003
# IMPORTS

F_T2003_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2003.dbf") 
names(F_T2003_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2003[11] <- cor(F_T2003_All$PM25, F_T2003_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2003_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2003_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] #sort by SiteCode
t2003m <- merge(F_T2003_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2003m)

t2003m$delpm <- t2003m$PM25-t2003m$barpm
t2003m$delpm_mod3 <-t2003m$pm_mod3-t2003m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2003[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2003m)
summary(mod_temporal)
cvtable$r2003[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2003_All)



# sorts
F_T2003_All <- F_T2003_All[order(F_T2003_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2003_merged <- merge(F_T2003_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2003_merged = T2003_merged[complete.cases(T2003_merged$pm_mod3),]
names(T2003_merged)


#create residual mp3 variable
T2003_merged$resm3<-T2003_merged$PM25-T2003_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2003_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2003_merged$pm_mod3locm <-pm_mod3_locm
T2003_merged$OApm_mod3 <- T2003_merged$pm_mod3+T2003_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2003[15] <- cor(T2003_merged$PM25, T2003_merged$OApm_mod3,use = "complete")*cor(T2003_merged$PM25, T2003_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2003_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2003_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2003_merged <- T2003_merged[order(T2003_merged$SiteCode),] #sort by SiteCode
t2003m <- merge(T2003_merged,aggf,by="SiteCode") #merge by SiteCode


t2003m$delpm <- t2003m$PM25-t2003m$barpm
t2003m$delpm_mod3 <-t2003m$OApm_mod3-t2003m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2003[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2003m)
summary(mod_temporal)
cvtable$r2003[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2004
# IMPORTS

F_T2004_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2004.dbf") 
names(F_T2004_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2004[11] <- cor(F_T2004_All$PM25, F_T2004_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2004_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2004_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] #sort by SiteCode
t2004m <- merge(F_T2004_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2004m)

t2004m$delpm <- t2004m$PM25-t2004m$barpm
t2004m$delpm_mod3 <-t2004m$pm_mod3-t2004m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2004[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2004m)
summary(mod_temporal)
cvtable$r2004[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2004_All)



# sorts
F_T2004_All <- F_T2004_All[order(F_T2004_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2004_merged <- merge(F_T2004_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2004_merged = T2004_merged[complete.cases(T2004_merged$pm_mod3),]
names(T2004_merged)


#create residual mp3 variable
T2004_merged$resm3<-T2004_merged$PM25-T2004_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2004_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2004_merged$pm_mod3locm <-pm_mod3_locm
T2004_merged$OApm_mod3 <- T2004_merged$pm_mod3+T2004_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2004[15] <- cor(T2004_merged$PM25, T2004_merged$OApm_mod3,use = "complete")*cor(T2004_merged$PM25, T2004_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2004_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2004_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2004_merged <- T2004_merged[order(T2004_merged$SiteCode),] #sort by SiteCode
t2004m <- merge(T2004_merged,aggf,by="SiteCode") #merge by SiteCode


t2004m$delpm <- t2004m$PM25-t2004m$barpm
t2004m$delpm_mod3 <-t2004m$OApm_mod3-t2004m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2004[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2004m)
summary(mod_temporal)
cvtable$r2004[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2005
# IMPORTS

F_T2005_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2005.dbf") 
names(F_T2005_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2005[11] <- cor(F_T2005_All$PM25, F_T2005_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2005_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2005_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] #sort by SiteCode
t2005m <- merge(F_T2005_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2005m)

t2005m$delpm <- t2005m$PM25-t2005m$barpm
t2005m$delpm_mod3 <-t2005m$pm_mod3-t2005m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2005[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2005m)
summary(mod_temporal)
cvtable$r2005[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2005_All)



# sorts
F_T2005_All <- F_T2005_All[order(F_T2005_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2005_merged <- merge(F_T2005_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2005_merged = T2005_merged[complete.cases(T2005_merged$pm_mod3),]
names(T2005_merged)


#create residual mp3 variable
T2005_merged$resm3<-T2005_merged$PM25-T2005_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2005_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2005_merged$pm_mod3locm <-pm_mod3_locm
T2005_merged$OApm_mod3 <- T2005_merged$pm_mod3+T2005_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2005[15] <- cor(T2005_merged$PM25, T2005_merged$OApm_mod3,use = "complete")*cor(T2005_merged$PM25, T2005_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2005_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2005_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2005_merged <- T2005_merged[order(T2005_merged$SiteCode),] #sort by SiteCode
t2005m <- merge(T2005_merged,aggf,by="SiteCode") #merge by SiteCode


t2005m$delpm <- t2005m$PM25-t2005m$barpm
t2005m$delpm_mod3 <-t2005m$OApm_mod3-t2005m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2005[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2005m)
summary(mod_temporal)
cvtable$r2005[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2006
# IMPORTS

F_T2006_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2006.dbf") 
names(F_T2006_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2006[11] <- cor(F_T2006_All$PM25, F_T2006_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2006_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2006_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] #sort by SiteCode
t2006m <- merge(F_T2006_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2006m)

t2006m$delpm <- t2006m$PM25-t2006m$barpm
t2006m$delpm_mod3 <-t2006m$pm_mod3-t2006m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2006[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2006m)
summary(mod_temporal)
cvtable$r2006[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2006_All)



# sorts
F_T2006_All <- F_T2006_All[order(F_T2006_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2006_merged <- merge(F_T2006_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2006_merged = T2006_merged[complete.cases(T2006_merged$pm_mod3),]
names(T2006_merged)


#create residual mp3 variable
T2006_merged$resm3<-T2006_merged$PM25-T2006_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2006_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2006_merged$pm_mod3locm <-pm_mod3_locm
T2006_merged$OApm_mod3 <- T2006_merged$pm_mod3+T2006_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2006[15] <- cor(T2006_merged$PM25, T2006_merged$OApm_mod3,use = "complete")*cor(T2006_merged$PM25, T2006_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2006_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2006_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2006_merged <- T2006_merged[order(T2006_merged$SiteCode),] #sort by SiteCode
t2006m <- merge(T2006_merged,aggf,by="SiteCode") #merge by SiteCode


t2006m$delpm <- t2006m$PM25-t2006m$barpm
t2006m$delpm_mod3 <-t2006m$OApm_mod3-t2006m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2006[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2006m)
summary(mod_temporal)
cvtable$r2006[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2007
# IMPORTS

F_T2007_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2007.dbf") 
names(F_T2007_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2007[11] <- cor(F_T2007_All$PM25, F_T2007_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2007_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2007_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] #sort by SiteCode
t2007m <- merge(F_T2007_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2007m)

t2007m$delpm <- t2007m$PM25-t2007m$barpm
t2007m$delpm_mod3 <-t2007m$pm_mod3-t2007m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2007[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2007m)
summary(mod_temporal)
cvtable$r2007[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2007_All)



# sorts
F_T2007_All <- F_T2007_All[order(F_T2007_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2007_merged <- merge(F_T2007_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2007_merged = T2007_merged[complete.cases(T2007_merged$pm_mod3),]
names(T2007_merged)


#create residual mp3 variable
T2007_merged$resm3<-T2007_merged$PM25-T2007_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2007_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2007_merged$pm_mod3locm <-pm_mod3_locm
T2007_merged$OApm_mod3 <- T2007_merged$pm_mod3+T2007_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2007[15] <- cor(T2007_merged$PM25, T2007_merged$OApm_mod3,use = "complete")*cor(T2007_merged$PM25, T2007_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2007_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2007_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2007_merged <- T2007_merged[order(T2007_merged$SiteCode),] #sort by SiteCode
t2007m <- merge(T2007_merged,aggf,by="SiteCode") #merge by SiteCode


t2007m$delpm <- t2007m$PM25-t2007m$barpm
t2007m$delpm_mod3 <-t2007m$OApm_mod3-t2007m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2007[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2007m)
summary(mod_temporal)
cvtable$r2007[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2008
# IMPORTS

F_T2008_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2008.dbf") 
names(F_T2008_All)






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2008[11] <- cor(F_T2008_All$PM25, F_T2008_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2008_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2008_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] #sort by SiteCode
t2008m <- merge(F_T2008_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2008m)

t2008m$delpm <- t2008m$PM25-t2008m$barpm
t2008m$delpm_mod3 <-t2008m$pm_mod3-t2008m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2008[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2008m)
summary(mod_temporal)
cvtable$r2008[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2008_All)



# sorts
F_T2008_All <- F_T2008_All[order(F_T2008_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2008_merged <- merge(F_T2008_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2008_merged = T2008_merged[complete.cases(T2008_merged$pm_mod3),]
names(T2008_merged)


#create residual mp3 variable
T2008_merged$resm3<-T2008_merged$PM25-T2008_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2008_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2008_merged$pm_mod3locm <-pm_mod3_locm
T2008_merged$OApm_mod3 <- T2008_merged$pm_mod3+T2008_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2008[15] <- cor(T2008_merged$PM25, T2008_merged$OApm_mod3,use = "complete")*cor(T2008_merged$PM25, T2008_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2008_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2008_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2008_merged <- T2008_merged[order(T2008_merged$SiteCode),] #sort by SiteCode
t2008m <- merge(T2008_merged,aggf,by="SiteCode") #merge by SiteCode


t2008m$delpm <- t2008m$PM25-t2008m$barpm
t2008m$delpm_mod3 <-t2008m$OApm_mod3-t2008m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2008[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2008m)
summary(mod_temporal)
cvtable$r2008[17] <-summary(mod_temporal)$r.squared





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2009
# IMPORTS

F_T2009_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2009.dbf") 
names(F_T2009_All)

library(reshape)
F_T2009_All<-rename(F_T2009_All,c(pm25="PM25")) 
F_T2009_All<-rename(F_T2009_All,c(sitecode="SiteCode"))




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2009[11] <- cor(F_T2009_All$PM25, F_T2009_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2009_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2009_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2009_All <- F_T2009_All[order(F_T2009_All$SiteCode),] #sort by SiteCode
t2009m <- merge(F_T2009_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2009m)

t2009m$delpm <- t2009m$PM25-t2009m$barpm
t2009m$delpm_mod3 <-t2009m$pm_mod3-t2009m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2009[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2009m)
summary(mod_temporal)
cvtable$r2009[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2009_All)



# sorts
F_T2009_All <- F_T2009_All[order(F_T2009_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2009_merged <- merge(F_T2009_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2009_merged = T2009_merged[complete.cases(T2009_merged$pm_mod3),]
names(T2009_merged)


#create residual mp3 variable
T2009_merged$resm3<-T2009_merged$PM25-T2009_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2009_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2009_merged$pm_mod3locm <-pm_mod3_locm
T2009_merged$OApm_mod3 <- T2009_merged$pm_mod3+T2009_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2009[15] <- cor(T2009_merged$PM25, T2009_merged$OApm_mod3,use = "complete")*cor(T2009_merged$PM25, T2009_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2009_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2009_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2009_merged <- T2009_merged[order(T2009_merged$SiteCode),] #sort by SiteCode
t2009m <- merge(T2009_merged,aggf,by="SiteCode") #merge by SiteCode


t2009m$delpm <- t2009m$PM25-t2009m$barpm
t2009m$delpm_mod3 <-t2009m$OApm_mod3-t2009m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2009[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2009m)
summary(mod_temporal)
cvtable$r2009[17] <-summary(mod_temporal)$r.squared


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2010
# IMPORTS

F_T2010_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2010.dbf") 
names(F_T2010_All)

library(reshape)
F_T2010_All<-rename(F_T2010_All,c(pm25="PM25")) 
F_T2010_All<-rename(F_T2010_All,c(sitecode="SiteCode"))




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2010[11] <- cor(F_T2010_All$PM25, F_T2010_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2010_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2010_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2010_All <- F_T2010_All[order(F_T2010_All$SiteCode),] #sort by SiteCode
t2010m <- merge(F_T2010_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2010m)

t2010m$delpm <- t2010m$PM25-t2010m$barpm
t2010m$delpm_mod3 <-t2010m$pm_mod3-t2010m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2010[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2010m)
summary(mod_temporal)
cvtable$r2010[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2010_All)



# sorts
F_T2010_All <- F_T2010_All[order(F_T2010_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2010_merged <- merge(F_T2010_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2010_merged = T2010_merged[complete.cases(T2010_merged$pm_mod3),]
names(T2010_merged)


#create residual mp3 variable
T2010_merged$resm3<-T2010_merged$PM25-T2010_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2010_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2010_merged$pm_mod3locm <-pm_mod3_locm
T2010_merged$OApm_mod3 <- T2010_merged$pm_mod3+T2010_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2010[15] <- cor(T2010_merged$PM25, T2010_merged$OApm_mod3,use = "complete")*cor(T2010_merged$PM25, T2010_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2010_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2010_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2010_merged <- T2010_merged[order(T2010_merged$SiteCode),] #sort by SiteCode
t2010m <- merge(T2010_merged,aggf,by="SiteCode") #merge by SiteCode


t2010m$delpm <- t2010m$PM25-t2010m$barpm
t2010m$delpm_mod3 <-t2010m$OApm_mod3-t2010m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2010[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2010m)
summary(mod_temporal)
cvtable$r2010[17] <-summary(mod_temporal)$r.squared

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2011
# IMPORTS

F_T2011_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN008_mod3_r2_predictions/T2011.dbf") 
names(F_T2011_All)

library(reshape)
F_T2011_All<-rename(F_T2011_All,c(pm25="PM25")) 
F_T2011_All<-rename(F_T2011_All,c(sitecode="SiteCode"))




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall R2
cvtable$r2011[11] <- cor(F_T2011_All$PM25, F_T2011_All$pm_mod3,use = "complete")^2



########################
#calculate R2 pre local pm and spatial vs temporal
########################



#create barpm and barpm_mod3
attach(F_T2011_All)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(pm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(F_T2011_All)

names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")

#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
F_T2011_All <- F_T2011_All[order(F_T2011_All$SiteCode),] #sort by SiteCode
t2011m <- merge(F_T2011_All,aggf,by="SiteCode") #merge by SiteCode

summary(t2011m)

t2011m$delpm <- t2011m$PM25-t2011m$barpm
t2011m$delpm_mod3 <-t2011m$pm_mod3-t2011m$barpm_mod3

######################################################################
#get R2 for the spatial and temporal:

######################################################################


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2011[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2011m)
summary(mod_temporal)
cvtable$r2011[13] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE
#import mod1 DB (PM-AOD)
lu <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
names(lu)

names(F_T2011_All)



# sorts
F_T2011_All <- F_T2011_All[order(F_T2011_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 

# merge two dataframes by ID
T2011_merged <- merge(F_T2011_All,lu,by="SiteCode")

#delete missing cases in pm_mod3ictions
T2011_merged = T2011_merged[complete.cases(T2011_merged$pm_mod3),]
names(T2011_merged)


#create residual mp3 variable
T2011_merged$resm3<-T2011_merged$PM25-T2011_merged$pm_mod3


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2011_merged)

summary(bp.model.ps)




pm_mod3_locm <-predict(bp.model.ps)
T2011_merged$pm_mod3locm <-pm_mod3_locm
T2011_merged$OApm_mod3 <- T2011_merged$pm_mod3+T2011_merged$pm_mod3locm



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> overall FINAL R2
cvtable$r2011[15] <- cor(T2011_merged$PM25, T2011_merged$OApm_mod3,use = "complete")*cor(T2011_merged$PM25, T2011_merged$OApm_mod3,use = "complete")



######################################################################
#spatial vs temporal
######################################################################


#create barpm and barpm_mod3
attach(T2011_merged)
agg1<- aggregate(PM25 ~ SiteCode,FUN=mean, na.rm=TRUE)
agg2<- aggregate(OApm_mod3 ~ SiteCode,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SiteCode")
detach(T2011_merged)


names(aggf) <- c("SiteCode", "barpm", "barpm_mod3")


#merge back site mean
aggf <- aggf[order(aggf$SiteCode),]  #sort by SiteCode
T2011_merged <- T2011_merged[order(T2011_merged$SiteCode),] #sort by SiteCode
t2011m <- merge(T2011_merged,aggf,by="SiteCode") #merge by SiteCode


t2011m$delpm <- t2011m$PM25-t2011m$barpm
t2011m$delpm_mod3 <-t2011m$OApm_mod3-t2011m$barpm_mod3


#spatial
mod_spatial <- lm(barpm ~ barpm_mod3, data=aggf)
summary(mod_spatial)
cvtable$r2011[16] <-summary(mod_spatial)$r.squared 

#temporal
mod_temporal <- lm(delpm ~ delpm_mod3, data=t2011m)
summary(mod_temporal)
cvtable$r2011[17] <-summary(mod_temporal)$r.squared




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

cvtable$mean<- rowMeans(cvtable[,2:10])


write.csv(cvtable,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.5.Results/mod3/Final_mod3_table.csv") 
  









