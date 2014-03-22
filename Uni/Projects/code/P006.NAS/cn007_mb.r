library(foreign)
library(stats) 
library(mgcv)
library(splines)
library(MASS)
library(nlme)

# IMPORTS

#import mod1 DB (PM-AOD)
mb1 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.4.Work/3.Analysis/MB_analysis/mb1_met.csv", header=T) 

names(mb1)
# Outcomes: Fibrinogen, CRP, ICAM-1, VCAM-1




#########################################################################################################################
##############################################################################a###########################################
# ICAM


#create results table

ICAM_restable <- data.frame(lag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

ICAM_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$icam)
# tst<- lm(icam ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logicam ~ lag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
ICAM_restable$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
ICAM_restable$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
ICAM_restable$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[1] <- (exp(ICAM_restable$beta[1]*mlag001_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[1] <- (exp((ICAM_restable$beta[1]-1.96*ICAM_restable$se[1])*mlag001_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[1] <- (exp((ICAM_restable$beta[1]+1.96*ICAM_restable$se[1])*mlag001_irq)-1)*100
ICAM_restable$ciw[1] <-ICAM_restable$L_CI[1]+ICAM_restable$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$lag3day)
mlag003<-glmmPQL(logicam ~ lag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
ICAM_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
ICAM_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
ICAM_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[2] <- (exp(ICAM_restable$beta[1]*mlag003_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[2] <- (exp((ICAM_restable$beta[2]-1.96*ICAM_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[2] <- (exp((ICAM_restable$beta[2]+1.96*ICAM_restable$se[2])*mlag003_irq)-1)*100
ICAM_restable$ciw[2] <-ICAM_restable$L_CI[2]+ICAM_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logicam ~ lagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
ICAM_restable$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
ICAM_restable$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
ICAM_restable$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[3] <- (exp(ICAM_restable$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[3] <- (exp((ICAM_restable$beta[3]-1.96*ICAM_restable$se[3])*mlagweek_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[3] <- (exp((ICAM_restable$beta[3]+1.96*ICAM_restable$se[3])*mlagweek_irq)-1)*100
ICAM_restable$ciw[3] <-ICAM_restable$L_CI[3]+ICAM_restable$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$lag2week)
mlag2week<-glmmPQL(logicam ~ lag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
ICAM_restable$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
ICAM_restable$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
ICAM_restable$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[4] <- (exp(ICAM_restable$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[4] <- (exp((ICAM_restable$beta[4]-1.96*ICAM_restable$se[4])*mlag2week_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[4] <- (exp((ICAM_restable$beta[4]+1.96*ICAM_restable$se[4])*mlag2week_irq)-1)*100
ICAM_restable$ciw[4] <-ICAM_restable$L_CI[4]+ICAM_restable$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$lag3week)
mlag3week<-glmmPQL(logicam ~ lag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
ICAM_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
ICAM_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
ICAM_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[5] <- (exp(ICAM_restable$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[5] <- (exp((ICAM_restable$beta[5]-1.96*ICAM_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[5] <- (exp((ICAM_restable$beta[5]+1.96*ICAM_restable$se[5])*mlag3week_irq)-1)*100
ICAM_restable$ciw[5] <-ICAM_restable$L_CI[5]+ICAM_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logicam ~ lagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
ICAM_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
ICAM_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
ICAM_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[6] <- (exp(ICAM_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[6] <- (exp((ICAM_restable$beta[6]-1.96*ICAM_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[6] <- (exp((ICAM_restable$beta[6]+1.96*ICAM_restable$se[6])*mlagmonth_irq)-1)*100
ICAM_restable$ciw[6] <-ICAM_restable$L_CI[6]+ICAM_restable$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logicam ~ lagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
ICAM_restable$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
ICAM_restable$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
ICAM_restable$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[7] <- (exp(ICAM_restable$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[7] <- (exp((ICAM_restable$beta[7]-1.96*ICAM_restable$se[7])*mlagyear_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[7] <- (exp((ICAM_restable$beta[7]+1.96*ICAM_restable$se[7])*mlagyear_irq)-1)*100
ICAM_restable$ciw[7] <-ICAM_restable$L_CI[7]+ICAM_restable$H_CI[7]
##################################################################################################################################################################################################################################################








#########################################################################################################################
##############################################################################a###########################################
# VCAM


#create results table

VCAM_restable <- data.frame(lag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

VCAM_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$VCAM)
# tst<- lm(VCAM ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logvcam ~ lag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
VCAM_restable$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
VCAM_restable$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
VCAM_restable$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[1] <- (exp(VCAM_restable$beta[1]*mlag001_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[1] <- (exp((VCAM_restable$beta[1]-1.96*VCAM_restable$se[1])*mlag001_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[1] <- (exp((VCAM_restable$beta[1]+1.96*VCAM_restable$se[1])*mlag001_irq)-1)*100
VCAM_restable$ciw[1] <-VCAM_restable$L_CI[1]+VCAM_restable$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$lag3day)
mlag003<-glmmPQL(logvcam ~ lag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
VCAM_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
VCAM_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
VCAM_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[2] <- (exp(VCAM_restable$beta[1]*mlag003_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[2] <- (exp((VCAM_restable$beta[2]-1.96*VCAM_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[2] <- (exp((VCAM_restable$beta[2]+1.96*VCAM_restable$se[2])*mlag003_irq)-1)*100
VCAM_restable$ciw[2] <-VCAM_restable$L_CI[2]+VCAM_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logvcam ~ lagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
VCAM_restable$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
VCAM_restable$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
VCAM_restable$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[3] <- (exp(VCAM_restable$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[3] <- (exp((VCAM_restable$beta[3]-1.96*VCAM_restable$se[3])*mlagweek_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[3] <- (exp((VCAM_restable$beta[3]+1.96*VCAM_restable$se[3])*mlagweek_irq)-1)*100
VCAM_restable$ciw[3] <-VCAM_restable$L_CI[3]+VCAM_restable$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$lag2week)
mlag2week<-glmmPQL(logvcam ~ lag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
VCAM_restable$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
VCAM_restable$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
VCAM_restable$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[4] <- (exp(VCAM_restable$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[4] <- (exp((VCAM_restable$beta[4]-1.96*VCAM_restable$se[4])*mlag2week_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[4] <- (exp((VCAM_restable$beta[4]+1.96*VCAM_restable$se[4])*mlag2week_irq)-1)*100
VCAM_restable$ciw[4] <-VCAM_restable$L_CI[4]+VCAM_restable$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$lag3week)
mlag3week<-glmmPQL(logvcam ~ lag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
VCAM_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
VCAM_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
VCAM_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[5] <- (exp(VCAM_restable$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[5] <- (exp((VCAM_restable$beta[5]-1.96*VCAM_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[5] <- (exp((VCAM_restable$beta[5]+1.96*VCAM_restable$se[5])*mlag3week_irq)-1)*100
VCAM_restable$ciw[5] <-VCAM_restable$L_CI[5]+VCAM_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logvcam ~ lagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
VCAM_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
VCAM_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
VCAM_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[6] <- (exp(VCAM_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[6] <- (exp((VCAM_restable$beta[6]-1.96*VCAM_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[6] <- (exp((VCAM_restable$beta[6]+1.96*VCAM_restable$se[6])*mlagmonth_irq)-1)*100
VCAM_restable$ciw[6] <-VCAM_restable$L_CI[6]+VCAM_restable$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logvcam ~ lagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
VCAM_restable$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
VCAM_restable$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
VCAM_restable$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[7] <- (exp(VCAM_restable$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[7] <- (exp((VCAM_restable$beta[7]-1.96*VCAM_restable$se[7])*mlagyear_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[7] <- (exp((VCAM_restable$beta[7]+1.96*VCAM_restable$se[7])*mlagyear_irq)-1)*100
VCAM_restable$ciw[7] <-VCAM_restable$L_CI[7]+VCAM_restable$H_CI[7]
##################################################################################################################################################################################################################################################











#########################################################################################################################
##############################################################################a###########################################
# CRP


#create results table

CRP_restable <- data.frame(lag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

CRP_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$CRP)
# tst<- lm(CRP ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logcrp ~ lag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
CRP_restable$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
CRP_restable$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
CRP_restable$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[1] <- (exp(CRP_restable$beta[1]*mlag001_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[1] <- (exp((CRP_restable$beta[1]-1.96*CRP_restable$se[1])*mlag001_irq)-1)*100
# High CI bound
CRP_restable$H_CI[1] <- (exp((CRP_restable$beta[1]+1.96*CRP_restable$se[1])*mlag001_irq)-1)*100
CRP_restable$ciw[1] <-CRP_restable$L_CI[1]+CRP_restable$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$lag3day)
mlag003<-glmmPQL(logcrp ~ lag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
CRP_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
CRP_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
CRP_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[2] <- (exp(CRP_restable$beta[1]*mlag003_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[2] <- (exp((CRP_restable$beta[2]-1.96*CRP_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
CRP_restable$H_CI[2] <- (exp((CRP_restable$beta[2]+1.96*CRP_restable$se[2])*mlag003_irq)-1)*100
CRP_restable$ciw[2] <-CRP_restable$L_CI[2]+CRP_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logcrp ~ lagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
CRP_restable$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
CRP_restable$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
CRP_restable$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[3] <- (exp(CRP_restable$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[3] <- (exp((CRP_restable$beta[3]-1.96*CRP_restable$se[3])*mlagweek_irq)-1)*100
# High CI bound
CRP_restable$H_CI[3] <- (exp((CRP_restable$beta[3]+1.96*CRP_restable$se[3])*mlagweek_irq)-1)*100
CRP_restable$ciw[3] <-CRP_restable$L_CI[3]+CRP_restable$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$lag2week)
mlag2week<-glmmPQL(logcrp ~ lag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
CRP_restable$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
CRP_restable$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
CRP_restable$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[4] <- (exp(CRP_restable$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[4] <- (exp((CRP_restable$beta[4]-1.96*CRP_restable$se[4])*mlag2week_irq)-1)*100
# High CI bound
CRP_restable$H_CI[4] <- (exp((CRP_restable$beta[4]+1.96*CRP_restable$se[4])*mlag2week_irq)-1)*100
CRP_restable$ciw[4] <-CRP_restable$L_CI[4]+CRP_restable$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$lag3week)
mlag3week<-glmmPQL(logcrp ~ lag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
CRP_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
CRP_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
CRP_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[5] <- (exp(CRP_restable$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[5] <- (exp((CRP_restable$beta[5]-1.96*CRP_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
CRP_restable$H_CI[5] <- (exp((CRP_restable$beta[5]+1.96*CRP_restable$se[5])*mlag3week_irq)-1)*100
CRP_restable$ciw[5] <-CRP_restable$L_CI[5]+CRP_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logcrp ~ lagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
CRP_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
CRP_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
CRP_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[6] <- (exp(CRP_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[6] <- (exp((CRP_restable$beta[6]-1.96*CRP_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
CRP_restable$H_CI[6] <- (exp((CRP_restable$beta[6]+1.96*CRP_restable$se[6])*mlagmonth_irq)-1)*100
CRP_restable$ciw[6] <-CRP_restable$L_CI[6]+CRP_restable$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logcrp ~ lagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
CRP_restable$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
CRP_restable$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
CRP_restable$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restable$pc[7] <- (exp(CRP_restable$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
CRP_restable$L_CI[7] <- (exp((CRP_restable$beta[7]-1.96*CRP_restable$se[7])*mlagyear_irq)-1)*100
# High CI bound
CRP_restable$H_CI[7] <- (exp((CRP_restable$beta[7]+1.96*CRP_restable$se[7])*mlagyear_irq)-1)*100
CRP_restable$ciw[7] <-CRP_restable$L_CI[7]+CRP_restable$H_CI[7]
##################################################################################################################################################################################################################################################













#########################################################################################################################
##############################################################################a###########################################
# FIB


#create results table

FIB_restable <- data.frame(lag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

FIB_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$FIB)
# tst<- lm(FIB ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logfib ~ lag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
FIB_restable$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
FIB_restable$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
FIB_restable$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[1] <- (exp(FIB_restable$beta[1]*mlag001_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[1] <- (exp((FIB_restable$beta[1]-1.96*FIB_restable$se[1])*mlag001_irq)-1)*100
# High CI bound
FIB_restable$H_CI[1] <- (exp((FIB_restable$beta[1]+1.96*FIB_restable$se[1])*mlag001_irq)-1)*100
FIB_restable$ciw[1] <-FIB_restable$L_CI[1]+FIB_restable$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$lag3day)
mlag003<-glmmPQL(logfib ~ lag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
FIB_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
FIB_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
FIB_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[2] <- (exp(FIB_restable$beta[1]*mlag003_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[2] <- (exp((FIB_restable$beta[2]-1.96*FIB_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
FIB_restable$H_CI[2] <- (exp((FIB_restable$beta[2]+1.96*FIB_restable$se[2])*mlag003_irq)-1)*100
FIB_restable$ciw[2] <-FIB_restable$L_CI[2]+FIB_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logfib ~ lagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
FIB_restable$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
FIB_restable$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
FIB_restable$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[3] <- (exp(FIB_restable$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[3] <- (exp((FIB_restable$beta[3]-1.96*FIB_restable$se[3])*mlagweek_irq)-1)*100
# High CI bound
FIB_restable$H_CI[3] <- (exp((FIB_restable$beta[3]+1.96*FIB_restable$se[3])*mlagweek_irq)-1)*100
FIB_restable$ciw[3] <-FIB_restable$L_CI[3]+FIB_restable$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$lag2week)
mlag2week<-glmmPQL(logfib ~ lag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
FIB_restable$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
FIB_restable$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
FIB_restable$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[4] <- (exp(FIB_restable$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[4] <- (exp((FIB_restable$beta[4]-1.96*FIB_restable$se[4])*mlag2week_irq)-1)*100
# High CI bound
FIB_restable$H_CI[4] <- (exp((FIB_restable$beta[4]+1.96*FIB_restable$se[4])*mlag2week_irq)-1)*100
FIB_restable$ciw[4] <-FIB_restable$L_CI[4]+FIB_restable$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$lag3week)
mlag3week<-glmmPQL(logfib ~ lag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
FIB_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
FIB_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
FIB_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[5] <- (exp(FIB_restable$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[5] <- (exp((FIB_restable$beta[5]-1.96*FIB_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
FIB_restable$H_CI[5] <- (exp((FIB_restable$beta[5]+1.96*FIB_restable$se[5])*mlag3week_irq)-1)*100
FIB_restable$ciw[5] <-FIB_restable$L_CI[5]+FIB_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logfib ~ lagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
FIB_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
FIB_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
FIB_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[6] <- (exp(FIB_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[6] <- (exp((FIB_restable$beta[6]-1.96*FIB_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
FIB_restable$H_CI[6] <- (exp((FIB_restable$beta[6]+1.96*FIB_restable$se[6])*mlagmonth_irq)-1)*100
FIB_restable$ciw[6] <-FIB_restable$L_CI[6]+FIB_restable$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logfib ~ lagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
FIB_restable$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
FIB_restable$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
FIB_restable$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restable$pc[7] <- (exp(FIB_restable$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
FIB_restable$L_CI[7] <- (exp((FIB_restable$beta[7]-1.96*FIB_restable$se[7])*mlagyear_irq)-1)*100
# High CI bound
FIB_restable$H_CI[7] <- (exp((FIB_restable$beta[7]+1.96*FIB_restable$se[7])*mlagyear_irq)-1)*100
FIB_restable$ciw[7] <-FIB_restable$L_CI[7]+FIB_restable$H_CI[7]
##################################################################################################################################################################################################################################################









