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

ICAM_restablecw <- data.frame(cwlag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

ICAM_restablecw$cwlag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$icam)
# tst<- lm(icam ~ cwlag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$cwlag24h)
mlag001<-glmmPQL(logicam ~ cwlag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
ICAM_restablecw$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[1] <- (exp(ICAM_restablecw$beta[1]*mlag001_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[1] <- (exp((ICAM_restablecw$beta[1]-1.96*ICAM_restablecw$se[1])*mlag001_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[1] <- (exp((ICAM_restablecw$beta[1]+1.96*ICAM_restablecw$se[1])*mlag001_irq)-1)*100
ICAM_restablecw$ciw[1] <-ICAM_restablecw$L_CI[1]+ICAM_restablecw$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$cwlag3day)
mlag003<-glmmPQL(logicam ~ cwlag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
ICAM_restablecw$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[2] <- (exp(ICAM_restablecw$beta[1]*mlag003_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[2] <- (exp((ICAM_restablecw$beta[2]-1.96*ICAM_restablecw$se[2])*mlag003_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[2] <- (exp((ICAM_restablecw$beta[2]+1.96*ICAM_restablecw$se[2])*mlag003_irq)-1)*100
ICAM_restablecw$ciw[2] <-ICAM_restablecw$L_CI[2]+ICAM_restablecw$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$cwlagweek)
mlagweek<-glmmPQL(logicam ~ cwlagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
ICAM_restablecw$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[3] <- (exp(ICAM_restablecw$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[3] <- (exp((ICAM_restablecw$beta[3]-1.96*ICAM_restablecw$se[3])*mlagweek_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[3] <- (exp((ICAM_restablecw$beta[3]+1.96*ICAM_restablecw$se[3])*mlagweek_irq)-1)*100
ICAM_restablecw$ciw[3] <-ICAM_restablecw$L_CI[3]+ICAM_restablecw$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$cwlag2week)
mlag2week<-glmmPQL(logicam ~ cwlag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
ICAM_restablecw$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[4] <- (exp(ICAM_restablecw$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[4] <- (exp((ICAM_restablecw$beta[4]-1.96*ICAM_restablecw$se[4])*mlag2week_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[4] <- (exp((ICAM_restablecw$beta[4]+1.96*ICAM_restablecw$se[4])*mlag2week_irq)-1)*100
ICAM_restablecw$ciw[4] <-ICAM_restablecw$L_CI[4]+ICAM_restablecw$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$cwlag3week)
mlag3week<-glmmPQL(logicam ~ cwlag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
ICAM_restablecw$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[5] <- (exp(ICAM_restablecw$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[5] <- (exp((ICAM_restablecw$beta[5]-1.96*ICAM_restablecw$se[5])*mlag3week_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[5] <- (exp((ICAM_restablecw$beta[5]+1.96*ICAM_restablecw$se[5])*mlag3week_irq)-1)*100
ICAM_restablecw$ciw[5] <-ICAM_restablecw$L_CI[5]+ICAM_restablecw$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$cwlagmonth)
mlagmonth<-glmmPQL(logicam ~ cwlagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
ICAM_restablecw$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[6] <- (exp(ICAM_restablecw$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[6] <- (exp((ICAM_restablecw$beta[6]-1.96*ICAM_restablecw$se[6])*mlagmonth_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[6] <- (exp((ICAM_restablecw$beta[6]+1.96*ICAM_restablecw$se[6])*mlagmonth_irq)-1)*100
ICAM_restablecw$ciw[6] <-ICAM_restablecw$L_CI[6]+ICAM_restablecw$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$cwlagyear)
mlagyear<-glmmPQL(logicam ~ cwlagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
ICAM_restablecw$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
ICAM_restablecw$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
ICAM_restablecw$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restablecw$pc[7] <- (exp(ICAM_restablecw$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
ICAM_restablecw$L_CI[7] <- (exp((ICAM_restablecw$beta[7]-1.96*ICAM_restablecw$se[7])*mlagyear_irq)-1)*100
# High CI bound
ICAM_restablecw$H_CI[7] <- (exp((ICAM_restablecw$beta[7]+1.96*ICAM_restablecw$se[7])*mlagyear_irq)-1)*100
ICAM_restablecw$ciw[7] <-ICAM_restablecw$L_CI[7]+ICAM_restablecw$H_CI[7]
##################################################################################################################################################################################################################################################








#########################################################################################################################
##############################################################################a###########################################
# VCAM


#create results table

VCAM_restablecw <- data.frame(cwlag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

VCAM_restablecw$cwlag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$VCAM)
# tst<- lm(VCAM ~ cwlag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$cwlag24h)
mlag001<-glmmPQL(logvcam ~ cwlag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
VCAM_restablecw$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[1] <- (exp(VCAM_restablecw$beta[1]*mlag001_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[1] <- (exp((VCAM_restablecw$beta[1]-1.96*VCAM_restablecw$se[1])*mlag001_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[1] <- (exp((VCAM_restablecw$beta[1]+1.96*VCAM_restablecw$se[1])*mlag001_irq)-1)*100
VCAM_restablecw$ciw[1] <-VCAM_restablecw$L_CI[1]+VCAM_restablecw$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$cwlag3day)
mlag003<-glmmPQL(logvcam ~ cwlag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
VCAM_restablecw$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[2] <- (exp(VCAM_restablecw$beta[1]*mlag003_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[2] <- (exp((VCAM_restablecw$beta[2]-1.96*VCAM_restablecw$se[2])*mlag003_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[2] <- (exp((VCAM_restablecw$beta[2]+1.96*VCAM_restablecw$se[2])*mlag003_irq)-1)*100
VCAM_restablecw$ciw[2] <-VCAM_restablecw$L_CI[2]+VCAM_restablecw$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$cwlagweek)
mlagweek<-glmmPQL(logvcam ~ cwlagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
VCAM_restablecw$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[3] <- (exp(VCAM_restablecw$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[3] <- (exp((VCAM_restablecw$beta[3]-1.96*VCAM_restablecw$se[3])*mlagweek_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[3] <- (exp((VCAM_restablecw$beta[3]+1.96*VCAM_restablecw$se[3])*mlagweek_irq)-1)*100
VCAM_restablecw$ciw[3] <-VCAM_restablecw$L_CI[3]+VCAM_restablecw$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$cwlag2week)
mlag2week<-glmmPQL(logvcam ~ cwlag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
VCAM_restablecw$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[4] <- (exp(VCAM_restablecw$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[4] <- (exp((VCAM_restablecw$beta[4]-1.96*VCAM_restablecw$se[4])*mlag2week_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[4] <- (exp((VCAM_restablecw$beta[4]+1.96*VCAM_restablecw$se[4])*mlag2week_irq)-1)*100
VCAM_restablecw$ciw[4] <-VCAM_restablecw$L_CI[4]+VCAM_restablecw$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$cwlag3week)
mlag3week<-glmmPQL(logvcam ~ cwlag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
VCAM_restablecw$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[5] <- (exp(VCAM_restablecw$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[5] <- (exp((VCAM_restablecw$beta[5]-1.96*VCAM_restablecw$se[5])*mlag3week_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[5] <- (exp((VCAM_restablecw$beta[5]+1.96*VCAM_restablecw$se[5])*mlag3week_irq)-1)*100
VCAM_restablecw$ciw[5] <-VCAM_restablecw$L_CI[5]+VCAM_restablecw$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$cwlagmonth)
mlagmonth<-glmmPQL(logvcam ~ cwlagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
VCAM_restablecw$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[6] <- (exp(VCAM_restablecw$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[6] <- (exp((VCAM_restablecw$beta[6]-1.96*VCAM_restablecw$se[6])*mlagmonth_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[6] <- (exp((VCAM_restablecw$beta[6]+1.96*VCAM_restablecw$se[6])*mlagmonth_irq)-1)*100
VCAM_restablecw$ciw[6] <-VCAM_restablecw$L_CI[6]+VCAM_restablecw$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$cwlagyear)
mlagyear<-glmmPQL(logvcam ~ cwlagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
VCAM_restablecw$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
VCAM_restablecw$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
VCAM_restablecw$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restablecw$pc[7] <- (exp(VCAM_restablecw$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
VCAM_restablecw$L_CI[7] <- (exp((VCAM_restablecw$beta[7]-1.96*VCAM_restablecw$se[7])*mlagyear_irq)-1)*100
# High CI bound
VCAM_restablecw$H_CI[7] <- (exp((VCAM_restablecw$beta[7]+1.96*VCAM_restablecw$se[7])*mlagyear_irq)-1)*100
VCAM_restablecw$ciw[7] <-VCAM_restablecw$L_CI[7]+VCAM_restablecw$H_CI[7]
##################################################################################################################################################################################################################################################











#########################################################################################################################
##############################################################################a###########################################
# CRP


#create results table

CRP_restablecw <- data.frame(cwlag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

CRP_restablecw$cwlag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$CRP)
# tst<- lm(CRP ~ cwlag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$cwlag24h)
mlag001<-glmmPQL(logcrp ~ cwlag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
CRP_restablecw$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
CRP_restablecw$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
CRP_restablecw$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[1] <- (exp(CRP_restablecw$beta[1]*mlag001_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[1] <- (exp((CRP_restablecw$beta[1]-1.96*CRP_restablecw$se[1])*mlag001_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[1] <- (exp((CRP_restablecw$beta[1]+1.96*CRP_restablecw$se[1])*mlag001_irq)-1)*100
CRP_restablecw$ciw[1] <-CRP_restablecw$L_CI[1]+CRP_restablecw$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$cwlag3day)
mlag003<-glmmPQL(logcrp ~ cwlag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
CRP_restablecw$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
CRP_restablecw$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
CRP_restablecw$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[2] <- (exp(CRP_restablecw$beta[1]*mlag003_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[2] <- (exp((CRP_restablecw$beta[2]-1.96*CRP_restablecw$se[2])*mlag003_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[2] <- (exp((CRP_restablecw$beta[2]+1.96*CRP_restablecw$se[2])*mlag003_irq)-1)*100
CRP_restablecw$ciw[2] <-CRP_restablecw$L_CI[2]+CRP_restablecw$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$cwlagweek)
mlagweek<-glmmPQL(logcrp ~ cwlagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
CRP_restablecw$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
CRP_restablecw$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
CRP_restablecw$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[3] <- (exp(CRP_restablecw$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[3] <- (exp((CRP_restablecw$beta[3]-1.96*CRP_restablecw$se[3])*mlagweek_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[3] <- (exp((CRP_restablecw$beta[3]+1.96*CRP_restablecw$se[3])*mlagweek_irq)-1)*100
CRP_restablecw$ciw[3] <-CRP_restablecw$L_CI[3]+CRP_restablecw$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$cwlag2week)
mlag2week<-glmmPQL(logcrp ~ cwlag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
CRP_restablecw$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
CRP_restablecw$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
CRP_restablecw$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[4] <- (exp(CRP_restablecw$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[4] <- (exp((CRP_restablecw$beta[4]-1.96*CRP_restablecw$se[4])*mlag2week_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[4] <- (exp((CRP_restablecw$beta[4]+1.96*CRP_restablecw$se[4])*mlag2week_irq)-1)*100
CRP_restablecw$ciw[4] <-CRP_restablecw$L_CI[4]+CRP_restablecw$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$cwlag3week)
mlag3week<-glmmPQL(logcrp ~ cwlag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
CRP_restablecw$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
CRP_restablecw$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
CRP_restablecw$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[5] <- (exp(CRP_restablecw$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[5] <- (exp((CRP_restablecw$beta[5]-1.96*CRP_restablecw$se[5])*mlag3week_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[5] <- (exp((CRP_restablecw$beta[5]+1.96*CRP_restablecw$se[5])*mlag3week_irq)-1)*100
CRP_restablecw$ciw[5] <-CRP_restablecw$L_CI[5]+CRP_restablecw$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$cwlagmonth)
mlagmonth<-glmmPQL(logcrp ~ cwlagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
CRP_restablecw$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
CRP_restablecw$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
CRP_restablecw$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[6] <- (exp(CRP_restablecw$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[6] <- (exp((CRP_restablecw$beta[6]-1.96*CRP_restablecw$se[6])*mlagmonth_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[6] <- (exp((CRP_restablecw$beta[6]+1.96*CRP_restablecw$se[6])*mlagmonth_irq)-1)*100
CRP_restablecw$ciw[6] <-CRP_restablecw$L_CI[6]+CRP_restablecw$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$cwlagyear)
mlagyear<-glmmPQL(logcrp ~ cwlagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
CRP_restablecw$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
CRP_restablecw$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
CRP_restablecw$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
CRP_restablecw$pc[7] <- (exp(CRP_restablecw$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
CRP_restablecw$L_CI[7] <- (exp((CRP_restablecw$beta[7]-1.96*CRP_restablecw$se[7])*mlagyear_irq)-1)*100
# High CI bound
CRP_restablecw$H_CI[7] <- (exp((CRP_restablecw$beta[7]+1.96*CRP_restablecw$se[7])*mlagyear_irq)-1)*100
CRP_restablecw$ciw[7] <-CRP_restablecw$L_CI[7]+CRP_restablecw$H_CI[7]
##################################################################################################################################################################################################################################################













#########################################################################################################################
##############################################################################a###########################################
# FIB


#create results table

FIB_restablecw <- data.frame(cwlag=character(7),beta=numeric(7),se=numeric(7),pc=numeric(7),L_CI=numeric(7),H_CI=numeric(7),sig=numeric(7),ciw=numeric(7))

FIB_restablecw$cwlag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lagyear")


#test normality
# hist(mb1$FIB)
# tst<- lm(FIB ~ cwlag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$cwlag24h)
mlag001<-glmmPQL(logfib ~ cwlag24h +PREDICTED +age + cwtemplag24h + bmi+as.factor(smk2)+cwhumlag24h +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag001)$tTable
FIB_restablecw$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
FIB_restablecw$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
FIB_restablecw$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[1] <- (exp(FIB_restablecw$beta[1]*mlag001_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[1] <- (exp((FIB_restablecw$beta[1]-1.96*FIB_restablecw$se[1])*mlag001_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[1] <- (exp((FIB_restablecw$beta[1]+1.96*FIB_restablecw$se[1])*mlag001_irq)-1)*100
FIB_restablecw$ciw[1] <-FIB_restablecw$L_CI[1]+FIB_restablecw$H_CI[1]




#mlag003
mlag003_irq<- IQR(mb1$cwlag3day)
mlag003<-glmmPQL(logfib ~ cwlag3day +PREDICTED +age +cwtemplag3day+ bmi+as.factor(smk2)+cwhumlag3day +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
FIB_restablecw$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
FIB_restablecw$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
FIB_restablecw$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[2] <- (exp(FIB_restablecw$beta[1]*mlag003_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[2] <- (exp((FIB_restablecw$beta[2]-1.96*FIB_restablecw$se[2])*mlag003_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[2] <- (exp((FIB_restablecw$beta[2]+1.96*FIB_restablecw$se[2])*mlag003_irq)-1)*100
FIB_restablecw$ciw[2] <-FIB_restablecw$L_CI[2]+FIB_restablecw$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$cwlagweek)
mlagweek<-glmmPQL(logfib ~ cwlagweek +PREDICTED +age +cwtemplagweek+ bmi+as.factor(smk2)+cwhumlagweek +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagweek)$tTable
FIB_restablecw$beta[3] <- mlagweek$coef$fixed[2]  #extract Betas
FIB_restablecw$se[3] <-(summary(mlagweek)$tTable[2,2]) #extract SE
FIB_restablecw$sig[3] <-(summary(mlagweek)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[3] <- (exp(FIB_restablecw$beta[3]*mlagweek_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[3] <- (exp((FIB_restablecw$beta[3]-1.96*FIB_restablecw$se[3])*mlagweek_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[3] <- (exp((FIB_restablecw$beta[3]+1.96*FIB_restablecw$se[3])*mlagweek_irq)-1)*100
FIB_restablecw$ciw[3] <-FIB_restablecw$L_CI[3]+FIB_restablecw$H_CI[3]



#mlag2week
mlag2week_irq<- IQR(mb1$cwlag2week)
mlag2week<-glmmPQL(logfib ~ cwlag2week +PREDICTED +age +cwtemplag2week+ bmi+as.factor(smk2)+cwhumlag2week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2week)$tTable
FIB_restablecw$beta[4] <- mlag2week$coef$fixed[2]  #extract Betas
FIB_restablecw$se[4] <-(summary(mlag2week)$tTable[2,2]) #extract SE
FIB_restablecw$sig[4] <-(summary(mlag2week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[4] <- (exp(FIB_restablecw$beta[4]*mlag2week_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[4] <- (exp((FIB_restablecw$beta[4]-1.96*FIB_restablecw$se[4])*mlag2week_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[4] <- (exp((FIB_restablecw$beta[4]+1.96*FIB_restablecw$se[4])*mlag2week_irq)-1)*100
FIB_restablecw$ciw[4] <-FIB_restablecw$L_CI[4]+FIB_restablecw$H_CI[4]


#mlag3week
mlag3week_irq<- IQR(mb1$cwlag3week)
mlag3week<-glmmPQL(logfib ~ cwlag3week +PREDICTED +age +cwtemplag3week+ bmi+as.factor(smk2)+cwhumlag3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
FIB_restablecw$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
FIB_restablecw$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
FIB_restablecw$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[5] <- (exp(FIB_restablecw$beta[1]*mlag3week_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[5] <- (exp((FIB_restablecw$beta[5]-1.96*FIB_restablecw$se[5])*mlag3week_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[5] <- (exp((FIB_restablecw$beta[5]+1.96*FIB_restablecw$se[5])*mlag3week_irq)-1)*100
FIB_restablecw$ciw[5] <-FIB_restablecw$L_CI[5]+FIB_restablecw$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$cwlagmonth)
mlagmonth<-glmmPQL(logfib ~ cwlagmonth +PREDICTED +age +cwtemplagmonth+ bmi+as.factor(smk2)+cwhumlagmonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
FIB_restablecw$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
FIB_restablecw$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
FIB_restablecw$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[6] <- (exp(FIB_restablecw$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[6] <- (exp((FIB_restablecw$beta[6]-1.96*FIB_restablecw$se[6])*mlagmonth_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[6] <- (exp((FIB_restablecw$beta[6]+1.96*FIB_restablecw$se[6])*mlagmonth_irq)-1)*100
FIB_restablecw$ciw[6] <-FIB_restablecw$L_CI[6]+FIB_restablecw$H_CI[6]


#mlagyear
mlagyear_irq<- IQR(mb1$cwlagyear)
mlagyear<-glmmPQL(logfib ~ cwlagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
FIB_restablecw$beta[7] <- mlagyear$coef$fixed[2]  #extract Betas
FIB_restablecw$se[7] <-(summary(mlagyear)$tTable[2,2]) #extract SE
FIB_restablecw$sig[7] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
FIB_restablecw$pc[7] <- (exp(FIB_restablecw$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
FIB_restablecw$L_CI[7] <- (exp((FIB_restablecw$beta[7]-1.96*FIB_restablecw$se[7])*mlagyear_irq)-1)*100
# High CI bound
FIB_restablecw$H_CI[7] <- (exp((FIB_restablecw$beta[7]+1.96*FIB_restablecw$se[7])*mlagyear_irq)-1)*100
FIB_restablecw$ciw[7] <-FIB_restablecw$L_CI[7]+FIB_restablecw$H_CI[7]
##################################################################################################################################################################################################################################################









