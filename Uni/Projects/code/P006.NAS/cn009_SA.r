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

ICAM_restable <- data.frame(lag=character(9),beta=numeric(9),se=numeric(9),pc=numeric(9),L_CI=numeric(9),H_CI=numeric(9),sig=numeric(9),ciw=numeric(9))

ICAM_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lag2month", "lag3month", "lagyear")


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


#mlag2month
mlagmonth_irq<- IQR(mb1$lag2month)
mlagmonth<-glmmPQL(logicam ~ lag2month +PREDICTED +age +cwtemplag2month+ bmi+as.factor(smk2)+cwhumlag2month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
ICAM_restable$beta[7] <- mlagmonth$coef$fixed[2]  #extract Betas
ICAM_restable$se[7] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
ICAM_restable$sig[7] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[7] <- (exp(ICAM_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[7] <- (exp((ICAM_restable$beta[7]-1.96*ICAM_restable$se[7])*mlagmonth_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[7] <- (exp((ICAM_restable$beta[7]+1.96*ICAM_restable$se[7])*mlagmonth_irq)-1)*100
ICAM_restable$ciw[7] <-ICAM_restable$L_CI[7]+ICAM_restable$H_CI[7]



#mlagmonth
mlagmonth_irq<- IQR(mb1$lag3month)
mlagmonth<-glmmPQL(logicam ~ lag3month +PREDICTED +age +cwtemplag3month+ bmi+as.factor(smk2)+cwhumlag3month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
ICAM_restable$beta[8] <- mlagmonth$coef$fixed[2]  #extract Betas
ICAM_restable$se[8] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
ICAM_restable$sig[8] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[8] <- (exp(ICAM_restable$beta[1]*mlagmonth_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[8] <- (exp((ICAM_restable$beta[8]-1.96*ICAM_restable$se[8])*mlagmonth_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[8] <- (exp((ICAM_restable$beta[8]+1.96*ICAM_restable$se[8])*mlagmonth_irq)-1)*100
ICAM_restable$ciw[8] <-ICAM_restable$L_CI[8]+ICAM_restable$H_CI[8]


#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logicam ~ lagyear +PREDICTED +age +cwtemplagyear+ bmi+as.factor(smk2)+cwhumlagyear +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
ICAM_restable$beta[9] <- mlagyear$coef$fixed[2]  #extract Betas
ICAM_restable$se[9] <-(summary(mlagyear)$tTable[2,2]) #extract SE
ICAM_restable$sig[9] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[9] <- (exp(ICAM_restable$beta[1]*mlagyear_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[9] <- (exp((ICAM_restable$beta[9]-1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[9] <- (exp((ICAM_restable$beta[9]+1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
ICAM_restable$ciw[9] <-ICAM_restable$L_CI[9]+ICAM_restable$H_CI[9]
##################################################################################################################################################################################################################################################
