library(ggplot2)
library(foreign)
library(car)
library(stats) 
library(mgcv)
library(splines)
library(MASS)
library(nlme)

#{{{ IMPORTS and setup

#import mod1 DB (PM-AOD)
mb1 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.4.Work/3.Analysis/MB_analysis/mb_expo.csv", header=T) 

names(mb1)

# # Outcomes: Fibrinogen, CRP, ICAM-1, VCAM-1
# 
# mb2$rdate <- as.Date(mb1$date,format='%m/%d/%Y')
# 
# 
# mb3 <- mb2[order(mb2$rdate),]
# 
# 
# mb4 <-subset(mb3, as.Date(rdate) >= '2000-03-03' & as.Date(rdate) <= '2008-12-31') 
# 
hist(mb1$icam)
hist(mb1$vcam)
summary(mb1$bmi)

#rename

attach(mb1)
mb1$obes[bmi > 30] <- 1
mb1$obes[bmi <= 30] <- 0
detach(mb1) 
table(mb1$obes)

library(reshape)
mb1<-rename(mb1,c(pmnewma1="lag24h")) 
mb1<-rename(mb1,c(pmnewma3="lag3day"))
mb1<-rename(mb1,c(pmnewmaweek="lagweek"))
mb1<-rename(mb1,c(pmnewma2week="lag2week"))
mb1<-rename(mb1,c(pmnewma3week="lag3week"))
mb1<-rename(mb1,c(pmnewmamonth="lagmonth"))
mb1<-rename(mb1,c(pmnewma2month="lag2month"))
mb1<-rename(mb1,c(pmnewma3month="lag3month"))
mb1<-rename(mb1,c(pmnewmayear="lagyear"))

#delete missing

mb1 = mb1[complete.cases(mb1$lag24h),]
#}}}


#{{{ Prime analysis ##########################################################################
#                             Prime analysis                              #
###########################################################################

#{{{{ICAM
###############
# ICAM
###############

#create results table

ICAM_restable <- data.frame(lag=character(9),beta=numeric(9),se=numeric(9),pc=numeric(9),L_CI=numeric(9),H_CI=numeric(9),sig=numeric(9),ciw=numeric(9))

ICAM_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lag2month","lag3month","lagyear")

#test normality
# hist(mb1$icam)
# tst<- lm(icam ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin + cos+ sin,
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
mlag003<-glmmPQL(logicam ~ lag3day  +age +temp_fma3+ bmi+as.factor(smk2)+ah_gm3_Fma3 +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
ICAM_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
ICAM_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
ICAM_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[2] <- (exp(ICAM_restable$beta[2]*mlag003_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[2] <- (exp((ICAM_restable$beta[2]-1.96*ICAM_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[2] <- (exp((ICAM_restable$beta[2]+1.96*ICAM_restable$se[2])*mlag003_irq)-1)*100
ICAM_restable$ciw[2] <-ICAM_restable$L_CI[2]+ICAM_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logicam ~ lagweek  +age +temp_fmaweek+ bmi+as.factor(smk2)+ah_gm3_Fmaweek +  diabete+statin + cos+ sin,
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
mlag2week<-glmmPQL(logicam ~ lag2week  +age +temp_fma2week+ bmi+as.factor(smk2)+ah_gm3_Fma2week+  diabete+statin + cos+ sin,
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
mlag3week<-glmmPQL(logicam ~ lag3week  +age +temp_fma3week+bmi+as.factor(smk2)+ah_gm3_Fma3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
ICAM_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
ICAM_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
ICAM_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[5] <- (exp(ICAM_restable$beta[5]*mlag3week_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[5] <- (exp((ICAM_restable$beta[5]-1.96*ICAM_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[5] <- (exp((ICAM_restable$beta[5]+1.96*ICAM_restable$se[5])*mlag3week_irq)-1)*100
ICAM_restable$ciw[5] <-ICAM_restable$L_CI[5]+ICAM_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logicam ~ lagmonth  +age +temp_fmamonth+ bmi+as.factor(smk2)+ah_gm3_Fmamonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
ICAM_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
ICAM_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
ICAM_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[6] <- (exp(ICAM_restable$beta[6]*mlagmonth_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[6] <- (exp((ICAM_restable$beta[6]-1.96*ICAM_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[6] <- (exp((ICAM_restable$beta[6]+1.96*ICAM_restable$se[6])*mlagmonth_irq)-1)*100
ICAM_restable$ciw[6] <-ICAM_restable$L_CI[6]+ICAM_restable$H_CI[6]



#mlag2month
mlag2month_irq<- IQR(mb1$lag2month)
mlag2month<-glmmPQL(logicam ~ lag2month  +age +temp_fma2month+ bmi+as.factor(smk2)+ah_gm3_Fma2month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2month)$tTable
ICAM_restable$beta[7] <- mlag2month$coef$fixed[2]  #extract Betas
ICAM_restable$se[7] <-(summary(mlag2month)$tTable[2,2]) #extract SE
ICAM_restable$sig[7] <-(summary(mlag2month)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[7] <- (exp(ICAM_restable$beta[7]*mlag2month_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[7] <- (exp((ICAM_restable$beta[7]-1.96*ICAM_restable$se[7])*mlag2month_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[7] <- (exp((ICAM_restable$beta[7]+1.96*ICAM_restable$se[7])*mlag2month_irq)-1)*100
ICAM_restable$ciw[7] <-ICAM_restable$L_CI[7]+ICAM_restable$H_CI[7]

#mlag3month
mlag3month_irq<- IQR(mb1$lag3month)
mlag3month<-glmmPQL(logicam ~ lag3month  +age +temp_fma3month+ bmi+as.factor(smk2)+ah_gm3_Fma3month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3month)$tTable
ICAM_restable$beta[8] <- mlag3month$coef$fixed[2]  #extract Betas
ICAM_restable$se[8] <-(summary(mlag3month)$tTable[2,2]) #extract SE
ICAM_restable$sig[8] <-(summary(mlag3month)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[8] <- (exp(ICAM_restable$beta[8]*mlag3month_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[8] <- (exp((ICAM_restable$beta[8]-1.96*ICAM_restable$se[8])*mlag3month_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[8] <- (exp((ICAM_restable$beta[8]+1.96*ICAM_restable$se[8])*mlag3month_irq)-1)*100
ICAM_restable$ciw[8] <-ICAM_restable$L_CI[8]+ICAM_restable$H_CI[8]



#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logicam ~ lagyear  +age +temp_fmayear+ bmi+as.factor(smk2)+ ah_gm3_Fmayear+  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
ICAM_restable$beta[9] <- mlagyear$coef$fixed[2]  #extract Betas
ICAM_restable$se[9] <-(summary(mlagyear)$tTable[2,2]) #extract SE
ICAM_restable$sig[9] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[9] <- (exp(ICAM_restable$beta[9]*mlagyear_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[9] <- (exp((ICAM_restable$beta[9]-1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[9] <- (exp((ICAM_restable$beta[9]+1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
ICAM_restable$ciw[9] <-ICAM_restable$L_CI[9]+ICAM_restable$H_CI[9]


#mlagyear
mlagyear2<-glmmPQL(logicam ~ lagyear+lagmonth  +age +temp_fmayear+ bmi+as.factor(smk2)+ ah_gm3_Fmayear+  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear2)$tTable

#mlagyear
mlagyear2<-glmmPQL(logvcam ~ lagyear+lagmonth  +age +temp_fmayear+ bmi+as.factor(smk2)+ ah_gm3_Fmayear+  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear2)$tTable




ICAM_restable$beta[9] <- mlagyear$coef$fixed[2]  #extract Betas
ICAM_restable$se[9] <-(summary(mlagyear)$tTable[2,2]) #extract SE
ICAM_restable$sig[9] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[9] <- (exp(ICAM_restable$beta[9]*mlagyear_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[9] <- (exp((ICAM_restable$beta[9]-1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[9] <- (exp((ICAM_restable$beta[9]+1.96*ICAM_restable$se[9])*mlagyear_irq)-1)*100
ICAM_restable$ciw[9] <-ICAM_restable$L_CI[9]+ICAM_restable$H_CI[9]




##################################################################################################################################################################################################################################################
#}}}}

#{{{{VCAM
###############
# VCAM
###############

#create results table

VCAM_restable <- data.frame(lag=character(9),beta=numeric(9),se=numeric(9),pc=numeric(9),L_CI=numeric(9),H_CI=numeric(9),sig=numeric(9),ciw=numeric(9))

VCAM_restable$lag <- c("lag24h", "lag3day", "lagweek", "lag2week", "lag3week", "lagmonth", "lag2month","lag3month","lagyear")

#test normality
# hist(mb1$vcam)
# tst<- lm(vcam ~ lag001, data=mb1)
# tst$resid
# plot(tst$resid)


#mlag001
mlag001_irq<- IQR(mb1$lag24h)
mlag001<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin + cos+ sin,
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
mlag003<-glmmPQL(logvcam ~ lag3day  +age +temp_fma3+ bmi+as.factor(smk2)+ah_gm3_Fma3 +  diabete+statin + cos+ sin,random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag003)$tTable
VCAM_restable$beta[2] <- mlag003$coef$fixed[2]  #extract Betas
VCAM_restable$se[2] <-(summary(mlag003)$tTable[2,2]) #extract SE
VCAM_restable$sig[2] <-(summary(mlag003)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[2] <- (exp(VCAM_restable$beta[2]*mlag003_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[2] <- (exp((VCAM_restable$beta[2]-1.96*VCAM_restable$se[2])*mlag003_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[2] <- (exp((VCAM_restable$beta[2]+1.96*VCAM_restable$se[2])*mlag003_irq)-1)*100
VCAM_restable$ciw[2] <-VCAM_restable$L_CI[2]+VCAM_restable$H_CI[2]




#mlagweek
mlagweek_irq<- IQR(mb1$lagweek)
mlagweek<-glmmPQL(logvcam ~ lagweek  +age +temp_fmaweek+ bmi+as.factor(smk2)+ah_gm3_Fmaweek +  diabete+statin + cos+ sin,
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
mlag2week<-glmmPQL(logvcam ~ lag2week  +age +temp_fma2week+ bmi+as.factor(smk2)+ah_gm3_Fma2week+  diabete+statin + cos+ sin,
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
mlag3week<-glmmPQL(logvcam ~ lag3week  +age +temp_fma3week+bmi+as.factor(smk2)+ah_gm3_Fma3week +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3week)$tTable
VCAM_restable$beta[5] <- mlag3week$coef$fixed[2]  #extract Betas
VCAM_restable$se[5] <-(summary(mlag3week)$tTable[2,2]) #extract SE
VCAM_restable$sig[5] <-(summary(mlag3week)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[5] <- (exp(VCAM_restable$beta[5]*mlag3week_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[5] <- (exp((VCAM_restable$beta[5]-1.96*VCAM_restable$se[5])*mlag3week_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[5] <- (exp((VCAM_restable$beta[5]+1.96*VCAM_restable$se[5])*mlag3week_irq)-1)*100
VCAM_restable$ciw[5] <-VCAM_restable$L_CI[5]+VCAM_restable$H_CI[5]


#mlagmonth
mlagmonth_irq<- IQR(mb1$lagmonth)
mlagmonth<-glmmPQL(logvcam ~ lagmonth  +age +temp_fmamonth+ bmi+as.factor(smk2)+ah_gm3_Fmamonth +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagmonth)$tTable
VCAM_restable$beta[6] <- mlagmonth$coef$fixed[2]  #extract Betas
VCAM_restable$se[6] <-(summary(mlagmonth)$tTable[2,2]) #extract SE
VCAM_restable$sig[6] <-(summary(mlagmonth)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[6] <- (exp(VCAM_restable$beta[6]*mlagmonth_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[6] <- (exp((VCAM_restable$beta[6]-1.96*VCAM_restable$se[6])*mlagmonth_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[6] <- (exp((VCAM_restable$beta[6]+1.96*VCAM_restable$se[6])*mlagmonth_irq)-1)*100
VCAM_restable$ciw[6] <-VCAM_restable$L_CI[6]+VCAM_restable$H_CI[6]



#mlag2month
mlag2month_irq<- IQR(mb1$lag2month)
mlag2month<-glmmPQL(logvcam ~ lag2month  +age +temp_fma2month+ bmi+as.factor(smk2)+ah_gm3_Fma2month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag2month)$tTable
VCAM_restable$beta[7] <- mlag2month$coef$fixed[2]  #extract Betas
VCAM_restable$se[7] <-(summary(mlag2month)$tTable[2,2]) #extract SE
VCAM_restable$sig[7] <-(summary(mlag2month)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[7] <- (exp(VCAM_restable$beta[7]*mlag2month_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[7] <- (exp((VCAM_restable$beta[7]-1.96*VCAM_restable$se[7])*mlag2month_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[7] <- (exp((VCAM_restable$beta[7]+1.96*VCAM_restable$se[7])*mlag2month_irq)-1)*100
VCAM_restable$ciw[7] <-VCAM_restable$L_CI[7]+VCAM_restable$H_CI[7]

#mlag3month
mlag3month_irq<- IQR(mb1$lag3month)
mlag3month<-glmmPQL(logvcam ~ lag3month  +age +temp_fma3month+ bmi+as.factor(smk2)+ah_gm3_Fma3month +  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlag3month)$tTable
VCAM_restable$beta[8] <- mlag3month$coef$fixed[2]  #extract Betas
VCAM_restable$se[8] <-(summary(mlag3month)$tTable[2,2]) #extract SE
VCAM_restable$sig[8] <-(summary(mlag3month)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[8] <- (exp(VCAM_restable$beta[8]*mlag3month_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[8] <- (exp((VCAM_restable$beta[8]-1.96*VCAM_restable$se[8])*mlag3month_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[8] <- (exp((VCAM_restable$beta[8]+1.96*VCAM_restable$se[8])*mlag3month_irq)-1)*100
VCAM_restable$ciw[8] <-VCAM_restable$L_CI[8]+VCAM_restable$H_CI[8]



#mlagyear
mlagyear_irq<- IQR(mb1$lagyear)
mlagyear<-glmmPQL(logvcam ~ lagyear  +age +temp_fmayear+ bmi+as.factor(smk2)+ ah_gm3_Fmayear+  diabete+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(mlagyear)$tTable
VCAM_restable$beta[9] <- mlagyear$coef$fixed[2]  #extract Betas
VCAM_restable$se[9] <-(summary(mlagyear)$tTable[2,2]) #extract SE
VCAM_restable$sig[9] <-(summary(mlagyear)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
VCAM_restable$pc[9] <- (exp(VCAM_restable$beta[9]*mlagyear_irq)-1)*100
# Low CI bound
VCAM_restable$L_CI[9] <- (exp((VCAM_restable$beta[9]-1.96*VCAM_restable$se[9])*mlagyear_irq)-1)*100
# High CI bound
VCAM_restable$H_CI[9] <- (exp((VCAM_restable$beta[9]+1.96*VCAM_restable$se[9])*mlagyear_irq)-1)*100
VCAM_restable$ciw[9] <-VCAM_restable$L_CI[9]+VCAM_restable$H_CI[9]
##################################################################################################################################################################################################################################################
#}}}}



all<- rbind(ICAM_restable,VCAM_restable)

write.csv(all, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.5.Results/stacey_ analysis/results_primary.csv")



ICAM_g <- ICAM_restable[6:8,]


qplot(data=ICAM_g,x=as.factor(lag), y=pc, ymin=L_CI, ymax=H_CI, geom='pointrange')+ 
        geom_point(aes(x=as.factor(lag), y=pc), color='red')+ ylab("Percnt change") + xlab("Lag") + theme_bw() 

VCAM_g <- VCAM_restable[6:8,]


qplot(data=VCAM_g,x=as.factor(lag), y=pc, ymin=L_CI, ymax=H_CI, geom='pointrange')+ 
        geom_point(aes(x=as.factor(lag), y=pc), color='red')+ ylab("Percnt change") + xlab("Lag") + theme_bw() 






#}}}


#{{{Interactions
# Interactions ##########################################################################
#                              Interactions                               #
###########################################################################

table(mb1$diabete)
table(mb1$statin)
summary(mb1$bmi)

mb1$statin2<- as.numeric(mb1$statin)

mb1$diab_flip <- recode(mb1$diabete, ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mb1$statin_flip <- recode(mb1$statin2, ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mb1$obes_flip<- recode(mb1$obes, ' 0="1"; 1="0" ',  as.factor.result=FALSE)

table(mb1$diabete)
table(mb1$diab_flip)
table(mb1$statin)
table(mb1$statin_flip)
table(mb1$obes)
table(mb1$obes_flip)


#pre interaction mini test
# mb_nond <- subset(mb1,mb1$diabete== "0")
# mb_d <- subset(mb1,mb1$diabete== "1")
# 
# #lagmonth
# 
# lagmonthnd<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  statin + cos+ sin,
# random=~1|id,family=gaussian, data=mb_nond,na=na.omit)
# summary(lagmonthnd)$tTable
# 
# lagmonth_flipd<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +statin + cos+ sin,
# random=~1|id,family=gaussian, data=mb_d,na=na.omit)
# summary(lagmonth_flipd)$tTable
# 





#{{{{diabetes
# diabetes  ##########################################################################
#                                  diab                                   #
###########################################################################

###############
# ICAM 
###############


#create results table

ICAM_diab <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

ICAM_diab$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag24h+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag24h+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

ICAM_diab$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
ICAM_diab$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
ICAM_diab$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[1] <- (exp(ICAM_diab$beta[1]*lag24h_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[1] <- (exp((ICAM_diab$beta[1]-1.96*ICAM_diab$se[1])*lag24h_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[1] <- (exp((ICAM_diab$beta[1]+1.96*ICAM_diab$se[1])*lag24h_irq)-1)*100
#CI Width
ICAM_diab$ciw[1] <-ICAM_diab$L_CI[1]+ICAM_diab$H_CI[1]

##group 1

ICAM_diab$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[2] <- (exp(ICAM_diab$beta[2]*lag24h_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[2] <- (exp((ICAM_diab$beta[2]-1.96*ICAM_diab$se[2])*lag24h_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[2] <- (exp((ICAM_diab$beta[2]+1.96*ICAM_diab$se[2])*lag24h_irq)-1)*100
#CI Width
ICAM_diab$ciw[2] <-ICAM_diab$L_CI[2]+ICAM_diab$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3day+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3day+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

ICAM_diab$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
ICAM_diab$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
ICAM_diab$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[3] <- (exp(ICAM_diab$beta[3]*lag3day_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[3] <- (exp((ICAM_diab$beta[3]-1.96*ICAM_diab$se[3])*lag3day_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[3] <- (exp((ICAM_diab$beta[3]+1.96*ICAM_diab$se[3])*lag3day_irq)-1)*100
#CI Width
ICAM_diab$ciw[3] <-ICAM_diab$L_CI[3]+ICAM_diab$H_CI[3]

##group 1

ICAM_diab$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[4] <- (exp(ICAM_diab$beta[4]*lag3day_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[4] <- (exp((ICAM_diab$beta[4]-1.96*ICAM_diab$se[4])*lag3day_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[4] <- (exp((ICAM_diab$beta[4]+1.96*ICAM_diab$se[4])*lag3day_irq)-1)*100
#CI Width
ICAM_diab$ciw[4] <-ICAM_diab$L_CI[4]+ICAM_diab$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagweek+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagweek+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

ICAM_diab$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
ICAM_diab$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
ICAM_diab$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[5] <- (exp(ICAM_diab$beta[5]*lagweek_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[5] <- (exp((ICAM_diab$beta[5]-1.96*ICAM_diab$se[5])*lagweek_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[5] <- (exp((ICAM_diab$beta[5]+1.96*ICAM_diab$se[5])*lagweek_irq)-1)*100
#CI Width
ICAM_diab$ciw[5] <-ICAM_diab$L_CI[5]+ICAM_diab$H_CI[5]

##group 1

ICAM_diab$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[6] <- (exp(ICAM_diab$beta[6]*lagweek_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[6] <- (exp((ICAM_diab$beta[6]-1.96*ICAM_diab$se[6])*lagweek_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[6] <- (exp((ICAM_diab$beta[6]+1.96*ICAM_diab$se[6])*lagweek_irq)-1)*100
#CI Width
ICAM_diab$ciw[6] <-ICAM_diab$L_CI[6]+ICAM_diab$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag2week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag2week+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

ICAM_diab$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
ICAM_diab$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
ICAM_diab$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[7] <- (exp(ICAM_diab$beta[7]*lag2week_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[7] <- (exp((ICAM_diab$beta[7]-1.96*ICAM_diab$se[7])*lag2week_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[7] <- (exp((ICAM_diab$beta[7]+1.96*ICAM_diab$se[7])*lag2week_irq)-1)*100
#CI Width
ICAM_diab$ciw[7] <-ICAM_diab$L_CI[7]+ICAM_diab$H_CI[7]

##group 1

ICAM_diab$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[8] <- (exp(ICAM_diab$beta[8]*lag2week_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[8] <- (exp((ICAM_diab$beta[8]-1.96*ICAM_diab$se[8])*lag2week_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[8] <- (exp((ICAM_diab$beta[8]+1.96*ICAM_diab$se[8])*lag2week_irq)-1)*100
#CI Width
ICAM_diab$ciw[8] <-ICAM_diab$L_CI[8]+ICAM_diab$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3week+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

ICAM_diab$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
ICAM_diab$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
ICAM_diab$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[9] <- (exp(ICAM_diab$beta[9]*lag3week_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[9] <- (exp((ICAM_diab$beta[9]-1.96*ICAM_diab$se[9])*lag3week_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[9] <- (exp((ICAM_diab$beta[9]+1.96*ICAM_diab$se[9])*lag3week_irq)-1)*100
#CI Width
ICAM_diab$ciw[9] <-ICAM_diab$L_CI[9]+ICAM_diab$H_CI[9]

##group 1

ICAM_diab$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[10] <- (exp(ICAM_diab$beta[10]*lag3week_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[10] <- (exp((ICAM_diab$beta[10]-1.96*ICAM_diab$se[10])*lag3week_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[10] <- (exp((ICAM_diab$beta[10]+1.96*ICAM_diab$se[10])*lag3week_irq)-1)*100
#CI Width
ICAM_diab$ciw[10] <-ICAM_diab$L_CI[10]+ICAM_diab$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagmonth+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagmonth+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

ICAM_diab$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
ICAM_diab$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
ICAM_diab$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[11] <- (exp(ICAM_diab$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[11] <- (exp((ICAM_diab$beta[11]-1.96*ICAM_diab$se[11])*lagmonth_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[11] <- (exp((ICAM_diab$beta[11]+1.96*ICAM_diab$se[11])*lagmonth_irq)-1)*100
#CI Width
ICAM_diab$ciw[11] <-ICAM_diab$L_CI[11]+ICAM_diab$H_CI[11]

##group 1

ICAM_diab$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[12] <- (exp(ICAM_diab$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[12] <- (exp((ICAM_diab$beta[12]-1.96*ICAM_diab$se[12])*lagmonth_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[12] <- (exp((ICAM_diab$beta[12]+1.96*ICAM_diab$se[12])*lagmonth_irq)-1)*100
#CI Width
ICAM_diab$ciw[12] <-ICAM_diab$L_CI[12]+ICAM_diab$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag2month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag2month+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

ICAM_diab$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
ICAM_diab$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
ICAM_diab$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[13] <- (exp(ICAM_diab$beta[13]*lag2month_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[13] <- (exp((ICAM_diab$beta[13]-1.96*ICAM_diab$se[13])*lag2month_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[13] <- (exp((ICAM_diab$beta[13]+1.96*ICAM_diab$se[13])*lag2month_irq)-1)*100
#CI Width
ICAM_diab$ciw[13] <-ICAM_diab$L_CI[13]+ICAM_diab$H_CI[13]

##group 1

ICAM_diab$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[14] <- (exp(ICAM_diab$beta[14]*lag2month_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[14] <- (exp((ICAM_diab$beta[14]-1.96*ICAM_diab$se[14])*lag2month_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[14] <- (exp((ICAM_diab$beta[14]+1.96*ICAM_diab$se[14])*lag2month_irq)-1)*100
#CI Width
ICAM_diab$ciw[14] <-ICAM_diab$L_CI[14]+ICAM_diab$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3month+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

ICAM_diab$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
ICAM_diab$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
ICAM_diab$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[15] <- (exp(ICAM_diab$beta[15]*lag3month_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[15] <- (exp((ICAM_diab$beta[15]-1.96*ICAM_diab$se[15])*lag3month_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[15] <- (exp((ICAM_diab$beta[15]+1.96*ICAM_diab$se[15])*lag3month_irq)-1)*100
#CI Width
ICAM_diab$ciw[15] <-ICAM_diab$L_CI[15]+ICAM_diab$H_CI[15]

##group 1

ICAM_diab$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[16] <- (exp(ICAM_diab$beta[16]*lag3month_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[16] <- (exp((ICAM_diab$beta[16]-1.96*ICAM_diab$se[16])*lag3month_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[16] <- (exp((ICAM_diab$beta[16]+1.96*ICAM_diab$se[16])*lag3month_irq)-1)*100
#CI Width
ICAM_diab$ciw[16] <-ICAM_diab$L_CI[16]+ICAM_diab$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagyear+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagyear+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

ICAM_diab$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
ICAM_diab$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
ICAM_diab$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[17] <- (exp(ICAM_diab$beta[17]*lagyear_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[17] <- (exp((ICAM_diab$beta[17]-1.96*ICAM_diab$se[17])*lagyear_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[17] <- (exp((ICAM_diab$beta[17]+1.96*ICAM_diab$se[17])*lagyear_irq)-1)*100
#CI Width
ICAM_diab$ciw[17] <-ICAM_diab$L_CI[17]+ICAM_diab$H_CI[17]

##group 1

ICAM_diab$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
ICAM_diab$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
ICAM_diab$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_diab$pc[18] <- (exp(ICAM_diab$beta[18]*lagyear_irq)-1)*100

# Low CI bound
ICAM_diab$L_CI[18] <- (exp((ICAM_diab$beta[18]-1.96*ICAM_diab$se[18])*lagyear_irq)-1)*100
# High CI bound
ICAM_diab$H_CI[18] <- (exp((ICAM_diab$beta[18]+1.96*ICAM_diab$se[18])*lagyear_irq)-1)*100
#CI Width
ICAM_diab$ciw[18] <-ICAM_diab$L_CI[18]+ICAM_diab$H_CI[18]


###############
# VCAM 
###############


#create results table

VCAM_diab <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

VCAM_diab$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag24h+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag24h+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

VCAM_diab$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
VCAM_diab$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
VCAM_diab$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[1] <- (exp(VCAM_diab$beta[1]*lag24h_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[1] <- (exp((VCAM_diab$beta[1]-1.96*VCAM_diab$se[1])*lag24h_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[1] <- (exp((VCAM_diab$beta[1]+1.96*VCAM_diab$se[1])*lag24h_irq)-1)*100
#CI Width
VCAM_diab$ciw[1] <-VCAM_diab$L_CI[1]+VCAM_diab$H_CI[1]

##group 1

VCAM_diab$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[2] <- (exp(VCAM_diab$beta[2]*lag24h_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[2] <- (exp((VCAM_diab$beta[2]-1.96*VCAM_diab$se[2])*lag24h_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[2] <- (exp((VCAM_diab$beta[2]+1.96*VCAM_diab$se[2])*lag24h_irq)-1)*100
#CI Width
VCAM_diab$ciw[2] <-VCAM_diab$L_CI[2]+VCAM_diab$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3day+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3day+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

VCAM_diab$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
VCAM_diab$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
VCAM_diab$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[3] <- (exp(VCAM_diab$beta[3]*lag3day_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[3] <- (exp((VCAM_diab$beta[3]-1.96*VCAM_diab$se[3])*lag3day_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[3] <- (exp((VCAM_diab$beta[3]+1.96*VCAM_diab$se[3])*lag3day_irq)-1)*100
#CI Width
VCAM_diab$ciw[3] <-VCAM_diab$L_CI[3]+VCAM_diab$H_CI[3]

##group 1

VCAM_diab$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[4] <- (exp(VCAM_diab$beta[4]*lag3day_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[4] <- (exp((VCAM_diab$beta[4]-1.96*VCAM_diab$se[4])*lag3day_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[4] <- (exp((VCAM_diab$beta[4]+1.96*VCAM_diab$se[4])*lag3day_irq)-1)*100
#CI Width
VCAM_diab$ciw[4] <-VCAM_diab$L_CI[4]+VCAM_diab$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagweek+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagweek+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

VCAM_diab$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
VCAM_diab$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
VCAM_diab$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[5] <- (exp(VCAM_diab$beta[5]*lagweek_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[5] <- (exp((VCAM_diab$beta[5]-1.96*VCAM_diab$se[5])*lagweek_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[5] <- (exp((VCAM_diab$beta[5]+1.96*VCAM_diab$se[5])*lagweek_irq)-1)*100
#CI Width
VCAM_diab$ciw[5] <-VCAM_diab$L_CI[5]+VCAM_diab$H_CI[5]

##group 1

VCAM_diab$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[6] <- (exp(VCAM_diab$beta[6]*lagweek_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[6] <- (exp((VCAM_diab$beta[6]-1.96*VCAM_diab$se[6])*lagweek_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[6] <- (exp((VCAM_diab$beta[6]+1.96*VCAM_diab$se[6])*lagweek_irq)-1)*100
#CI Width
VCAM_diab$ciw[6] <-VCAM_diab$L_CI[6]+VCAM_diab$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag2week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag2week+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

VCAM_diab$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
VCAM_diab$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
VCAM_diab$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[7] <- (exp(VCAM_diab$beta[7]*lag2week_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[7] <- (exp((VCAM_diab$beta[7]-1.96*VCAM_diab$se[7])*lag2week_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[7] <- (exp((VCAM_diab$beta[7]+1.96*VCAM_diab$se[7])*lag2week_irq)-1)*100
#CI Width
VCAM_diab$ciw[7] <-VCAM_diab$L_CI[7]+VCAM_diab$H_CI[7]

##group 1

VCAM_diab$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[8] <- (exp(VCAM_diab$beta[8]*lag2week_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[8] <- (exp((VCAM_diab$beta[8]-1.96*VCAM_diab$se[8])*lag2week_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[8] <- (exp((VCAM_diab$beta[8]+1.96*VCAM_diab$se[8])*lag2week_irq)-1)*100
#CI Width
VCAM_diab$ciw[8] <-VCAM_diab$L_CI[8]+VCAM_diab$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3week+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

VCAM_diab$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
VCAM_diab$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
VCAM_diab$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[9] <- (exp(VCAM_diab$beta[9]*lag3week_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[9] <- (exp((VCAM_diab$beta[9]-1.96*VCAM_diab$se[9])*lag3week_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[9] <- (exp((VCAM_diab$beta[9]+1.96*VCAM_diab$se[9])*lag3week_irq)-1)*100
#CI Width
VCAM_diab$ciw[9] <-VCAM_diab$L_CI[9]+VCAM_diab$H_CI[9]

##group 1

VCAM_diab$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[10] <- (exp(VCAM_diab$beta[10]*lag3week_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[10] <- (exp((VCAM_diab$beta[10]-1.96*VCAM_diab$se[10])*lag3week_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[10] <- (exp((VCAM_diab$beta[10]+1.96*VCAM_diab$se[10])*lag3week_irq)-1)*100
#CI Width
VCAM_diab$ciw[10] <-VCAM_diab$L_CI[10]+VCAM_diab$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagmonth+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagmonth+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

VCAM_diab$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
VCAM_diab$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
VCAM_diab$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[11] <- (exp(VCAM_diab$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[11] <- (exp((VCAM_diab$beta[11]-1.96*VCAM_diab$se[11])*lagmonth_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[11] <- (exp((VCAM_diab$beta[11]+1.96*VCAM_diab$se[11])*lagmonth_irq)-1)*100
#CI Width
VCAM_diab$ciw[11] <-VCAM_diab$L_CI[11]+VCAM_diab$H_CI[11]

##group 1

VCAM_diab$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[12] <- (exp(VCAM_diab$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[12] <- (exp((VCAM_diab$beta[12]-1.96*VCAM_diab$se[12])*lagmonth_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[12] <- (exp((VCAM_diab$beta[12]+1.96*VCAM_diab$se[12])*lagmonth_irq)-1)*100
#CI Width
VCAM_diab$ciw[12] <-VCAM_diab$L_CI[12]+VCAM_diab$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag2month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag2month+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

VCAM_diab$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
VCAM_diab$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
VCAM_diab$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[13] <- (exp(VCAM_diab$beta[13]*lag2month_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[13] <- (exp((VCAM_diab$beta[13]-1.96*VCAM_diab$se[13])*lag2month_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[13] <- (exp((VCAM_diab$beta[13]+1.96*VCAM_diab$se[13])*lag2month_irq)-1)*100
#CI Width
VCAM_diab$ciw[13] <-VCAM_diab$L_CI[13]+VCAM_diab$H_CI[13]

##group 1

VCAM_diab$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[14] <- (exp(VCAM_diab$beta[14]*lag2month_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[14] <- (exp((VCAM_diab$beta[14]-1.96*VCAM_diab$se[14])*lag2month_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[14] <- (exp((VCAM_diab$beta[14]+1.96*VCAM_diab$se[14])*lag2month_irq)-1)*100
#CI Width
VCAM_diab$ciw[14] <-VCAM_diab$L_CI[14]+VCAM_diab$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lag3month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lag3month+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

VCAM_diab$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
VCAM_diab$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
VCAM_diab$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[15] <- (exp(VCAM_diab$beta[15]*lag3month_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[15] <- (exp((VCAM_diab$beta[15]-1.96*VCAM_diab$se[15])*lag3month_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[15] <- (exp((VCAM_diab$beta[15]+1.96*VCAM_diab$se[15])*lag3month_irq)-1)*100
#CI Width
VCAM_diab$ciw[15] <-VCAM_diab$L_CI[15]+VCAM_diab$H_CI[15]

##group 1

VCAM_diab$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[16] <- (exp(VCAM_diab$beta[16]*lag3month_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[16] <- (exp((VCAM_diab$beta[16]-1.96*VCAM_diab$se[16])*lag3month_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[16] <- (exp((VCAM_diab$beta[16]+1.96*VCAM_diab$se[16])*lag3month_irq)-1)*100
#CI Width
VCAM_diab$ciw[16] <-VCAM_diab$L_CI[16]+VCAM_diab$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+diabete*lagyear+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diab_flip+diab_flip*lagyear+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

VCAM_diab$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
VCAM_diab$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
VCAM_diab$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[17] <- (exp(VCAM_diab$beta[17]*lagyear_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[17] <- (exp((VCAM_diab$beta[17]-1.96*VCAM_diab$se[17])*lagyear_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[17] <- (exp((VCAM_diab$beta[17]+1.96*VCAM_diab$se[17])*lagyear_irq)-1)*100
#CI Width
VCAM_diab$ciw[17] <-VCAM_diab$L_CI[17]+VCAM_diab$H_CI[17]

##group 1

VCAM_diab$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
VCAM_diab$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
VCAM_diab$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_diab$pc[18] <- (exp(VCAM_diab$beta[18]*lagyear_irq)-1)*100

# Low CI bound
VCAM_diab$L_CI[18] <- (exp((VCAM_diab$beta[18]-1.96*VCAM_diab$se[18])*lagyear_irq)-1)*100
# High CI bound
VCAM_diab$H_CI[18] <- (exp((VCAM_diab$beta[18]+1.96*VCAM_diab$se[18])*lagyear_irq)-1)*100
#CI Width
VCAM_diab$ciw[18] <-VCAM_diab$L_CI[18]+VCAM_diab$H_CI[18]



all_diab<- rbind(ICAM_diab,VCAM_diab)

write.csv(all_diab, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.5.Results/stacey_ analysis/results_diab.csv")


ICAM_diab_g <- ICAM_diab[11:16,]

qplot(data=ICAM_diab_g,x=as.factor(lag), y=pc, ymin=L_CI, ymax=H_CI, geom='pointrange')+ 
        geom_point(aes(x=as.factor(lag), y=pc), color='red')+ ylab("Percnt change") + xlab("Lag") + theme_bw() 

#}}}}





#{{{{statin
#  statin##########################################################################
#                                statin                                   #
###########################################################################

###############
# ICAM 
###############


#create results table

ICAM_statin <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

ICAM_statin$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag24h+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag24h+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

ICAM_statin$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
ICAM_statin$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
ICAM_statin$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[1] <- (exp(ICAM_statin$beta[1]*lag24h_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[1] <- (exp((ICAM_statin$beta[1]-1.96*ICAM_statin$se[1])*lag24h_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[1] <- (exp((ICAM_statin$beta[1]+1.96*ICAM_statin$se[1])*lag24h_irq)-1)*100
#CI Width
ICAM_statin$ciw[1] <-ICAM_statin$L_CI[1]+ICAM_statin$H_CI[1]

##group 1

ICAM_statin$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[2] <- (exp(ICAM_statin$beta[2]*lag24h_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[2] <- (exp((ICAM_statin$beta[2]-1.96*ICAM_statin$se[2])*lag24h_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[2] <- (exp((ICAM_statin$beta[2]+1.96*ICAM_statin$se[2])*lag24h_irq)-1)*100
#CI Width
ICAM_statin$ciw[2] <-ICAM_statin$L_CI[2]+ICAM_statin$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3day+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3day+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

ICAM_statin$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
ICAM_statin$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
ICAM_statin$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[3] <- (exp(ICAM_statin$beta[3]*lag3day_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[3] <- (exp((ICAM_statin$beta[3]-1.96*ICAM_statin$se[3])*lag3day_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[3] <- (exp((ICAM_statin$beta[3]+1.96*ICAM_statin$se[3])*lag3day_irq)-1)*100
#CI Width
ICAM_statin$ciw[3] <-ICAM_statin$L_CI[3]+ICAM_statin$H_CI[3]

##group 1

ICAM_statin$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[4] <- (exp(ICAM_statin$beta[4]*lag3day_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[4] <- (exp((ICAM_statin$beta[4]-1.96*ICAM_statin$se[4])*lag3day_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[4] <- (exp((ICAM_statin$beta[4]+1.96*ICAM_statin$se[4])*lag3day_irq)-1)*100
#CI Width
ICAM_statin$ciw[4] <-ICAM_statin$L_CI[4]+ICAM_statin$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagweek+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagweek+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

ICAM_statin$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
ICAM_statin$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
ICAM_statin$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[5] <- (exp(ICAM_statin$beta[5]*lagweek_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[5] <- (exp((ICAM_statin$beta[5]-1.96*ICAM_statin$se[5])*lagweek_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[5] <- (exp((ICAM_statin$beta[5]+1.96*ICAM_statin$se[5])*lagweek_irq)-1)*100
#CI Width
ICAM_statin$ciw[5] <-ICAM_statin$L_CI[5]+ICAM_statin$H_CI[5]

##group 1

ICAM_statin$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[6] <- (exp(ICAM_statin$beta[6]*lagweek_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[6] <- (exp((ICAM_statin$beta[6]-1.96*ICAM_statin$se[6])*lagweek_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[6] <- (exp((ICAM_statin$beta[6]+1.96*ICAM_statin$se[6])*lagweek_irq)-1)*100
#CI Width
ICAM_statin$ciw[6] <-ICAM_statin$L_CI[6]+ICAM_statin$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag2week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag2week+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

ICAM_statin$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
ICAM_statin$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
ICAM_statin$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[7] <- (exp(ICAM_statin$beta[7]*lag2week_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[7] <- (exp((ICAM_statin$beta[7]-1.96*ICAM_statin$se[7])*lag2week_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[7] <- (exp((ICAM_statin$beta[7]+1.96*ICAM_statin$se[7])*lag2week_irq)-1)*100
#CI Width
ICAM_statin$ciw[7] <-ICAM_statin$L_CI[7]+ICAM_statin$H_CI[7]

##group 1

ICAM_statin$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[8] <- (exp(ICAM_statin$beta[8]*lag2week_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[8] <- (exp((ICAM_statin$beta[8]-1.96*ICAM_statin$se[8])*lag2week_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[8] <- (exp((ICAM_statin$beta[8]+1.96*ICAM_statin$se[8])*lag2week_irq)-1)*100
#CI Width
ICAM_statin$ciw[8] <-ICAM_statin$L_CI[8]+ICAM_statin$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3week+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

ICAM_statin$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
ICAM_statin$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
ICAM_statin$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[9] <- (exp(ICAM_statin$beta[9]*lag3week_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[9] <- (exp((ICAM_statin$beta[9]-1.96*ICAM_statin$se[9])*lag3week_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[9] <- (exp((ICAM_statin$beta[9]+1.96*ICAM_statin$se[9])*lag3week_irq)-1)*100
#CI Width
ICAM_statin$ciw[9] <-ICAM_statin$L_CI[9]+ICAM_statin$H_CI[9]

##group 1

ICAM_statin$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[10] <- (exp(ICAM_statin$beta[10]*lag3week_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[10] <- (exp((ICAM_statin$beta[10]-1.96*ICAM_statin$se[10])*lag3week_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[10] <- (exp((ICAM_statin$beta[10]+1.96*ICAM_statin$se[10])*lag3week_irq)-1)*100
#CI Width
ICAM_statin$ciw[10] <-ICAM_statin$L_CI[10]+ICAM_statin$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagmonth+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagmonth+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

ICAM_statin$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
ICAM_statin$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
ICAM_statin$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[11] <- (exp(ICAM_statin$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[11] <- (exp((ICAM_statin$beta[11]-1.96*ICAM_statin$se[11])*lagmonth_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[11] <- (exp((ICAM_statin$beta[11]+1.96*ICAM_statin$se[11])*lagmonth_irq)-1)*100
#CI Width
ICAM_statin$ciw[11] <-ICAM_statin$L_CI[11]+ICAM_statin$H_CI[11]

##group 1

ICAM_statin$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[12] <- (exp(ICAM_statin$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[12] <- (exp((ICAM_statin$beta[12]-1.96*ICAM_statin$se[12])*lagmonth_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[12] <- (exp((ICAM_statin$beta[12]+1.96*ICAM_statin$se[12])*lagmonth_irq)-1)*100
#CI Width
ICAM_statin$ciw[12] <-ICAM_statin$L_CI[12]+ICAM_statin$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag2month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag2month+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

ICAM_statin$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
ICAM_statin$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
ICAM_statin$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[13] <- (exp(ICAM_statin$beta[13]*lag2month_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[13] <- (exp((ICAM_statin$beta[13]-1.96*ICAM_statin$se[13])*lag2month_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[13] <- (exp((ICAM_statin$beta[13]+1.96*ICAM_statin$se[13])*lag2month_irq)-1)*100
#CI Width
ICAM_statin$ciw[13] <-ICAM_statin$L_CI[13]+ICAM_statin$H_CI[13]

##group 1

ICAM_statin$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[14] <- (exp(ICAM_statin$beta[14]*lag2month_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[14] <- (exp((ICAM_statin$beta[14]-1.96*ICAM_statin$se[14])*lag2month_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[14] <- (exp((ICAM_statin$beta[14]+1.96*ICAM_statin$se[14])*lag2month_irq)-1)*100
#CI Width
ICAM_statin$ciw[14] <-ICAM_statin$L_CI[14]+ICAM_statin$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3month+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

ICAM_statin$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
ICAM_statin$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
ICAM_statin$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[15] <- (exp(ICAM_statin$beta[15]*lag3month_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[15] <- (exp((ICAM_statin$beta[15]-1.96*ICAM_statin$se[15])*lag3month_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[15] <- (exp((ICAM_statin$beta[15]+1.96*ICAM_statin$se[15])*lag3month_irq)-1)*100
#CI Width
ICAM_statin$ciw[15] <-ICAM_statin$L_CI[15]+ICAM_statin$H_CI[15]

##group 1

ICAM_statin$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[16] <- (exp(ICAM_statin$beta[16]*lag3month_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[16] <- (exp((ICAM_statin$beta[16]-1.96*ICAM_statin$se[16])*lag3month_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[16] <- (exp((ICAM_statin$beta[16]+1.96*ICAM_statin$se[16])*lag3month_irq)-1)*100
#CI Width
ICAM_statin$ciw[16] <-ICAM_statin$L_CI[16]+ICAM_statin$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagyear+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagyear+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

ICAM_statin$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
ICAM_statin$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
ICAM_statin$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[17] <- (exp(ICAM_statin$beta[17]*lagyear_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[17] <- (exp((ICAM_statin$beta[17]-1.96*ICAM_statin$se[17])*lagyear_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[17] <- (exp((ICAM_statin$beta[17]+1.96*ICAM_statin$se[17])*lagyear_irq)-1)*100
#CI Width
ICAM_statin$ciw[17] <-ICAM_statin$L_CI[17]+ICAM_statin$H_CI[17]

##group 1

ICAM_statin$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
ICAM_statin$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
ICAM_statin$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_statin$pc[18] <- (exp(ICAM_statin$beta[18]*lagyear_irq)-1)*100

# Low CI bound
ICAM_statin$L_CI[18] <- (exp((ICAM_statin$beta[18]-1.96*ICAM_statin$se[18])*lagyear_irq)-1)*100
# High CI bound
ICAM_statin$H_CI[18] <- (exp((ICAM_statin$beta[18]+1.96*ICAM_statin$se[18])*lagyear_irq)-1)*100
#CI Width
ICAM_statin$ciw[18] <-ICAM_statin$L_CI[18]+ICAM_statin$H_CI[18]


###############
# VCAM 
###############


#create results table

VCAM_statin <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

VCAM_statin$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag24h+statin + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag24h+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

VCAM_statin$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
VCAM_statin$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
VCAM_statin$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[1] <- (exp(VCAM_statin$beta[1]*lag24h_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[1] <- (exp((VCAM_statin$beta[1]-1.96*VCAM_statin$se[1])*lag24h_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[1] <- (exp((VCAM_statin$beta[1]+1.96*VCAM_statin$se[1])*lag24h_irq)-1)*100
#CI Width
VCAM_statin$ciw[1] <-VCAM_statin$L_CI[1]+VCAM_statin$H_CI[1]

##group 1

VCAM_statin$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[2] <- (exp(VCAM_statin$beta[2]*lag24h_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[2] <- (exp((VCAM_statin$beta[2]-1.96*VCAM_statin$se[2])*lag24h_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[2] <- (exp((VCAM_statin$beta[2]+1.96*VCAM_statin$se[2])*lag24h_irq)-1)*100
#CI Width
VCAM_statin$ciw[2] <-VCAM_statin$L_CI[2]+VCAM_statin$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3day+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3day+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

VCAM_statin$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
VCAM_statin$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
VCAM_statin$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[3] <- (exp(VCAM_statin$beta[3]*lag3day_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[3] <- (exp((VCAM_statin$beta[3]-1.96*VCAM_statin$se[3])*lag3day_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[3] <- (exp((VCAM_statin$beta[3]+1.96*VCAM_statin$se[3])*lag3day_irq)-1)*100
#CI Width
VCAM_statin$ciw[3] <-VCAM_statin$L_CI[3]+VCAM_statin$H_CI[3]

##group 1

VCAM_statin$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[4] <- (exp(VCAM_statin$beta[4]*lag3day_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[4] <- (exp((VCAM_statin$beta[4]-1.96*VCAM_statin$se[4])*lag3day_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[4] <- (exp((VCAM_statin$beta[4]+1.96*VCAM_statin$se[4])*lag3day_irq)-1)*100
#CI Width
VCAM_statin$ciw[4] <-VCAM_statin$L_CI[4]+VCAM_statin$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagweek+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagweek+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

VCAM_statin$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
VCAM_statin$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
VCAM_statin$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[5] <- (exp(VCAM_statin$beta[5]*lagweek_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[5] <- (exp((VCAM_statin$beta[5]-1.96*VCAM_statin$se[5])*lagweek_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[5] <- (exp((VCAM_statin$beta[5]+1.96*VCAM_statin$se[5])*lagweek_irq)-1)*100
#CI Width
VCAM_statin$ciw[5] <-VCAM_statin$L_CI[5]+VCAM_statin$H_CI[5]

##group 1

VCAM_statin$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[6] <- (exp(VCAM_statin$beta[6]*lagweek_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[6] <- (exp((VCAM_statin$beta[6]-1.96*VCAM_statin$se[6])*lagweek_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[6] <- (exp((VCAM_statin$beta[6]+1.96*VCAM_statin$se[6])*lagweek_irq)-1)*100
#CI Width
VCAM_statin$ciw[6] <-VCAM_statin$L_CI[6]+VCAM_statin$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag2week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag2week+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

VCAM_statin$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
VCAM_statin$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
VCAM_statin$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[7] <- (exp(VCAM_statin$beta[7]*lag2week_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[7] <- (exp((VCAM_statin$beta[7]-1.96*VCAM_statin$se[7])*lag2week_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[7] <- (exp((VCAM_statin$beta[7]+1.96*VCAM_statin$se[7])*lag2week_irq)-1)*100
#CI Width
VCAM_statin$ciw[7] <-VCAM_statin$L_CI[7]+VCAM_statin$H_CI[7]

##group 1

VCAM_statin$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[8] <- (exp(VCAM_statin$beta[8]*lag2week_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[8] <- (exp((VCAM_statin$beta[8]-1.96*VCAM_statin$se[8])*lag2week_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[8] <- (exp((VCAM_statin$beta[8]+1.96*VCAM_statin$se[8])*lag2week_irq)-1)*100
#CI Width
VCAM_statin$ciw[8] <-VCAM_statin$L_CI[8]+VCAM_statin$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3week+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3week+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

VCAM_statin$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
VCAM_statin$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
VCAM_statin$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[9] <- (exp(VCAM_statin$beta[9]*lag3week_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[9] <- (exp((VCAM_statin$beta[9]-1.96*VCAM_statin$se[9])*lag3week_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[9] <- (exp((VCAM_statin$beta[9]+1.96*VCAM_statin$se[9])*lag3week_irq)-1)*100
#CI Width
VCAM_statin$ciw[9] <-VCAM_statin$L_CI[9]+VCAM_statin$H_CI[9]

##group 1

VCAM_statin$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[10] <- (exp(VCAM_statin$beta[10]*lag3week_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[10] <- (exp((VCAM_statin$beta[10]-1.96*VCAM_statin$se[10])*lag3week_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[10] <- (exp((VCAM_statin$beta[10]+1.96*VCAM_statin$se[10])*lag3week_irq)-1)*100
#CI Width
VCAM_statin$ciw[10] <-VCAM_statin$L_CI[10]+VCAM_statin$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagmonth+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagmonth+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

VCAM_statin$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
VCAM_statin$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
VCAM_statin$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[11] <- (exp(VCAM_statin$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[11] <- (exp((VCAM_statin$beta[11]-1.96*VCAM_statin$se[11])*lagmonth_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[11] <- (exp((VCAM_statin$beta[11]+1.96*VCAM_statin$se[11])*lagmonth_irq)-1)*100
#CI Width
VCAM_statin$ciw[11] <-VCAM_statin$L_CI[11]+VCAM_statin$H_CI[11]

##group 1

VCAM_statin$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[12] <- (exp(VCAM_statin$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[12] <- (exp((VCAM_statin$beta[12]-1.96*VCAM_statin$se[12])*lagmonth_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[12] <- (exp((VCAM_statin$beta[12]+1.96*VCAM_statin$se[12])*lagmonth_irq)-1)*100
#CI Width
VCAM_statin$ciw[12] <-VCAM_statin$L_CI[12]+VCAM_statin$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag2month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag2month+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

VCAM_statin$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
VCAM_statin$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
VCAM_statin$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[13] <- (exp(VCAM_statin$beta[13]*lag2month_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[13] <- (exp((VCAM_statin$beta[13]-1.96*VCAM_statin$se[13])*lag2month_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[13] <- (exp((VCAM_statin$beta[13]+1.96*VCAM_statin$se[13])*lag2month_irq)-1)*100
#CI Width
VCAM_statin$ciw[13] <-VCAM_statin$L_CI[13]+VCAM_statin$H_CI[13]

##group 1

VCAM_statin$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[14] <- (exp(VCAM_statin$beta[14]*lag2month_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[14] <- (exp((VCAM_statin$beta[14]-1.96*VCAM_statin$se[14])*lag2month_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[14] <- (exp((VCAM_statin$beta[14]+1.96*VCAM_statin$se[14])*lag2month_irq)-1)*100
#CI Width
VCAM_statin$ciw[14] <-VCAM_statin$L_CI[14]+VCAM_statin$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lag3month+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lag3month+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

VCAM_statin$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
VCAM_statin$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
VCAM_statin$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[15] <- (exp(VCAM_statin$beta[15]*lag3month_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[15] <- (exp((VCAM_statin$beta[15]-1.96*VCAM_statin$se[15])*lag3month_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[15] <- (exp((VCAM_statin$beta[15]+1.96*VCAM_statin$se[15])*lag3month_irq)-1)*100
#CI Width
VCAM_statin$ciw[15] <-VCAM_statin$L_CI[15]+VCAM_statin$H_CI[15]

##group 1

VCAM_statin$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[16] <- (exp(VCAM_statin$beta[16]*lag3month_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[16] <- (exp((VCAM_statin$beta[16]-1.96*VCAM_statin$se[16])*lag3month_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[16] <- (exp((VCAM_statin$beta[16]+1.96*VCAM_statin$se[16])*lag3month_irq)-1)*100
#CI Width
VCAM_statin$ciw[16] <-VCAM_statin$L_CI[16]+VCAM_statin$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +  diabete+statin*lagyear+statin + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 + bmi+as.factor(smk2)+ah_gm3_Fma1 +diabete+statin_flip*lagyear+statin_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

VCAM_statin$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
VCAM_statin$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
VCAM_statin$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[17] <- (exp(VCAM_statin$beta[17]*lagyear_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[17] <- (exp((VCAM_statin$beta[17]-1.96*VCAM_statin$se[17])*lagyear_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[17] <- (exp((VCAM_statin$beta[17]+1.96*VCAM_statin$se[17])*lagyear_irq)-1)*100
#CI Width
VCAM_statin$ciw[17] <-VCAM_statin$L_CI[17]+VCAM_statin$H_CI[17]

##group 1

VCAM_statin$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
VCAM_statin$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
VCAM_statin$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_statin$pc[18] <- (exp(VCAM_statin$beta[18]*lagyear_irq)-1)*100

# Low CI bound
VCAM_statin$L_CI[18] <- (exp((VCAM_statin$beta[18]-1.96*VCAM_statin$se[18])*lagyear_irq)-1)*100
# High CI bound
VCAM_statin$H_CI[18] <- (exp((VCAM_statin$beta[18]+1.96*VCAM_statin$se[18])*lagyear_irq)-1)*100
#CI Width
VCAM_statin$ciw[18] <-VCAM_statin$L_CI[18]+VCAM_statin$H_CI[18]




#Graphs
all_statin<- rbind(ICAM_statin,VCAM_statin)

write.csv(all_statin, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.5.Results/stacey_ analysis/results_statin.csv")


ICAM_statin_g <- ICAM_statin[11:16,]

qplot(data=ICAM_statin_g,x=as.factor(lag), y=pc, ymin=L_CI, ymax=H_CI, geom='pointrange')+ 
        geom_point(aes(x=as.factor(lag), y=pc), color='red')+ ylab("Percnt change") + xlab("Lag") + theme_bw() 



#}}}}


#{{{{Obesity


###############
#ICAM 
###############


#create results table

ICAM_obes <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

ICAM_obes$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag24h+obes + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logicam ~ lag24h  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag24h+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

ICAM_obes$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
ICAM_obes$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
ICAM_obes$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[1] <- (exp(ICAM_obes$beta[1]*lag24h_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[1] <- (exp((ICAM_obes$beta[1]-1.96*ICAM_obes$se[1])*lag24h_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[1] <- (exp((ICAM_obes$beta[1]+1.96*ICAM_obes$se[1])*lag24h_irq)-1)*100
#CI Width
ICAM_obes$ciw[1] <-ICAM_obes$L_CI[1]+ICAM_obes$H_CI[1]

##group 1

ICAM_obes$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[2] <- (exp(ICAM_obes$beta[2]*lag24h_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[2] <- (exp((ICAM_obes$beta[2]-1.96*ICAM_obes$se[2])*lag24h_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[2] <- (exp((ICAM_obes$beta[2]+1.96*ICAM_obes$se[2])*lag24h_irq)-1)*100
#CI Width
ICAM_obes$ciw[2] <-ICAM_obes$L_CI[2]+ICAM_obes$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3day+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logicam ~ lag3day  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3day+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

ICAM_obes$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
ICAM_obes$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
ICAM_obes$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[3] <- (exp(ICAM_obes$beta[3]*lag3day_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[3] <- (exp((ICAM_obes$beta[3]-1.96*ICAM_obes$se[3])*lag3day_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[3] <- (exp((ICAM_obes$beta[3]+1.96*ICAM_obes$se[3])*lag3day_irq)-1)*100
#CI Width
ICAM_obes$ciw[3] <-ICAM_obes$L_CI[3]+ICAM_obes$H_CI[3]

##group 1

ICAM_obes$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[4] <- (exp(ICAM_obes$beta[4]*lag3day_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[4] <- (exp((ICAM_obes$beta[4]-1.96*ICAM_obes$se[4])*lag3day_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[4] <- (exp((ICAM_obes$beta[4]+1.96*ICAM_obes$se[4])*lag3day_irq)-1)*100
#CI Width
ICAM_obes$ciw[4] <-ICAM_obes$L_CI[4]+ICAM_obes$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagweek+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logicam ~ lagweek  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagweek+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

ICAM_obes$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
ICAM_obes$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
ICAM_obes$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[5] <- (exp(ICAM_obes$beta[5]*lagweek_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[5] <- (exp((ICAM_obes$beta[5]-1.96*ICAM_obes$se[5])*lagweek_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[5] <- (exp((ICAM_obes$beta[5]+1.96*ICAM_obes$se[5])*lagweek_irq)-1)*100
#CI Width
ICAM_obes$ciw[5] <-ICAM_obes$L_CI[5]+ICAM_obes$H_CI[5]

##group 1

ICAM_obes$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[6] <- (exp(ICAM_obes$beta[6]*lagweek_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[6] <- (exp((ICAM_obes$beta[6]-1.96*ICAM_obes$se[6])*lagweek_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[6] <- (exp((ICAM_obes$beta[6]+1.96*ICAM_obes$se[6])*lagweek_irq)-1)*100
#CI Width
ICAM_obes$ciw[6] <-ICAM_obes$L_CI[6]+ICAM_obes$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag2week+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logicam ~ lag2week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag2week+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

ICAM_obes$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
ICAM_obes$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
ICAM_obes$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[7] <- (exp(ICAM_obes$beta[7]*lag2week_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[7] <- (exp((ICAM_obes$beta[7]-1.96*ICAM_obes$se[7])*lag2week_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[7] <- (exp((ICAM_obes$beta[7]+1.96*ICAM_obes$se[7])*lag2week_irq)-1)*100
#CI Width
ICAM_obes$ciw[7] <-ICAM_obes$L_CI[7]+ICAM_obes$H_CI[7]

##group 1

ICAM_obes$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[8] <- (exp(ICAM_obes$beta[8]*lag2week_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[8] <- (exp((ICAM_obes$beta[8]-1.96*ICAM_obes$se[8])*lag2week_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[8] <- (exp((ICAM_obes$beta[8]+1.96*ICAM_obes$se[8])*lag2week_irq)-1)*100
#CI Width
ICAM_obes$ciw[8] <-ICAM_obes$L_CI[8]+ICAM_obes$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3week+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logicam ~ lag3week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3week+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

ICAM_obes$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
ICAM_obes$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
ICAM_obes$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[9] <- (exp(ICAM_obes$beta[9]*lag3week_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[9] <- (exp((ICAM_obes$beta[9]-1.96*ICAM_obes$se[9])*lag3week_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[9] <- (exp((ICAM_obes$beta[9]+1.96*ICAM_obes$se[9])*lag3week_irq)-1)*100
#CI Width
ICAM_obes$ciw[9] <-ICAM_obes$L_CI[9]+ICAM_obes$H_CI[9]

##group 1

ICAM_obes$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[10] <- (exp(ICAM_obes$beta[10]*lag3week_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[10] <- (exp((ICAM_obes$beta[10]-1.96*ICAM_obes$se[10])*lag3week_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[10] <- (exp((ICAM_obes$beta[10]+1.96*ICAM_obes$se[10])*lag3week_irq)-1)*100
#CI Width
ICAM_obes$ciw[10] <-ICAM_obes$L_CI[10]+ICAM_obes$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagmonth+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logicam ~ lagmonth  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagmonth+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

ICAM_obes$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
ICAM_obes$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
ICAM_obes$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[11] <- (exp(ICAM_obes$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[11] <- (exp((ICAM_obes$beta[11]-1.96*ICAM_obes$se[11])*lagmonth_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[11] <- (exp((ICAM_obes$beta[11]+1.96*ICAM_obes$se[11])*lagmonth_irq)-1)*100
#CI Width
ICAM_obes$ciw[11] <-ICAM_obes$L_CI[11]+ICAM_obes$H_CI[11]

##group 1

ICAM_obes$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[12] <- (exp(ICAM_obes$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[12] <- (exp((ICAM_obes$beta[12]-1.96*ICAM_obes$se[12])*lagmonth_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[12] <- (exp((ICAM_obes$beta[12]+1.96*ICAM_obes$se[12])*lagmonth_irq)-1)*100
#CI Width
ICAM_obes$ciw[12] <-ICAM_obes$L_CI[12]+ICAM_obes$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag2month+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logicam ~ lag2month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag2month+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

ICAM_obes$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
ICAM_obes$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
ICAM_obes$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[13] <- (exp(ICAM_obes$beta[13]*lag2month_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[13] <- (exp((ICAM_obes$beta[13]-1.96*ICAM_obes$se[13])*lag2month_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[13] <- (exp((ICAM_obes$beta[13]+1.96*ICAM_obes$se[13])*lag2month_irq)-1)*100
#CI Width
ICAM_obes$ciw[13] <-ICAM_obes$L_CI[13]+ICAM_obes$H_CI[13]

##group 1

ICAM_obes$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[14] <- (exp(ICAM_obes$beta[14]*lag2month_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[14] <- (exp((ICAM_obes$beta[14]-1.96*ICAM_obes$se[14])*lag2month_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[14] <- (exp((ICAM_obes$beta[14]+1.96*ICAM_obes$se[14])*lag2month_irq)-1)*100
#CI Width
ICAM_obes$ciw[14] <-ICAM_obes$L_CI[14]+ICAM_obes$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3month+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logicam ~ lag3month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3month+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

ICAM_obes$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
ICAM_obes$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
ICAM_obes$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[15] <- (exp(ICAM_obes$beta[15]*lag3month_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[15] <- (exp((ICAM_obes$beta[15]-1.96*ICAM_obes$se[15])*lag3month_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[15] <- (exp((ICAM_obes$beta[15]+1.96*ICAM_obes$se[15])*lag3month_irq)-1)*100
#CI Width
ICAM_obes$ciw[15] <-ICAM_obes$L_CI[15]+ICAM_obes$H_CI[15]

##group 1

ICAM_obes$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[16] <- (exp(ICAM_obes$beta[16]*lag3month_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[16] <- (exp((ICAM_obes$beta[16]-1.96*ICAM_obes$se[16])*lag3month_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[16] <- (exp((ICAM_obes$beta[16]+1.96*ICAM_obes$se[16])*lag3month_irq)-1)*100
#CI Width
ICAM_obes$ciw[16] <-ICAM_obes$L_CI[16]+ICAM_obes$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagyear+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logicam ~ lagyear  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagyear+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

ICAM_obes$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
ICAM_obes$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
ICAM_obes$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[17] <- (exp(ICAM_obes$beta[17]*lagyear_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[17] <- (exp((ICAM_obes$beta[17]-1.96*ICAM_obes$se[17])*lagyear_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[17] <- (exp((ICAM_obes$beta[17]+1.96*ICAM_obes$se[17])*lagyear_irq)-1)*100
#CI Width
ICAM_obes$ciw[17] <-ICAM_obes$L_CI[17]+ICAM_obes$H_CI[17]

##group 1

ICAM_obes$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
ICAM_obes$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
ICAM_obes$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
ICAM_obes$pc[18] <- (exp(ICAM_obes$beta[18]*lagyear_irq)-1)*100

# Low CI bound
ICAM_obes$L_CI[18] <- (exp((ICAM_obes$beta[18]-1.96*ICAM_obes$se[18])*lagyear_irq)-1)*100
# High CI bound
ICAM_obes$H_CI[18] <- (exp((ICAM_obes$beta[18]+1.96*ICAM_obes$se[18])*lagyear_irq)-1)*100
#CI Width
ICAM_obes$ciw[18] <-ICAM_obes$L_CI[18]+ICAM_obes$H_CI[18]


###############
#VCAM 
###############


#create results table

VCAM_obes <- data.frame(lag=character(18),beta=numeric(18),se=numeric(18),pc=numeric(18),L_CI=numeric(18),H_CI=numeric(18),sig=numeric(18),ciw=numeric(18))

VCAM_obes$lag <- c("lag24h-0", "lag24h-1",  "lag3day-0", "lag3day-1","lagweek-0", "lagweek-1","lag2week-0", "lag2week-1","lag3week-0", "lag3week-1","lagmonth-0", "lagmonth-1","lag2month-0","lag2month-1","lag3month-0","lag3month-1","lagyear-0","lagyear-1")



###############
# lag24h 
###############

#lag24h
lag24h_irq<- IQR(mb1$lag24h)

lag24h<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag24h+obes + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h)$tTable

lag24h_flip<-glmmPQL(logvcam ~ lag24h  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag24h+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag24h_flip)$tTable


##group 0

VCAM_obes$beta[1] <- lag24h$coef$fixed[2]  #extract Betas
VCAM_obes$se[1] <-(summary(lag24h)$tTable[2,2]) #extract SE
VCAM_obes$sig[1] <-(summary(lag24h)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[1] <- (exp(VCAM_obes$beta[1]*lag24h_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[1] <- (exp((VCAM_obes$beta[1]-1.96*VCAM_obes$se[1])*lag24h_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[1] <- (exp((VCAM_obes$beta[1]+1.96*VCAM_obes$se[1])*lag24h_irq)-1)*100
#CI Width
VCAM_obes$ciw[1] <-VCAM_obes$L_CI[1]+VCAM_obes$H_CI[1]

##group 1

VCAM_obes$beta[2] <- lag24h_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[2] <-(summary(lag24h_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[2] <-(summary(lag24h_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[2] <- (exp(VCAM_obes$beta[2]*lag24h_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[2] <- (exp((VCAM_obes$beta[2]-1.96*VCAM_obes$se[2])*lag24h_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[2] <- (exp((VCAM_obes$beta[2]+1.96*VCAM_obes$se[2])*lag24h_irq)-1)*100
#CI Width
VCAM_obes$ciw[2] <-VCAM_obes$L_CI[2]+VCAM_obes$H_CI[2]





###############
# lag3day 
###############

#lag3day
lag3day_irq<- IQR(mb1$lag3day)

lag3day<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3day+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day)$tTable

lag3day_flip<-glmmPQL(logvcam ~ lag3day  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3day+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3day_flip)$tTable


##group 0

VCAM_obes$beta[3] <- lag3day$coef$fixed[2]  #extract Betas
VCAM_obes$se[3] <-(summary(lag3day)$tTable[2,2]) #extract SE
VCAM_obes$sig[3] <-(summary(lag3day)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[3] <- (exp(VCAM_obes$beta[3]*lag3day_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[3] <- (exp((VCAM_obes$beta[3]-1.96*VCAM_obes$se[3])*lag3day_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[3] <- (exp((VCAM_obes$beta[3]+1.96*VCAM_obes$se[3])*lag3day_irq)-1)*100
#CI Width
VCAM_obes$ciw[3] <-VCAM_obes$L_CI[3]+VCAM_obes$H_CI[3]

##group 1

VCAM_obes$beta[4] <- lag3day_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[4] <-(summary(lag3day_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[4] <-(summary(lag3day_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[4] <- (exp(VCAM_obes$beta[4]*lag3day_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[4] <- (exp((VCAM_obes$beta[4]-1.96*VCAM_obes$se[4])*lag3day_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[4] <- (exp((VCAM_obes$beta[4]+1.96*VCAM_obes$se[4])*lag3day_irq)-1)*100
#CI Width
VCAM_obes$ciw[4] <-VCAM_obes$L_CI[4]+VCAM_obes$H_CI[4]


###############
# lagweek 
###############

#lagweek
lagweek_irq<- IQR(mb1$lagweek)

lagweek<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagweek+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek)$tTable

lagweek_flip<-glmmPQL(logvcam ~ lagweek  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagweek+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagweek_flip)$tTable


##group 0

VCAM_obes$beta[5] <- lagweek$coef$fixed[2]  #extract Betas
VCAM_obes$se[5] <-(summary(lagweek)$tTable[2,2]) #extract SE
VCAM_obes$sig[5] <-(summary(lagweek)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[5] <- (exp(VCAM_obes$beta[5]*lagweek_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[5] <- (exp((VCAM_obes$beta[5]-1.96*VCAM_obes$se[5])*lagweek_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[5] <- (exp((VCAM_obes$beta[5]+1.96*VCAM_obes$se[5])*lagweek_irq)-1)*100
#CI Width
VCAM_obes$ciw[5] <-VCAM_obes$L_CI[5]+VCAM_obes$H_CI[5]

##group 1

VCAM_obes$beta[6] <- lagweek_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[6] <-(summary(lagweek_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[6] <-(summary(lagweek_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[6] <- (exp(VCAM_obes$beta[6]*lagweek_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[6] <- (exp((VCAM_obes$beta[6]-1.96*VCAM_obes$se[6])*lagweek_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[6] <- (exp((VCAM_obes$beta[6]+1.96*VCAM_obes$se[6])*lagweek_irq)-1)*100
#CI Width
VCAM_obes$ciw[6] <-VCAM_obes$L_CI[6]+VCAM_obes$H_CI[6]


###############
# lag2week 
###############

#lag2week
lag2week_irq<- IQR(mb1$lag2week)

lag2week<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag2week+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week)$tTable

lag2week_flip<-glmmPQL(logvcam ~ lag2week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag2week+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2week_flip)$tTable


##group 0

VCAM_obes$beta[7] <- lag2week$coef$fixed[2]  #extract Betas
VCAM_obes$se[7] <-(summary(lag2week)$tTable[2,2]) #extract SE
VCAM_obes$sig[7] <-(summary(lag2week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[7] <- (exp(VCAM_obes$beta[7]*lag2week_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[7] <- (exp((VCAM_obes$beta[7]-1.96*VCAM_obes$se[7])*lag2week_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[7] <- (exp((VCAM_obes$beta[7]+1.96*VCAM_obes$se[7])*lag2week_irq)-1)*100
#CI Width
VCAM_obes$ciw[7] <-VCAM_obes$L_CI[7]+VCAM_obes$H_CI[7]

##group 1

VCAM_obes$beta[8] <- lag2week_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[8] <-(summary(lag2week_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[8] <-(summary(lag2week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[8] <- (exp(VCAM_obes$beta[8]*lag2week_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[8] <- (exp((VCAM_obes$beta[8]-1.96*VCAM_obes$se[8])*lag2week_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[8] <- (exp((VCAM_obes$beta[8]+1.96*VCAM_obes$se[8])*lag2week_irq)-1)*100
#CI Width
VCAM_obes$ciw[8] <-VCAM_obes$L_CI[8]+VCAM_obes$H_CI[8]


###############
# lag3week 
###############

#lag3week
lag3week_irq<- IQR(mb1$lag3week)

lag3week<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3week+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week)$tTable

lag3week_flip<-glmmPQL(logvcam ~ lag3week  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3week+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3week_flip)$tTable


##group 0

VCAM_obes$beta[9] <- lag3week$coef$fixed[2]  #extract Betas
VCAM_obes$se[9] <-(summary(lag3week)$tTable[2,2]) #extract SE
VCAM_obes$sig[9] <-(summary(lag3week)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[9] <- (exp(VCAM_obes$beta[9]*lag3week_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[9] <- (exp((VCAM_obes$beta[9]-1.96*VCAM_obes$se[9])*lag3week_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[9] <- (exp((VCAM_obes$beta[9]+1.96*VCAM_obes$se[9])*lag3week_irq)-1)*100
#CI Width
VCAM_obes$ciw[9] <-VCAM_obes$L_CI[9]+VCAM_obes$H_CI[9]

##group 1

VCAM_obes$beta[10] <- lag3week_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[10] <-(summary(lag3week_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[10] <-(summary(lag3week_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[10] <- (exp(VCAM_obes$beta[10]*lag3week_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[10] <- (exp((VCAM_obes$beta[10]-1.96*VCAM_obes$se[10])*lag3week_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[10] <- (exp((VCAM_obes$beta[10]+1.96*VCAM_obes$se[10])*lag3week_irq)-1)*100
#CI Width
VCAM_obes$ciw[10] <-VCAM_obes$L_CI[10]+VCAM_obes$H_CI[10]


###############
# lagmonth 
###############

#lagmonth
lagmonth_irq<- IQR(mb1$lagmonth)

lagmonth<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagmonth+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth)$tTable

lagmonth_flip<-glmmPQL(logvcam ~ lagmonth  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagmonth+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagmonth_flip)$tTable


##group 0

VCAM_obes$beta[11] <- lagmonth$coef$fixed[2]  #extract Betas
VCAM_obes$se[11] <-(summary(lagmonth)$tTable[2,2]) #extract SE
VCAM_obes$sig[11] <-(summary(lagmonth)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[11] <- (exp(VCAM_obes$beta[11]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[11] <- (exp((VCAM_obes$beta[11]-1.96*VCAM_obes$se[11])*lagmonth_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[11] <- (exp((VCAM_obes$beta[11]+1.96*VCAM_obes$se[11])*lagmonth_irq)-1)*100
#CI Width
VCAM_obes$ciw[11] <-VCAM_obes$L_CI[11]+VCAM_obes$H_CI[11]

##group 1

VCAM_obes$beta[12] <- lagmonth_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[12] <-(summary(lagmonth_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[12] <-(summary(lagmonth_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[12] <- (exp(VCAM_obes$beta[12]*lagmonth_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[12] <- (exp((VCAM_obes$beta[12]-1.96*VCAM_obes$se[12])*lagmonth_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[12] <- (exp((VCAM_obes$beta[12]+1.96*VCAM_obes$se[12])*lagmonth_irq)-1)*100
#CI Width
VCAM_obes$ciw[12] <-VCAM_obes$L_CI[12]+VCAM_obes$H_CI[12]


###############
# lag2month 
###############

#lag2month
lag2month_irq<- IQR(mb1$lag2month)

lag2month<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag2month+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month)$tTable

lag2month_flip<-glmmPQL(logvcam ~ lag2month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag2month+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag2month_flip)$tTable


##group 0

VCAM_obes$beta[13] <- lag2month$coef$fixed[2]  #extract Betas
VCAM_obes$se[13] <-(summary(lag2month)$tTable[2,2]) #extract SE
VCAM_obes$sig[13] <-(summary(lag2month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[13] <- (exp(VCAM_obes$beta[13]*lag2month_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[13] <- (exp((VCAM_obes$beta[13]-1.96*VCAM_obes$se[13])*lag2month_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[13] <- (exp((VCAM_obes$beta[13]+1.96*VCAM_obes$se[13])*lag2month_irq)-1)*100
#CI Width
VCAM_obes$ciw[13] <-VCAM_obes$L_CI[13]+VCAM_obes$H_CI[13]

##group 1

VCAM_obes$beta[14] <- lag2month_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[14] <-(summary(lag2month_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[14] <-(summary(lag2month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[14] <- (exp(VCAM_obes$beta[14]*lag2month_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[14] <- (exp((VCAM_obes$beta[14]-1.96*VCAM_obes$se[14])*lag2month_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[14] <- (exp((VCAM_obes$beta[14]+1.96*VCAM_obes$se[14])*lag2month_irq)-1)*100
#CI Width
VCAM_obes$ciw[14] <-VCAM_obes$L_CI[14]+VCAM_obes$H_CI[14]


###############
# lag3month 
###############

#lag3month
lag3month_irq<- IQR(mb1$lag3month)

lag3month<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lag3month+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month)$tTable

lag3month_flip<-glmmPQL(logvcam ~ lag3month  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lag3month+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lag3month_flip)$tTable


##group 0

VCAM_obes$beta[15] <- lag3month$coef$fixed[2]  #extract Betas
VCAM_obes$se[15] <-(summary(lag3month)$tTable[2,2]) #extract SE
VCAM_obes$sig[15] <-(summary(lag3month)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[15] <- (exp(VCAM_obes$beta[15]*lag3month_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[15] <- (exp((VCAM_obes$beta[15]-1.96*VCAM_obes$se[15])*lag3month_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[15] <- (exp((VCAM_obes$beta[15]+1.96*VCAM_obes$se[15])*lag3month_irq)-1)*100
#CI Width
VCAM_obes$ciw[15] <-VCAM_obes$L_CI[15]+VCAM_obes$H_CI[15]

##group 1

VCAM_obes$beta[16] <- lag3month_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[16] <-(summary(lag3month_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[16] <-(summary(lag3month_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[16] <- (exp(VCAM_obes$beta[16]*lag3month_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[16] <- (exp((VCAM_obes$beta[16]-1.96*VCAM_obes$se[16])*lag3month_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[16] <- (exp((VCAM_obes$beta[16]+1.96*VCAM_obes$se[16])*lag3month_irq)-1)*100
#CI Width
VCAM_obes$ciw[16] <-VCAM_obes$L_CI[16]+VCAM_obes$H_CI[16]


###############
# lagyear 
###############

#lagyear
lagyear_irq<- IQR(mb1$lagyear)

lagyear<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +  diabete+obes*lagyear+obes + cos+ sin,

random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear)$tTable

lagyear_flip<-glmmPQL(logvcam ~ lagyear  +age + temp_fma1 +statin+as.factor(smk2)+ah_gm3_Fma1 +diabete+obes_flip*lagyear+obes_flip + cos+ sin,
random=~1|id,family=gaussian, data=mb1,na=na.omit)
summary(lagyear_flip)$tTable


##group 0

VCAM_obes$beta[17] <- lagyear$coef$fixed[2]  #extract Betas
VCAM_obes$se[17] <-(summary(lagyear)$tTable[2,2]) #extract SE
VCAM_obes$sig[17] <-(summary(lagyear)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[17] <- (exp(VCAM_obes$beta[17]*lagyear_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[17] <- (exp((VCAM_obes$beta[17]-1.96*VCAM_obes$se[17])*lagyear_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[17] <- (exp((VCAM_obes$beta[17]+1.96*VCAM_obes$se[17])*lagyear_irq)-1)*100
#CI Width
VCAM_obes$ciw[17] <-VCAM_obes$L_CI[17]+VCAM_obes$H_CI[17]

##group 1

VCAM_obes$beta[18] <- lagyear_flip$coef$fixed[2]  #extract Betas
VCAM_obes$se[18] <-(summary(lagyear_flip)$tTable[2,2]) #extract SE
VCAM_obes$sig[18] <-(summary(lagyear_flip)$tTable[2,5]) #extract sig

# Percent change for 1 IQR change if outcome is logged:
VCAM_obes$pc[18] <- (exp(VCAM_obes$beta[18]*lagyear_irq)-1)*100

# Low CI bound
VCAM_obes$L_CI[18] <- (exp((VCAM_obes$beta[18]-1.96*VCAM_obes$se[18])*lagyear_irq)-1)*100
# High CI bound
VCAM_obes$H_CI[18] <- (exp((VCAM_obes$beta[18]+1.96*VCAM_obes$se[18])*lagyear_irq)-1)*100
#CI Width
VCAM_obes$ciw[18] <-VCAM_obes$L_CI[18]+VCAM_obes$H_CI[18]

all_obes<- rbind(ICAM_obes,VCAM_obes)

write.csv(all_obes, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.5.Results/stacey_ analysis/results_obes.csv")


ICAM_obes_g <- ICAM_obes[11:16,]

qplot(data=ICAM_obes_g,x=as.factor(lag), y=pc, ymin=L_CI, ymax=H_CI, geom='pointrange')+ 
        geom_point(aes(x=as.factor(lag), y=pc), color='red')+ ylab("Percnt change") + xlab("Lag") + theme_bw() 


#}}}}




#}}}







