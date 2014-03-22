library(nlme)
library(foreign)
library(psych)
library(mgcv)
library(reshape)
library(doBy)
library(car)




mort <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN003_logistic/cases.dbf")
names(mort)
summary(mort$lpm)
head(mort$EDUC,n=600)

mort <- subset(mort,lpm != -9999.000)

mort$ucd<-substr(mort$UCOD, 1, 1)

#note you will get a warnning message about missing being created
mort$mort <- as.numeric(recode(mort$ucd ,  "  'C'=0; 'D'=0; 'F'=0; 'K'=0; 'N'=0; 'N'=0;'V'=0;'X'=0;'Y'=0;'J'=1;'I'=1; " ,as.numeric.result=TRUE))




mort = mort[complete.cases(mort$mort),]

summaryBy(pmnewmayea~mort, data=mort, FUN=c(mean,var,median))


#flips symbols for interaction
mort$neduf <- recode(mort$nedu , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort = mort[complete.cases(mort$nrace),]
mort$nracef <- recode(mort$nrace , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort$mon20f <- recode(mort$mon20 , ' 0="1"; 1="0" ',  as.factor.result=FALSE)


attach(mort)
mort$nedu3[EDUC > 16] <- 2
mort$nedu3[EDUC >= 12 & EDUC <= 16 ] <- 1
mort$nedu3[EDUC < 12] <- 0
mort$ncolm[Avg_pctcol >  28.52] <- 1
mort$ncolm[Avg_pctcol <=  28.52] <- 0
mort$nmin[Avg_per_mi >  8.83] <- 1
mort$nmin[Avg_per_mi <=  8.83] <- 0
mort$nhs[Avg_pctnoh >  13.80] <- 1
mort$nhs[Avg_pctnoh <=  13.80] <- 0

detach(mort)
head(mort$nedu3,n=300)

mort$ncolmf <- recode(mort$ncolm , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort$nminf <- recode(mort$nmin , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort$nhsf <- recode(mort$nhs , ' 0="1"; 1="0" ',  as.factor.result=FALSE)


summary(mort)

#note you will get a warnning message about missing being created
mort$mort_resp <- as.numeric(recode(mort$ucd ,  "  'C'=0; 'D'=0; 'F'=0; 'K'=0; 'N'=0; 'N'=0;'V'=0;'X'=0;'Y'=0;'J'=1;'I'=0; " ,as.numeric.result=TRUE))
#note you will get a warnning message about missing being created
mort$mort_cvd <- as.numeric(recode(mort$ucd ,  "  'C'=0; 'D'=0; 'F'=0; 'K'=0; 'N'=0; 'N'=0;'V'=0;'X'=0;'Y'=0;'J'=0;'I'=1; " ,as.numeric.result=TRUE))



#logitsic regression

#paper

#full model
logmod71LT <- glmmPQL(mort ~ pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod71LT) 




#race
logmod91LT <- glmmPQL(mort ~ pmnewmayea+nrace*pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)


summary(logmod91LT) #beta for low colleauge


logmod91fLT <- glmmPQL(mort ~ pmnewmayea+nracef*pmnewmayea+lpm+AGE*AGE+AGE+nracef+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod91fLT)#high colleauge


summary(mort$nrace)






#urb/rural
logmod912LT <- glmmPQL(mort ~ pmnewmayea+mon20*pmnewmayea+lpm+mon20+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)

summary(logmod912LT) 

logmod912LTf <- glmmPQL(mort ~ pmnewmayea+mon20f*pmnewmayea+lpm+mon20f+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)

summary(logmod912LTf)





#col

logmod913LT <- glmmPQL(mort ~ pmnewmayea+ncolm*pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+ncolm,  random = ~ 1 | guid, family = binomial, data =  mort)

summary(logmod913LT)


#col

logmod913LTf <- glmmPQL(mort ~ pmnewmayea+ncolmf*pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+ncolmf,  random = ~ 1 | guid, family = binomial, data =  mort)

summary(logmod913LTf)




