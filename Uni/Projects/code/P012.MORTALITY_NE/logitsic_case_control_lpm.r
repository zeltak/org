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

# with recoded vars
logmod31 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+lpm+AGE+nrace+nedu, random = ~ 1 | guid,family = binomial, data =  mort))
####1
summary(logmod31)


# with recoded vars
logmod311 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi, random = ~ 1 | guid,family = binomial, data =  mort))
summary(logmod311)


#only Long term


# with recoded vars
logmod3LT <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol, random = ~ 1 | guid,family = binomial, data =  mort))
summary(logmod3LT)






##race
# RACE-white 1 other-0

logmod5 <- glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nedu+lpm*nrace,  random = ~ 1 | guid,family = binomial, data =  mort)
summary(logmod5) #beta when other (non white)

logmod5f <- glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nedu+lpm*nracef,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod5f)#beta when white



logmod5LT <- glmmPQL(mort ~ pmnewma1+AGE+nedu+lpm+pmnewmayea*nrace+pmnewmayea,  random = ~ 1 | guid,family = binomial, data =  mort)
summary(logmod5LT) #beta when other (non white)

logmod5fLT <- glmmPQL(mort ~ pmnewma1+AGE+nedu+lpm+pmnewmayea*nracef+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod5fLT)#beta when white

#per minority
logmod51LT <- glmmPQL(mort ~ pmnewma1+AGE+nedu+lpm+pmnewmayea*nmin+pmnewmayea,  random = ~ 1 | guid,family = binomial, data =  mort)
summary(logmod51LT) #beta low percent minority
logmod51fLT <- glmmPQL(mort ~ pmnewma1+AGE+nedu+lpm+pmnewmayea*nminf+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod51fLT)#beta when high minority 







##EDU
# high EDU 1 low EDU-0

logmod6 <- glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nrace+lpm*nedu,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod6) #beta when other (non white)

logmod6f <- glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nrace+lpm*neduf,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod6f)#beta when white


logmod6LT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*nedu+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod6LT) #beta for  high ed

logmod6fLT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*neduf+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod6fLT)


#college
logmod61LT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*ncolm+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod61LT) #beta for low colleauge
logmod61fLT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*ncolmf+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod61fLT)#high colleauge

#nhs
logmod62LT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*nhs+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod62LT) #beta for high edu

logmod62fLT <- glmmPQL(mort ~ pmnewma1+AGE+nrace+lpm+pmnewmayea*nhsf+pmnewmayea,  random = ~ 1 | guid, family = binomial, data =  mort)
summary(logmod62fLT)#low edu





#mon20-URBAN_vs_RURAL 'rural'=0 urban=1
logmod7 <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20+mon20, random = ~ 1 | guid,family = binomial, data =  mort))
####4
summary(logmod7)
logmod7f <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20f+mon20f, random = ~ 1 | guid,family = binomial, data =  mort))
###5
summary(logmod7f)

logmod7 <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE*AGE+AGE+nrace+nedu+pmnewmayea*mon20+mon20, random = ~ 1 | guid,family = binomial, data =  mort))
####4
summary(logmod7)#for rural
logmod7f <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20f+mon20f, random = ~ 1 | guid,family = binomial, data =  mort))
###5
summary(logmod7f) #for urban






##create subsets
mortCVDR <- subset(mort,mort == 1)
summary(mortCVDR)
mortCVDR$mortCVDR <- as.numeric(recode(mortCVDR$ucd ,  "  'J'=1;'I'=0; " ,as.numeric.result=TRUE))


# with recoded vars
logmod8 <- (glmmPQL(mortCVDR ~ pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol, random = ~ 1 | guid,family = binomial, data =  mortCVDR))
####1
summary(logmod8)









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





#race
logmod91LT <- glmmPQL(mort ~ pmnewmayea+nrace*pmnewmayea+nedu3*pmnewmayea+lpm+AGE*AGE+AGE+nrace+as.factor(nedu3)+SEX+med_inc+Avg_per_mi+Avg_pctcol,  random = ~ 1 | guid, family = binomial, data =  mort)
