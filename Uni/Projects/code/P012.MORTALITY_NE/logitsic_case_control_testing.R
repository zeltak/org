library(nlme)
library(foreign)
library(psych)
library(mgcv)
library(reshape)
library(doBy)
library(car)


#mortguid <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/2.Gather_data/FN001_Cases_guid/cases_guid_reg_final.dbf") 


mort <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN003_logistic/cases.dbf")
names(mort)
summary(mort$lpm)

mort <- subset(mort,lpm != -9999.000)

mort$ucd<-substr(mort$UCOD, 1, 1)

#note you will get a warnning message about missing being created
mort$mort <- as.numeric(recode(mort$ucd ,  "  'C'=0; 'D'=0; 'F'=0; 'K'=0; 'N'=0; 'N'=0;'V'=0;'X'=0;'Y'=0;'J'=1;'I'=1; " ,as.numeric.result=TRUE))

mort = mort[complete.cases(mort$mort),]

str(mort)

summaryBy(pmnewmayea~mort, data=mort, FUN=c(mean,var,median))


#flips symbols for interaction
mort$neduf <- recode(mort$nedu , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort$nracef <- recode(mort$nrace , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
mort$mon20f <- recode(mort$mon20 , ' 0="1"; 1="0" ',  as.factor.result=FALSE)



#logitsic regression

#logmod1 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+RACE+EDUC, random = ~ 1 | guid,family = binomial, data =  mort))
#summary(logmod1)


#logmod2 <- glm(mort ~ pmnewma1+pmnewmayea+lpm+AGE+RACE+EDUC, family = binomial, data =  mort)
#summary(logmod2)


# with recoded vars
logmod3 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu, random = ~ 1 | guid,family = binomial, data =  mort))
summary(logmod3)

logmod31 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+lpm+SEX+AGE+nrace+nedu, random = ~ 1 | guid,family = binomial, data =  mort))


logmod31 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+lpm+SEX+AGE+nrace+nedu+date+med_inc, random = ~ 1 | guid,family = binomial, data =  mort))



####1
summary(logmod31)

summary(mort$OCCUP)

#logmod4 <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu, family = binomial, data =  mort)
#summary(logmod4)




#interactions
logmod5 <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu+pmnewmayea*nrace+med_inc, family = binomial, data =  mort)
summary(logmod5)

logmod51 <- glm(mort ~ pmnewma1+lpm+AGE+nedu+pmnewmayea*nrace, family = binomial, data =  mort)
summary(logmod51)

logmod5f <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nracef+nedu+pmnewmayea*nracef, family = binomial, data =  mort)
summary(logmod5f)

#logmod6 <- (glmmPQL(mort ~ pmnewmayea+lpm++AGE+nrace+nedu+pmnewmayea*nrace, random = ~ 1 | guid,family = binomial, data =  mort))

###2




mort$pml_cen<-(mort$pmnewmayea-mean(mort$pmnewmayea))

summary(mort$pml_cen)

logmod6 <- (glmmPQL(mort ~ pmnewma1+lpm+AGE+nedu+pml_cennrace+med_inc+SEX+Avg_per_mi, random = ~ 1 | guid,family = binomial, data =  mort))
###3
summary(logmod6)

logmod6l <- (glmmPQL(mort ~ pmnewma1+AGE+nedu+pmnewmayea+med_inc+lpm*nrace+SEX+Avg_per_mi, random = ~ 1 | guid,family = binomial, data =  mort))
###3
summary(logmod6l)


summary(mort$pmnewmayea)

logmod6f <- (glmmPQL(mort ~ pmnewma1+lpm+AGE+nedu+pmnewmayea*nracef+med_inc, random = ~ 1 | guid,family = binomial, data =  mort))
###3
summary(logmod6f)


#mon20
logmod7 <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20+mon20, random = ~ 1 | guid,family = binomial, data =  mort))
####4
summary(logmod7)



logmod7f <- (glmmPQL(mort ~ pmnewma1+lpm+AGE+nrace+nedu+pml_cen*mon20+mon20, random = ~ 1 | guid,family = binomial, data =  mort))
###5
summary(logmod7f)

summary(mort$mon20)
