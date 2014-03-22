library(nlme)
library(foreign)
library(psych)
library(mgcv)
library(reshape)
library(doBy)


mortguid <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/2.Gather_data/FN001_Cases_guid/cases_guid_reg_final.dbf") 


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

logmod1 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+RACE+EDUC, random = ~ 1 | guid,family = binomial, data =  mort))
summary(logmod1)


logmod2 <- glm(mort ~ pmnewma1+pmnewmayea+lpm+AGE+RACE+EDUC, family = binomial, data =  mort)
summary(logmod2)


# with recoded vars
logmod3 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu, random = ~ 1 | guid,family = binomial, data =  mort))
summary(logmod3)

logmod31 <- (glmmPQL(mort ~ pmnewma1+pmnewmayea+lpm+AGE+nrace+nedu, random = ~ 1 | guid,family = binomial, data =  mort))

####1
ummary(logmod31)



logmod4 <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu, family = binomial, data =  mort)
summary(logmod4)




#interactions
logmod5 <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nrace+nedu+pmnewmayea*nrace, family = binomial, data =  mort)
summary(logmod5)

logmod5f <- glm(mort ~ pmnewma1+pmnewmayea+AGE+nracef+nedu+pmnewmayea*nracef, family = binomial, data =  mort)
summary(logmod5f)

logmod6 <- (glmmPQL(mort ~ pmnewmayea+lpm++AGE+nrace+nedu+pmnewmayea*nrace, random = ~ 1 | guid,family = binomial, data =  mort))

###2
summary(logmod6)
logmod6f <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nracef+nedu+pmnewmayea*nracef, random = ~ 1 | guid,family = binomial, data =  mort))
###3
summary(logmod6f)


#mon20
logmod7 <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20+mon20, random = ~ 1 | guid,family = binomial, data =  mort))
####4
summary(logmod7)
logmod7f <- (glmmPQL(mort ~ pmnewmayea+lpm+AGE+nrace+nedu+pmnewmayea*mon20f+mon20f, random = ~ 1 | guid,family = binomial, data =  mort))
###5
summary(logmod7f)


