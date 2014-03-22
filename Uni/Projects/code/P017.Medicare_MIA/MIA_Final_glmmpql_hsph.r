library (MASS)
library (splines)
library(nlme)
library(reshape)

sink("/usr1/home/ekloog/medicare_MIA_new.txt",append=TRUE,split=TRUE)


#heart type admissions

ari = read.csv("/usr1/home/ekloog/work/ari0006.csv", header=T)
#___________________________________________________

ari_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd, random = ~ 1 | guid,family = poisson, data = ari ))

summary(ari_ma1 )$tTable

ari_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = ari ))


summary(ari_lag0)$tTable

#___________________________________________________
mi = read.csv("/usr1/home/ekloog/work/mi0006.csv", header=T)
#___________________________________________________

mi_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = mi ))

summary(mi_ma1)$tTable

mi_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = mi ))

summary(mi_lag0)$tTable




#___________________________________________________
cvd = read.csv("/usr1/home/ekloog/work/cvd0006.csv", header=T)

#___________________________________________________

cvd_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = cvd ))

summary(cvd_ma1)$tTable

cvd_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = cvd ))

summary(cvd_lag0)$tTable




#resperatory related admissions

#resp

#___________________________________________________
resp = read.csv("/usr1/home/ekloog/work/resp0006.csv", header=T)

#___________________________________________________

resp_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = resp ))

summary(resp_ma1)$tTable


resp_ma3 <- (glmmPQL(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = resp ))

summary(resp_ma3)$tTable

resp_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = resp ))


summary(resp_lag0)$tTable


#___________________________________________________
#copd

copd = read.csv("/usr1/home/ekloog/work/copd0006.csv", header=T)

#___________________________________________________
copd_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = copd ))

summary(copd_ma1)$tTable

copd_ma3 <- (glmmPQL(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = copd ))

summary(copd_ma3)$tTable

copd_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = copd ))


summary(copd_lag0)$tTable


#___________________________________________________
#pneum

pneum = read.csv("/usr1/home/ekloog/work/pneum0006.csv", header=T)

#___________________________________________________

pneum_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = pneum ))

summary(pneum_ma1)$tTable

pneum_ma3 <- (glmmPQL(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = pneum ))

summary(pneum_ma3)$tTable

pneum_lag0 <- (glmmPQL(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = pneum ))

summary(pneum_lag0)$tTable


#rest of admissions we only look at lag01(moving average of lag0 and lag1)

#___________________________________________________
#strhem

strhem= read.csv("/usr1/home/ekloog/work/strhem0006.csv", header=T)

#___________________________________________________
strhem_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = strhem ))


summary(strhem_ma1)$tTable


#___________________________________________________
diab= read.csv("/usr1/home/ekloog/work/diab0006.csv", header=T)
#___________________________________________________
diab_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = diab ))


summary(diab_ma1)$tTable
#___________________________________________________
#strisc

strisc= read.csv("/usr1/home/ekloog/work/strisc0006.csv", header=T)
#___________________________________________________
strisc_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = strisc ))


summary(strisc_ma1)$tTable
#___________________________________________________
#stroke

stroke= read.csv("/usr1/home/ekloog/work/stroke0006.csv", header=T)
#___________________________________________________
stroke_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = stroke ))


summary(stroke_ma1)$tTable
#___________________________________________________
#chf

chf= read.csv("/usr1/home/ekloog/work/chf0006.csv", header=T)
#___________________________________________________
chf_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = chf ))


summary(chf_ma1)$tTable
#___________________________________________________
#ihd

ihd= read.csv("/usr1/home/ekloog/work/ihd0006.csv", header=T)
#___________________________________________________
ihd_ma1 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = ihd ))

summary(ihd_ma1)$tTable



