library (MASS)
library (splines)
library(nlme)
library(reshape)
library(lme4)
library(memisc)


#heart type admissions

ari = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.17.Medicare_MIA/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/ari0006.csv", header=T)


ari_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = ari ))

ari_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = ari ))

ari_table <- mtable( ari_ma1 , ari_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))



mi = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.17.Medicare_MIA/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/mi0006.csv", header=T)


mi_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = mi ))

mi_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = mi ))

mi_table <- mtable( mi_ma1 , mi_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))




cvd = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.17.Medicare_MIA/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/cvd0006.csv", header=T)


cvd_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = cvd ))

cvd_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = cvd ))

cvd_table <- mtable( cvd_ma1 , cvd_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))




#resperatory related admissions

#resp

resp_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = resp ))

resp_ma3 <- (glmer(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = resp ))


resp_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = resp ))


resp_table <- mtable( resp_ma1 , resp_ma3 , resp_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))


#copd

copd_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = copd ))

copd_ma3 <- (glmer(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = copd ))


copd_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = copd ))


copd_table <- mtable( copd_ma1 ,copd_ma3, copd_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))


#pneum

pneum_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = pneum ))

pneum_ma3 <- (glmer(count~ ns(date,df=35)+pmnewma3+pmnewmayear+temp_fma3+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = pneum ))


pneum_lag0 <- (glmer(count~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = pneum ))


pneum_table <- mtable( pneum_ma1 , pneum_ma3 , pneum_lag0, summary.stats=FALSE,digits=min(3,getOption("digits")))

#rest of admissions we only look at lag01(moving average of lag0 and lag1)

#strhem

strhem_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = strhem ))

strhem_table <- mtable( strhem_ma1 , summary.stats=FALSE,digits=min(3,getOption("digits")))


#diab

diab_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = diab ))

diab_table <- mtable( diab_ma1 , summary.stats=FALSE,digits=min(3,getOption("digits")))

#strisc

strisc_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = strisc ))

strisc_table <- mtable( strisc_ma1 , summary.stats=FALSE,digits=min(3,getOption("digits")))

#stroke

stroke_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = stroke ))

stroke_table <- mtable( stroke_ma1 , summary.stats=FALSE,digits=min(3,getOption("digits")))

#chf

chf_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = chf ))

chf_table <- mtable( chf_ma1 ,  summary.stats=FALSE,digits=min(3,getOption("digits")))

#ihd

ihd_ma1 <- (glmer(count~ ns(date,df=35)+pmnewma1+pmnewmayear+temp_fma1+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest+pctlowinc_wtd+ (1|guid), family = poisson, data = ihd ))

ihd_table <- mtable( ihd_ma1 ,  summary.stats=FALSE,digits=min(3,getOption("digits")))

sink("c://Users//ekloog//Documents//tmp//medicare_MIA_new.txt",append=TRUE,split=TRUE)


#tables
ari_table
mi_table
stroke_table
strhem_table
diab_table
strisc_table
copd_table
mi_table
pneum_table
chf_table
ihd_table
resp_table
cvd_table
