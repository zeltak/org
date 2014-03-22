library (MASS)
library (splines)
library(nlme) 
library(psych)
library(reshape)



ts0004lag = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/m25_allcases_reg_0106.csv", header=T) 

ts0004lag<-na.omit(ts0004lag)

# /*home death=0  /////// outside home=1*/



summary(ts0004lag)
lung_new <- (glmmPQL(lung ~ 1, random = ~ 1 | guid,family = poisson, data = ts0004lag)) 



lung_new$coef$random 
#this outputs the 2 colums to the terminal, copy that and paste in text editor and reimport to R

ranlung = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/lung_rancoef.csv", header=T) 


summary(ranlung)

ts0004lag <- ts0004lag[order(ts0004lag$guid),] 
ranlung <- ranlung[order(ranlung$guid),] 
ts <- merge(ts0004lag,ranlung,by="guid")
ts <- rename(ts, c(Intercept="ran"))
names(ts)

ts0004lag <- ts
# 
# ts0004lag<-read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/m25_fas.csv",header=T) 
# summary(ts0004lag)
###########FINAL

try0 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try0)$tTable

try1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+temp_f_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1)$tTable




try2 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l2+pmnewmayear+temp_f_l2+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts))
summary(try2)$tTable


ts0004lag$pmnewmayear_cen<-(ts0004lag$pmnewmayear-mean(ts0004lag$pmnewmayear))
ts0004lag$pmnew_l0_cen<-(ts0004lag$pmnew_l0 -mean(ts0004lag$pmnew_l0))
ts0004lag$pmnew_l1_cen<-(ts0004lag$pmnew_l1 -mean(ts0004lag$pmnew_l1))

summary(ts0004lag)


#place of death interaction

try0int_dp <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnew_l0*dp+dp+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try0int_dp)$tTable

try1int_dp <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnew_l1*dp+dp+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_dp)$tTable

#2 interactions (non were signifcant/madde sense)
names(ts0004lag)

try0int <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+pmnewmayear*min_bin_m+temp_f_l0+med_inc+min_bin_m+Avg_pctnoh+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try0int)$tTable #neg pmyear



try_hs_l0 <- (glmmPQL(count ~ ns(date,df=35)+temp_f_l0+pmnew_l0+pmnewmayear+pmnewmayear*hs_bin_m+hs_bin_m+med_inc+Avg_per_mi+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try_hs_l0)$tTable #ns pmyear



try_inc_l0 <- (glmmPQL(count ~ ns(date,df=35)+temp_f_l0+pmnew_l0+pmnewmayear+pmnewmayear*inc_bin_m+inc_bin_m+Avg_pctnoh+Avg_per_mi+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try_inc_l0)$tTable #neg low inc?




                            



