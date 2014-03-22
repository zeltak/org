library(foreign) 
library(nlme)
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lme4)
library(mgcv)

dat1<-fread("f:/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_noces.csv")

m <- lmer(birthw ~ fintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+diab+ hyper +lungd +diab_other+ prevpret+ as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+ as.factor(byob)+ (1 | FIPS), data = dat1)

gm <- gamm(birthw ~ fintempmabirth+s(date)+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+diab+ hyper +lungd +diab_other+ prevpret+ as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+ as.factor(byob),random=list(FIPS=~1), data = dat1)
