## load library

library(lme4)
library(lmerTest)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(gamm4)
library(memisc)
library(arm)
library(papeR)
## disable sci notations
options(scipen = 99)

#create results table
paper_table <- data.frame(object=character(8),bw270=numeric(8))
paper_table$object <- c("beta", "se", "LCI", "HCI", "tstat", "sig", "PH", "PH")



## Import
bd<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_nocesv2.csv")
names(bd)

#rename key variables
#ta model
setnames(bd,"IQRfintempmabirth","ta270")
setnames(bd,"IQRfintempma3month","ta90")
setnames(bd,"IQRfintempmamonth","ta30")
setnames(bd,"IQRfintempma2week","ta14")
setnames(bd,"IQRfintempmaweek","ta7")
setnames(bd,"IQRfintempma3","ta3")
setnames(bd,"IQRfintempma1","ta1")
setnames(bd,"IQRfintemp_l0","ta0") 
         
#tdnc
setnames(bd,"IQRtncdcmabirth","ncdc270")
setnames(bd,"IQRtncdcma3month","ncdc90")
setnames(bd,"IQRtncdcmamonth","ncdc30")
setnames(bd,"IQRtncdcma2week","ncdc14")
setnames(bd,"IQRtncdcmaweek","ncdc7")
setnames(bd,"IQRtncdcma3","ncdc3")
setnames(bd,"IQRtncdcma1","ncdc1")
setnames(bd,"IQRtncdc_l0","ncdc0")

#PM
setnames(bd,"pmnewmabirth","pm270")
setnames(bd,"pmnewma3month","pm90")
setnames(bd,"pmnewmamonth","pm30")
setnames(bd,"pmnewma2week","pm14")
setnames(bd,"pmnewmaweek","pm7")
setnames(bd,"pmnewma3","pm3")
setnames(bd,"pmnewma1","pm1")
setnames(bd,"pmnew_l0","pm0")
 
#preterm
setnames(bd,"NSGA","pterm")
setnames(bd,"medgrrenttr00","mgrossrent")


## dates
bd[, day:=as.Date(strptime(date, "%d/%m/%y"))]
bd$rfpisga[bd$rfpisga== 9] <- NA
bd$rfpisga[bd$rfpisga== 2] <- 0

###recoding
#finer race
bd<- bd[mother_race  == 1 , mrn.n  := 1] #white
bd<- bd[mother_race  == 5 , mrn.n  := 2] #hispanic
bd<- bd[mother_race  == 2 , mrn.n  := 3] #black
bd<- bd[mother_race  == 3 , mrn.n  := 4] #asian
bd<- bd[mother_race  == 4 , mrn.n  := 5] #other
bd<- bd[mother_race  == 8 , mrn.n  := 5] #other
bd<- bd[mother_race  == 9 , mrn.n  := 5] #other
#ptbirth


#delete bad data
bd<- bd[ges_calc  >= 20] #delete less then 20 weeks
bd<- bd[ges_calc  <= 45] #delete more then 45 weeks



## aggreagted PM
# 
# bd$tpm270<- bd$pmnewmabirth+bd$localpm
# bd$tpm90<- bd$pmnewma3month+bd$localpm
# bd$tpm30<- bd$pmnewmamonth+bd$localpm

bd<- bd[med_income  <=  52290 , mid  := 0] #low
bd<- bd[med_income  >  52290 , mid  := 1] #high

# #create preterm
# bd<- bd[ges_calc  >= 37 , pterm  := 0] #full term
# bd<- bd[ges_calc  < 37 , pterm  := 1] #preterm
## calcute SGA
sgamodel <- lm(birthw ~ges_calc+gender+as.factor(mrn.n),data =  bd)
bd$sga <- resid(sgamodel)
describe(bd$sga)
bd <- bd[sga  >= -584.3  , sgaq  := 0] # NON sga
bd <- bd[sga  < -584.3  , sgaq  := 1] # sga group
# ## calcute LBW
# bd<- bd[birthw  >= 37 , pterm  := 0] #full term
# bd<- bd[ges_calc  < 37 , pterm  := 1] #preterm

#discriptives
describe(bd$birthw[bd$mrn.n == 4])
sd(bd$birthw[bd$mrn.n == 4])
describe(bd$birthw[bd$mrn.n == 5])
sd(bd$birthw[bd$mrn.n == 5])


describe(bd$birthw[bd$mrn.n == 1])
describe(bd$birthw[bd$mrn.n == 2])
describe(bd$birthw[bd$mrn.n == 3])
describe(bd$birthw[bd$mrn.n == 4])
describe(bd$birthw[bd$mrn.n == 5])

describe(bd$aged)





########## paper

######################## Birth weight and PM

#### bw270
#### bw270
bw270.mod <- lmer(birthw ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1|FIPS),data =  bd)

bw270.res<- summary(bw270.mod)
#add to table
paper_table$bw270<-0
paper_table$pm270<-0
#ta
paper_table$bw270[1] <- bw270.res$coefficients[2,1]  #extract Betas
paper_table$bw270[2] <-bw270.res$coefficients[2,2] #extract SE
paper_table$bw270[3]<-confint(bw270.mod, method="Wald",'ta270', level=0.95)[1] #for low CI
paper_table$bw270[4]<-confint(bw270.mod, method="Wald",'ta270', level=0.95)[2]  #for high
paper_table$bw270[5] <-bw270.res$coefficients[2,4] #tstat
paper_table$bw270[6] <-bw270.res$coefficients[2,5] #sig
#pm
paper_table$pm270[1] <- bw270.res$coefficients[3,1]  #extract Betas
paper_table$pm270[2] <-bw270.res$coefficients[3,2] #extract SE
paper_table$pm270[3]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$pm270[4]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[2]  #for high CI
paper_table$pm270[5] <-bw270.res$coefficients[3,4] #tstat
paper_table$pm270[6] <-bw270.res$coefficients[3,5] #sig


#### bw90
bw90.mod <- lmer(birthw ~ ta90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw90.res<- summary(bw90.mod)

#add to table
paper_table$bw90<-0
paper_table$pm90<-0
#ta
paper_table$bw90[1] <- bw90.res$coefficients[2,1]  #extract Betas
paper_table$bw90[2] <-bw90.res$coefficients[2,2] #extract SE
paper_table$bw90[3]<-confint(bw90.mod, method="Wald",'ta90', level=0.95)[1] #for low CI
paper_table$bw90[4]<-confint(bw90.mod, method="Wald",'ta90', level=0.95)[2]  #for high
paper_table$bw90[5] <-bw90.res$coefficients[2,4] #tstat
paper_table$bw90[6] <-bw90.res$coefficients[2,5] #sig
#pm
paper_table$pm90[1] <- bw90.res$coefficients[3,1]  #extract Betas
paper_table$pm90[2] <-bw90.res$coefficients[3,2] #extract SE
paper_table$pm90[3]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$pm90[4]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[2]  #for high CI
paper_table$pm90[5] <-bw90.res$coefficients[3,4] #tstat
paper_table$pm90[6] <-bw90.res$coefficients[3,5] #sig


#### bw30
bw30.mod <- lmer(birthw ~ ta30+pm30+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw30.res<- summary(bw30.mod)

#add to table
paper_table$bw30<-0
paper_table$pm30<-0
#ta
paper_table$bw30[1] <- bw30.res$coefficients[2,1]  #extract Betas
paper_table$bw30[2] <-bw30.res$coefficients[2,2] #extract SE
paper_table$bw30[3]<-confint(bw30.mod, method="Wald",'ta30', level=0.95)[1] #for low CI
paper_table$bw30[4]<-confint(bw30.mod, method="Wald",'ta30', level=0.95)[2]  #for high
paper_table$bw30[5] <-bw30.res$coefficients[2,4] #tstat
paper_table$bw30[6] <-bw30.res$coefficients[2,5] #sig
#pm
paper_table$pm30[1] <- bw30.res$coefficients[3,1]  #extract Betas
paper_table$pm30[2] <-bw30.res$coefficients[3,2] #extract SE
paper_table$pm30[3]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[1] #for low CI
paper_table$pm30[4]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[2]  #for high CI
paper_table$pm30[5] <-bw30.res$coefficients[3,4] #tstat
paper_table$pm30[6] <-bw30.res$coefficients[3,5] #sig

#### bw14
bw14.mod <- lmer(birthw ~ ta14+pm14+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw14.res<- summary(bw14.mod)

#add to table
paper_table$bw14<-0
paper_table$pm14<-0
#ta
paper_table$bw14[1] <- bw14.res$coefficients[2,1]  #extract Betas
paper_table$bw14[2] <-bw14.res$coefficients[2,2] #extract SE
paper_table$bw14[3]<-confint(bw14.mod, method="Wald",'ta14', level=0.95)[1] #for low CI
paper_table$bw14[4]<-confint(bw14.mod, method="Wald",'ta14', level=0.95)[2]  #for high
paper_table$bw14[5] <-bw14.res$coefficients[2,4] #tstat
paper_table$bw14[6] <-bw14.res$coefficients[2,5] #sig
#pm
paper_table$pm14[1] <- bw14.res$coefficients[3,1]  #extract Betas
paper_table$pm14[2] <-bw14.res$coefficients[3,2] #extract SE
paper_table$pm14[3]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[1] #for low CI
paper_table$pm14[4]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[2]  #for high CI
paper_table$pm14[5] <-bw14.res$coefficients[3,4] #tstat
paper_table$pm14[6] <-bw14.res$coefficients[3,5] #sig


#### bw7
bw7.mod <- lmer(birthw ~ ta7+pm7+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw7.res<- summary(bw7.mod)

#add to table
paper_table$bw7<-0
paper_table$pm7<-0
#ta
paper_table$bw7[1] <- bw7.res$coefficients[2,1]  #extract Betas
paper_table$bw7[2] <-bw7.res$coefficients[2,2] #extract SE
paper_table$bw7[3]<-confint(bw7.mod, method="Wald",'ta7', level=0.95)[1] #for low CI
paper_table$bw7[4]<-confint(bw7.mod, method="Wald",'ta7', level=0.95)[2]  #for high
paper_table$bw7[5] <-bw7.res$coefficients[2,4] #tstat
paper_table$bw7[6] <-bw7.res$coefficients[2,5] #sig
#pm
paper_table$pm7[1] <- bw7.res$coefficients[3,1]  #extract Betas
paper_table$pm7[2] <-bw7.res$coefficients[3,2] #extract SE
paper_table$pm7[3]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[1] #for low CI
paper_table$pm7[4]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[2]  #for high CI
paper_table$pm7[5] <-bw7.res$coefficients[3,4] #tstat
paper_table$pm7[6] <-bw7.res$coefficients[3,5] #sig


#### bw3
bw3.mod <- lmer(birthw ~ ta3+pm3+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw3.res<- summary(bw3.mod)

#add to table
paper_table$bw3<-0
paper_table$pm3<-0
#ta
paper_table$bw3[1] <- bw3.res$coefficients[2,1]  #extract Betas
paper_table$bw3[2] <-bw3.res$coefficients[2,2] #extract SE
paper_table$bw3[3]<-confint(bw3.mod, method="Wald",'ta3', level=0.95)[1] #for low CI
paper_table$bw3[4]<-confint(bw3.mod, method="Wald",'ta3', level=0.95)[2]  #for high
paper_table$bw3[5] <-bw3.res$coefficients[2,4] #tstat
paper_table$bw3[6] <-bw3.res$coefficients[2,5] #sig
#pm
paper_table$pm3[1] <- bw3.res$coefficients[3,1]  #extract Betas
paper_table$pm3[2] <-bw3.res$coefficients[3,2] #extract SE
paper_table$pm3[3]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[1] #for low CI
paper_table$pm3[4]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[2]  #for high CI
paper_table$pm3[5] <-bw3.res$coefficients[3,4] #tstat
paper_table$pm3[6] <-bw3.res$coefficients[3,5] #sig


#### bw1
bw1.mod <- lmer(birthw ~ ta1+pm1+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw1.res<- summary(bw1.mod)

#add to table
paper_table$bw1<-0
paper_table$pm1<-0
#ta
paper_table$bw1[1] <- bw1.res$coefficients[2,1]  #extract Betas
paper_table$bw1[2] <-bw1.res$coefficients[2,2] #extract SE
paper_table$bw1[3]<-confint(bw1.mod, method="Wald",'ta1', level=0.95)[1] #for low CI
paper_table$bw1[4]<-confint(bw1.mod, method="Wald",'ta1', level=0.95)[2]  #for high
paper_table$bw1[5] <-bw1.res$coefficients[2,4] #tstat
paper_table$bw1[6] <-bw1.res$coefficients[2,5] #sig
#pm
paper_table$pm1[1] <- bw1.res$coefficients[3,1]  #extract Betas
paper_table$pm1[2] <-bw1.res$coefficients[3,2] #extract SE
paper_table$pm1[3]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[1] #for low CI
paper_table$pm1[4]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[2]  #for high CI
paper_table$pm1[5] <-bw1.res$coefficients[3,4] #tstat
paper_table$pm1[6] <-bw1.res$coefficients[3,5] #sig


#### bw0
bw0.mod <- lmer(birthw ~ ta0+pm0+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw0.res<- summary(bw0.mod)

#add to table
paper_table$bw0<-0
paper_table$pm0<-0
#ta
paper_table$bw0[1] <- bw0.res$coefficients[2,1]  #extract Betas
paper_table$bw0[2] <-bw0.res$coefficients[2,2] #extract SE
paper_table$bw0[3]<-confint(bw0.mod, method="Wald",'ta0', level=0.95)[1] #for low CI
paper_table$bw0[4]<-confint(bw0.mod, method="Wald",'ta0', level=0.95)[2]  #for high
paper_table$bw0[5] <-bw0.res$coefficients[2,4] #tstat
paper_table$bw0[6] <-bw0.res$coefficients[2,5] #sig
#pm
paper_table$pm0[1] <- bw0.res$coefficients[3,1]  #extract Betas
paper_table$pm0[2] <-bw0.res$coefficients[3,2] #extract SE
paper_table$pm0[3]<-confint(bw0.mod, method="Wald",'pm0', level=0.95)[1] #for low CI
paper_table$pm0[4]<-confint(bw0.mod, method="Wald",'pm0', level=0.95)[2]  #for high CI
paper_table$pm0[5] <-bw0.res$coefficients[3,4] #tstat
paper_table$pm0[6] <-bw0.res$coefficients[3,5] #sig


#check with NCDC

######################## Birth weight and PM

#### bw270
#### bw270
bw270.mod <- lmer(birthw ~ ncdc270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw270.res<- summary(bw270.mod)
#add to table
paper_table$ncdcbw270<-0
paper_table$ncdcpm270<-0
#ta
paper_table$ncdcbw270[1] <- bw270.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw270[2] <-bw270.res$coefficients[2,2] #extract SE
paper_table$ncdcbw270[3]<-confint(bw270.mod, method="Wald",'ncdc270', level=0.95)[1] #for low CI
paper_table$ncdcbw270[4]<-confint(bw270.mod, method="Wald",'ncdc270', level=0.95)[2]  #for high
paper_table$ncdcbw270[5] <-bw270.res$coefficients[2,4] #tstat
paper_table$ncdcbw270[6] <-bw270.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm270[1] <- bw270.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm270[2] <-bw270.res$coefficients[3,2] #extract SE
paper_table$ncdcpm270[3]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$ncdcpm270[4]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[2]  #for high CI
paper_table$ncdcpm270[5] <-bw270.res$coefficients[3,4] #tstat
paper_table$ncdcpm270[6] <-bw270.res$coefficients[3,5] #sig


#### bw90
bw90.mod <- lmer(birthw ~ ncdc90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw90.res<- summary(bw90.mod)

#add to table
paper_table$ncdcbw90<-0
paper_table$ncdcpm90<-0
#ta
paper_table$ncdcbw90[1] <- bw90.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw90[2] <-bw90.res$coefficients[2,2] #extract SE
paper_table$ncdcbw90[3]<-confint(bw90.mod, method="Wald",'ncdc90', level=0.95)[1] #for low CI
paper_table$ncdcbw90[4]<-confint(bw90.mod, method="Wald",'ncdc90', level=0.95)[2]  #for high
paper_table$ncdcbw90[5] <-bw90.res$coefficients[2,4] #tstat
paper_table$ncdcbw90[6] <-bw90.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm90[1] <- bw90.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm90[2] <-bw90.res$coefficients[3,2] #extract SE
paper_table$ncdcpm90[3]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$ncdcpm90[4]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[2]  #for high CI
paper_table$ncdcpm90[5] <-bw90.res$coefficients[3,4] #tstat
paper_table$ncdcpm90[6] <-bw90.res$coefficients[3,5] #sig


#### bw30
bw30.mod <- lmer(birthw ~ ncdc30+pm30+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw30.res<- summary(bw30.mod)

#add to table
paper_table$ncdcbw30<-0
paper_table$ncdcpm30<-0
#ta
paper_table$ncdcbw30[1] <- bw30.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw30[2] <-bw30.res$coefficients[2,2] #extract SE
paper_table$ncdcbw30[3]<-confint(bw30.mod, method="Wald",'ncdc30', level=0.95)[1] #for low CI
paper_table$ncdcbw30[4]<-confint(bw30.mod, method="Wald",'ncdc30', level=0.95)[2]  #for high
paper_table$ncdcbw30[5] <-bw30.res$coefficients[2,4] #tstat
paper_table$ncdcbw30[6] <-bw30.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm30[1] <- bw30.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm30[2] <-bw30.res$coefficients[3,2] #extract SE
paper_table$ncdcpm30[3]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[1] #for low CI
paper_table$ncdcpm30[4]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[2]  #for high CI
paper_table$ncdcpm30[5] <-bw30.res$coefficients[3,4] #tstat
paper_table$ncdcpm30[6] <-bw30.res$coefficients[3,5] #sig

#### bw14
bw14.mod <- lmer(birthw ~ ncdc14+pm14+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw14.res<- summary(bw14.mod)

#add to table
paper_table$ncdcbw14<-0
paper_table$ncdcpm14<-0
#ta
paper_table$ncdcbw14[1] <- bw14.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw14[2] <-bw14.res$coefficients[2,2] #extract SE
paper_table$ncdcbw14[3]<-confint(bw14.mod, method="Wald",'ncdc14', level=0.95)[1] #for low CI
paper_table$ncdcbw14[4]<-confint(bw14.mod, method="Wald",'ncdc14', level=0.95)[2]  #for high
paper_table$ncdcbw14[5] <-bw14.res$coefficients[2,4] #tstat
paper_table$ncdcbw14[6] <-bw14.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm14[1] <- bw14.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm14[2] <-bw14.res$coefficients[3,2] #extract SE
paper_table$ncdcpm14[3]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[1] #for low CI
paper_table$ncdcpm14[4]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[2]  #for high CI
paper_table$ncdcpm14[5] <-bw14.res$coefficients[3,4] #tstat
paper_table$ncdcpm14[6] <-bw14.res$coefficients[3,5] #sig


#### bw7
bw7.mod <- lmer(birthw ~ ncdc7+pm7+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw7.res<- summary(bw7.mod)

#add to table
paper_table$ncdcbw7<-0
paper_table$ncdcpm7<-0
#ta
paper_table$ncdcbw7[1] <- bw7.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw7[2] <-bw7.res$coefficients[2,2] #extract SE
paper_table$ncdcbw7[3]<-confint(bw7.mod, method="Wald",'ncdc7', level=0.95)[1] #for low CI
paper_table$ncdcbw7[4]<-confint(bw7.mod, method="Wald",'ncdc7', level=0.95)[2]  #for high
paper_table$ncdcbw7[5] <-bw7.res$coefficients[2,4] #tstat
paper_table$ncdcbw7[6] <-bw7.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm7[1] <- bw7.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm7[2] <-bw7.res$coefficients[3,2] #extract SE
paper_table$ncdcpm7[3]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[1] #for low CI
paper_table$ncdcpm7[4]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[2]  #for high CI
paper_table$ncdcpm7[5] <-bw7.res$coefficients[3,4] #tstat
paper_table$ncdcpm7[6] <-bw7.res$coefficients[3,5] #sig


#### bw3
bw3.mod <- lmer(birthw ~ ncdc3+pm3+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw3.res<- summary(bw3.mod)

#add to table
paper_table$ncdcbw3<-0
paper_table$ncdcpm3<-0
#ta
paper_table$ncdcbw3[1] <- bw3.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw3[2] <-bw3.res$coefficients[2,2] #extract SE
paper_table$ncdcbw3[3]<-confint(bw3.mod, method="Wald",'ncdc3', level=0.95)[1] #for low CI
paper_table$ncdcbw3[4]<-confint(bw3.mod, method="Wald",'ncdc3', level=0.95)[2]  #for high
paper_table$ncdcbw3[5] <-bw3.res$coefficients[2,4] #tstat
paper_table$ncdcbw3[6] <-bw3.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm3[1] <- bw3.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm3[2] <-bw3.res$coefficients[3,2] #extract SE
paper_table$ncdcpm3[3]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[1] #for low CI
paper_table$ncdcpm3[4]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[2]  #for high CI
paper_table$ncdcpm3[5] <-bw3.res$coefficients[3,4] #tstat
paper_table$ncdcpm3[6] <-bw3.res$coefficients[3,5] #sig


#### bw1
bw1.mod <- lmer(birthw ~ ncdc1+pm1+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw1.res<- summary(bw1.mod)

#add to table
paper_table$ncdcbw1<-0
paper_table$ncdcpm1<-0
#ta
paper_table$ncdcbw1[1] <- bw1.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw1[2] <-bw1.res$coefficients[2,2] #extract SE
paper_table$ncdcbw1[3]<-confint(bw1.mod, method="Wald",'ncdc1', level=0.95)[1] #for low CI
paper_table$ncdcbw1[4]<-confint(bw1.mod, method="Wald",'ncdc1', level=0.95)[2]  #for high
paper_table$ncdcbw1[5] <-bw1.res$coefficients[2,4] #tstat
paper_table$ncdcbw1[6] <-bw1.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm1[1] <- bw1.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm1[2] <-bw1.res$coefficients[3,2] #extract SE
paper_table$ncdcpm1[3]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[1] #for low CI
paper_table$ncdcpm1[4]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[2]  #for high CI
paper_table$ncdcpm1[5] <-bw1.res$coefficients[3,4] #tstat
paper_table$ncdcpm1[6] <-bw1.res$coefficients[3,5] #sig



#### bw1
bw1.mod <- lmer(birthw ~ ncdc0+pm0+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ med_income+ p_ospace+diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
bw1.res<- summary(bw1.mod)

#add to table
paper_table$ncdcbw0<-0
paper_table$ncdcpm0<-0
#ta
paper_table$ncdcbw0[1] <- bw1.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw0[2] <-bw1.res$coefficients[2,2] #extract SE
paper_table$ncdcbw0[3]<-confint(bw1.mod, method="Wald",'ncdc0', level=0.95)[1] #for low CI
paper_table$ncdcbw0[4]<-confint(bw1.mod, method="Wald",'ncdc0', level=0.95)[2]  #for high
paper_table$ncdcbw0[5] <-bw1.res$coefficients[2,4] #tstat
paper_table$ncdcbw0[6] <-bw1.res$coefficients[2,5] #sig
#pm
paper_table$ncdcpm0[1] <- bw1.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm0[2] <-bw1.res$coefficients[3,2] #extract SE
paper_table$ncdcpm0[3]<-confint(bw1.mod, method="Wald",'pm0', level=0.95)[1] #for low CI
paper_table$ncdcpm0[4]<-confint(bw1.mod, method="Wald",'pm0', level=0.95)[2]  #for high CI
paper_table$ncdcpm0[5] <-bw1.res$coefficients[3,4] #tstat
paper_table$ncdcpm0[6] <-bw1.res$coefficients[3,5] #sig














###crude results
#270
crude270.mod <- lm(birthw ~ ta270,data =  bd)
paper_table$bw.crude.270<-0
#ta
paper_table$bw.crude.270[1] <- round(summary(crude270.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.270[2] <-summary(crude270.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.270[3]<-confint(crude270.mod)[2,1] #for low CI
paper_table$bw.crude.270[4]<-confint(crude270.mod)[2,2]  #for high
paper_table$bw.crude.270[5] <-summary(crude270.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.270[6] <-summary(crude270.mod)$coefficients[2,4] #sig
paper_table$bw.crude.270<-round(paper_table$bw.crude.270,4)

#90
crude90.mod <- lm(birthw ~ ta90,data =  bd)
paper_table$bw.crude.90<-0
#ta
paper_table$bw.crude.90[1] <- round(summary(crude90.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.90[2] <-summary(crude90.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.90[3]<-confint(crude90.mod)[2,1] #for low CI
paper_table$bw.crude.90[4]<-confint(crude90.mod)[2,2]  #for high
paper_table$bw.crude.90[5] <-summary(crude90.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.90[6] <-summary(crude90.mod)$coefficients[2,4] #sig
paper_table$bw.crude.90<-round(paper_table$bw.crude.90,4)



#30
crude30.mod <- lm(birthw ~ ta30,data =  bd)
paper_table$bw.crude.30<-0
#ta
paper_table$bw.crude.30[1] <- round(summary(crude30.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.30[2] <-summary(crude30.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.30[3]<-confint(crude30.mod)[2,1] #for low CI
paper_table$bw.crude.30[4]<-confint(crude30.mod)[2,2]  #for high
paper_table$bw.crude.30[5] <-summary(crude30.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.30[6] <-summary(crude30.mod)$coefficients[2,4] #sig
paper_table$bw.crude.30<-round(paper_table$bw.crude.30,4)

#14
crude14.mod <- lm(birthw ~ ta14,data =  bd)
paper_table$bw.crude.14<-0
#ta
paper_table$bw.crude.14[1] <- round(summary(crude14.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.14[2] <-summary(crude14.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.14[3]<-confint(crude14.mod)[2,1] #for low CI
paper_table$bw.crude.14[4]<-confint(crude14.mod)[2,2]  #for high
paper_table$bw.crude.14[5] <-summary(crude14.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.14[6] <-summary(crude14.mod)$coefficients[2,4] #sig
paper_table$bw.crude.14<-round(paper_table$bw.crude.14,4)

#7
crude7.mod <- lm(birthw ~ ta7,data =  bd)
paper_table$bw.crude.7<-0
#ta
paper_table$bw.crude.7[1] <- round(summary(crude7.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.7[2] <-summary(crude7.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.7[3]<-confint(crude7.mod)[2,1] #for low CI
paper_table$bw.crude.7[4]<-confint(crude7.mod)[2,2]  #for high
paper_table$bw.crude.7[5] <-summary(crude7.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.7[6] <-summary(crude7.mod)$coefficients[2,4] #sig
paper_table$bw.crude.7<-round(paper_table$bw.crude.7,4)

#3
crude3.mod <- lm(birthw ~ ta3,data =  bd)
paper_table$bw.crude.3<-0
#ta
paper_table$bw.crude.3[1] <- round(summary(crude3.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.3[2] <-summary(crude3.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.3[3]<-confint(crude3.mod)[2,1] #for low CI
paper_table$bw.crude.3[4]<-confint(crude3.mod)[2,2]  #for high
paper_table$bw.crude.3[5] <-summary(crude3.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.3[6] <-summary(crude3.mod)$coefficients[2,4] #sig
paper_table$bw.crude.3<-round(paper_table$bw.crude.3,4)

#1
crude1.mod <- lm(birthw ~ ta1,data =  bd)
paper_table$bw.crude.1<-0
#ta
paper_table$bw.crude.1[1] <- round(summary(crude1.mod)$coefficients[2,1],3)  #extract Betas
paper_table$bw.crude.1[2] <-summary(crude1.mod)$coefficients[2,2] #extract SE
paper_table$bw.crude.1[3]<-confint(crude1.mod)[2,1] #for low CI
paper_table$bw.crude.1[4]<-confint(crude1.mod)[2,2]  #for high
paper_table$bw.crude.1[5] <-summary(crude1.mod)$coefficients[2,3] #tstat
paper_table$bw.crude.1[6] <-summary(crude1.mod)$coefficients[2,4] #sig
paper_table$bw.crude.1<-round(paper_table$bw.crude.1,4)


xod <- lm(birthw ~ IQRfintemp_l0,data =  bd)
summary(xod)
confint(xod)
######## preterm


pt270.mod<-(glmmPQL(NSGA ~ ta270+pm270+ +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))
pt270.res<- summary(pt270.mod)$tTable


#add to table
paper_table$pt.pt270<-0
paper_table$pt.pm270<-0
#ta
paper_table$pt.pt270[1] <- pt270.res[2,1]  #extract Betas
paper_table$pt.pt270[2] <-pt270.res[2,2] #extract SE
paper_table$pt.pt270[3]<-confint(pt270.mod, method="Wald",'ta270', level=0.95)[1] #for low CI
paper_table$pt.pt270[4]<-confint(pt270.mod, method="Wald",'ta270', level=0.95)[2]  #for high
paper_table$pt.pt270[5] <-pt270.res[2,4] #tstat
paper_table$pt.pt270[6] <-pt270.res[2,5] #sig
#pm
paper_table$pt.pm270[1] <- pt270.res[3,1]  #extract Betas
paper_table$pt.pm270[2] <-pt270.res[3,2] #extract SE
paper_table$pt.pm270[3]<-confint(pt270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$pt.pm270[4]<-confint(pt270.mod, method="Wald",'pm270', level=0.95)[2]  #for high CI
paper_table$pt.pm270[5] <-pt270.res[3,4] #tstat
paper_table$pt.pm270[6] <-pt270.res[3,5] #sig


write.csv(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/EHP_paper.csv")
