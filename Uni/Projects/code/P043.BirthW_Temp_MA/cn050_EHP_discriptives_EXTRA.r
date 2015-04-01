## load library

library(lme4)
library(lmTest)
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
library(psych)
library(pastecs)

## disable sci notations
options(scipen = 99)

#create results table
paper_table <- data.frame(object=character(8),bw270=numeric(8))
paper_table$object <- c("beta", "se", "LCI", "HCI", "tstat", "sig", "OR", "PH")

#2:473,997 - AFT,PT/LBW analysis
bd<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_22up.csv")
## Import
#1:453,658-birth weight analysis
bd<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_nocesv2.csv")





#create aodid
bd$res<-paste(bd$long,bd$lat,sep="-") 
bd$aodid<-paste(bd$glong,bd$glat,sep="-") 

#IQR for table 2
#IQR(bd$fintempmabirth)
#describe(bd$pmnew_l0)





#rename key variables
#ta model
setnames(bd,"IQRfintempmabirth","ta270")
setnames(bd,"IQRfintempma3month","ta90")
setnames(bd,"IQRfintempmamonth","ta30")
setnames(bd,"IQRfintempma2week","ta14")
setnames(bd,"IQRfintempmaweek","ta7")
setnames(bd,"IQRfintempma3","ta3")
setnames(bd,"IQRfintempma1","ta1")
#tdnc
setnames(bd,"IQRtncdcmabirth","ncdc270")
setnames(bd,"IQRtncdcma3month","ncdc90")
setnames(bd,"IQRtncdcmamonth","ncdc30")
setnames(bd,"IQRtncdcma2week","ncdc14")
setnames(bd,"IQRtncdcmaweek","ncdc7")
setnames(bd,"IQRtncdcma3","ncdc3")
setnames(bd,"IQRtncdcma1","ncdc1")
#PM
setnames(bd,"pmnewmabirth","pm270")
setnames(bd,"pmnewma3month","pm90")
setnames(bd,"pmnewmamonth","pm30")
setnames(bd,"pmnewma2week","pm14")
setnames(bd,"pmnewmaweek","pm7")
setnames(bd,"pmnewma3","pm3")
setnames(bd,"pmnewma1","pm1")
#preterm
setnames(bd,"NSGA","pterm")
setnames(bd,"medgrrenttr00","mgrossrent")


## dates
bd[, day:=as.Date(strptime(byob, "%m/%d/%y"))]
bd$rfpisga[bd$rfpisga== 9] <- NA
bd$rfpisga[bd$rfpisga== 2] <- 0

###recoding
#finer race
# bd<- bd[mother_race  == 1 , mrn.n  := 1] #white
# bd<- bd[mother_race  == 5 , mrn.n  := 2] #hispanic
# bd<- bd[mother_race  == 2 , mrn.n  := 3] #black
# bd<- bd[mother_race  == 3 , mrn.n  := 4] #asian
# bd<- bd[mother_race  == 4 , mrn.n  := 5] #other
# bd<- bd[mother_race  == 8 , mrn.n  := 5] #other
# bd<- bd[mother_race  == 9 , mrn.n  := 5] #other
#ptbirth

bd<- bd[mrace  == 1 , mrn.n  := 1] #white
bd<- bd[mrace  == 5 , mrn.n  := 2] #hispanic
bd<- bd[mrace  == 2 , mrn.n  := 3] #black
bd<- bd[mrace  == 3 , mrn.n  := 4] #asian
bd<- bd[mrace  == 4 , mrn.n  := 5] #other
bd<- bd[mrace  == 8 , mrn.n  := 5] #other
bd<- bd[mrace  == 9 , mrn.n  := 5] #other



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

dim(bd)

#birth discriptives (NOTE-RUN THIS TWICE EACH TIME SPERATLY FOR EACH BD FILE LOADED)

## AFT model

#delte missing data
summary(bd$mrn.n)
summary(bd$edu_group)
summary(bd$age)
summary(bd$diab)
summary(bd$diab_other)
summary(bd$prev_400)
summary(bd$hyper)
summary(bd$lungd)
summary(bd$prevpret)
summary(bd$ges_calc)
summary(bd$kotck)
summary(bd$med_income)
summary(bd$gender)
summary(bd$parity)
summary(bd$cig_pre)
summary(bd$cig_preg)





bd <- bd[!is.na(mrn.n) & !is.na(edu_group) & !is.na(edu_group) & !is.na(cig_pre) & !is.na(cig_preg) 
          & !is.na(med_income) & !is.na(p_ospace)  & !is.na(gender) & !is.na(prev_400)
                & !is.na(diab) & !is.na(diab_other) & !is.na(hyper) & !is.na(lungd) & !is.na(diab_other) & !is.na(prevpret)
                & !is.na(kess) & !is.na(byob) & !is.na(parity) & !is.na(age) & !is.na(ges_calc)
          , ]




#missing calculations


Hmisc::describe(bd$birthw)
sd(bd$birthw)

#race
Hmisc::describe(as.factor((bd$mrn.n)))
describe(bd$birthw[bd$mrn.n == 1])
describe(bd$birthw[bd$mrn.n == 2])
describe(bd$birthw[bd$mrn.n == 3])
describe(bd$birthw[bd$mrn.n == 4])
describe(bd$birthw[bd$mrn.n == 5])

#maternal education
Hmisc::describe(as.factor(bd$edu_group))
describe(bd$birthw[bd$edu_group == 1])
describe(bd$birthw[bd$edu_group == 2])
describe(bd$birthw[bd$edu_group == 3])
describe(bd$birthw[bd$edu_group == 4])


#age
bd<- bd[age  <= 20 , ageg  := 1] 
bd<- bd[age  >= 20 & age < 29 , ageg  := 2] 
bd<- bd[age  >= 29 & age < 34 , ageg  := 3] 
bd<- bd[age  >= 34 & age < 39 , ageg  := 4] 
bd<- bd[age  >= 39 , ageg  := 5] 

Hmisc::describe(as.factor((bd$ageg)))
describe(bd$birthw[bd$ageg == 1])
describe(bd$birthw[bd$ageg == 2])
describe(bd$birthw[bd$ageg == 3])
describe(bd$birthw[bd$ageg == 4])
describe(bd$birthw[bd$ageg == 5])

#maternal conditions
#
describe(bd$diab)
Hmisc::describe(as.factor(bd$diab))
describe(bd$birthw[bd$diab  ==1])
#
describe(bd$diab_other)
Hmisc::describe(as.factor(bd$diab_other))
describe(bd$birthw[bd$diab_other == 1])
#
describe(bd$prev_400)
Hmisc::describe(as.factor(bd$prev_400))
describe(bd$birthw[bd$prev_400 == 1])
#
describe(bd$hyper) 
Hmisc::describe(as.factor(bd$hyper))
describe(bd$birthw[bd$hyper == 1])
#
describe(bd$lungd)
Hmisc::describe(as.factor(bd$lungd))
describe(bd$birthw[bd$lungd == 1])
#
describe(bd$prevpret)
Hmisc::describe(as.factor(bd$prevpret))
describe(bd$birthw[bd$prevpret == 1])
#
describe(bd$ges_calc)
#APNCU
Hmisc::describe(as.factor(bd$kotck))
describe(bd$birthw[bd$kotck == 1])
describe(bd$birthw[bd$kotck == 2])
describe(bd$birthw[bd$kotck == 3])
describe(bd$birthw[bd$kotck == 4])
#inc
describe(bd$med_income)
#gender
describe(bd$gender) 
describe(bd$birthw[bd$gender == 1])
describe(bd$birthw[bd$gender == 2])
#other
describe(bd$parity)
describe(bd$cig_pre)
describe(bd$cig_preg)
describe(bd$tden)
describe(bd$p_ospace)  
describe(bd$elev) 
#Seasons
library(car)
#for bw db
#bd[, day:=as.Date(strptime(bdob, "%Y-%m-%d"))]
#for AFT db
bd[, day:=as.Date(strptime(bdob, "%m/%d/%y"))]
bd$yr <- as.numeric(format(bd$day, "%Y"))
describe(bd$yr)
bd$month <- as.numeric(format(bd$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
bd$season<-recode(bd$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
Hmisc::describe(as.factor(bd$season))
describe(bd$birthw[bd$season == 1])
describe(bd$birthw[bd$season == 2])
describe(bd$birthw[bd$season == 3])
describe(bd$birthw[bd$season == 4])





# describe(bd$ta270)
# describe(bd$pm270)
# describe(bd$sinetime)
# describe(bd$costime) 
#describe(bd$age)
# describe(bd$age_centered_sq)


### SES correlations

cor(bd$ta270,bd$med_income)
cor(bd$ta270,bd$myredu)
cor(bd$ta270,bd$mother_race)


#birth discriptives (NOTE-RUN THIS TWICE EACH TIME SPERATLY FOR EACH BD FILE LOADED)


## Birth weight model
dim(bd)
#delte missing data
summary(bd$mrn.n)
summary(bd$edu_group)
summary(bd$age)
summary(bd$diab)
summary(bd$diab_other)
summary(bd$prev_400)
summary(bd$hyper)
summary(bd$lungd)
summary(bd$prevpret)
summary(bd$ges_calc)
summary(bd$kotck)
summary(bd$med_income)
summary(bd$gender)
summary(bd$parity)
summary(bd$cig_pre)
summary(bd$cig_preg)
#full before clean 453658




bd <- bd[!is.na(mrn.n) & !is.na(edu_group) & !is.na(edu_group) & !is.na(cig_pre) & !is.na(cig_preg) 
          & !is.na(med_income) & !is.na(p_ospace)  & !is.na(gender) & !is.na(prev_400)
                & !is.na(diab) & !is.na(diab_other) & !is.na(hyper) & !is.na(lungd) & !is.na(diab_other) & !is.na(prevpret)
                & !is.na(kess) & !is.na(byob) & !is.na(parity) & !is.na(age) & !is.na(ges_calc)
          , ]




#missing calculations


Hmisc::describe(bd$birthw)
sd(bd$birthw)

#race
Hmisc::describe(as.factor((bd$mrn.n)))
describe(bd$birthw[bd$mrn.n == 1])
describe(bd$birthw[bd$mrn.n == 2])
describe(bd$birthw[bd$mrn.n == 3])
describe(bd$birthw[bd$mrn.n == 4])
describe(bd$birthw[bd$mrn.n == 5])

#maternal education
Hmisc::describe(as.factor(bd$edu_group))
describe(bd$birthw[bd$edu_group == 1])
describe(bd$birthw[bd$edu_group == 2])
describe(bd$birthw[bd$edu_group == 3])
describe(bd$birthw[bd$edu_group == 4])


#age
bd<- bd[age  <= 20 , ageg  := 1] 
bd<- bd[age  >= 20 & age < 29 , ageg  := 2] 
bd<- bd[age  >= 29 & age < 34 , ageg  := 3] 
bd<- bd[age  >= 34 & age < 39 , ageg  := 4] 
bd<- bd[age  >= 39 , ageg  := 5] 

Hmisc::describe(as.factor((bd$ageg)))
describe(bd$birthw[bd$ageg == 1])
describe(bd$birthw[bd$ageg == 2])
describe(bd$birthw[bd$ageg == 3])
describe(bd$birthw[bd$ageg == 4])
describe(bd$birthw[bd$ageg == 5])

#maternal conditions
#
describe(bd$diab)
Hmisc::describe(as.factor(bd$diab))
describe(bd$birthw[bd$diab  ==1])
#
describe(bd$diab_other)
Hmisc::describe(as.factor(bd$diab_other))
describe(bd$birthw[bd$diab_other == 1])
#
describe(bd$prev_400)
Hmisc::describe(as.factor(bd$prev_400))
describe(bd$birthw[bd$prev_400 == 1])
#
describe(bd$hyper) 
Hmisc::describe(as.factor(bd$hyper))
describe(bd$birthw[bd$hyper == 1])
#
describe(bd$lungd)
Hmisc::describe(as.factor(bd$lungd))
describe(bd$birthw[bd$lungd == 1])
#
describe(bd$prevpret)
Hmisc::describe(as.factor(bd$prevpret))
describe(bd$birthw[bd$prevpret == 1])
#
describe(bd$ges_calc)
#APNCU
Hmisc::describe(as.factor(bd$kotck))
describe(bd$birthw[bd$kotck == 1])
describe(bd$birthw[bd$kotck == 2])
describe(bd$birthw[bd$kotck == 3])
describe(bd$birthw[bd$kotck == 4])
#inc
describe(bd$med_income)
#gender
describe(bd$gender) 
describe(bd$birthw[bd$gender == 1])
describe(bd$birthw[bd$gender == 2])
#other
describe(bd$parity)
describe(bd$cig_pre)
describe(bd$cig_preg)
describe(bd$tden)
describe(bd$p_ospace)  
describe(bd$elev) 
#Seasons
library(car)
#for bw db
#bd[, day:=as.Date(strptime(bdob, "%Y-%m-%d"))]
#for AFT db
bd[, day:=as.Date(strptime(bdob, "%m/%d/%y"))]
bd$yr <- as.numeric(format(bd$day, "%Y"))
describe(bd$yr)
bd$month <- as.numeric(format(bd$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
bd$season<-recode(bd$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
Hmisc::describe(as.factor(bd$season))
describe(bd$birthw[bd$season == 1])
describe(bd$birthw[bd$season == 2])
describe(bd$birthw[bd$season == 3])
describe(bd$birthw[bd$season == 4])





# describe(bd$ta270)
# describe(bd$pm270)
# describe(bd$sinetime)
# describe(bd$costime) 
#describe(bd$age)
# describe(bd$age_centered_sq)


### SES correlations

cor(bd$ta270,bd$med_income)
cor(bd$ta270,bd$myredu)
cor(bd$ta270,bd$mother_race)
