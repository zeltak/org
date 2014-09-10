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
## disable sci notations
options(scipen = 99)

#create results table
paper_table <- data.frame(object=character(8),bw270=numeric(8))
paper_table$object <- c("beta", "se", "LCI", "HCI", "tstat", "sig", "OR", "PH")



## Import
bd<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_nocesv2.csv")
names(bd)
dim(bd)
#  [1] "bthctyn"              "bthctyc"              "sex"                  "plur"                 "bthord"               "byob"                
#   [7] "bmob"                 "bdayob"               "bdob"                 "myob"                 "mmob"                 "mbpstt"              
#  [13] "mbpstc"               "mresctn"              "mresctc"              "mrescnn"              "mrescnc"              "marstat"             
#  [19] "fbpstt"               "fbpstc"               "fage"                 "mrace"                "methn"                "mlangp"              
#  [25] "medu"                 "frace"                "fethn"                "flangp"               "fedu"                 "lbnl"                
#  [31] "lbnd"                 "yrllb"                "mtllb"                "dyllb"                "dtllb"                "parit"               
#  [37] "gravid"               "gacalc"               "clinega"              "yrfpncv"              "mtfpncv"              "dyfpncv"             
#  [43] "dtfpncv"              "mpncp"                "npncv"                "pncgov"               "kess"                 "kotck"               
#  [49] "cigdpp"               "cigddp"               "mwgl"                 "modvag"               "modvbac"              "modfor"              
#  [55] "modvac"               "modpcs"               "modrcs"               "bwg"                  "apgar1"               "apgar5"              
#  [61] "breast"               "disyr"                "dismt"                "disdy"                "disdt"                "distim"              
#  [67] "dhwmot"               "balive"               "rflungd"              "rfanem"               "rfcard"               "rfdiabg"             
#  [73] "rfdiabo"              "rfhypc"               "rfhyppr"              "rfincer"              "rfpiwbd"              "rfpi4k"              
#  [79] "rfpisga"              "rfrenal"              "rfrhsen"              "rfsickl"              "rfutbld"              "avnlt30"             
#  [85] "avnge30"              "jaund"                "mocode"               "micode"               "focode"               "ficode"              
#  [91] "tract"                "long"                 "lat"                  "yrod"                 "mtod"                 "dyod"                
#  [97] "dtod"                 "randint"              "duprandint"           "uniqueid"             "uniqueid_y"           "myredu"              
# [103] "gridadt"              "FIPS"                 "medhhinctr"           "pctreccono"           "date"                 "xx"                  
# [109] "yy"                   "stn"                  "guid"                 "glong"                "glat"                 "fintemp"             
# [115] "fintemp_l0"           "fintemp_l1"           "fintemp_l2"           "fintemp_l3"           "fintempma1"           "fintempma3"          
# [121] "fintempmaweek"        "fintempma2week"       "fintempma3week"       "fintempmamonth"       "fintempma3month"      "fintempmabirth"      
# [127] "fintempmayear"        "gender1"              "mother_race"          "father_race"          "csect1"               "birthw"              
# [133] "prevpre"              "age"                  "med_income"           "p_ospace"             "fage1"                "frace1"              
# [139] "fethn1"               "fedu1"                "flangp1"              "mlangp1"              "methn1"               "marstat1"            
# [145] "mbpstc1"              "gravid1"              "mpncp1"               "pncgov1"              "methnic"              "fethnic"             
# [151] "gender"               "pre_vists"            "parity"               "prevpret"             "cig_pre"              "cig_preg"            
# [157] "cig_preq"             "cig_pregq"            "ges_calc"             "ges_clinic"           "lges"                 "lgescl"              
# [163] "csect"                "utbleed"              "lungd"                "renal"                "hyper_other"          "hyper"               
# [169] "diab"                 "diab_other"           "prev_400"             "prev_sga"             "MRN"                  "lbw"                 
# [175] "m"                    "season"               "npar"                 "year"                 "age_centered"         "age_centered_sq"     
# [181] "edu_group"            "adtmean"              "med_incomeq"          "f_age"                "f_age_centered"       "f_age_centered_sq"   
# [187] "f_race"               "FRN"                  "f_lang"               "p_birth"              "m_lang"               "mstat"               
# [193] "m_care"               "bw"                   "plural"               "pcare"                "aged"                 "pctwhttr00"          
# [199] "pctblktr00"           "pctasiantr00"         "pctothracetr00"       "pct2ormoreracetr00"   "pctwhtnhtr00"         "pctblknhtr00"        
# [205] "pctasiannhtr00"       "pctothracenhtr00"     "pct2ormoreracenhtr00" "pcthisptr00"          "pctnonwhttr00"        "medhhinctr00"        
# [211] "pctfamfemhdtr00"      "pctcrowdtr00"         "pcthsdropouttr00"     "pct16_64mntemptr00"   "pctlowinctr00"        "pcthiinctr00"        
# [217] "pctbelpovlt18tr00"    "pctbelpov18_64tr00"   "pctbelpovgt64tr00"    "pctbelpovtr00"        "pctnohstr00"          "pcthstr00"           
# [223] "pctbachtr00"          "pctadvdegtr00"        "pctbachorhighertr00"  "sevdisnbhdpov"        "sevdisnbhdfam"        "sevdisnbhdhs"        
# [229] "sevdisnbhdemp"        "sevdisnhbdscoretr00"  "pctocchutr00"         "pctownerocctr00"      "mgrossrent"           "medvalspecownoctr00" 
# [235] "TEMP_Cma1"            "TEMP_Cma3"            "TEMP_Cmaweek"         "TEMP_Cma2week"        "TEMP_Cmamonth"        "TEMP_Cma3month"      
# [241] "TEMP_Cmabirth"        "TEMP_Cmayear"         "tncdc"                "tncdc_l0"             "tncdc_l1"             "tncdc_l2"            
# [247] "tncdc_l3"             "tncdcma1"             "tncdcma3"             "tncdcmaweek"          "tncdcma2week"         "tncdcmamonth"        
# [253] "tncdcma3month"        "tncdcmabirth"         "tncdcmayear"          "dow"                  "doy"                  "doy2"                
# [259] "sinetime"             "costime"              "newvar"               "udate"                "devtemp"              "findevtemp"          
# [265] "findevtemplag3"       "IQRfintemp"           "IQRfintemp_l0"        "IQRfintemp_l1"        "IQRfintemp_l2"        "IQRfintemp_l3"       
# [271] "ta1"                  "ta3"                  "ta7"                  "ta14"                 "IQRfintempma3week"    "ta30"                
# [277] "ta90"                 "ta270"                "IQRtncdc"             "IQRtncdc_l0"          "IQRtncdc_l1"          "IQRtncdc_l2"         
# [283] "IQRtncdc_l3"          "ncdc1"                "ncdc3"                "ncdc7"                "ncdc14"               "ncdc30"              
# [289] "ncdc90"               "ncdc270"              "bwcat"                "edu01"                "mrn01"                "minc01"              
# [295] "fedu01"               "fmrn01"               "fminc01"              "lowbw"                "pterm"                "pmnew"               
# [301] "pmnew_l0"             "pmnew_l1"             "pmnew_l2"             "pmnew_l3"             "pm1"                  "pm3"                 
# [307] "pm7"                  "pm14"                 "pm30"                 "pm90"                 "pm270"                "pm12_24"             
# [313] "popden"               "pcturban"             "elev"                 "tden"                 "dist_A1"              "dist_pemis"          
# [319] "localpm"              "day"                  "mrn.n"                "mid"                  "sga"                  "sgaq"     



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

dim(bd)

library(psych)
library(pastecs)

# describe(bd$ta270)
# describe(bd$pm270)
# describe(bd$sinetime)
# describe(bd$costime) 
# describe(bd$age_centered)
# describe(bd$age_centered_sq)
Hmisc::describe(bd$birthw)
describe(bd$cig_pre)
describe(bd$cig_preg)
describe(bd$tden)
describe(bd$med_income)
describe(bd$p_ospace)  
describe(bd$cig_pre)
describe(bd$gender)  
describe(bd$prev_400)
describe(bd$diab)
describe(bd$hyper) 
describe(bd$lungd)
describe(bd$diab_other)
describe(bd$prevpret)
stat.desc(bd$kess)  
describe(bd$mrn.n)
Hmisc::describe(as.factor(bd$edu_group))
describe(bd$byob)     
describe(bd$parity)
Hmisc::describe(as.factor((bd$mrn.n))
describe(bd$ges_calc)
describe(bd$elev)
describe(bd$parity)

#season

#Seasons
library(car)
bd$month <- as.numeric(format(bd$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
bd$season<-recode(bd$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")

Hmisc::describe(as.factor(bd$season))
describe(bd$birthw[bd$season == 1])
describe(bd$birthw[bd$season == 2])
describe(bd$birthw[bd$season == 3])
describe(bd$birthw[bd$season == 4])





Hmisc::describe(as.factor(bd$diab_other))
describe(bd$birthw[bd$diab_other == 1])

Hmisc::describe(as.factor(bd$diab))
describe(bd$birthw[bd$diab == 1])

Hmisc::describe(as.factor(bd$prev_400))
describe(bd$birthw[bd$prev_400 == 1])

Hmisc::describe(as.factor(bd$prevpret))
describe(bd$birthw[bd$prevpret == 1])

Hmisc::describe(as.factor(bd$hyper))
describe(bd$birthw[bd$hyper == 1])

Hmisc::describe(as.factor(bd$lungd))
describe(bd$birthw[bd$lungd == 1])

Hmisc::describe(as.factor(bd$kotck))
describe(bd$birthw[bd$kotck == 1])
describe(bd$birthw[bd$kotck == 2])
describe(bd$birthw[bd$kotck == 3])
describe(bd$birthw[bd$kotck == 4])



  


### SES correlations

cor(bd$ta270,bd$med_income)
cor(bd$ta270,bd$myredu)
cor(bd$ta270,bd$mother_race)





########## paper

######################## Birth weight and PM

#### bw270
#### bw270
bw270.mod <- lm(birthw ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw270.res<- summary(bw270.mod)
#add to table
paper_table$bw270<-0
paper_table$pm270<-0
#ta
paper_table$bw270[1] <- bw270.res$coefficients[2,1]  #extract Betas
paper_table$bw270[2] <-bw270.res$coefficients[2,2] #extract SE
paper_table$bw270[3]<-confint(bw270.mod, method="Wald",'ta270', level=0.95)[1] #for low CI
paper_table$bw270[4]<-confint(bw270.mod, method="Wald",'ta270', level=0.95)[2]  #for high
paper_table$bw270[5] <-bw270.res$coefficients[2,3] #tstat
paper_table$bw270[6] <-bw270.res$coefficients[2,4] #sig
#pm
paper_table$pm270[1] <- bw270.res$coefficients[3,1]  #extract Betas
paper_table$pm270[2] <-bw270.res$coefficients[3,2] #extract SE
paper_table$pm270[3]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$pm270[4]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[2]  #for high CI
paper_table$pm270[5] <-bw270.res$coefficients[3,3] #tstat
paper_table$pm270[6] <-bw270.res$coefficients[3,4] #sig


#### bw90
bw90.mod <- lm(birthw ~ ta90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw90.res<- summary(bw90.mod)

#add to table
paper_table$bw90<-0
paper_table$pm90<-0
#ta
paper_table$bw90[1] <- bw90.res$coefficients[2,1]  #extract Betas
paper_table$bw90[2] <-bw90.res$coefficients[2,2] #extract SE
paper_table$bw90[3]<-confint(bw90.mod, method="Wald",'ta90', level=0.95)[1] #for low CI
paper_table$bw90[4]<-confint(bw90.mod, method="Wald",'ta90', level=0.95)[2]  #for high
paper_table$bw90[5] <-bw90.res$coefficients[2,3] #tstat
paper_table$bw90[6] <-bw90.res$coefficients[2,4] #sig
#pm
paper_table$pm90[1] <- bw90.res$coefficients[3,1]  #extract Betas
paper_table$pm90[2] <-bw90.res$coefficients[3,2] #extract SE
paper_table$pm90[3]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$pm90[4]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[2]  #for high CI
paper_table$pm90[5] <-bw90.res$coefficients[3,3] #tstat
paper_table$pm90[6] <-bw90.res$coefficients[3,4] #sig


#### bw30
bw30.mod <- lm(birthw ~ ta30+pm30+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw30.res<- summary(bw30.mod)

#add to table
paper_table$bw30<-0
paper_table$pm30<-0
#ta
paper_table$bw30[1] <- bw30.res$coefficients[2,1]  #extract Betas
paper_table$bw30[2] <-bw30.res$coefficients[2,2] #extract SE
paper_table$bw30[3]<-confint(bw30.mod, method="Wald",'ta30', level=0.95)[1] #for low CI
paper_table$bw30[4]<-confint(bw30.mod, method="Wald",'ta30', level=0.95)[2]  #for high
paper_table$bw30[5] <-bw30.res$coefficients[2,3] #tstat
paper_table$bw30[6] <-bw30.res$coefficients[2,4] #sig
#pm
paper_table$pm30[1] <- bw30.res$coefficients[3,1]  #extract Betas
paper_table$pm30[2] <-bw30.res$coefficients[3,2] #extract SE
paper_table$pm30[3]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[1] #for low CI
paper_table$pm30[4]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[2]  #for high CI
paper_table$pm30[5] <-bw30.res$coefficients[3,3] #tstat
paper_table$pm30[6] <-bw30.res$coefficients[3,4] #sig

#### bw14
bw14.mod <- lm(birthw ~ ta14+pm14+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw14.res<- summary(bw14.mod)

#add to table
paper_table$bw14<-0
paper_table$pm14<-0
#ta
paper_table$bw14[1] <- bw14.res$coefficients[2,1]  #extract Betas
paper_table$bw14[2] <-bw14.res$coefficients[2,2] #extract SE
paper_table$bw14[3]<-confint(bw14.mod, method="Wald",'ta14', level=0.95)[1] #for low CI
paper_table$bw14[4]<-confint(bw14.mod, method="Wald",'ta14', level=0.95)[2]  #for high
paper_table$bw14[5] <-bw14.res$coefficients[2,3] #tstat
paper_table$bw14[6] <-bw14.res$coefficients[2,4] #sig
#pm
paper_table$pm14[1] <- bw14.res$coefficients[3,1]  #extract Betas
paper_table$pm14[2] <-bw14.res$coefficients[3,2] #extract SE
paper_table$pm14[3]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[1] #for low CI
paper_table$pm14[4]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[2]  #for high CI
paper_table$pm14[5] <-bw14.res$coefficients[3,3] #tstat
paper_table$pm14[6] <-bw14.res$coefficients[3,4] #sig


#### bw7
bw7.mod <- lm(birthw ~ ta7+pm7+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw7.res<- summary(bw7.mod)

#add to table
paper_table$bw7<-0
paper_table$pm7<-0
#ta
paper_table$bw7[1] <- bw7.res$coefficients[2,1]  #extract Betas
paper_table$bw7[2] <-bw7.res$coefficients[2,2] #extract SE
paper_table$bw7[3]<-confint(bw7.mod, method="Wald",'ta7', level=0.95)[1] #for low CI
paper_table$bw7[4]<-confint(bw7.mod, method="Wald",'ta7', level=0.95)[2]  #for high
paper_table$bw7[5] <-bw7.res$coefficients[2,3] #tstat
paper_table$bw7[6] <-bw7.res$coefficients[2,4] #sig
#pm
paper_table$pm7[1] <- bw7.res$coefficients[3,1]  #extract Betas
paper_table$pm7[2] <-bw7.res$coefficients[3,2] #extract SE
paper_table$pm7[3]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[1] #for low CI
paper_table$pm7[4]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[2]  #for high CI
paper_table$pm7[5] <-bw7.res$coefficients[3,3] #tstat
paper_table$pm7[6] <-bw7.res$coefficients[3,4] #sig


#### bw3
bw3.mod <- lm(birthw ~ ta3+pm3+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw3.res<- summary(bw3.mod)

#add to table
paper_table$bw3<-0
paper_table$pm3<-0
#ta
paper_table$bw3[1] <- bw3.res$coefficients[2,1]  #extract Betas
paper_table$bw3[2] <-bw3.res$coefficients[2,2] #extract SE
paper_table$bw3[3]<-confint(bw3.mod, method="Wald",'ta3', level=0.95)[1] #for low CI
paper_table$bw3[4]<-confint(bw3.mod, method="Wald",'ta3', level=0.95)[2]  #for high
paper_table$bw3[5] <-bw3.res$coefficients[2,3] #tstat
paper_table$bw3[6] <-bw3.res$coefficients[2,4] #sig
#pm
paper_table$pm3[1] <- bw3.res$coefficients[3,1]  #extract Betas
paper_table$pm3[2] <-bw3.res$coefficients[3,2] #extract SE
paper_table$pm3[3]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[1] #for low CI
paper_table$pm3[4]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[2]  #for high CI
paper_table$pm3[5] <-bw3.res$coefficients[3,3] #tstat
paper_table$pm3[6] <-bw3.res$coefficients[3,4] #sig


#### bw1
bw1.mod <- lm(birthw ~ ta1+pm1+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw1.res<- summary(bw1.mod)

#add to table
paper_table$bw1<-0
paper_table$pm1<-0
#ta
paper_table$bw1[1] <- bw1.res$coefficients[2,1]  #extract Betas
paper_table$bw1[2] <-bw1.res$coefficients[2,2] #extract SE
paper_table$bw1[3]<-confint(bw1.mod, method="Wald",'ta1', level=0.95)[1] #for low CI
paper_table$bw1[4]<-confint(bw1.mod, method="Wald",'ta1', level=0.95)[2]  #for high
paper_table$bw1[5] <-bw1.res$coefficients[2,3] #tstat
paper_table$bw1[6] <-bw1.res$coefficients[2,4] #sig
#pm
paper_table$pm1[1] <- bw1.res$coefficients[3,1]  #extract Betas
paper_table$pm1[2] <-bw1.res$coefficients[3,2] #extract SE
paper_table$pm1[3]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[1] #for low CI
paper_table$pm1[4]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[2]  #for high CI
paper_table$pm1[5] <-bw1.res$coefficients[3,3] #tstat
paper_table$pm1[6] <-bw1.res$coefficients[3,4] #sig

saveRDS(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.rds")

# save(list=c("bw270.res","bw90.res","bw30.res","bw14.res","bw7.res","bw1.res","bw3.res"), file="/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwres.rda")


#check with NCDC

######################## Birth weight and PM

#### bw270
#### bw270
bw270.mod <- lm(birthw ~ ncdc270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw270.res<- summary(bw270.mod)
#add to table
paper_table$ncdcbw270<-0
paper_table$ncdcpm270<-0
#ta
paper_table$ncdcbw270[1] <- bw270.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw270[2] <-bw270.res$coefficients[2,2] #extract SE
paper_table$ncdcbw270[3]<-confint(bw270.mod, method="Wald",'ncdc270', level=0.95)[1] #for low CI
paper_table$ncdcbw270[4]<-confint(bw270.mod, method="Wald",'ncdc270', level=0.95)[2]  #for high
paper_table$ncdcbw270[5] <-bw270.res$coefficients[2,3] #tstat
paper_table$ncdcbw270[6] <-bw270.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm270[1] <- bw270.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm270[2] <-bw270.res$coefficients[3,2] #extract SE
paper_table$ncdcpm270[3]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$ncdcpm270[4]<-confint(bw270.mod, method="Wald",'pm270', level=0.95)[2]  #for high CI
paper_table$ncdcpm270[5] <-bw270.res$coefficients[3,3] #tstat
paper_table$ncdcpm270[6] <-bw270.res$coefficients[3,4] #sig


#### bw90
bw90.mod <- lm(birthw ~ ncdc90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw90.res<- summary(bw90.mod)

#add to table
paper_table$ncdcbw90<-0
paper_table$ncdcpm90<-0
#ta
paper_table$ncdcbw90[1] <- bw90.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw90[2] <-bw90.res$coefficients[2,2] #extract SE
paper_table$ncdcbw90[3]<-confint(bw90.mod, method="Wald",'ncdc90', level=0.95)[1] #for low CI
paper_table$ncdcbw90[4]<-confint(bw90.mod, method="Wald",'ncdc90', level=0.95)[2]  #for high
paper_table$ncdcbw90[5] <-bw90.res$coefficients[2,3] #tstat
paper_table$ncdcbw90[6] <-bw90.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm90[1] <- bw90.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm90[2] <-bw90.res$coefficients[3,2] #extract SE
paper_table$ncdcpm90[3]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$ncdcpm90[4]<-confint(bw90.mod, method="Wald",'pm90', level=0.95)[2]  #for high CI
paper_table$ncdcpm90[5] <-bw90.res$coefficients[3,3] #tstat
paper_table$ncdcpm90[6] <-bw90.res$coefficients[3,4] #sig


#### bw30
bw30.mod <- lm(birthw ~ ncdc30+pm30+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw30.res<- summary(bw30.mod)

#add to table
paper_table$ncdcbw30<-0
paper_table$ncdcpm30<-0
#ta
paper_table$ncdcbw30[1] <- bw30.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw30[2] <-bw30.res$coefficients[2,2] #extract SE
paper_table$ncdcbw30[3]<-confint(bw30.mod, method="Wald",'ncdc30', level=0.95)[1] #for low CI
paper_table$ncdcbw30[4]<-confint(bw30.mod, method="Wald",'ncdc30', level=0.95)[2]  #for high
paper_table$ncdcbw30[5] <-bw30.res$coefficients[2,3] #tstat
paper_table$ncdcbw30[6] <-bw30.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm30[1] <- bw30.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm30[2] <-bw30.res$coefficients[3,2] #extract SE
paper_table$ncdcpm30[3]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[1] #for low CI
paper_table$ncdcpm30[4]<-confint(bw30.mod, method="Wald",'pm30', level=0.95)[2]  #for high CI
paper_table$ncdcpm30[5] <-bw30.res$coefficients[3,3] #tstat
paper_table$ncdcpm30[6] <-bw30.res$coefficients[3,4] #sig

#### bw14
bw14.mod <- lm(birthw ~ ncdc14+pm14+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw14.res<- summary(bw14.mod)

#add to table
paper_table$ncdcbw14<-0
paper_table$ncdcpm14<-0
#ta
paper_table$ncdcbw14[1] <- bw14.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw14[2] <-bw14.res$coefficients[2,2] #extract SE
paper_table$ncdcbw14[3]<-confint(bw14.mod, method="Wald",'ncdc14', level=0.95)[1] #for low CI
paper_table$ncdcbw14[4]<-confint(bw14.mod, method="Wald",'ncdc14', level=0.95)[2]  #for high
paper_table$ncdcbw14[5] <-bw14.res$coefficients[2,3] #tstat
paper_table$ncdcbw14[6] <-bw14.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm14[1] <- bw14.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm14[2] <-bw14.res$coefficients[3,2] #extract SE
paper_table$ncdcpm14[3]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[1] #for low CI
paper_table$ncdcpm14[4]<-confint(bw14.mod, method="Wald",'pm14', level=0.95)[2]  #for high CI
paper_table$ncdcpm14[5] <-bw14.res$coefficients[3,3] #tstat
paper_table$ncdcpm14[6] <-bw14.res$coefficients[3,4] #sig


#### bw7
bw7.mod <- lm(birthw ~ ncdc7+pm7+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw7.res<- summary(bw7.mod)

#add to table
paper_table$ncdcbw7<-0
paper_table$ncdcpm7<-0
#ta
paper_table$ncdcbw7[1] <- bw7.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw7[2] <-bw7.res$coefficients[2,2] #extract SE
paper_table$ncdcbw7[3]<-confint(bw7.mod, method="Wald",'ncdc7', level=0.95)[1] #for low CI
paper_table$ncdcbw7[4]<-confint(bw7.mod, method="Wald",'ncdc7', level=0.95)[2]  #for high
paper_table$ncdcbw7[5] <-bw7.res$coefficients[2,3] #tstat
paper_table$ncdcbw7[6] <-bw7.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm7[1] <- bw7.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm7[2] <-bw7.res$coefficients[3,2] #extract SE
paper_table$ncdcpm7[3]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[1] #for low CI
paper_table$ncdcpm7[4]<-confint(bw7.mod, method="Wald",'pm7', level=0.95)[2]  #for high CI
paper_table$ncdcpm7[5] <-bw7.res$coefficients[3,3] #tstat
paper_table$ncdcpm7[6] <-bw7.res$coefficients[3,4] #sig


#### bw3
bw3.mod <- lm(birthw ~ ncdc3+pm3+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw3.res<- summary(bw3.mod)

#add to table
paper_table$ncdcbw3<-0
paper_table$ncdcpm3<-0
#ta
paper_table$ncdcbw3[1] <- bw3.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw3[2] <-bw3.res$coefficients[2,2] #extract SE
paper_table$ncdcbw3[3]<-confint(bw3.mod, method="Wald",'ncdc3', level=0.95)[1] #for low CI
paper_table$ncdcbw3[4]<-confint(bw3.mod, method="Wald",'ncdc3', level=0.95)[2]  #for high
paper_table$ncdcbw3[5] <-bw3.res$coefficients[2,3] #tstat
paper_table$ncdcbw3[6] <-bw3.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm3[1] <- bw3.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm3[2] <-bw3.res$coefficients[3,2] #extract SE
paper_table$ncdcpm3[3]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[1] #for low CI
paper_table$ncdcpm3[4]<-confint(bw3.mod, method="Wald",'pm3', level=0.95)[2]  #for high CI
paper_table$ncdcpm3[5] <-bw3.res$coefficients[3,3] #tstat
paper_table$ncdcpm3[6] <-bw3.res$coefficients[3,4] #sig


#### bw1
bw1.mod <- lm(birthw ~ ncdc1+pm1+sinetime+costime+age_centered+age_centered_sq+cig_pre+mgrossrent+p_ospace+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev,data =  bd)
bw1.res<- summary(bw1.mod)

#add to table
paper_table$ncdcbw1<-0
paper_table$ncdcpm1<-0
#ta
paper_table$ncdcbw1[1] <- bw1.res$coefficients[2,1]  #extract Betas
paper_table$ncdcbw1[2] <-bw1.res$coefficients[2,2] #extract SE
paper_table$ncdcbw1[3]<-confint(bw1.mod, method="Wald",'ncdc1', level=0.95)[1] #for low CI
paper_table$ncdcbw1[4]<-confint(bw1.mod, method="Wald",'ncdc1', level=0.95)[2]  #for high
paper_table$ncdcbw1[5] <-bw1.res$coefficients[2,3] #tstat
paper_table$ncdcbw1[6] <-bw1.res$coefficients[2,4] #sig
#pm
paper_table$ncdcpm1[1] <- bw1.res$coefficients[3,1]  #extract Betas
paper_table$ncdcpm1[2] <-bw1.res$coefficients[3,2] #extract SE
paper_table$ncdcpm1[3]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[1] #for low CI
paper_table$ncdcpm1[4]<-confint(bw1.mod, method="Wald",'pm1', level=0.95)[2]  #for high CI
paper_table$ncdcpm1[5] <-bw1.res$coefficients[3,3] #tstat
paper_table$ncdcpm1[6] <-bw1.res$coefficients[3,4] #sig


saveRDS(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.rds")

# save(list=c("bw270.res","bw90.res","bw30.res","bw14.res","bw7.res","bw1.res","bw3.res"), file="/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwncdcres.rda")

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


saveRDS(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.rds")












####Binary outcomes


######## preterm

##works
#######preterm
#######preterm

#pt270
pt270.mod <- gee(pterm ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),id=FIPS,data=bd,family=binomial)
pt270.res<- pt270.mod
#add to table
paper_table$pt.pt270<-0
paper_table$pt.pm270<-0
#ta
paper_table$pt.pt270[1] <- coef(summary(pt270.res))[2,"Estimate"]  #extract Betas
paper_table$pt.pt270[2] <-coef(summary(pt270.res))[2,"Naive S.E."] #extract SE
paper_table$pt.pt270[3]<- exp((coef(summary(pt270.res))[2,"Estimate"]-1.96)*(coef(summary(pt270.res))[2,"Naive S.E."]))#for low CI
paper_table$pt.pt270[4]<- exp((coef(summary(pt270.res))[2,"Estimate"]+1.96)*(coef(summary(pt270.res))[2,"Naive S.E."])) #for high
paper_table$pt.pt270[5] <-coef(summary(pt270.res))[2,"Naive z"] #tstat
paper_table$pt.pt270[6] <-2 * pnorm(abs(coef(summary(pt270.res))[2,3]), lower.tail = FALSE) #get sig
paper_table$pt.pt270[7] <-exp(coef(summary(pt270.res))[2,"Estimate"])
  

paper_table$pt.pm270[1] <- coef(summary(pt270.res))[3,"Estimate"]  #extract Betas
paper_table$pt.pm270[2] <-coef(summary(pt270.res))[3,"Naive S.E."] #extract SE
paper_table$pt.pm270[3]<- exp((coef(summary(pt270.res))[3,"Estimate"]-1.96)*(coef(summary(pt270.res))[3,"Naive S.E."]))#for low CI
paper_table$pt.pm270[4]<- exp((coef(summary(pt270.res))[3,"Estimate"]+1.96)*(coef(summary(pt270.res))[3,"Naive S.E."])) #for high
paper_table$pt.pm270[5] <-coef(summary(pt270.res))[3,"Naive z"] #tstat
paper_table$pt.pm270[6] <-2 * pnorm(abs(coef(summary(pt270.res))[3,3]), lower.tail = FALSE) #get sig#sig
paper_table$pt.pm270[7] <-exp(coef(summary(pt270.res))[3,"Estimate"])
  

  









######## low birth weight
#lbw270
lbw270.mod <- gee(lowbw ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+mgrossrent+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),id=FIPS,data=bd,family=binomial)
lbw270.res<- summary(lbw270.mod)
#add to table
paper_table$lbw.lbw270<-0
paper_table$lbw.pm270<-0
#ta
paper_table$lbw.lbw270[1] <- coef(lbw270.res)[2,"Estimate"]  #extract Betas
paper_table$lbw.lbw270[2] <-coef(lbw270.res)[2,"Std. Error"] #extract SE
paper_table$lbw.lbw270[3]<-confint(lbw270.mod, method="Wald",'ta270', level=0.95)[1] #for low CI
paper_table$lbw.lbw270[4]<-confint(lbw270.mod, method="Wald",'ta270', level=0.95)[2]  #for high
paper_table$lbw.lbw270[5] <-coef(lbw270.res)[2,"z value"] #tstat
paper_table$lbw.lbw270[6] <-coef(lbw270.res)[2,"Pr(>|z|)"] #sig
#pm
paper_table$lbw.pm270[1] <- coef(lbw270.res)[3,"Estimate"]  #extract Betas
paper_table$lbw.pm270[2] <-coef(lbw270.res)[3,"Std. Error"] #extract SE
paper_table$lbw.pm270[3]<-confint(lbw270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$lbw.pm270[4]<-confint(lbw270.mod, method="Wald",'pm270', level=0.95)[2]  #for high
paper_table$lbw.pm270[5] <-coef(lbw270.res)[3,"z value"] #tstat
paper_table$lbw.pm270[6] <-coef(lbw270.res)[3,"Pr(>|z|)"] #sig

#lbw90
lbw90.mod <- gee(lowbw ~ ta90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+mgrossrent+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),id=FIPS,data=bd,family=binomial)
lbw90.res<- summary(lbw90.mod)
#add to table
paper_table$lbw.lbw90<-0
paper_table$lbw.pm90<-0
#ta
paper_table$lbw.lbw90[1] <- coef(lbw90.res)[2,"Estimate"]  #extract Betas
paper_table$lbw.lbw90[2] <-coef(lbw90.res)[2,"Std. Error"] #extract SE
paper_table$lbw.lbw90[3]<-confint(lbw90.mod, method="Wald",'ta90', level=0.95)[1] #for low CI
paper_table$lbw.lbw90[4]<-confint(lbw90.mod, method="Wald",'ta90', level=0.95)[2]  #for high
paper_table$lbw.lbw90[5] <-coef(lbw90.res)[2,"z value"] #tstat
paper_table$lbw.lbw90[6] <-coef(lbw90.res)[2,"Pr(>|z|)"] #sig
#pm
paper_table$lbw.pm90[1] <- coef(lbw90.res)[3,"Estimate"]  #extract Betas
paper_table$lbw.pm90[2] <-coef(lbw90.res)[3,"Std. Error"] #extract SE
paper_table$lbw.pm90[3]<-confint(lbw90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$lbw.pm90[4]<-confint(lbw90.mod, method="Wald",'pm90', level=0.95)[2]  #for high
paper_table$lbw.pm90[5] <-coef(lbw90.res)[3,"z value"] #tstat
paper_table$lbw.pm90[6] <-coef(lbw90.res)[3,"Pr(>|z|)"] #sig

saveRDS(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.rds")

####SGA

sgaq270.mod <- gee(sgaq ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+mgrossrent+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),id=FIPS,data=bd,family=binomial)
sgaq270.res<- summary(sgaq270.mod)
#add to table
paper_table$sgaq.sgaq270<-0
paper_table$sgaq.pm270<-0
#ta
paper_table$sgaq.sgaq270[1] <- coef(sgaq270.res)[2,"Estimate"]  #extract Betas
paper_table$sgaq.sgaq270[2] <-coef(sgaq270.res)[2,"Std. Error"] #extract SE
paper_table$sgaq.sgaq270[3]<-confint(sgaq270.mod, method="Wald",'ta270', level=0.95)[1] #for low CI
paper_table$sgaq.sgaq270[4]<-confint(sgaq270.mod, method="Wald",'ta270', level=0.95)[2]  #for high
paper_table$sgaq.sgaq270[5] <-coef(sgaq270.res)[2,"z value"] #tstat
paper_table$sgaq.sgaq270[6] <-coef(sgaq270.res)[2,"Pr(>|z|)"] #sig
#pm
paper_table$sgaq.pm270[1] <- coef(sgaq270.res)[3,"Estimate"]  #extract Betas
paper_table$sgaq.pm270[2] <-coef(sgaq270.res)[3,"Std. Error"] #extract SE
paper_table$sgaq.pm270[3]<-confint(sgaq270.mod, method="Wald",'pm270', level=0.95)[1] #for low CI
paper_table$sgaq.pm270[4]<-confint(sgaq270.mod, method="Wald",'pm270', level=0.95)[2]  #for high
paper_table$sgaq.pm270[5] <-coef(sgaq270.res)[3,"z value"] #tstatbos
paper_table$sgaq.pm270[6] <-coef(sgaq270.res)[3,"Pr(>|z|)"] #sig



####SGA

sgaq90.mod <- gee(sgaq ~ ta90+pm90+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+mgrossrent+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),id=FIPS,data=bd,family=binomial)
sgaq90.res<- summary(sgaq90.mod)
#add to table
paper_table$sgaq.sgaq90<-0
paper_table$sgaq.pm90<-0
#ta
paper_table$sgaq.sgaq90[1] <- coef(sgaq90.res)[2,"Estimate"]  #extract Betas
paper_table$sgaq.sgaq90[2] <-coef(sgaq90.res)[2,"Std. Error"] #extract SE
paper_table$sgaq.sgaq90[3]<-confint(sgaq90.mod, method="Wald",'ta90', level=0.95)[1] #for low CI
paper_table$sgaq.sgaq90[4]<-confint(sgaq90.mod, method="Wald",'ta90', level=0.95)[2]  #for high
paper_table$sgaq.sgaq90[5] <-coef(sgaq90.res)[2,"z value"] #tstat
paper_table$sgaq.sgaq90[6] <-coef(sgaq90.res)[2,"Pr(>|z|)"] #sig
#pm
paper_table$sgaq.pm90[1] <- coef(sgaq90.res)[3,"Estimate"]  #extract Betas
paper_table$sgaq.pm90[2] <-coef(sgaq90.res)[3,"Std. Error"] #extract SE
paper_table$sgaq.pm90[3]<-confint(sgaq90.mod, method="Wald",'pm90', level=0.95)[1] #for low CI
paper_table$sgaq.pm90[4]<-confint(sgaq90.mod, method="Wald",'pm90', level=0.95)[2]  #for high
paper_table$sgaq.pm90[5] <-coef(sgaq90.res)[3,"z value"] #tstatbos
paper_table$sgaq.pm90[6] <-coef(sgaq90.res)[3,"Pr(>|z|)"] #sig

saveRDS(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.rds")

write.csv(paper_table,"/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bwrestable_lm.csv")
