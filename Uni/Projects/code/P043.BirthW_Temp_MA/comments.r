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

## disable sci notations

options(scipen = 99)

## Import

bd<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_nocesv2.csv")

names(bd)

## dates

bd[, day:=as.Date(strptime(date, "%d/%m/%y"))]
bd$rfpisga[bd$rfpisga== 9] <- NA
bd$rfpisga[bd$rfpisga== 2] <- 0

## aggreagted PM

bd$tpm270<- bd$pmnewmabirth+bd$localpm
bd$tpm90<- bd$pmnewma3month+bd$localpm
bd$tpm30<- bd$pmnewmamonth+bd$localpm

## calcute SGA

sgamodel <- lm(birthw ~ges_calc+gender,data =  bd)
bd$sga <- resid(sgamodel)
## paper base model



bw_full_birth <- lm(birthw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data =  bd)
summary(bw_full_birth)

## DONE base model with PMdata

bw_full_birth <- lm(birthw ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data =  bd)
summary(bw_full_birth)

## DONE base model with PMdata

bw_full_birth <- lm(birthw ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data =  bd)
summary(bw_full_birth)

## paper

bw_full_3m <- lm(birthw ~ IQRfintempma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data =  bd)
summary(bw_full_3m)

## paper with PM

bw_full_3m <- lm(birthw ~ IQRfintempma3month+pmnewma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data =  bd)
summary(bw_full_3m)

## NEXT full birth

bw_full_birth <- lmer(birthw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+ (1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT full birth+pm

## #+begin_example
##   [1] "bthctyn"              "bthctyc"              "sex"                 
##   [4] "plur"                 "bthord"               "byob"                
##   [7] "bmob"                 "bdayob"               "bdob"                
##  [10] "myob"                 "mmob"                 "mbpstt"              
##  [13] "mbpstc"               "mresctn"              "mresctc"             
##  [16] "mrescnn"              "mrescnc"              "marstat"             
##  [19] "fbpstt"               "fbpstc"               "fage"                
##  [22] "mrace"                "methn"                "mlangp"              
##  [25] "medu"                 "frace"                "fethn"               
##  [28] "flangp"               "fedu"                 "lbnl"                
##  [31] "lbnd"                 "yrllb"                "mtllb"               
##  [34] "dyllb"                "dtllb"                "parit"               
##  [37] "gravid"               "gacalc"               "clinega"             
##  [40] "yrfpncv"              "mtfpncv"              "dyfpncv"             
##  [43] "dtfpncv"              "mpncp"                "npncv"               
##  [46] "pncgov"               "kess"                 "kotck"               
##  [49] "cigdpp"               "cigddp"               "mwgl"                
##  [52] "modvag"               "modvbac"              "modfor"              
##  [55] "modvac"               "modpcs"               "modrcs"              
##  [58] "bwg"                  "apgar1"               "apgar5"              
##  [61] "breast"               "disyr"                "dismt"               
##  [64] "disdy"                "disdt"                "distim"              
##  [67] "dhwmot"               "balive"               "rflungd"             
##  [70] "rfanem"               "rfcard"               "rfdiabg"             
##  [73] "rfdiabo"              "rfhypc"               "rfhyppr"             
##  [76] "rfincer"              "rfpiwbd"              "rfpi4k"              
##  [79] "rfpisga"              "rfrenal"              "rfrhsen"             
##  [82] "rfsickl"              "rfutbld"              "avnlt30"             
##  [85] "avnge30"              "jaund"                "mocode"              
##  [88] "micode"               "focode"               "ficode"              
##  [91] "tract"                "long"                 "lat"                 
##  [94] "yrod"                 "mtod"                 "dyod"                
##  [97] "dtod"                 "randint"              "duprandint"          
## [100] "uniqueid"             "uniqueid_y"           "myredu"              
## [103] "gridadt"              "FIPS"                 "medhhinctr"          
## [106] "pctreccono"           "date"                 "xx"                  
## [109] "yy"                   "stn"                  "guid"                
## [112] "glong"                "glat"                 "fintemp"             
## [115] "fintemp_l0"           "fintemp_l1"           "fintemp_l2"          
## [118] "fintemp_l3"           "fintempma1"           "fintempma3"          
## [121] "fintempmaweek"        "fintempma2week"       "fintempma3week"      
## [124] "fintempmamonth"       "fintempma3month"      "fintempmabirth"      
## [127] "fintempmayear"        "gender1"              "mother_race"         
## [130] "father_race"          "csect1"               "birthw"              
## [133] "prevpre"              "age"                  "med_income"          
## [136] "p_ospace"             "fage1"                "frace1"              
## [139] "fethn1"               "fedu1"                "flangp1"             
## [142] "mlangp1"              "methn1"               "marstat1"            
## [145] "mbpstc1"              "gravid1"              "mpncp1"              
## [148] "pncgov1"              "methnic"              "fethnic"             
## [151] "gender"               "pre_vists"            "parity"              
## [154] "prevpret"             "cig_pre"              "cig_preg"            
## [157] "cig_preq"             "cig_pregq"            "ges_calc"            
## [160] "ges_clinic"           "lges"                 "lgescl"              
## [163] "csect"                "utbleed"              "lungd"               
## [166] "renal"                "hyper_other"          "hyper"               
## [169] "diab"                 "diab_other"           "prev_400"            
## [172] "prev_sga"             "MRN"                  "lbw"                 
## [175] "m"                    "season"               "npar"                
## [178] "year"                 "age_centered"         "age_centered_sq"     
## [181] "edu_group"            "adtmean"              "med_incomeq"         
## [184] "f_age"                "f_age_centered"       "f_age_centered_sq"   
## [187] "f_race"               "FRN"                  "f_lang"              
## [190] "p_birth"              "m_lang"               "mstat"               
## [193] "m_care"               "bw"                   "plural"              
## [196] "pcare"                "aged"                 "pctwhttr00"          
## [199] "pctblktr00"           "pctasiantr00"         "pctothracetr00"      
## [202] "pct2ormoreracetr00"   "pctwhtnhtr00"         "pctblknhtr00"        
## [205] "pctasiannhtr00"       "pctothracenhtr00"     "pct2ormoreracenhtr00"
## [208] "pcthisptr00"          "pctnonwhttr00"        "medhhinctr00"        
## [211] "pctfamfemhdtr00"      "pctcrowdtr00"         "pcthsdropouttr00"    
## [214] "pct16_64mntemptr00"   "pctlowinctr00"        "pcthiinctr00"        
## [217] "pctbelpovlt18tr00"    "pctbelpov18_64tr00"   "pctbelpovgt64tr00"   
## [220] "pctbelpovtr00"        "pctnohstr00"          "pcthstr00"           
## [223] "pctbachtr00"          "pctadvdegtr00"        "pctbachorhighertr00" 
## [226] "sevdisnbhdpov"        "sevdisnbhdfam"        "sevdisnbhdhs"        
## [229] "sevdisnbhdemp"        "sevdisnhbdscoretr00"  "pctocchutr00"        
## [232] "pctownerocctr00"      "medgrrenttr00"        "medvalspecownoctr00" 
## [235] "TEMP_Cma1"            "TEMP_Cma3"            "TEMP_Cmaweek"        
## [238] "TEMP_Cma2week"        "TEMP_Cmamonth"        "TEMP_Cma3month"      
## [241] "TEMP_Cmabirth"        "TEMP_Cmayear"         "tncdc"               
## [244] "tncdc_l0"             "tncdc_l1"             "tncdc_l2"            
## [247] "tncdc_l3"             "tncdcma1"             "tncdcma3"            
## [250] "tncdcmaweek"          "tncdcma2week"         "tncdcmamonth"        
## [253] "tncdcma3month"        "tncdcmabirth"         "tncdcmayear"         
## [256] "dow"                  "doy"                  "doy2"                
## [259] "sinetime"             "costime"              "newvar"              
## [262] "udate"                "devtemp"              "findevtemp"          
## [265] "findevtemplag3"       "IQRfintemp"           "IQRfintemp_l0"       
## [268] "IQRfintemp_l1"        "IQRfintemp_l2"        "IQRfintemp_l3"       
## [271] "IQRfintempma1"        "IQRfintempma3"        "IQRfintempmaweek"    
## [274] "IQRfintempma2week"    "IQRfintempma3week"    "IQRfintempmamonth"   
## [277] "IQRfintempma3month"   "IQRfintempmabirth"    "IQRtncdc"            
## [280] "IQRtncdc_l0"          "IQRtncdc_l1"          "IQRtncdc_l2"         
## [283] "IQRtncdc_l3"          "IQRtncdcma1"          "IQRtncdcma3"         
## [286] "IQRtncdcmaweek"       "IQRtncdcma2week"      "IQRtncdcmamonth"     
## [289] "IQRtncdcma3month"     "IQRtncdcmabirth"      "bwcat"               
## [292] "edu01"                "mrn01"                "minc01"              
## [295] "fedu01"               "fmrn01"               "fminc01"             
## [298] "lowbw"                "NSGA"                 "pmnew"               
## [301] "pmnew_l0"             "pmnew_l1"             "pmnew_l2"            
## [304] "pmnew_l3"             "pmnewma1"             "pmnewma3"            
## [307] "pmnewmaweek"          "pmnewma2week"         "pmnewmamonth"        
## [310] "pmnewma3month"        "pmnewmabirth"         "pm12_24"             
## [313] "popden"               "pcturban"             "elev"                
## [316] "tden"                 "dist_A1"              "dist_pemis"          
## [319] "localpm"
## #+end_example

bw_full_birth <- lmer(birthw ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT 90d

bw_full_birth <- lmer(birthw ~ IQRfintempma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT 90d+pm

bw_full_birth <- lmer(birthw ~ IQRfintempma3month+pmnewma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT 30d

bw_full_birth <- lmer(birthw ~ IQRfintempmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT 30d

bw_full_birth <- lmer(birthw ~ IQRfintempmamonth+pmnewmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## NEXT full birth

bw_full_birth <- lmer(birthw ~ IQRfintempmaweek+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

## #+RESULTS:
## #+begin_example
## Linear mixed model fit by REML ['lmerMod']
## Formula: birthw ~ IQRfintempmaweek + sinetime + costime + age_centered +      age_centered_sq + cig_preg + cig_pre + gender + prev_400 +      diab + hyper + lungd + diab_other + prevpret + as.factor(kess) +      as.factor(MRN) + as.factor(edu_group) + as.factor(byob) +      parity + as.factor(FRN) + ges_calc + elev + (1 | FIPS) 
##    Data: bd 

## REML criterion at convergence: 6796368 

## Random effects:
##  Groups   Name        Variance Std.Dev.
##  FIPS     (Intercept)    744.1  27.28  
##  Residual             198662.3 445.72  
## Number of obs: 451908, groups: FIPS, 1358

## Fixed effects:
##                         Estimate Std. Error t value
## (Intercept)            -31.56230   15.47958   -2.04
## IQRfintempmaweek        -9.60440    3.67536   -2.61
## sinetime                -2.33270    1.65442   -1.41
## costime                -11.64564    2.61361   -4.46
## age_centered             6.13325    0.13202   46.46
## age_centered_sq         -0.27094    0.01584  -17.10
## cig_preg               -14.50218    0.35436  -40.92
## cig_pre                 -2.33367    0.19392  -12.03
## gender                -126.06252    1.32793  -94.93
## prev_400               473.11170    7.57657   62.44
## diab                    52.02677    3.76442   13.82
## hyper                  -89.21652    4.02732  -22.15
## lungd                  -38.52789    3.82558  -10.07
## diab_other              95.46636    8.11847   11.76
## prevpret              -195.10356    6.83833  -28.53
## as.factor(kess)2       -58.40127    1.79664  -32.51
## as.factor(kess)3       -61.37630    3.78362  -16.22
## as.factor(kess)4      -106.79610   10.21186  -10.46
## as.factor(kess)5      -154.24716   14.74595  -10.46
## as.factor(MRN)1       -105.13422    3.80166  -27.65
## as.factor(MRN)2        -79.91110    2.51718  -31.75
## as.factor(edu_group)2  -17.23128    4.20294   -4.10
## as.factor(edu_group)3    2.08883    4.39698    0.48
## as.factor(edu_group)4   -2.29726    4.39460   -0.52
## as.factor(byob)2001     -6.36254    2.65378   -2.40
## as.factor(byob)2002    -11.50828    2.66475   -4.32
## as.factor(byob)2003    -14.43993    2.76138   -5.23
## as.factor(byob)2005    -27.01275    2.74165   -9.85
## as.factor(byob)2006    -31.31153    2.73328  -11.46
## as.factor(byob)2007    -33.17587    2.81569  -11.78
## as.factor(byob)2008    -35.84883    2.74148  -13.08
## parity                   6.53657    0.24921   26.23
## as.factor(FRN)1        -19.04115    3.73984   -5.09
## as.factor(FRN)2        -60.27401    2.42684  -24.84
## ges_calc                95.21227    0.35145  270.91
## elev                    -0.01905    0.01427   -1.33

## Correlation matrix not shown by default, as p = 36 > 20.
## Use print(...., correlation=TRUE)  or
##     vcov(....)	 if you need it
## #+end_example

ICAM_restable$beta[1] <- bw_full_birth$coef$fixed[2]  #extract Betas

## NEXT 270

bw_full_birth <- lme(birthw ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ pcthstr00,random=~1|FIPS,na.action=na.omit,
data =  bd)
summary(bw_full_birth)

## base

log.sga.270 <- glm(NSGA ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data=bd,family=binomial)
summary(log.sga.270)

## paper with PM

log.sga.270 <- glm(NSGA ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+f_age_centered+f_age_centered_sq,data=bd,family=binomial)
summary(log.sga.270)

## paper

sga.90<-glm(NSGA ~ IQRfintempma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),family=binomial, data =  bd)
summary(sga.90)

## paper PM

log.sga.90 <- glm(NSGA ~ IQRfintempma3month+pmnewma3month++sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+f_age_centered+f_age_centered_sq,data=bd,family=binomial)
summary(log.sga.90)

## paper

sga.30<-glm(NSGA ~ IQRfintempmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),family=binomial, data =  bd)
summary(sga.30)

## paper PM

log.sga.30 <- glm(NSGA ~ IQRfintempmamonth+pmnewmamonth++sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+f_age_centered+f_age_centered_sq,data=bd,family=binomial)
summary(log.sga.30)

## NEXT 270d

sga.270<-(glmmPQL(NSGA ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+elev, random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.270 )$tTable

## NEXT 270d+PM

sga.270<-(glmmPQL(NSGA ~ IQRfintempmabirth+pmnewmabirth+ +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))
summary(sga.270 )$tTable

## NEXT full birth NCDC

sga.270<-(glmmPQL(NSGA ~ IQRtncdcmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.270 )$tTable

## 90d

sga.90<-(glmmPQL(NSGA ~ IQRfintempma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+elev, random = ~ 1 | tract ,family=binomial, data =  bd))

summary(sga.90)$tTable

## 90d/pm

sga.90<-(glmmPQL(NSGA ~ IQRfintempma3month+pmnewma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.90)$tTable

## 90d NCDC

sga.90<-(glmmPQL(NSGA ~ IQRtncdcma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | tract ,family=binomial, data =  bd))

summary(sga.90)$tTable

## 30d

sga.30<-(glmmPQL(NSGA ~ IQRfintempmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.30)$tTable

## 3d

sga.3<-(glmmPQL(NSGA ~ IQRfintempma3 +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.3)$tTable

## 1d

sga.1<-(glmmPQL(NSGA ~ IQRfintemp_l1 +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(sga.1)$tTable

## fullbirth

log.lbw.270 <- glm(lowbw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN),data=bd,family=binomial)

## #+RESULTS:

summary(log.lbw.270)
con

## 90d

log.lbw.90 <- glm(lowbw ~ IQRfintempma3month+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+f_age_centered+f_age_centered_sq,data=bd,family=binomial)
summary(log.lbw.90)

## 30d

log.lbw.30 <- glm(lowbw ~ IQRfintempmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+f_age_centered+f_age_centered_sq,data=bd,family=binomial)
summary(log.lbw.30)

## 270d+PM							  :CANCELLED:

lbw.270<-(glmmPQL(lowbw ~ IQRfintempmabirth+pmnewmabirth+ +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bd))
summary(lbw.270 )$tTable

## 270d

lbw.270<-(glmmPQL(lowbw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+elev, random = ~ 1 | FIPS ,family=binomial, data =  bd))
summary(lbw.270)$tTable

## 270d+PM

lbw.270<-(glmmPQL(lowbw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+elev, random = ~ 1 | FIPS ,family=binomial, data =  bd))
summary(lbw.270)$tTable

## 90d

lbw.90<-(glmmPQL(lowbw ~ IQRfintempma3month+ sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+elev+ges_calc, random = ~ 1 | FIPS ,family=binomial, data =  bd))
summary(lbw.90)$tTable

## 30d

lbw.30<-(glmmPQL(lowbw ~ pmnewmabirthi+IQfintempmamonth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity, random = ~ 1 | FIPS ,family=binomial, data =  bd))

summary(lbw.30)$tTable

## CI

ICAM_restable$beta[1] <- mlag001$coef$fixed[2]  #extract Betas
ICAM_restable$se[1] <-(summary(mlag001)$tTable[2,2]) #extract SE
ICAM_restable$sig[1] <-(summary(mlag001)$tTable[2,5]) #extract sig
# Percent change for 1 IQR change if outcome is logged:
ICAM_restable$pc[1] <- (exp(ICAM_restable$beta[1]*mlag001_irq)-1)*100
# Low CI bound
ICAM_restable$L_CI[1] <- (exp((ICAM_restable$beta[1]-1.96*ICAM_restable$se[1])*mlag001_irq)-1)*100
# High CI bound
ICAM_restable$H_CI[1] <- (exp((ICAM_restable$beta[1]+1.96*ICAM_restable$se[1])*mlag001_irq)-1)*100
ICAM_restable$ciw[1] <-ICAM_restable$L_CI[1]+ICAM_restable$H_CI[1]








######## sga

bw_full_birth <- lmer(sga ~ IQRfintempmabirth+pmnewmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN)+ges_calc+elev+(1 |FIPS),data =  bd)
summary(bw_full_birth)

