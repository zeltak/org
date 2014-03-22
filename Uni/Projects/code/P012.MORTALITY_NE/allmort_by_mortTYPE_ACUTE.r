library (MASS)
library (splines)
library(nlme) 
#library(psych)
library(reshape)
library(car)






###########################################################################
#                                  cvd                                    #
###########################################################################



ts0004lagcv = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/cvd_mort.csv", header=T) 

ts0004lagcv<-na.omit(ts0004lagcv)



###########FINAL

names(ts0004lagcv)

try1_cvd <- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004lagcv))
summary(try1_cvd)$tTable














###########################################################################
#                                  stroke                                    #
###########################################################################



ts0004lagst = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/stroke_mort.csv", header=T) 

ts0004lagst<-na.omit(ts0004lagst)

# /*home death=0  /////// outside home=1*/



###########FINAL



try1_st<- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004lagst))
summary(try1_st)$tTable








###########################################################################
#                              resp                                       #
###########################################################################



ts0004lagres = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/resp_mort.csv", header=T) 

ts0004lagres<-na.omit(ts0004lagres)


###########FINAL



try1_st<- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004lagres))
summary(try1_st)$tTable



###########################################################################
#                           diab                                     #
###########################################################################



ts0004lagdiab = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/diab_mort.csv", header=T) 

ts0004lagdiab<-na.omit(ts0004lagdiab)


###########FINAL



try1_diab<- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004lagdiab))
summary(try1_diab)$tTable



###########################################################################
#                             lung                                      #
###########################################################################



ts0004laglung = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/lung_mort.csv", header=T) 

ts0004laglung<-na.omit(ts0004laglung)


###########FINAL



try1_lung<- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004laglung))
summary(try1_lung)$tTable

