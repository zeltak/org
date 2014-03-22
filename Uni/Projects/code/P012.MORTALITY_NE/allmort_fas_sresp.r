library (MASS)
library (splines)
library(nlme) 
library(car)


###########################################################################
#                                allmort                                 #
###########################################################################


setwd("/n/home12/ekloog/work/")

sink("rout.txt",append=TRUE,split=TRUE)


ts0004lagres = read.csv("allmort_resp.csv", header=T) 
###########

##only acute




###########################################################################
#                              resp                                       #
###########################################################################


###########FINAL



try1_st<- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1  +temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = ts0004lagres))
summary(try1_st)$tTable





