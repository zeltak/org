library (MASS)
library (splines)
library(nlme) 
#library(psych)
library(reshape)
library(car)

setwd("/usr1/home/ekloog/R/")

sink("rout.txt",append=TRUE,split=TRUE)


ts0004lag = read.csv("allmort.csv", header=T) 

ts0004lag<-na.omit(ts0004lag)

# /*home death=0  /////// outside home=1*/


# 
# summary(ts0004lag)
# lung_new <- (glmmPQL(lung ~ 1, random = ~ 1 | guid,family = poisson, data = ts0004lag)) 
# 
# 
# 
# lung_new$coef$random 
# #this outputs the 2 colums to the terminal, copy that and paste in text editor and reimport to R
# 

ranlung = read.csv("allmort_lung.csv", header=T) 


summary(ranlung)

ts0004lag <- ts0004lag[order(ts0004lag$guid),] 
ranlung <- ranlung[order(ranlung$guid),] 
ts <- merge(ts0004lag,ranlung,by="guid")
ts <- rename(ts, c(Intercept="ran"))
names(ts)

ts0004lag <- ts



###########FINAL
# 
# try0 <- (glmmPQL(count ~ ns(date,df=45)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
# summary(try0)$tTable
# 
# try1 <- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1+temp_f_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
# summary(try1)$tTable
# 
# 
names(ts0004lag)

try1 <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1)$tTable

#sensativity

try2 <- (glmmPQL(count ~ ns(date,df=45)+deltapm2+mpm2+temp_f_l2+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try2)$tTable

try0 <- (glmmPQL(count ~ ns(date,df=45)+deltapm0+mpm0+temp_f_l0+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try0)$tTable



###########################################################################
#                             #interactions
#
###########################################################################

summary(ts0004lag)
ts0004lag$mpm1_cen<-(ts0004lag$mpm1-mean(ts0004lag$mpm1))
ts0004lag$deltapm1_cen<-(ts0004lag$deltapm1 -mean(ts0004lag$deltapm1))
summary(ts0004lag)


#place of death interaction

try1int_dp <- (glmmPQL(count ~ ns(date,df=45)+deltapm1_cen+deltapm1_cen*dp+dp+mpm1+temp_f_l0+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_dp)$tTable







#percent minority intearaction


#flips symbols for interaction
ts0004lag$min_bin_mf <- recode(ts0004lag$min_bin_m , ' 0="1"; 1="0" ',  as.factor.result=FALSE)

try1int_minority <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*min_bin_m+temp_f_l1+med_inc+min_bin_m+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_minority)$tTable #neg pmyear

try1int_minorityf <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*min_bin_mf+temp_f_l1+med_inc+min_bin_mf+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_minorityf)$tTable #neg pmyear



#percent low educ intearaction
#flips symbols for interaction
ts0004lag$hs_bin_mf <- recode(ts0004lag$hs_bin_m , ' 0="1"; 1="0" ',  as.factor.result=FALSE)

try1int_edu <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*hs_bin_m+temp_f_l1+med_inc+hs_bin_m+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_edu)$tTable #neg pmyear

try1int_eduf <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*hs_bin_mf+temp_f_l1+med_inc+hs_bin_mf+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_eduf)$tTable #neg pmyear

#percent low educ intearaction for college/no college
#flips symbols for interaction
ts0004lag$col_bin_mf <- recode(ts0004lag$col_bin_m , ' 0="1"; 1="0" ',  as.factor.result=FALSE)

try1int_col <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*col_bin_m+temp_f_l1+med_inc+col_bin_m+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_col)$tTable #neg pmyear

try1int_colf <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*col_bin_mf+temp_f_l1+med_inc+col_bin_mf+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_colf)$tTable #neg pmyear




#rural vs mon20an interaction                            
#flips symbols for interaction
ts0004lag$mon20f <- recode(ts0004lag$mon20 , ' 0="1"; 1="0" ',  as.factor.result=FALSE)

try1int_mon20 <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*mon20+mon20+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_mon20)$tTable #neg pmyear

try1int_mon20f <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1_cen+mpm1_cen*mon20f+mon20f+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1int_mon20f)$tTable #neg pmyear

summary(ts0004lag$mon20)

###########################################################################
#                                  resp                                   #
###########################################################################



ts0004lagr = read.csv("resp_mort.csv", header=T) 

ts0004lagr<-na.omit(ts0004lagr)

# /*home death=0  /////// outside home=1*/


# 
# summary(ts0004lagr)
# lung_newr <- (glmmPQL(lung ~ 1, random = ~ 1 | guid,family = poisson, data = ts0004lagr)) 
# 
# 
# 
# lung_newr$coef$random 
# #this outputs the 2 colums to the terminal, copy that and paste in text editor and reimport to R

ranlung = read.csv("respmort_lung.csv", header=T) 


summary(ranlung)

ts0004lagr <- ts0004lagr[order(ts0004lagr$guid),] 
ranlung <- ranlung[order(ranlung$guid),] 
ts <- merge(ts0004lagr,ranlung,by="guid")
ts <- rename(ts, c(Intercept="ran"))
names(ts)

ts0004lagr <- ts



###########FINAL
 
names(ts0004lagr)

try1_resp <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lagr))
summary(try1_resp)$tTable



###########################################################################
#                                  cvd                                    #
###########################################################################



ts0004lagcv = read.csv("cvd_mort.csv", header=T) 

ts0004lagcv<-na.omit(ts0004lagcv)

# /*home death=0  /////// outside home=1*/



summary(ts0004lagcv)
# lung_newr <- (glmmPQL(lung ~ 1, random = ~ 1 | guid,family = poisson, data = ts0004lagcv)) 
# 
# 
# 
# lung_newr$coef$random 
# #this outputs the 2 colums to the terminal, copy that and paste in text editor and reimport to R
# 
ranlung = read.csv("cvdmort_lung.csv", header=T) 


summary(ranlung)

ts0004lagcv <- ts0004lagcv[order(ts0004lagcv$guid),] 
ranlung <- ranlung[order(ranlung$guid),] 
ts <- merge(ts0004lagcv,ranlung,by="guid")
ts <- rename(ts, c(Intercept="ran"))
names(ts)

ts0004lagcv <- ts



###########FINAL
 
names(ts0004lagcv)

try1_cvd <- (glmmPQL(count ~ ns(date,df=45)+deltapm1+mpm1+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lagcv))
summary(try1_cvd)$tTable


save.image(file="WS.R")


###########################################################################
#                                 results                                 #
###########################################################################

# #urban Vs Rural
# summary(try1int_urb)$tTable #neg pmyear
# summary(try1int_urbf)$tTable #neg pmyear
# 
# #percent minority
# summary(try1int_minority)$tTable #neg pmyear
# summary(try1int_minorityf)$tTable #neg pmyear
# 
# 
