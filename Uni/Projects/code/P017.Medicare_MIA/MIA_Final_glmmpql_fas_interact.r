library (MASS)
library (splines)
library(nlme)

#interactions

stroke= read.csv("/n/home12/ekloog/DATA/stroke0006.csv", header=T)
#___________________________________________________

stroke$pmnewmayear_cen<-(stroke$pmnewmayear-mean(stroke$pmnewmayear))
#stroke$pmnewma1_cen  <-(stroke$pmnewma1-mean(stroke$pmnewma1))
summary(stroke)

stroke_ma1_age50 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*age_bin_m+age_bin_m+pctnonwht_wtd+pctbachorhigher_wtd+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_age25 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*age_bin_25+age_bin_25+pctnonwht_wtd+pctbachorhigher_wtd+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_age75 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*age_bin_75+age_bin_75+pctnonwht_wtd+pctbachorhigher_wtd+pctlowinc_wtd,random = ~ 1 | guid,family = poisson, data = stroke ))


stroke_ma1_inc50 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*inc_bin_m+inc_bin_m+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_inc25 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*inc_bin_25+inc_bin_25+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_inc75 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*inc_bin_75+inc_bin_75+pctnonwht_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))



stroke_ma1_min50 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*min_bin_m+min_bin_m+pctlowinc_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_min25 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*min_bin_25+min_bin_25+pctlowinc_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))

stroke_ma1_min75 <- (glmmPQL(count~ ns(date,df=35)+pmnewma1+pmnewmayear_cen+temp_fma1+pmnewmayear_cen*min_bin_75+min_bin_75+pctlowinc_wtd+pctbachorhigher_wtd+pct65upest,random = ~ 1 | guid,family = poisson, data = stroke ))





summary(stroke_ma1_age25)$tTable
summary(stroke_ma1_age50)$tTable
summary(stroke_ma1_age75)$tTable

summary(stroke_ma1_inc25)$tTable
summary(stroke_ma1_inc50)$tTable
summary(stroke_ma1_inc75)$tTable

summary(stroke_ma1_min25)$tTable
summary(stroke_ma1_min50)$tTable
summary(stroke_ma1_min75)$tTable
