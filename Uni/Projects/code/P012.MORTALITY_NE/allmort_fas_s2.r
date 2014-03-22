


##only acute interaction
#rural

try1ac <- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1++pmnew_l1*urb+urb+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1ac)$tTable


##only acute interaction

ts0004lag$urbf <- recode(ts0004lag$urb , ' 0="1"; 1="0" ',  as.factor.result=FALSE)
#flip (urban)
try1acurbf <- (glmmPQL(count ~ ns(date,df=45)+pmnew_l1++pmnew_l1*urbf+urbf+temp_f_l1+med_inc+Avg_per_mi+Avg_pctcol+Avg_p_A65+ran, random = ~ 1 | guid, family = poisson, data = ts0004lag))
summary(try1acurbf)$tTable



