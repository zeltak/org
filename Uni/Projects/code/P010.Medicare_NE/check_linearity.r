library (MASS)
library (splines)
library(nlme)
library(mgcv)


sink("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.5.Results/2.new model/tst.txt" , append=TRUE, split=TRUE)


#allresp

allresp = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/diab0106.csv", header=T)
names(allresp)

allresp<- na.omit(allresp)




Rresp <- (gamm(count ~ ns(date,df=35) +s(pmnew_l0)+s(pmnewmayear)+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random=list(guid=~1), family = poisson, data = allresp))



Rresp <- (gamm(count ~ ns(date,df=35) +pmnew_l0+ pmnewmayear +I((pmnewmayear-10)*(pmnewmayear >10))+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace, random=list(guid=~1), family = poisson, data = allresp))


Rresp2 <- (gamm(count ~ ns(date,df=35) +pmnew_l0+ pmnewmayear +I((pmnewmayear-10)*(pmnewmayear >10))+temp_f_l0, random=list(guid=~1), family = poisson, data = allresp))

summary(Rresp2$gam)

pmnewmayear                                 0.0075074  0.0022327   3.362 0.000773 ***
I((pmnewmayear - 10) * (pmnewmayear > 10)) -0.0065143  0.0027600  -2.360 0.018263 *  




Rresp <- (gamm(count ~ ns(date,df=35) +pmnew_l0+ pmnewmayear +I((pmnewmayear-10)*(pmnewmayear >10))+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace, random=list(guid=~1), family = poisson, data = allresp))
pmnew_l0                                    5.038e-04  1.699e-04   2.966 0.003019 ** 
pmnewmayear                                 4.270e-03  2.494e-03   1.712 0.086933 .  
I((pmnewmayear - 10) * (pmnewmayear > 10))  7.818e-04  3.079e-03   0.254 0.799584    
temp_f_l0                                   1.850e-03  1.253e-04  14.767  < 2e-16 ***
med_inc                                     1.368e-06  4.240e-07   3.228 0.001248 ** 
Avg_per_mi                                  7.932e-03  1.005e-03   7.892 2.99e-15 ***
Avg_pctnoh                                  5.446e-03  1.168e-03   4.661 3.15e-06 ***
Avg_p_A65                                   2.727e-03  1.409e-03   1.936 0.052898 .  
P_Ospace                                   -5.151e-03  3.253e-04 -15.833  < 2e-16 ***

summary(Rresp$gam)


Rresp3 <- (gamm(count ~ ns(date,df=35) +pmnew_l0+ pmnewmayear +I((pmnewmayear-10)*(pmnewmayear >10))+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random=list(guid=~1), family = poisson, data = allresp))





#shows there is no sig interaction and thus linear

summary(Rresp3$gam)
pmnew_l0                                    5.047e-04  1.699e-04   2.971 0.002972 ** 
pmnewmayear                                 3.928e-03  2.502e-03   1.570 0.116351    
I((pmnewmayear - 10) * (pmnewmayear > 10))  9.209e-04  3.084e-03   0.299 0.765198    
temp_f_l0                                   1.843e-03  1.253e-04  14.709  < 2e-16 ***
med_inc                                     2.849e-06  4.715e-07   6.042 1.53e-09 ***
Avg_per_mi                                  1.700e-02  9.482e-04  17.930  < 2e-16 ***
Avg_pctnoh                                  6.666e-03  1.328e-03   5.021 5.13e-07 ***
Avg_p_A65                                   7.487e-03  1.561e-03   4.797 1.61e-06 ***
---
#shows there is no sig interaction and thus linear

library(gamm4)



Rresp4 <- (gamm4(count ~ ns(date,df=35) +pmnew_l0+ pmnewmayear+temp_f_l0, random = ~ 1 | guid,  family = poisson, data = allresp))






