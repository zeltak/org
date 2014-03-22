library(foreign) 
library(nlme)


F_T2003_All <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T) 
names(F_T2003_All)



#emisivity offset correction
F_T2003_All$em2 <- F_T2003_All$emis_scale+0.49

#kinetic temprature
F_T2003_All$temp_kin <- F_T2003_All$st_faren/F_T2003_All$em2^0.25

#convert both to c
F_T2003_All$tsc=(5/9)*(F_T2003_All$temp_kin-32)
F_T2003_All$tac=(5/9)*(F_T2003_All$TMIN-32)
F_T2003_All$tmac=(5/9)*(F_T2003_All$TMEAN-32)


#with ramdom slope for surface_temp and an interaction between wdsp and surface temp 

# old non kinetic model
# out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+em2+ndvi+per_built, 
#   random = ~1 + st_faren| DATE, 
# 	data =  F_T2003_All) 
# summary(out.model_T2003)


#new model for tmin
out.model_T2003 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built, 
  random = ~1 + tsc| DATE, 
  data =  F_T2003_All) 
summary(out.model_T2003)


F_T2003_All$predicted <- predict(out.model_T2003)
sum_tac <- lm(F_T2003_All$tac~F_T2003_All$predicted)
summary(sum_tac)


#new model for tmean
out.model_T2003m = lme( tmac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built, 
  random = ~1 + tsc| DATE, 
  data =  F_T2003_All) 
summary(out.model_T2003m)


F_T2003_All$mpredicted <- predict(out.model_T2003m)
sum_tmac <- lm(F_T2003_All$tmac~F_T2003_All$mpredicted)
summary(sum_tmac)


 cor(F_T2003_All$tmac,F_T2003_All$tac)




#check for Bias
beta_bias <- lm(tac ~predicted,data =  F_T2003_All)
summary(beta_bias)






