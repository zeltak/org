library(foreign) 
library(reshape)



###Import data:

#original mixed model file

F_T2003_All = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T) 



#emisivity offset correction
F_T2003_All$em2 <- F_T2003_All$emis_scale+0.49

#kinetic temprature
F_T2003_All$temp_kin <- F_T2003_All$st_faren/F_T2003_All$em2^0.25

#convert both to c
F_T2003_All$tsc=(5/9)*(F_T2003_All$temp_kin-32)
F_T2003_All$tac=(5/9)*(F_T2003_All$TMIN-32)




#All AOD stations in study area (not only ones with PM2.5 station data
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))




#emisivity offset correction
mod2_T2003$em2 <- mod2_T2003$emis_scale+0.49
#kinetic temprature
mod2_T2003$temp_kin <- mod2_T2003$st_faren/mod2_T2003$em2^0.25
#convert both to c
mod2_T2003$tsc=(5/9)*(mod2_T2003$temp_kin-32)






###model start:

library(nlme) 


 
out.model_T2003 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
	random = ~1 + tsc| DATE,
	data =  F_T2003_All) 
summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred/T2003_mod2pred.dbf")

