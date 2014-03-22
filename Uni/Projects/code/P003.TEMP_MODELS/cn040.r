library(foreign) 
library(reshape)

#split function
splitcv <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/10))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}



#s1




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s1 <- splitcv(mod1_T2003)

mod1_T2003_10_s1 <- splits_s1$trainset
mod1_T2003_90_s1 <- splits_s1$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
	data = mod1_T2003_90_s1) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s1.dbf")





#s2




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s2 <- splitcv(mod1_T2003)

mod1_T2003_10_s2 <- splits_s2$trainset
mod1_T2003_90_s2 <- splits_s2$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s2) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s2.dbf")





#s3




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s3 <- splitcv(mod1_T2003)

mod1_T2003_10_s3 <- splits_s3$trainset
mod1_T2003_90_s3 <- splits_s3$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s3) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s3.dbf")




#s4




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s4 <- splitcv(mod1_T2003)

mod1_T2003_10_s4 <- splits_s4$trainset
mod1_T2003_90_s4 <- splits_s4$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s4) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s4.dbf")




#s5




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s5 <- splitcv(mod1_T2003)

mod1_T2003_10_s5 <- splits_s5$trainset
mod1_T2003_90_s5 <- splits_s5$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s5) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s5.dbf")


#s6




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s6 <- splitcv(mod1_T2003)

mod1_T2003_10_s6 <- splits_s6$trainset
mod1_T2003_90_s6 <- splits_s6$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s6) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s6.dbf")




#s7




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s7 <- splitcv(mod1_T2003)

mod1_T2003_10_s7 <- splits_s7$trainset
mod1_T2003_90_s7 <- splits_s7$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s7) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s7.dbf")


#s8




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s8 <- splitcv(mod1_T2003)

mod1_T2003_10_s8 <- splits_s8$trainset
mod1_T2003_90_s8 <- splits_s8$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s8) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s8.dbf")




#s9




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s9 <- splitcv(mod1_T2003)

mod1_T2003_10_s9 <- splits_s9$trainset
mod1_T2003_90_s9 <- splits_s9$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s9) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s9.dbf")



#s10




#mod1 file
mod1_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T)

#mod2 file
mod2_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2/T2003.csv", header=T) 

mod2_T2003 <- rename(mod2_T2003, c(date="DATE"))

#splits

splits_s10 <- splitcv(mod1_T2003)

mod1_T2003_10_s10 <- splits_s10$trainset
mod1_T2003_90_s10 <- splits_s10$testset



###model start:

library(nlme) 


 
out.model_T2003 = lme( TMIN ~ st_faren+windsp_krig*st_faren+elevation+emis_scale+ndvi+per_built,
  random = ~1 + st_faren| DATE,
  data = mod1_T2003_90_s10) 



summary(out.model_T2003) 

newpredT2003 = predict(object=out.model_T2003, newdata=mod2_T2003 )

mod2_T2003$predicted = newpredT2003





##exports:

#export to FULL AOD dataset with prediction values

write.dbf(mod2_T2003, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod2_pred_cv/T2003_mod2pred_s10.dbf")





