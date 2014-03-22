library(foreign) 
library(stats)
library(mgcv)
library(splines)
library(nlme)
library(car)
library(reshape)



res_all <- read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.5.NE_RESIDUAL_TEST/3.1.5.4.Work/3.Analysis/mod1/mod1_both_lu.dbf") 

nas <- read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.9_Jamie_Worcester/3.1.9.4.Work/2.Gather_data/lu_guid_final/lu_guid_final.dbf") 



#create diffrences variables from 10x10 grid to the local monitor
res_all$delev <- res_all$elev- res_all$elev10 
res_all$dpopden <- res_all$popden - res_all$popden10 
res_all$durban <- res_all$pcturban -res_all$pcturban10


###NAS###
names(nas)
summary(nas)


#create diffrences variables from 10x10 grid to the local monitor
nas$delev <- nas$elev- nas$elev10 
nas$dpopden <- nas$popden - nas$poden10 
nas$durban <- nas$pcturban -nas$pcturban10


#The GAM model
bp.model.ps<-gam(residpm~s(tden,fx=FALSE,k=10,bs='cr')+delev+
dpopden+durban,
na.action=na.omit,data=res_all)

summary(bp.model.ps)

res_all$pred_locm <-predict(bp.model.ps)

resid_pred <- predict(bp.model.ps,nas)

nas$predicted <- resid_pred


write.csv (nas, "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.9_Jamie_Worcester/3.1.9.4.Work/2.Gather_data/lu_guis_resid_final/lu_resid.csv")



summary(res_all$tden)
summary(nas$tden)
summary(nas$predicted)

