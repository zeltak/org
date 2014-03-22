
GAM_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_mod2predall.csv", header=T) 


library(nlme)

smooth_T2003_yearly = lme(predicted ~ mtmp  ,
	random = list(guid= ~1 + mtmp ),
data= GAM_T2003 )

res_T2003   <- residuals(smooth_T2003_yearly)

GAM_T2003$resid= res_T2003  


T2003_bimon1 <- subset(GAM_T2003 ,GAM_T2003$bimon == "1")
T2003_bimon2 <- subset(GAM_T2003 ,GAM_T2003$bimon == "2")
T2003_bimon3 <- subset(GAM_T2003 ,GAM_T2003$bimon == "3")
T2003_bimon4 <- subset(GAM_T2003 ,GAM_T2003$bimon == "4")
T2003_bimon5 <- subset(GAM_T2003 ,GAM_T2003$bimon == "5")
T2003_bimon6 <- subset(GAM_T2003 ,GAM_T2003$bimon == "6")


library (mgcv)


fit2_1 = gam(resid ~ s(X,Y),
data= T2003_bimon1 )

fit2_2 = gam(resid ~ s(X,Y),
data= T2003_bimon2 )

fit2_3 = gam(resid ~ s(X,Y),
data= T2003_bimon3 )

fit2_4 = gam(resid ~ s(X,Y),
data= T2003_bimon4 )

fit2_5 = gam(resid ~ s(X,Y),
data= T2003_bimon5 )

fit2_6 = gam(resid ~ s(X,Y),
data= T2003_bimon6 )

Xpred_1=(T2003_bimon1$predicted - fit2_1$fitted)
Xpred_2=(T2003_bimon2$predicted - fit2_2$fitted)
Xpred_3=(T2003_bimon3$predicted - fit2_3$fitted)
Xpred_4=(T2003_bimon4$predicted - fit2_4$fitted)
Xpred_5=(T2003_bimon5$predicted - fit2_5$fitted)
Xpred_6=(T2003_bimon6$predicted - fit2_6$fitted)


ALL_pred <- c(Xpred_1, Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)


GAM_T2003$newpred = ALL_pred



Final_pred_2003  = lme(predicted ~ mtmp  ,
	random = list(guid= ~1 + mtmp ),
data= GAM_T2003  )



####import all xy points across new-england
grid_2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_grid.csv", header=T) 


grid_pred <- predict(Final_pred_2003,grid_2003,newdata.guaranteed=True)

grid_2003$mixpred <- grid_pred

names(grid_2003)







#export grid and predictions

library(foreign) 

grid_2003_bimon1 <- subset(grid_2003 ,grid_2003$bimon == "1")
grid_2003_bimon2 <- subset(grid_2003 ,grid_2003$bimon == "2")
grid_2003_bimon3 <- subset(grid_2003 ,grid_2003$bimon == "3")
grid_2003_bimon4 <- subset(grid_2003 ,grid_2003$bimon == "4")
grid_2003_bimon5 <- subset(grid_2003 ,grid_2003$bimon == "5")
grid_2003_bimon6 <- subset(grid_2003 ,grid_2003$bimon == "6")


write.dbf(grid_2003_bimon1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon1.dbf") 
write.dbf(grid_2003_bimon2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon2.dbf") 
write.dbf(grid_2003_bimon3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon3.dbf") 
write.dbf(grid_2003_bimon4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon4.dbf") 
write.dbf(grid_2003_bimon5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon5.dbf") 
write.dbf(grid_2003_bimon6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon6.dbf") 





## export bimon part

uniq_gid_bimon1 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred

uniq_gid_bimon2 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred

uniq_gid_bimon3 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

uniq_gid_bimon4 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred


uniq_gid_bimon5 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred

uniq_gid_bimon6 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred

write.csv(uniq_gid_bimon1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon1.csv") 
write.csv(uniq_gid_bimon2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon2.csv") 
write.csv(uniq_gid_bimon3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon3.csv") 
write.csv(uniq_gid_bimon4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon4.csv") 
write.csv(uniq_gid_bimon5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon5.csv") 
write.csv(uniq_gid_bimon6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon6.csv") 





