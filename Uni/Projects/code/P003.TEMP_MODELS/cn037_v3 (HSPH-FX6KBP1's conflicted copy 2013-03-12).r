library (mgcv)
library(nlme)
library(foreign)
library(reshape)

GAM_T2003 <-  read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/old/T2003_mod2predall.csv", header=T)  



summary(GAM_T2003)
#omit missing predictions
GAM_T2003 = na.omit(GAM_T2003)



library(nlme)

smooth_T2003_yearly = lme(predicted ~ mtmp  ,
                          random = list(guid= ~1 + mtmp ),
                          data= GAM_T2003 )



GAM_T2003$tst1<-predict(smooth_T2003_yearly)

cor(GAM_T2003$tst1,GAM_T2003$predicted)^2


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



Final_pred_2003  = lme(newpred ~ mtmp  ,
                       random = list(guid= ~1 + mtmp ),
                       data= GAM_T2003  )

GAM_T2003$tst2<-predict(Final_pred_2003)

cor(GAM_T2003$tst2,GAM_T2003$predicted)^2

summary(GAM_T2003)

####import all xy points across new-england
grid_2003 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_grid_reg.csv", header=T) 
summary(grid_2003)

DELLIST <- names(grid_2003) %in% c("OBJECTID.x", "Id.x", "X.x", "Y.x", "Shape_Leng", "Shape_Area", "jy", "jul",  "mtemp","OBJECTID.y", "Id.y", "X.y", "Y.y")
grid_2003 <- grid_2003[!DELLIST]



grid_pred <- predict(Final_pred_2003,grid_2003,newdata.guaranteed=True)
grid_2003$mixpred <- grid_pred
summary(grid_2003)

#export grid and predictions
library(foreign) 
grid_2003_bimon1 <- subset(grid_2003 ,grid_2003$bimon == "1")
grid_2003_bimon2 <- subset(grid_2003 ,grid_2003$bimon == "2")
grid_2003_bimon3 <- subset(grid_2003 ,grid_2003$bimon == "3")
grid_2003_bimon4 <- subset(grid_2003 ,grid_2003$bimon == "4")
grid_2003_bimon5 <- subset(grid_2003 ,grid_2003$bimon == "5")
grid_2003_bimon6 <- subset(grid_2003 ,grid_2003$bimon == "6")


write.dbf(grid_2003_bimon1,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon1.dbf") 
write.dbf(grid_2003_bimon2,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon2.dbf") 
write.dbf(grid_2003_bimon3,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon3.dbf") 
write.dbf(grid_2003_bimon4,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon4.dbf") 
write.dbf(grid_2003_bimon5,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon5.dbf") 
write.dbf(grid_2003_bimon6,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_bimon6.dbf") 



library(reshape)

## export bimon part

uniq_gid_bimon1 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon1 <- rename(uniq_gid_bimon1, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim2_pred


uniq_gid_bimon2 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon2 <- rename(uniq_gid_bimon2, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred

uniq_gid_bimon3 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon3 <- rename(uniq_gid_bimon3, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim2_pred

uniq_gid_bimon4 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon4 <- rename(uniq_gid_bimon4, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim2_pred

uniq_gid_bimon5 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon5 <- rename(uniq_gid_bimon5, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim2_pred

uniq_gid_bimon6 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon6 <- rename(uniq_gid_bimon6, c(X.x="X",Y.y="Y")) 
bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim2_pred






write.csv(uniq_gid_bimon1,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon1.csv") 
write.csv(uniq_gid_bimon2,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon2.csv") 
write.csv(uniq_gid_bimon3,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon3.csv") 
write.csv(uniq_gid_bimon4,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon4.csv") 
write.csv(uniq_gid_bimon5,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon5.csv") 
write.csv(uniq_gid_bimon6,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003_bimon6.csv") 





