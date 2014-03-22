library (mgcv)
library(nlme)
library(gamm4)
library(reshape)
library(foreign)

cvtable <- data.frame(type=character(17), r2000=numeric(17),r2001=numeric(17),r2002=numeric(17),r2003=numeric(17),r2004=numeric(17),r2005=numeric(17),r2006=numeric(17),r2007=numeric(17),r2008=numeric(17))

cvtable$type <- c("it_1", "it_2","it_3","it_4","it_5","it_6","it_7","it_8","it_9","it_10","R2_preloc","R2_spat_pre","R2_tem_pre","R","R2","spatial","temporal")




#T2000


GAM_T2000 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2000_m2_pred_mpm.csv", header=T) 
summary(GAM_T2000)
GAM_T2000<-na.omit(GAM_T2000)


names(GAM_T2000)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2000_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2000 )

#get the residuals from the above fit
GAM_T2000$resid   <- residuals(smooth_T2000_yearly)

#split the files to the separate bi monthly datsets

T2000_bimon2 <- subset(GAM_T2000 ,GAM_T2000$bimon == "2")
T2000_bimon3 <- subset(GAM_T2000 ,GAM_T2000$bimon == "3")
T2000_bimon4 <- subset(GAM_T2000 ,GAM_T2000$bimon == "4")
T2000_bimon5 <- subset(GAM_T2000 ,GAM_T2000$bimon == "5")
T2000_bimon6 <- subset(GAM_T2000 ,GAM_T2000$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon



fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2000_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2000_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2000_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2000_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2000_bimon6 )

#get the predicted-fitted 

Xpred_2=(T2000_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2000_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2000_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2000_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2000_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2000$newpred <- c(Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2000  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2000  )


GAM_T2000$tpred <- predict(Final_pred_2000)

cor(GAM_T2000$pred,GAM_T2000$tpred)



####import all xy points across new-england
grid_2000 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2000.csv", header=T) 
summary(grid_2000)


grid_2000<-na.omit(grid_2000)


grid_pred2 <- predict(Final_pred_2000,grid_2000,level=0)

augmented.re <- matrix(0,dim(grid_2000)[1],2)
n.guid <- dim(Final_pred_2000$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2000$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2000$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2000$guid==guid.fit[i],])[1]
    augmented.re[grid_2000$guid==guid.fit[i],] <- cbind(rep(Final_pred_2000$coeff$random$guid[i,1],n.obs), rep(Final_pred_2000$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2000$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2000$mixpred <-brandnew.pred



grid_2000_bimon2 <- subset(grid_2000 ,grid_2000$bimon == "2")
grid_2000_bimon3 <- subset(grid_2000 ,grid_2000$bimon == "3")
grid_2000_bimon4 <- subset(grid_2000 ,grid_2000$bimon == "4")
grid_2000_bimon5 <- subset(grid_2000 ,grid_2000$bimon == "5")
grid_2000_bimon6 <- subset(grid_2000 ,grid_2000$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon



bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges


uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2000_bimon2 <- grid_2000_bimon2[order(grid_2000_bimon2$guid),] 
grid_2000_bimon2_merged <- merge(grid_2000_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2000_bimon3 <- grid_2000_bimon3[order(grid_2000_bimon3$guid),] 
grid_2000_bimon3_merged <- merge(grid_2000_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2000_bimon4 <- grid_2000_bimon4[order(grid_2000_bimon4$guid),] 
grid_2000_bimon4_merged <- merge(grid_2000_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2000_bimon5 <- grid_2000_bimon5[order(grid_2000_bimon5$guid),] 
grid_2000_bimon5_merged <- merge(grid_2000_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2000_bimon6 <- grid_2000_bimon6[order(grid_2000_bimon6$guid),] 
grid_2000_bimon6_merged <- merge(grid_2000_bimon6,uniq_gid_bimon6,by="guid")


T2000allbimon <- rbind(grid_2000_bimon2_merged,grid_2000_bimon3_merged,grid_2000_bimon4_merged,grid_2000_bimon5_merged,grid_2000_bimon6_merged)

# create PM_mod3
T2000allbimon$pm_mod3 <-T2000allbimon$mixpred+T2000allbimon$gpred
summary(T2000allbimon$pm_mod3)

#delete negative values
# T2000allbimon <- subset(T2000allbimon,T2000allbimon$pm_mod3 >= "0")
dev.off(null)
hist(T2000allbimon$pm_mod3)

write.dbf(T2000allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2000.dbf") 















#T2001


GAM_T2001 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2001_m2_pred_mpm.csv", header=T) 
summary(GAM_T2001)



names(GAM_T2001)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2001_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2001 )

#get the residuals from the above fit
GAM_T2001$resid   <- residuals(smooth_T2001_yearly)

#split the files to the separate bi monthly datsets
T2001_bimon1 <- subset(GAM_T2001 ,GAM_T2001$bimon == "1")
T2001_bimon2 <- subset(GAM_T2001 ,GAM_T2001$bimon == "2")
T2001_bimon3 <- subset(GAM_T2001 ,GAM_T2001$bimon == "3")
T2001_bimon4 <- subset(GAM_T2001 ,GAM_T2001$bimon == "4")
T2001_bimon5 <- subset(GAM_T2001 ,GAM_T2001$bimon == "5")
T2001_bimon6 <- subset(GAM_T2001 ,GAM_T2001$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2001_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2001_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2001_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2001_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2001_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2001_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2001_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2001$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2001  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2001  )


GAM_T2001$tpred <- predict(Final_pred_2001)

cor(GAM_T2001$pred,GAM_T2001$tpred)



####import all xy points across new-england
grid_2001 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2001.csv", header=T) 

grid_pred2 <- predict(Final_pred_2001,grid_2001,level=0)

augmented.re <- matrix(0,dim(grid_2001)[1],2)
n.guid <- dim(Final_pred_2001$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2001$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2001$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2001$guid==guid.fit[i],])[1]
    augmented.re[grid_2001$guid==guid.fit[i],] <- cbind(rep(Final_pred_2001$coeff$random$guid[i,1],n.obs), rep(Final_pred_2001$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2001$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2001$mixpred <-brandnew.pred



grid_2001_bimon1 <- subset(grid_2001 ,grid_2001$bimon == "1")
grid_2001_bimon2 <- subset(grid_2001 ,grid_2001$bimon == "2")
grid_2001_bimon3 <- subset(grid_2001 ,grid_2001$bimon == "3")
grid_2001_bimon4 <- subset(grid_2001 ,grid_2001$bimon == "4")
grid_2001_bimon5 <- subset(grid_2001 ,grid_2001$bimon == "5")
grid_2001_bimon6 <- subset(grid_2001 ,grid_2001$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2001_bimon1 <- grid_2001_bimon1[order(grid_2001_bimon1$guid),] 
grid_2001_bimon1_merged <- merge(grid_2001_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2001_bimon2 <- grid_2001_bimon2[order(grid_2001_bimon2$guid),] 
grid_2001_bimon2_merged <- merge(grid_2001_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2001_bimon3 <- grid_2001_bimon3[order(grid_2001_bimon3$guid),] 
grid_2001_bimon3_merged <- merge(grid_2001_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2001_bimon4 <- grid_2001_bimon4[order(grid_2001_bimon4$guid),] 
grid_2001_bimon4_merged <- merge(grid_2001_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2001_bimon5 <- grid_2001_bimon5[order(grid_2001_bimon5$guid),] 
grid_2001_bimon5_merged <- merge(grid_2001_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2001_bimon6 <- grid_2001_bimon6[order(grid_2001_bimon6$guid),] 
grid_2001_bimon6_merged <- merge(grid_2001_bimon6,uniq_gid_bimon6,by="guid")


T2001allbimon <- rbind(grid_2001_bimon1_merged,grid_2001_bimon2_merged,grid_2001_bimon3_merged,grid_2001_bimon4_merged,grid_2001_bimon5_merged,grid_2001_bimon6_merged)

# create PM_mod3
T2001allbimon$pm_mod3 <-T2001allbimon$mixpred+T2001allbimon$gpred
summary(T2001allbimon$pm_mod3)

#delete negative values
# T2001allbimon <- subset(T2001allbimon,T2001allbimon$pm_mod3 >= "0")
hist(T2001allbimon$pm_mod3)

write.dbf(T2001allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2001.dbf") 






#T2002


GAM_T2002 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2002_m2_pred_mpm.csv", header=T) 
summary(GAM_T2002)



names(GAM_T2002)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2002_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2002 )

#get the residuals from the above fit
GAM_T2002$resid   <- residuals(smooth_T2002_yearly)

#split the files to the separate bi monthly datsets
T2002_bimon1 <- subset(GAM_T2002 ,GAM_T2002$bimon == "1")
T2002_bimon2 <- subset(GAM_T2002 ,GAM_T2002$bimon == "2")
T2002_bimon3 <- subset(GAM_T2002 ,GAM_T2002$bimon == "3")
T2002_bimon4 <- subset(GAM_T2002 ,GAM_T2002$bimon == "4")
T2002_bimon5 <- subset(GAM_T2002 ,GAM_T2002$bimon == "5")
T2002_bimon6 <- subset(GAM_T2002 ,GAM_T2002$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2002_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2002_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2002_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2002_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2002_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2002_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2002_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2002$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2002  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2002  )


GAM_T2002$tpred <- predict(Final_pred_2002)

cor(GAM_T2002$pred,GAM_T2002$tpred)



####import all xy points across new-england
grid_2002 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2002.csv", header=T) 

grid_pred2 <- predict(Final_pred_2002,grid_2002,level=0)

augmented.re <- matrix(0,dim(grid_2002)[1],2)
n.guid <- dim(Final_pred_2002$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2002$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2002$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2002$guid==guid.fit[i],])[1]
    augmented.re[grid_2002$guid==guid.fit[i],] <- cbind(rep(Final_pred_2002$coeff$random$guid[i,1],n.obs), rep(Final_pred_2002$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2002$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2002$mixpred <-brandnew.pred



grid_2002_bimon1 <- subset(grid_2002 ,grid_2002$bimon == "1")
grid_2002_bimon2 <- subset(grid_2002 ,grid_2002$bimon == "2")
grid_2002_bimon3 <- subset(grid_2002 ,grid_2002$bimon == "3")
grid_2002_bimon4 <- subset(grid_2002 ,grid_2002$bimon == "4")
grid_2002_bimon5 <- subset(grid_2002 ,grid_2002$bimon == "5")
grid_2002_bimon6 <- subset(grid_2002 ,grid_2002$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2002_bimon1 <- grid_2002_bimon1[order(grid_2002_bimon1$guid),] 
grid_2002_bimon1_merged <- merge(grid_2002_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2002_bimon2 <- grid_2002_bimon2[order(grid_2002_bimon2$guid),] 
grid_2002_bimon2_merged <- merge(grid_2002_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2002_bimon3 <- grid_2002_bimon3[order(grid_2002_bimon3$guid),] 
grid_2002_bimon3_merged <- merge(grid_2002_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2002_bimon4 <- grid_2002_bimon4[order(grid_2002_bimon4$guid),] 
grid_2002_bimon4_merged <- merge(grid_2002_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2002_bimon5 <- grid_2002_bimon5[order(grid_2002_bimon5$guid),] 
grid_2002_bimon5_merged <- merge(grid_2002_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2002_bimon6 <- grid_2002_bimon6[order(grid_2002_bimon6$guid),] 
grid_2002_bimon6_merged <- merge(grid_2002_bimon6,uniq_gid_bimon6,by="guid")


T2002allbimon <- rbind(grid_2002_bimon1_merged,grid_2002_bimon2_merged,grid_2002_bimon3_merged,grid_2002_bimon4_merged,grid_2002_bimon5_merged,grid_2002_bimon6_merged)

# create PM_mod3
T2002allbimon$pm_mod3 <-T2002allbimon$mixpred+T2002allbimon$gpred
summary(T2002allbimon$pm_mod3)

#delete negative values
# T2002allbimon <- subset(T2002allbimon,T2002allbimon$pm_mod3 >= "0")
hist(T2002allbimon$pm_mod3)

write.dbf(T2002allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2002.dbf") 




#T2003


GAM_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2003_m2_pred_mpm.csv", header=T) 
summary(GAM_T2003)



names(GAM_T2003)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2003_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2003 )

#get the residuals from the above fit
GAM_T2003$resid   <- residuals(smooth_T2003_yearly)

#split the files to the separate bi monthly datsets
T2003_bimon1 <- subset(GAM_T2003 ,GAM_T2003$bimon == "1")
T2003_bimon2 <- subset(GAM_T2003 ,GAM_T2003$bimon == "2")
T2003_bimon3 <- subset(GAM_T2003 ,GAM_T2003$bimon == "3")
T2003_bimon4 <- subset(GAM_T2003 ,GAM_T2003$bimon == "4")
T2003_bimon5 <- subset(GAM_T2003 ,GAM_T2003$bimon == "5")
T2003_bimon6 <- subset(GAM_T2003 ,GAM_T2003$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2003_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2003_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2003_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2003_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2003_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2003_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2003_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2003$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2003  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2003  )


GAM_T2003$tpred <- predict(Final_pred_2003)

cor(GAM_T2003$pred,GAM_T2003$tpred)



####import all xy points across new-england
grid_2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2003.csv", header=T) 

grid_pred2 <- predict(Final_pred_2003,grid_2003,level=0)

augmented.re <- matrix(0,dim(grid_2003)[1],2)
n.guid <- dim(Final_pred_2003$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2003$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2003$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2003$guid==guid.fit[i],])[1]
    augmented.re[grid_2003$guid==guid.fit[i],] <- cbind(rep(Final_pred_2003$coeff$random$guid[i,1],n.obs), rep(Final_pred_2003$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2003$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2003$mixpred <-brandnew.pred



grid_2003_bimon1 <- subset(grid_2003 ,grid_2003$bimon == "1")
grid_2003_bimon2 <- subset(grid_2003 ,grid_2003$bimon == "2")
grid_2003_bimon3 <- subset(grid_2003 ,grid_2003$bimon == "3")
grid_2003_bimon4 <- subset(grid_2003 ,grid_2003$bimon == "4")
grid_2003_bimon5 <- subset(grid_2003 ,grid_2003$bimon == "5")
grid_2003_bimon6 <- subset(grid_2003 ,grid_2003$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2003_bimon1 <- grid_2003_bimon1[order(grid_2003_bimon1$guid),] 
grid_2003_bimon1_merged <- merge(grid_2003_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2003_bimon2 <- grid_2003_bimon2[order(grid_2003_bimon2$guid),] 
grid_2003_bimon2_merged <- merge(grid_2003_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2003_bimon3 <- grid_2003_bimon3[order(grid_2003_bimon3$guid),] 
grid_2003_bimon3_merged <- merge(grid_2003_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2003_bimon4 <- grid_2003_bimon4[order(grid_2003_bimon4$guid),] 
grid_2003_bimon4_merged <- merge(grid_2003_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2003_bimon5 <- grid_2003_bimon5[order(grid_2003_bimon5$guid),] 
grid_2003_bimon5_merged <- merge(grid_2003_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2003_bimon6 <- grid_2003_bimon6[order(grid_2003_bimon6$guid),] 
grid_2003_bimon6_merged <- merge(grid_2003_bimon6,uniq_gid_bimon6,by="guid")


T2003allbimon <- rbind(grid_2003_bimon1_merged,grid_2003_bimon2_merged,grid_2003_bimon3_merged,grid_2003_bimon4_merged,grid_2003_bimon5_merged,grid_2003_bimon6_merged)

# create PM_mod3
T2003allbimon$pm_mod3 <-T2003allbimon$mixpred+T2003allbimon$gpred
summary(T2003allbimon$pm_mod3)

#delete negative values
# T2003allbimon <- subset(T2003allbimon,T2003allbimon$pm_mod3 >= "0")
hist(T2003allbimon$pm_mod3)

write.dbf(T2003allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2003.dbf") 





#T2004


GAM_T2004 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2004_m2_pred_mpm.csv", header=T) 
summary(GAM_T2004)



names(GAM_T2004)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2004_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2004 )

#get the residuals from the above fit
GAM_T2004$resid   <- residuals(smooth_T2004_yearly)

#split the files to the separate bi monthly datsets
T2004_bimon1 <- subset(GAM_T2004 ,GAM_T2004$bimon == "1")
T2004_bimon2 <- subset(GAM_T2004 ,GAM_T2004$bimon == "2")
T2004_bimon3 <- subset(GAM_T2004 ,GAM_T2004$bimon == "3")
T2004_bimon4 <- subset(GAM_T2004 ,GAM_T2004$bimon == "4")
T2004_bimon5 <- subset(GAM_T2004 ,GAM_T2004$bimon == "5")
T2004_bimon6 <- subset(GAM_T2004 ,GAM_T2004$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2004_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2004_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2004_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2004_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2004_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2004_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2004_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2004$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2004  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2004  )


GAM_T2004$tpred <- predict(Final_pred_2004)

cor(GAM_T2004$pred,GAM_T2004$tpred)



####import all xy points across new-england
grid_2004 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2004.csv", header=T) 

grid_pred2 <- predict(Final_pred_2004,grid_2004,level=0)

augmented.re <- matrix(0,dim(grid_2004)[1],2)
n.guid <- dim(Final_pred_2004$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2004$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2004$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2004$guid==guid.fit[i],])[1]
    augmented.re[grid_2004$guid==guid.fit[i],] <- cbind(rep(Final_pred_2004$coeff$random$guid[i,1],n.obs), rep(Final_pred_2004$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2004$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2004$mixpred <-brandnew.pred



grid_2004_bimon1 <- subset(grid_2004 ,grid_2004$bimon == "1")
grid_2004_bimon2 <- subset(grid_2004 ,grid_2004$bimon == "2")
grid_2004_bimon3 <- subset(grid_2004 ,grid_2004$bimon == "3")
grid_2004_bimon4 <- subset(grid_2004 ,grid_2004$bimon == "4")
grid_2004_bimon5 <- subset(grid_2004 ,grid_2004$bimon == "5")
grid_2004_bimon6 <- subset(grid_2004 ,grid_2004$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2004_bimon1 <- grid_2004_bimon1[order(grid_2004_bimon1$guid),] 
grid_2004_bimon1_merged <- merge(grid_2004_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2004_bimon2 <- grid_2004_bimon2[order(grid_2004_bimon2$guid),] 
grid_2004_bimon2_merged <- merge(grid_2004_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2004_bimon3 <- grid_2004_bimon3[order(grid_2004_bimon3$guid),] 
grid_2004_bimon3_merged <- merge(grid_2004_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2004_bimon4 <- grid_2004_bimon4[order(grid_2004_bimon4$guid),] 
grid_2004_bimon4_merged <- merge(grid_2004_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2004_bimon5 <- grid_2004_bimon5[order(grid_2004_bimon5$guid),] 
grid_2004_bimon5_merged <- merge(grid_2004_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2004_bimon6 <- grid_2004_bimon6[order(grid_2004_bimon6$guid),] 
grid_2004_bimon6_merged <- merge(grid_2004_bimon6,uniq_gid_bimon6,by="guid")


T2004allbimon <- rbind(grid_2004_bimon1_merged,grid_2004_bimon2_merged,grid_2004_bimon3_merged,grid_2004_bimon4_merged,grid_2004_bimon5_merged,grid_2004_bimon6_merged)

# create PM_mod3
T2004allbimon$pm_mod3 <-T2004allbimon$mixpred+T2004allbimon$gpred
summary(T2004allbimon$pm_mod3)

#delete negative values
# T2004allbimon <- subset(T2004allbimon,T2004allbimon$pm_mod3 >= "0")
hist(T2004allbimon$pm_mod3)

write.dbf(T2004allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2004.dbf") 




#T2005


GAM_T2005 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2005_m2_pred_mpm.csv", header=T) 
summary(GAM_T2005)



names(GAM_T2005)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2005_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2005 )

#get the residuals from the above fit
GAM_T2005$resid   <- residuals(smooth_T2005_yearly)

#split the files to the separate bi monthly datsets
T2005_bimon1 <- subset(GAM_T2005 ,GAM_T2005$bimon == "1")
T2005_bimon2 <- subset(GAM_T2005 ,GAM_T2005$bimon == "2")
T2005_bimon3 <- subset(GAM_T2005 ,GAM_T2005$bimon == "3")
T2005_bimon4 <- subset(GAM_T2005 ,GAM_T2005$bimon == "4")
T2005_bimon5 <- subset(GAM_T2005 ,GAM_T2005$bimon == "5")
T2005_bimon6 <- subset(GAM_T2005 ,GAM_T2005$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2005_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2005_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2005_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2005_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2005_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2005_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2005_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2005$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2005  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2005  )


GAM_T2005$tpred <- predict(Final_pred_2005)

cor(GAM_T2005$pred,GAM_T2005$tpred)



####import all xy points across new-england
grid_2005 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2005.csv", header=T) 

grid_pred2 <- predict(Final_pred_2005,grid_2005,level=0)

augmented.re <- matrix(0,dim(grid_2005)[1],2)
n.guid <- dim(Final_pred_2005$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2005$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2005$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2005$guid==guid.fit[i],])[1]
    augmented.re[grid_2005$guid==guid.fit[i],] <- cbind(rep(Final_pred_2005$coeff$random$guid[i,1],n.obs), rep(Final_pred_2005$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2005$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2005$mixpred <-brandnew.pred



grid_2005_bimon1 <- subset(grid_2005 ,grid_2005$bimon == "1")
grid_2005_bimon2 <- subset(grid_2005 ,grid_2005$bimon == "2")
grid_2005_bimon3 <- subset(grid_2005 ,grid_2005$bimon == "3")
grid_2005_bimon4 <- subset(grid_2005 ,grid_2005$bimon == "4")
grid_2005_bimon5 <- subset(grid_2005 ,grid_2005$bimon == "5")
grid_2005_bimon6 <- subset(grid_2005 ,grid_2005$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2005_bimon1 <- grid_2005_bimon1[order(grid_2005_bimon1$guid),] 
grid_2005_bimon1_merged <- merge(grid_2005_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2005_bimon2 <- grid_2005_bimon2[order(grid_2005_bimon2$guid),] 
grid_2005_bimon2_merged <- merge(grid_2005_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2005_bimon3 <- grid_2005_bimon3[order(grid_2005_bimon3$guid),] 
grid_2005_bimon3_merged <- merge(grid_2005_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2005_bimon4 <- grid_2005_bimon4[order(grid_2005_bimon4$guid),] 
grid_2005_bimon4_merged <- merge(grid_2005_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2005_bimon5 <- grid_2005_bimon5[order(grid_2005_bimon5$guid),] 
grid_2005_bimon5_merged <- merge(grid_2005_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2005_bimon6 <- grid_2005_bimon6[order(grid_2005_bimon6$guid),] 
grid_2005_bimon6_merged <- merge(grid_2005_bimon6,uniq_gid_bimon6,by="guid")


T2005allbimon <- rbind(grid_2005_bimon1_merged,grid_2005_bimon2_merged,grid_2005_bimon3_merged,grid_2005_bimon4_merged,grid_2005_bimon5_merged,grid_2005_bimon6_merged)

# create PM_mod3
T2005allbimon$pm_mod3 <-T2005allbimon$mixpred+T2005allbimon$gpred
summary(T2005allbimon$pm_mod3)

#delete negative values
# T2005allbimon <- subset(T2005allbimon,T2005allbimon$pm_mod3 >= "0")
hist(T2005allbimon$pm_mod3)

write.dbf(T2005allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2005.dbf") 





#T2006


GAM_T2006 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2006_m2_pred_mpm.csv", header=T) 
summary(GAM_T2006)



names(GAM_T2006)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2006_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2006 )

#get the residuals from the above fit
GAM_T2006$resid   <- residuals(smooth_T2006_yearly)

#split the files to the separate bi monthly datsets
T2006_bimon1 <- subset(GAM_T2006 ,GAM_T2006$bimon == "1")
T2006_bimon2 <- subset(GAM_T2006 ,GAM_T2006$bimon == "2")
T2006_bimon3 <- subset(GAM_T2006 ,GAM_T2006$bimon == "3")
T2006_bimon4 <- subset(GAM_T2006 ,GAM_T2006$bimon == "4")
T2006_bimon5 <- subset(GAM_T2006 ,GAM_T2006$bimon == "5")
T2006_bimon6 <- subset(GAM_T2006 ,GAM_T2006$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2006_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2006_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2006_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2006_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2006_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2006_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2006_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2006$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2006  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2006  )


GAM_T2006$tpred <- predict(Final_pred_2006)

cor(GAM_T2006$pred,GAM_T2006$tpred)



####import all xy points across new-england
grid_2006 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2006.csv", header=T) 

grid_pred2 <- predict(Final_pred_2006,grid_2006,level=0)

augmented.re <- matrix(0,dim(grid_2006)[1],2)
n.guid <- dim(Final_pred_2006$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2006$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2006$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2006$guid==guid.fit[i],])[1]
    augmented.re[grid_2006$guid==guid.fit[i],] <- cbind(rep(Final_pred_2006$coeff$random$guid[i,1],n.obs), rep(Final_pred_2006$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2006$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2006$mixpred <-brandnew.pred



grid_2006_bimon1 <- subset(grid_2006 ,grid_2006$bimon == "1")
grid_2006_bimon2 <- subset(grid_2006 ,grid_2006$bimon == "2")
grid_2006_bimon3 <- subset(grid_2006 ,grid_2006$bimon == "3")
grid_2006_bimon4 <- subset(grid_2006 ,grid_2006$bimon == "4")
grid_2006_bimon5 <- subset(grid_2006 ,grid_2006$bimon == "5")
grid_2006_bimon6 <- subset(grid_2006 ,grid_2006$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2006_bimon1 <- grid_2006_bimon1[order(grid_2006_bimon1$guid),] 
grid_2006_bimon1_merged <- merge(grid_2006_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2006_bimon2 <- grid_2006_bimon2[order(grid_2006_bimon2$guid),] 
grid_2006_bimon2_merged <- merge(grid_2006_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2006_bimon3 <- grid_2006_bimon3[order(grid_2006_bimon3$guid),] 
grid_2006_bimon3_merged <- merge(grid_2006_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2006_bimon4 <- grid_2006_bimon4[order(grid_2006_bimon4$guid),] 
grid_2006_bimon4_merged <- merge(grid_2006_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2006_bimon5 <- grid_2006_bimon5[order(grid_2006_bimon5$guid),] 
grid_2006_bimon5_merged <- merge(grid_2006_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2006_bimon6 <- grid_2006_bimon6[order(grid_2006_bimon6$guid),] 
grid_2006_bimon6_merged <- merge(grid_2006_bimon6,uniq_gid_bimon6,by="guid")


T2006allbimon <- rbind(grid_2006_bimon1_merged,grid_2006_bimon2_merged,grid_2006_bimon3_merged,grid_2006_bimon4_merged,grid_2006_bimon5_merged,grid_2006_bimon6_merged)

# create PM_mod3
T2006allbimon$pm_mod3 <-T2006allbimon$mixpred+T2006allbimon$gpred
summary(T2006allbimon$pm_mod3)

#delete negative values
# T2006allbimon <- subset(T2006allbimon,T2006allbimon$pm_mod3 >= "0")
hist(T2006allbimon$pm_mod3)

write.dbf(T2006allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2006.dbf") 





#T2007


GAM_T2007 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2007_m2_pred_mpm.csv", header=T) 
summary(GAM_T2007)



names(GAM_T2007)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2007_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2007 )

#get the residuals from the above fit
GAM_T2007$resid   <- residuals(smooth_T2007_yearly)

#split the files to the separate bi monthly datsets
T2007_bimon1 <- subset(GAM_T2007 ,GAM_T2007$bimon == "1")
T2007_bimon2 <- subset(GAM_T2007 ,GAM_T2007$bimon == "2")
T2007_bimon3 <- subset(GAM_T2007 ,GAM_T2007$bimon == "3")
T2007_bimon4 <- subset(GAM_T2007 ,GAM_T2007$bimon == "4")
T2007_bimon5 <- subset(GAM_T2007 ,GAM_T2007$bimon == "5")
T2007_bimon6 <- subset(GAM_T2007 ,GAM_T2007$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2007_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2007_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2007_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2007_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2007_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2007_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2007_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2007$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2007  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2007  )


GAM_T2007$tpred <- predict(Final_pred_2007)

cor(GAM_T2007$pred,GAM_T2007$tpred)



####import all xy points across new-england
grid_2007 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2007.csv", header=T) 

grid_pred2 <- predict(Final_pred_2007,grid_2007,level=0)

augmented.re <- matrix(0,dim(grid_2007)[1],2)
n.guid <- dim(Final_pred_2007$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2007$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2007$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2007$guid==guid.fit[i],])[1]
    augmented.re[grid_2007$guid==guid.fit[i],] <- cbind(rep(Final_pred_2007$coeff$random$guid[i,1],n.obs), rep(Final_pred_2007$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2007$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2007$mixpred <-brandnew.pred



grid_2007_bimon1 <- subset(grid_2007 ,grid_2007$bimon == "1")
grid_2007_bimon2 <- subset(grid_2007 ,grid_2007$bimon == "2")
grid_2007_bimon3 <- subset(grid_2007 ,grid_2007$bimon == "3")
grid_2007_bimon4 <- subset(grid_2007 ,grid_2007$bimon == "4")
grid_2007_bimon5 <- subset(grid_2007 ,grid_2007$bimon == "5")
grid_2007_bimon6 <- subset(grid_2007 ,grid_2007$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2007_bimon1 <- grid_2007_bimon1[order(grid_2007_bimon1$guid),] 
grid_2007_bimon1_merged <- merge(grid_2007_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2007_bimon2 <- grid_2007_bimon2[order(grid_2007_bimon2$guid),] 
grid_2007_bimon2_merged <- merge(grid_2007_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2007_bimon3 <- grid_2007_bimon3[order(grid_2007_bimon3$guid),] 
grid_2007_bimon3_merged <- merge(grid_2007_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2007_bimon4 <- grid_2007_bimon4[order(grid_2007_bimon4$guid),] 
grid_2007_bimon4_merged <- merge(grid_2007_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2007_bimon5 <- grid_2007_bimon5[order(grid_2007_bimon5$guid),] 
grid_2007_bimon5_merged <- merge(grid_2007_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2007_bimon6 <- grid_2007_bimon6[order(grid_2007_bimon6$guid),] 
grid_2007_bimon6_merged <- merge(grid_2007_bimon6,uniq_gid_bimon6,by="guid")


T2007allbimon <- rbind(grid_2007_bimon1_merged,grid_2007_bimon2_merged,grid_2007_bimon3_merged,grid_2007_bimon4_merged,grid_2007_bimon5_merged,grid_2007_bimon6_merged)

# create PM_mod3
T2007allbimon$pm_mod3 <-T2007allbimon$mixpred+T2007allbimon$gpred
summary(T2007allbimon$pm_mod3)

#delete negative values
# T2007allbimon <- subset(T2007allbimon,T2007allbimon$pm_mod3 >= "0")
hist(T2007allbimon$pm_mod3)

write.dbf(T2007allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2007.dbf") 





#T2008


GAM_T2008 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2008_m2_pred_mpm.csv", header=T) 
summary(GAM_T2008)



names(GAM_T2008)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2008_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2008 )

#get the residuals from the above fit
GAM_T2008$resid   <- residuals(smooth_T2008_yearly)

#split the files to the separate bi monthly datsets
T2008_bimon1 <- subset(GAM_T2008 ,GAM_T2008$bimon == "1")
T2008_bimon2 <- subset(GAM_T2008 ,GAM_T2008$bimon == "2")
T2008_bimon3 <- subset(GAM_T2008 ,GAM_T2008$bimon == "3")
T2008_bimon4 <- subset(GAM_T2008 ,GAM_T2008$bimon == "4")
T2008_bimon5 <- subset(GAM_T2008 ,GAM_T2008$bimon == "5")
T2008_bimon6 <- subset(GAM_T2008 ,GAM_T2008$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2008_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2008_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2008_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2008_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2008_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2008_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2008_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2008$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2008  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2008  )


GAM_T2008$tpred <- predict(Final_pred_2008)

cor(GAM_T2008$pred,GAM_T2008$tpred)



####import all xy points across new-england
grid_2008 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2008.csv", header=T) 

grid_pred2 <- predict(Final_pred_2008,grid_2008,level=0)

augmented.re <- matrix(0,dim(grid_2008)[1],2)
n.guid <- dim(Final_pred_2008$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2008$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2008$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2008$guid==guid.fit[i],])[1]
    augmented.re[grid_2008$guid==guid.fit[i],] <- cbind(rep(Final_pred_2008$coeff$random$guid[i,1],n.obs), rep(Final_pred_2008$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2008$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2008$mixpred <-brandnew.pred



grid_2008_bimon1 <- subset(grid_2008 ,grid_2008$bimon == "1")
grid_2008_bimon2 <- subset(grid_2008 ,grid_2008$bimon == "2")
grid_2008_bimon3 <- subset(grid_2008 ,grid_2008$bimon == "3")
grid_2008_bimon4 <- subset(grid_2008 ,grid_2008$bimon == "4")
grid_2008_bimon5 <- subset(grid_2008 ,grid_2008$bimon == "5")
grid_2008_bimon6 <- subset(grid_2008 ,grid_2008$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


bim1_pred <- predict.gam(fit2_1,uniq_gid_bimon1)
uniq_gid_bimon1$gpred <-bim1_pred


bim2_pred <- predict.gam(fit2_2,uniq_gid_bimon2)
uniq_gid_bimon2$gpred <-bim2_pred


bim3_pred <- predict.gam(fit2_3,uniq_gid_bimon3)
uniq_gid_bimon3$gpred <-bim3_pred

bim4_pred <- predict.gam(fit2_4,uniq_gid_bimon4)
uniq_gid_bimon4$gpred <-bim4_pred

bim5_pred <- predict.gam(fit2_5,uniq_gid_bimon5)
uniq_gid_bimon5$gpred <-bim5_pred


bim6_pred <- predict.gam(fit2_6,uniq_gid_bimon6)
uniq_gid_bimon6$gpred <-bim6_pred


#merge things back togheter


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> merges

uniq_gid_bimon1 <- uniq_gid_bimon1[order(uniq_gid_bimon1$guid),] 
grid_2008_bimon1 <- grid_2008_bimon1[order(grid_2008_bimon1$guid),] 
grid_2008_bimon1_merged <- merge(grid_2008_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2008_bimon2 <- grid_2008_bimon2[order(grid_2008_bimon2$guid),] 
grid_2008_bimon2_merged <- merge(grid_2008_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2008_bimon3 <- grid_2008_bimon3[order(grid_2008_bimon3$guid),] 
grid_2008_bimon3_merged <- merge(grid_2008_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2008_bimon4 <- grid_2008_bimon4[order(grid_2008_bimon4$guid),] 
grid_2008_bimon4_merged <- merge(grid_2008_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2008_bimon5 <- grid_2008_bimon5[order(grid_2008_bimon5$guid),] 
grid_2008_bimon5_merged <- merge(grid_2008_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2008_bimon6 <- grid_2008_bimon6[order(grid_2008_bimon6$guid),] 
grid_2008_bimon6_merged <- merge(grid_2008_bimon6,uniq_gid_bimon6,by="guid")


T2008allbimon <- rbind(grid_2008_bimon1_merged,grid_2008_bimon2_merged,grid_2008_bimon3_merged,grid_2008_bimon4_merged,grid_2008_bimon5_merged,grid_2008_bimon6_merged)

# create PM_mod3
T2008allbimon$pm_mod3 <-T2008allbimon$mixpred+T2008allbimon$gpred
summary(T2008allbimon$pm_mod3)

#delete negative values
# T2008allbimon <- subset(T2008allbimon,T2008allbimon$pm_mod3 >= "0")
hist(T2008allbimon$pm_mod3)

write.dbf(T2008allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2008.dbf") 

