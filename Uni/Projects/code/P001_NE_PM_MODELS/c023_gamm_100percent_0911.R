library (mgcv)
library(nlme)
library(gamm4)
library(reshape)
library(foreign)

cvtable <- data.frame(type=character(17), r2000=numeric(17),r2001=numeric(17),r2002=numeric(17),r2003=numeric(17),r2004=numeric(17),r2005=numeric(17),r2006=numeric(17),r2007=numeric(17),r2008=numeric(17))

cvtable$type <- c("it_1", "it_2","it_3","it_4","it_5","it_6","it_7","it_8","it_9","it_10","R2_preloc","R2_spat_pre","R2_tem_pre","R","R2","spatial","temporal")





#T2009


GAM_T2009 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2009_m2_pred_mpm.csv", header=T) 
summary(GAM_T2009)


#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2009_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2009 )

#get the residuals from the above fit
GAM_T2009$resid   <- residuals(smooth_T2009_yearly)

#split the files to the separate bi monthly datsets
T2009_bimon1 <- subset(GAM_T2009 ,GAM_T2009$bimon == "1")
T2009_bimon2 <- subset(GAM_T2009 ,GAM_T2009$bimon == "2")
T2009_bimon3 <- subset(GAM_T2009 ,GAM_T2009$bimon == "3")
T2009_bimon4 <- subset(GAM_T2009 ,GAM_T2009$bimon == "4")
T2009_bimon5 <- subset(GAM_T2009 ,GAM_T2009$bimon == "5")
T2009_bimon6 <- subset(GAM_T2009 ,GAM_T2009$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2009_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2009_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2009_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2009_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2009_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2009_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2009_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2009$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2009  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2009  )


GAM_T2009$tpred <- predict(Final_pred_2009)

cor(GAM_T2009$pred,GAM_T2009$tpred)

plot(GAM_T2009$mpm_F,GAM_T2009$pred)

####import all xy points across new-england
grid_2009 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2009.csv", header=T) 

grid_pred2 <- predict(Final_pred_2009,grid_2009,level=0)

augmented.re <- matrix(0,dim(grid_2009)[1],2)
n.guid <- dim(Final_pred_2009$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2009$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2009$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2009$guid==guid.fit[i],])[1]
    augmented.re[grid_2009$guid==guid.fit[i],] <- cbind(rep(Final_pred_2009$coeff$random$guid[i,1],n.obs), rep(Final_pred_2009$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2009$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2009$mixpred <-brandnew.pred



grid_2009_bimon1 <- subset(grid_2009 ,grid_2009$bimon == "1")
grid_2009_bimon2 <- subset(grid_2009 ,grid_2009$bimon == "2")
grid_2009_bimon3 <- subset(grid_2009 ,grid_2009$bimon == "3")
grid_2009_bimon4 <- subset(grid_2009 ,grid_2009$bimon == "4")
grid_2009_bimon5 <- subset(grid_2009 ,grid_2009$bimon == "5")
grid_2009_bimon6 <- subset(grid_2009 ,grid_2009$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


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
grid_2009_bimon1 <- grid_2009_bimon1[order(grid_2009_bimon1$guid),] 
grid_2009_bimon1_merged <- merge(grid_2009_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2009_bimon2 <- grid_2009_bimon2[order(grid_2009_bimon2$guid),] 
grid_2009_bimon2_merged <- merge(grid_2009_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2009_bimon3 <- grid_2009_bimon3[order(grid_2009_bimon3$guid),] 
grid_2009_bimon3_merged <- merge(grid_2009_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2009_bimon4 <- grid_2009_bimon4[order(grid_2009_bimon4$guid),] 
grid_2009_bimon4_merged <- merge(grid_2009_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2009_bimon5 <- grid_2009_bimon5[order(grid_2009_bimon5$guid),] 
grid_2009_bimon5_merged <- merge(grid_2009_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2009_bimon6 <- grid_2009_bimon6[order(grid_2009_bimon6$guid),] 
grid_2009_bimon6_merged <- merge(grid_2009_bimon6,uniq_gid_bimon6,by="guid")


T2009allbimon <- rbind(grid_2009_bimon1_merged,grid_2009_bimon2_merged,grid_2009_bimon3_merged,grid_2009_bimon4_merged,grid_2009_bimon5_merged,grid_2009_bimon6_merged)

# create PM_mod3
T2009allbimon$pm_mod3 <-T2009allbimon$mixpred+T2009allbimon$gpred
summary(T2009allbimon$pm_mod3)

#delete negative values
# T2009allbimon <- subset(T2009allbimon,T2009allbimon$pm_mod3 >= "0")
hist(T2009allbimon$pm_mod3)

write.dbf(T2009allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2009.dbf") 









#T2010


GAM_T2010 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2010_m2_pred_mpm.csv", header=T) 
summary(GAM_T2010)

plot(GAM_T2010$mpm_F,GAM_T2010$pred)


names(GAM_T2010)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2010_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2010 )

#get the residuals from the above fit
GAM_T2010$resid   <- residuals(smooth_T2010_yearly)

#split the files to the separate bi monthly datsets
T2010_bimon1 <- subset(GAM_T2010 ,GAM_T2010$bimon == "1")
T2010_bimon2 <- subset(GAM_T2010 ,GAM_T2010$bimon == "2")
T2010_bimon3 <- subset(GAM_T2010 ,GAM_T2010$bimon == "3")
T2010_bimon4 <- subset(GAM_T2010 ,GAM_T2010$bimon == "4")
T2010_bimon5 <- subset(GAM_T2010 ,GAM_T2010$bimon == "5")
T2010_bimon6 <- subset(GAM_T2010 ,GAM_T2010$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2010_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2010_bimon1$pred - fit2_1$fitted)
Xpred_2=(T2010_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2010_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2010_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2010_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2010_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2010$newpred <- c( Xpred_1,Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2010  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2010  )


GAM_T2010$tpred <- predict(Final_pred_2010)

cor(GAM_T2010$pred,GAM_T2010$tpred)



####import all xy points across new-england
grid_2010 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2010.csv", header=T) 

grid_pred2 <- predict(Final_pred_2010,grid_2010,level=0)

augmented.re <- matrix(0,dim(grid_2010)[1],2)
n.guid <- dim(Final_pred_2010$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2010$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2010$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2010$guid==guid.fit[i],])[1]
    augmented.re[grid_2010$guid==guid.fit[i],] <- cbind(rep(Final_pred_2010$coeff$random$guid[i,1],n.obs), rep(Final_pred_2010$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2010$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2010$mixpred <-brandnew.pred



grid_2010_bimon1 <- subset(grid_2010 ,grid_2010$bimon == "1")
grid_2010_bimon2 <- subset(grid_2010 ,grid_2010$bimon == "2")
grid_2010_bimon3 <- subset(grid_2010 ,grid_2010$bimon == "3")
grid_2010_bimon4 <- subset(grid_2010 ,grid_2010$bimon == "4")
grid_2010_bimon5 <- subset(grid_2010 ,grid_2010$bimon == "5")
grid_2010_bimon6 <- subset(grid_2010 ,grid_2010$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


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
grid_2010_bimon1 <- grid_2010_bimon1[order(grid_2010_bimon1$guid),] 
grid_2010_bimon1_merged <- merge(grid_2010_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2010_bimon2 <- grid_2010_bimon2[order(grid_2010_bimon2$guid),] 
grid_2010_bimon2_merged <- merge(grid_2010_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2010_bimon3 <- grid_2010_bimon3[order(grid_2010_bimon3$guid),] 
grid_2010_bimon3_merged <- merge(grid_2010_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2010_bimon4 <- grid_2010_bimon4[order(grid_2010_bimon4$guid),] 
grid_2010_bimon4_merged <- merge(grid_2010_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2010_bimon5 <- grid_2010_bimon5[order(grid_2010_bimon5$guid),] 
grid_2010_bimon5_merged <- merge(grid_2010_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2010_bimon6 <- grid_2010_bimon6[order(grid_2010_bimon6$guid),] 
grid_2010_bimon6_merged <- merge(grid_2010_bimon6,uniq_gid_bimon6,by="guid")


T2010allbimon <- rbind(grid_2010_bimon1_merged,grid_2010_bimon2_merged,grid_2010_bimon3_merged,grid_2010_bimon4_merged,grid_2010_bimon5_merged,grid_2010_bimon6_merged)

# create PM_mod3
T2010allbimon$pm_mod3 <-T2010allbimon$mixpred+T2010allbimon$gpred
summary(T2010allbimon$pm_mod3)

#delete negative values
# T2010allbimon <- subset(T2010allbimon,T2010allbimon$pm_mod3 >= "0")
hist(T2010allbimon$pm_mod3)

write.dbf(T2010allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2010.dbf") 









#T2011


GAM_T2011 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN004_mod2pred/T2011_m2_pred_mpm.csv", header=T) 
summary(GAM_T2011)

plot(GAM_T2011$mpm_F,GAM_T2011$pred)


names(GAM_T2011)

#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2011_yearly = lme(pred ~ mpm_F*as.factor(bimon)  ,
                          random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),
                          data= GAM_T2011 )

#get the residuals from the above fit
GAM_T2011$resid   <- residuals(smooth_T2011_yearly)

#split the files to the separate bi monthly datsets
T2011_bimon1 <- subset(GAM_T2011 ,GAM_T2011$bimon == "1")
T2011_bimon2 <- subset(GAM_T2011 ,GAM_T2011$bimon == "2")
T2011_bimon3 <- subset(GAM_T2011 ,GAM_T2011$bimon == "3")
T2011_bimon4 <- subset(GAM_T2011 ,GAM_T2011$bimon == "4")
T2011_bimon5 <- subset(GAM_T2011 ,GAM_T2011$bimon == "5")
T2011_bimon6 <- subset(GAM_T2011 ,GAM_T2011$bimon == "6")

#run the separate splines (smooth) for x and y for each bimon
#fit2_1 = gam(resid ~ s(Long_AOD,Lat_AOD),
#            data= T2011_bimon1 )


fit2_2 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2011_bimon2 )

fit2_3 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2011_bimon3 )

fit2_4 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2011_bimon4 )

fit2_5 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2011_bimon5 )

fit2_6 = gam(resid ~ s(Long_AOD,Lat_AOD),
             data= T2011_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2011_bimon1$pred)
Xpred_2=(T2011_bimon2$pred - fit2_2$fitted)
Xpred_3=(T2011_bimon3$pred - fit2_3$fitted)
Xpred_4=(T2011_bimon4$pred - fit2_4$fitted)
Xpred_5=(T2011_bimon5$pred - fit2_5$fitted)
Xpred_6=(T2011_bimon6$pred - fit2_6$fitted)


#remerge to 1 file
GAM_T2011$newpred <- c( Xpred_1, Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)

#rerun the lme on the predictions including the spatial spline (smooth)
Final_pred_2011  = lme(newpred ~ mpm_F  ,
                       random = list(guid= ~1 + mpm_F ),control=lmeControl(opt = "optim"),data= GAM_T2011  )


GAM_T2011$tpred <- predict(Final_pred_2011)

cor(GAM_T2011$pred,GAM_T2011$tpred)



####import all xy points across new-england
grid_2011 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/ufullgrid_mpm_2011.csv", header=T) 

grid_pred2 <- predict(Final_pred_2011,grid_2011,level=0)

augmented.re <- matrix(0,dim(grid_2011)[1],2)
n.guid <- dim(Final_pred_2011$coeff$random$guid)[1]
guid.fit <- as.numeric(row.names(Final_pred_2011$coeff$random$guid))


for (i in 1:n.guid)
{
  
  present <- guid.fit[i]%in%unique(grid_2011$guid)
  
  if (present) 
  {  
    n.obs <- dim(augmented.re[grid_2011$guid==guid.fit[i],])[1]
    augmented.re[grid_2011$guid==guid.fit[i],] <- cbind(rep(Final_pred_2011$coeff$random$guid[i,1],n.obs), rep(Final_pred_2011$coeff$random$guid[i,2],n.obs))      
  }  
  
}

zb <- augmented.re[,1] + augmented.re[,2]*grid_2011$mpm_F

brandnew.pred <- grid_pred2 + zb

grid_2011$mixpred <-brandnew.pred


#get smooth predictions


grid_2011_bimon1 <- subset(grid_2011 ,grid_2011$bimon == "1")
grid_2011_bimon2 <- subset(grid_2011 ,grid_2011$bimon == "2")
grid_2011_bimon3 <- subset(grid_2011 ,grid_2011$bimon == "3")
grid_2011_bimon4 <- subset(grid_2011 ,grid_2011$bimon == "4")
grid_2011_bimon5 <- subset(grid_2011 ,grid_2011$bimon == "5")
grid_2011_bimon6 <- subset(grid_2011 ,grid_2011$bimon == "6")


#get predictions for Bimon residuals
uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN005_mod3/uniq_grid.csv", header=T) 


uniq_gid_bimon<-rename(uniq_gid_bimon,c(long_aod="Long_AOD")) 
uniq_gid_bimon<-rename(uniq_gid_bimon,c(lat_aod="Lat_AOD"))

uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon6 <- uniq_gid_bimon


#faking a no smooth prediction
uniq_gid_bimon1$gpred <-0


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
grid_2011_bimon1 <- grid_2011_bimon1[order(grid_2011_bimon1$guid),] 
grid_2011_bimon1_merged <- merge(grid_2011_bimon1,uniq_gid_bimon1,by="guid")

uniq_gid_bimon2 <- uniq_gid_bimon2[order(uniq_gid_bimon2$guid),] 
grid_2011_bimon2 <- grid_2011_bimon2[order(grid_2011_bimon2$guid),] 
grid_2011_bimon2_merged <- merge(grid_2011_bimon2,uniq_gid_bimon2,by="guid")

uniq_gid_bimon3 <- uniq_gid_bimon3[order(uniq_gid_bimon3$guid),] 
grid_2011_bimon3 <- grid_2011_bimon3[order(grid_2011_bimon3$guid),] 
grid_2011_bimon3_merged <- merge(grid_2011_bimon3,uniq_gid_bimon3,by="guid")

uniq_gid_bimon4 <- uniq_gid_bimon4[order(uniq_gid_bimon4$guid),] 
grid_2011_bimon4 <- grid_2011_bimon4[order(grid_2011_bimon4$guid),] 
grid_2011_bimon4_merged <- merge(grid_2011_bimon4,uniq_gid_bimon4,by="guid")

uniq_gid_bimon5 <- uniq_gid_bimon5[order(uniq_gid_bimon5$guid),] 
grid_2011_bimon5 <- grid_2011_bimon5[order(grid_2011_bimon5$guid),] 
grid_2011_bimon5_merged <- merge(grid_2011_bimon5,uniq_gid_bimon5,by="guid")

uniq_gid_bimon6 <- uniq_gid_bimon6[order(uniq_gid_bimon6$guid),] 
grid_2011_bimon6 <- grid_2011_bimon6[order(grid_2011_bimon6$guid),] 
grid_2011_bimon6_merged <- merge(grid_2011_bimon6,uniq_gid_bimon6,by="guid")



# create PM_mod3


T2011allbimonb26 <- rbind(grid_2011_bimon2_merged,grid_2011_bimon3_merged,grid_2011_bimon4_merged,grid_2011_bimon5_merged,grid_2011_bimon6_merged)


T2011allbimonb26$pm_mod3 <-T2011allbimonb26$mixpred+T2011allbimonb26$gpred
grid_2011_bimon1_merged$pm_mod3 <-grid_2011_bimon1_merged$mixpred

T2011allbimon <- rbind(grid_2011_bimon1_merged,T2011allbimonb26)

summary(T2011allbimon$pm_mod3)
summary(T2011allbimon)

#delete negative values
# T2011allbimon <- subset(T2011allbimon,T2011allbimon$pm_mod3 >= "0")
hist(T2011allbimon$pm_mod3)

write.dbf(T2011allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN007_mod3_Final_poll/poll_T2011.dbf") 


