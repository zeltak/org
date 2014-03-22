library (mgcv)
library(nlme)
library(foreign)
library(reshape)


GAM_T2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_mod2predall.csv", header=T) 

summary(GAM_T2003)
#omit missing predictions
GAM_T2003 = na.omit(GAM_T2003)


#run the lme part regressing stage 2 pred Vs mean pm
smooth_T2003_yearly = lme(predicted ~ mtemp  ,
  random = list(guid= ~1 + mtemp ),
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
fit2_1 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon1 )


fit2_2 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon2 )

fit2_3 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon3 )

fit2_4 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon4 )

fit2_5 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon5 )

fit2_6 = gam(resid ~ s(X.x,Y.y),
data= T2003_bimon6 )

#get the predicted-fitted 
Xpred_1=(T2003_bimon1$predicted - fit2_1$fitted)
Xpred_2=(T2003_bimon2$predicted - fit2_2$fitted)
Xpred_3=(T2003_bimon3$predicted - fit2_3$fitted)
Xpred_4=(T2003_bimon4$predicted - fit2_4$fitted)
Xpred_5=(T2003_bimon5$predicted - fit2_5$fitted)
Xpred_6=(T2003_bimon6$predicted - fit2_6$fitted)

#remerge to 1 file
GAM_T2003$newpred <- c(Xpred_1, Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)


#rerun the lme on the predictions including the spatial spline (smooth)
# Final_pred_2003  = lme(newpred ~ mtemp  ,
#   random = list(guid= ~1 + mtemp ),control=lmeControl(opt = "optim") ,data= GAM_T2003  )

Final_pred_2003 = lme(newpred ~ mtemp  ,
  random = list(guid= ~1 + mtemp ),
data= GAM_T2003 )


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> get predictions for guid
####import all xy points across new-england
grid_2003 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_grid_reg.csv", header=T) 
summary(grid_2003)


#predict from the Final_pred_2003 fit to the larger datset
grid_2003$mixpred <- predict(Final_pred_2003,grid_2003,newdata.guaranteed=True)
summary(grid_2003$mixpred)


grid_2003_bimon1 <- subset(grid_2003 ,grid_2003$bimon == "1")
grid_2003_bimon2 <- subset(grid_2003 ,grid_2003$bimon == "2")
grid_2003_bimon3 <- subset(grid_2003 ,grid_2003$bimon == "3")
grid_2003_bimon4 <- subset(grid_2003 ,grid_2003$bimon == "4")
grid_2003_bimon5 <- subset(grid_2003 ,grid_2003$bimon == "5")
grid_2003_bimon6 <- subset(grid_2003 ,grid_2003$bimon == "6")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> get predictions for bimon resids

uniq_gid_bimon = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/Guid_unique.csv", header=T) 
uniq_gid_bimon <- rename(uniq_gid_bimon, c(X="X.x"))
uniq_gid_bimon <- rename(uniq_gid_bimon, c(Y="Y.y"))


uniq_gid_bimon1 <- uniq_gid_bimon
uniq_gid_bimon1$gpred <- predict.gam(fit2_1,uniq_gid_bimon1)
 
uniq_gid_bimon2 <- uniq_gid_bimon
uniq_gid_bimon2$gpred <- predict.gam(fit2_2,uniq_gid_bimon2)

uniq_gid_bimon3 <- uniq_gid_bimon
uniq_gid_bimon3$gpred <- predict.gam(fit2_3,uniq_gid_bimon3)

uniq_gid_bimon4 <- uniq_gid_bimon
uniq_gid_bimon4$gpred <- predict.gam(fit2_4,uniq_gid_bimon4)

uniq_gid_bimon5 <- uniq_gid_bimon
uniq_gid_bimon5$gpred <- predict.gam(fit2_5,uniq_gid_bimon5)

uniq_gid_bimon6 <- uniq_gid_bimon
uniq_gid_bimon6$gpred <- predict.gam(fit2_6,uniq_gid_bimon6)

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


summary(T2003allbimon)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export

write.dbf(T2003allbimon,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/T2003allbimon.dbf") 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> correlations

mpm2003 <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/2.Gather_data/met_2_dbf/met2003.dbf") 


pm_ids_reg <-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/2.Gather_data/key_tables/Sid_guid_region.dbf")

#add region to pm stations


#sort
pm_ids_reg <- pm_ids_reg[order(pm_ids_reg$SID),]
mpm2003 <- mpm2003[order(mpm2003$SID),]
#merge
mpm2003reg <- merge(mpm2003,pm_ids_reg,by="SID")
mpm2003reg$TMINC=(5/9)*(mpm2003reg$TMIN-32)
mpm2003reg$date <- as.Date(mpm2003reg$DATE, "%m/%d/%y")

# merge datasets

rm(grid_2003_bimon6_merged,grid_2003_bimon5_merged)

#sort
mpm2003reg <- mpm2003reg[order(mpm2003reg$guid,mpm2003reg$date),]
T2003allbimon <- T2003allbimon[order(T2003allbimon$guid,T2003allbimon$date),]
#merge
corset2003 <- merge(mpm2003reg,T2003allbimon,by="guid","date",all.x=T,all.y=F)


summary(corset2003)


corset2003$tmp_mod3 <- corset2003$mixpred+corset2003$gpred

corset2003n <- na.omit(corset2003) 


cor(corset2003n$tmp_mod3,corset2003n$TMINC)
