###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


#-------------------->> RES TABLE
res <- matrix(nrow=12, ncol=45)
res <- data.frame(res)
colnames(res) <- c(
          "year"
          ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
          ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"mod2_R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"mod3_pm_mod3","mod3_int"
          ,"mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE"
          ,"mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
          "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                   "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial"
                   )
res$year <- c(2000:2011); 

### import data
m1.2005 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2005.dbf"))
m1.2005[,humidity:=NULL]
m1.2005[,DTckin:=NULL]
m1.2005[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2005)
m1.2005<-na.omit(m1.2005)

#rescale
m1.2005[,ndvi.s:= scale(NDVI)]
m1.2005[,purban.s:= scale(purban)]
m1.2005[,elev.s:= scale(elev)]





m1.formula <- as.formula(tempc~ NTckin+purban.s+elev.s+(1+NTckin|day)) #0.812

#full fit
m1.fit.2005 <-  lmer(m1.formula,data=m1.2005)
m1.2005$pred.m1 <- predict(m1.fit.2005)
res[res$year=="2005", 'm1.R2'] <- print(summary(lm(tempc~pred.m1,data=m1.2005))$r.squared)
#RMSPE
#res[res$year=="2005", 'm1.PE'] <- print(rmse(residuals(m1.fit.2005)))

#spatial
###to check
spatial2005<-m1.2005 %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.2005.spat<- lm(barpm ~ barpred, data=spatial2005)
res[res$year=="2005", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatial2005))$r.squared)
res[res$year=="2005", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.2005.spat)))
       
#temporal
tempo2005<-left_join(m1.2005,spatial2005)
tempo2005$delpm <-tempo2005$PM25-tempo2005$barpm
tempo2005$delpred <-tempo2005$pred.m1-tempo2005$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempo2005)
res[res$year=="2005", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempo2005))$r.squared)
saveRDS(m1.2005,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.pred.rds")
