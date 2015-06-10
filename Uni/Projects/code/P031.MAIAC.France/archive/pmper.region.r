
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
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.clean.rds")



describe(m1.all$region)
m1.all.reg1<-m1.all[region==1]
m1.all.reg2<-m1.all[region==2]
m1.all.reg3<-m1.all[region==3]
m1.all.reg4<-m1.all[region==4]
m1.all.reg5<-m1.all[region==5]
m1.all.reg6<-m1.all[region==6]
m1.all.reg7<-m1.all[region==7]

m1.formula <- as.formula(pm25~ aod+tempa.s+Raina.s+WSa.s+elev.s+tden.s+pden.s+ndvi.s +p_urb.s+dist2A1.s+(1+aod|day/region)) 
m1.formula <- as.formula(pm25~ aod+(1+aod|day/region)) 


#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg1)
m1.all.reg1[,pred.m1 := NULL]
m1.all.reg1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg1))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))


#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg2)
m1.all.reg2[,pred.m1 := NULL]
m1.all.reg2$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg2))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))


#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg3)
m1.all.reg3[,pred.m1 := NULL]
m1.all.reg3$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg3))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))

#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg4)
m1.all.reg4[,pred.m1 := NULL]
m1.all.reg4$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg4))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))

#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg5)
m1.all.reg5[,pred.m1 := NULL]
m1.all.reg5$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg5))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))


#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg6)
m1.all.reg6[,pred.m1 := NULL]
m1.all.reg6$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg6))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))

#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all.reg7)
m1.all.reg7[,pred.m1 := NULL]
m1.all.reg7$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=m1.all.reg7))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))





