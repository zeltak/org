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

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2005 <-  lmer(m1.formula,data=m1.2005)
m1.2005$pred.m1 <- predict(m1.fit.2005)
print(summary(lm(tempc~pred.m1,data=m1.2005))$r.squared)
#all
coall.2005<-tidy(m1.fit.2005,effects = "fixed")
write.csv(coall.2005,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2005.csv")
#fixed
cofixed.2005 <- data.table(coef(m1.fit.2005)[["day"]], keep.rownames = T)
write.csv(cofixed.2005,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2005.csv")
#random
cofran.2005  <- data.table(ranef(m1.fit.2005)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2005, c("day", "ranint", "ranslope"))
write.csv(cofran.2005,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2005.csv")




