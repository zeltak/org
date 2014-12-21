
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
m1.2000 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2000.dbf"))
m1.2000[,humidity:=NULL]
m1.2000[,DTckin:=NULL]
m1.2000[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2000)
m1.2000<-na.omit(m1.2000)

#rescale
m1.2000[,ndvi.s:= scale(NDVI)]
m1.2000[,purban.s:= scale(purban)]
m1.2000[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2000 <-  lmer(m1.formula,data=m1.2000)
m1.2000$pred.m1 <- predict(m1.fit.2000)
print(summary(lm(tempc~pred.m1,data=m1.2000))$r.squared)
#all
coall.2000<-tidy(m1.fit.2000,effects = "fixed")
write.csv(coall.2000,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2000.csv")
#fixed
cofixed.2000 <- data.table(coef(m1.fit.2000)[["day"]], keep.rownames = T)
write.csv(cofixed.2000,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2000.csv")
#random
cofran.2000  <- data.table(ranef(m1.fit.2000)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2000, c("day", "ranint", "ranslope"))
write.csv(cofran.2000,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2000.csv")





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
m1.2001 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2001.dbf"))
m1.2001[,humidity:=NULL]
m1.2001[,DTckin:=NULL]
m1.2001[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2001)
m1.2001<-na.omit(m1.2001)

#rescale
m1.2001[,ndvi.s:= scale(NDVI)]
m1.2001[,purban.s:= scale(purban)]
m1.2001[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2001 <-  lmer(m1.formula,data=m1.2001)
m1.2001$pred.m1 <- predict(m1.fit.2001)
print(summary(lm(tempc~pred.m1,data=m1.2001))$r.squared)
#all
coall.2001<-tidy(m1.fit.2001,effects = "fixed")
write.csv(coall.2001,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2001.csv")
#fixed
cofixed.2001 <- data.table(coef(m1.fit.2001)[["day"]], keep.rownames = T)
write.csv(cofixed.2001,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2001.csv")
#random
cofran.2001  <- data.table(ranef(m1.fit.2001)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2001, c("day", "ranint", "ranslope"))
write.csv(cofran.2001,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2001.csv")





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
m1.2002 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2002.dbf"))
m1.2002[,humidity:=NULL]
m1.2002[,DTckin:=NULL]
m1.2002[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2002)
m1.2002<-na.omit(m1.2002)

#rescale
m1.2002[,ndvi.s:= scale(NDVI)]
m1.2002[,purban.s:= scale(purban)]
m1.2002[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2002 <-  lmer(m1.formula,data=m1.2002)
m1.2002$pred.m1 <- predict(m1.fit.2002)
print(summary(lm(tempc~pred.m1,data=m1.2002))$r.squared)
#all
coall.2002<-tidy(m1.fit.2002,effects = "fixed")
write.csv(coall.2002,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2002.csv")
#fixed
cofixed.2002 <- data.table(coef(m1.fit.2002)[["day"]], keep.rownames = T)
write.csv(cofixed.2002,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2002.csv")
#random
cofran.2002  <- data.table(ranef(m1.fit.2002)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2002, c("day", "ranint", "ranslope"))
write.csv(cofran.2002,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2002.csv")





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
m1.2003 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2003.dbf"))
m1.2003[,humidity:=NULL]
m1.2003[,DTckin:=NULL]
m1.2003[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2003)
m1.2003<-na.omit(m1.2003)

#rescale
m1.2003[,ndvi.s:= scale(NDVI)]
m1.2003[,purban.s:= scale(purban)]
m1.2003[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2003 <-  lmer(m1.formula,data=m1.2003)
m1.2003$pred.m1 <- predict(m1.fit.2003)
print(summary(lm(tempc~pred.m1,data=m1.2003))$r.squared)
#all
coall.2003<-tidy(m1.fit.2003,effects = "fixed")
write.csv(coall.2003,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2003.csv")
#fixed
cofixed.2003 <- data.table(coef(m1.fit.2003)[["day"]], keep.rownames = T)
write.csv(cofixed.2003,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2003.csv")
#random
cofran.2003  <- data.table(ranef(m1.fit.2003)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2003, c("day", "ranint", "ranslope"))
write.csv(cofran.2003,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2003.csv")





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
m1.2004 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2004.dbf"))
m1.2004[,humidity:=NULL]
m1.2004[,DTckin:=NULL]
m1.2004[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2004)
m1.2004<-na.omit(m1.2004)

#rescale
m1.2004[,ndvi.s:= scale(NDVI)]
m1.2004[,purban.s:= scale(purban)]
m1.2004[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2004 <-  lmer(m1.formula,data=m1.2004)
m1.2004$pred.m1 <- predict(m1.fit.2004)
print(summary(lm(tempc~pred.m1,data=m1.2004))$r.squared)
#all
coall.2004<-tidy(m1.fit.2004,effects = "fixed")
write.csv(coall.2004,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2004.csv")
#fixed
cofixed.2004 <- data.table(coef(m1.fit.2004)[["day"]], keep.rownames = T)
write.csv(cofixed.2004,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2004.csv")
#random
cofran.2004  <- data.table(ranef(m1.fit.2004)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2004, c("day", "ranint", "ranslope"))
write.csv(cofran.2004,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2004.csv")





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
m1.2006 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2006.dbf"))
m1.2006[,humidity:=NULL]
m1.2006[,DTckin:=NULL]
m1.2006[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2006)
m1.2006<-na.omit(m1.2006)

#rescale
m1.2006[,ndvi.s:= scale(NDVI)]
m1.2006[,purban.s:= scale(purban)]
m1.2006[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2006 <-  lmer(m1.formula,data=m1.2006)
m1.2006$pred.m1 <- predict(m1.fit.2006)
print(summary(lm(tempc~pred.m1,data=m1.2006))$r.squared)
#all
coall.2006<-tidy(m1.fit.2006,effects = "fixed")
write.csv(coall.2006,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2006.csv")
#fixed
cofixed.2006 <- data.table(coef(m1.fit.2006)[["day"]], keep.rownames = T)
write.csv(cofixed.2006,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2006.csv")
#random
cofran.2006  <- data.table(ranef(m1.fit.2006)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2006, c("day", "ranint", "ranslope"))
write.csv(cofran.2006,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2006.csv")





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
m1.2007 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2007.dbf"))
m1.2007[,humidity:=NULL]
m1.2007[,DTckin:=NULL]
m1.2007[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2007)
m1.2007<-na.omit(m1.2007)

#rescale
m1.2007[,ndvi.s:= scale(NDVI)]
m1.2007[,purban.s:= scale(purban)]
m1.2007[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2007 <-  lmer(m1.formula,data=m1.2007)
m1.2007$pred.m1 <- predict(m1.fit.2007)
print(summary(lm(tempc~pred.m1,data=m1.2007))$r.squared)
#all
coall.2007<-tidy(m1.fit.2007,effects = "fixed")
write.csv(coall.2007,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2007.csv")
#fixed
cofixed.2007 <- data.table(coef(m1.fit.2007)[["day"]], keep.rownames = T)
write.csv(cofixed.2007,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2007.csv")
#random
cofran.2007  <- data.table(ranef(m1.fit.2007)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2007, c("day", "ranint", "ranslope"))
write.csv(cofran.2007,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2007.csv")





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
m1.2008 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2008.dbf"))
m1.2008[,humidity:=NULL]
m1.2008[,DTckin:=NULL]
m1.2008[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2008)
m1.2008<-na.omit(m1.2008)

#rescale
m1.2008[,ndvi.s:= scale(NDVI)]
m1.2008[,purban.s:= scale(purban)]
m1.2008[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2008 <-  lmer(m1.formula,data=m1.2008)
m1.2008$pred.m1 <- predict(m1.fit.2008)
print(summary(lm(tempc~pred.m1,data=m1.2008))$r.squared)
#all
coall.2008<-tidy(m1.fit.2008,effects = "fixed")
write.csv(coall.2008,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2008.csv")
#fixed
cofixed.2008 <- data.table(coef(m1.fit.2008)[["day"]], keep.rownames = T)
write.csv(cofixed.2008,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2008.csv")
#random
cofran.2008  <- data.table(ranef(m1.fit.2008)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2008, c("day", "ranint", "ranslope"))
write.csv(cofran.2008,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2008.csv")





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
m1.2009 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2009.dbf"))
m1.2009[,humidity:=NULL]
m1.2009[,DTckin:=NULL]
m1.2009[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2009)
m1.2009<-na.omit(m1.2009)

#rescale
m1.2009[,ndvi.s:= scale(NDVI)]
m1.2009[,purban.s:= scale(purban)]
m1.2009[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2009 <-  lmer(m1.formula,data=m1.2009)
m1.2009$pred.m1 <- predict(m1.fit.2009)
print(summary(lm(tempc~pred.m1,data=m1.2009))$r.squared)
#all
coall.2009<-tidy(m1.fit.2009,effects = "fixed")
write.csv(coall.2009,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2009.csv")
#fixed
cofixed.2009 <- data.table(coef(m1.fit.2009)[["day"]], keep.rownames = T)
write.csv(cofixed.2009,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2009.csv")
#random
cofran.2009  <- data.table(ranef(m1.fit.2009)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2009, c("day", "ranint", "ranslope"))
write.csv(cofran.2009,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2009.csv")





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
m1.2010 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2010.dbf"))
m1.2010[,humidity:=NULL]
m1.2010[,DTckin:=NULL]
m1.2010[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2010)
m1.2010<-na.omit(m1.2010)

#rescale
m1.2010[,ndvi.s:= scale(NDVI)]
m1.2010[,purban.s:= scale(purban)]
m1.2010[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2010 <-  lmer(m1.formula,data=m1.2010)
m1.2010$pred.m1 <- predict(m1.fit.2010)
print(summary(lm(tempc~pred.m1,data=m1.2010))$r.squared)
#all
coall.2010<-tidy(m1.fit.2010,effects = "fixed")
write.csv(coall.2010,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2010.csv")
#fixed
cofixed.2010 <- data.table(coef(m1.fit.2010)[["day"]], keep.rownames = T)
write.csv(cofixed.2010,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2010.csv")
#random
cofran.2010  <- data.table(ranef(m1.fit.2010)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2010, c("day", "ranint", "ranslope"))
write.csv(cofran.2010,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2010.csv")





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
m1.2011 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2011.dbf"))
m1.2011[,humidity:=NULL]
m1.2011[,DTckin:=NULL]
m1.2011[, day:=as.Date(strptime(date, "%Y-%m-%d"))]
summary(m1.2011)
m1.2011<-na.omit(m1.2011)

#rescale
m1.2011[,ndvi.s:= scale(NDVI)]
m1.2011[,purban.s:= scale(purban)]
m1.2011[,elev.s:= scale(elev)]

m1.formula <- as.formula(tempc~ NTckin+purban+elev+NDVI+(1+NTckin|day)) #0.812

#full fit
m1.fit.2011 <-  lmer(m1.formula,data=m1.2011)
m1.2011$pred.m1 <- predict(m1.fit.2011)
print(summary(lm(tempc~pred.m1,data=m1.2011))$r.squared)
#all
coall.2011<-tidy(m1.fit.2011,effects = "fixed")
write.csv(coall.2011,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/coall.2011.csv")
#fixed
cofixed.2011 <- data.table(coef(m1.fit.2011)[["day"]], keep.rownames = T)
write.csv(cofixed.2011,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofixed.2011.csv")
#random
cofran.2011  <- data.table(ranef(m1.fit.2011)[["day"]], keep.rownames = T)
#finally we can rebanme the collumns
setnames(cofran.2011, c("day", "ranint", "ranslope"))
write.csv(cofran.2011,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/cambridge_harvard_heatmap/R/cofran.2011.csv")




