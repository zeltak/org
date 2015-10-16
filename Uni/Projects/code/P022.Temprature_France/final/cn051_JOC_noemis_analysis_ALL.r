library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#create CV table
res <- data.frame(type=character(12), R2=numeric(12),Bias=numeric(12),RMSPE=numeric(12),s.r2=numeric(12),s.RMSPE=numeric(12),t.r2=numeric(12))
#name columns
res$type <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011")


##prepare models
#####################
#2000
#####################
#m1
mod2000<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2000.csv")
mod2001<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2001.csv")
mod2002<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2002.csv")
mod2003<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2003.csv")
mod2004<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2004.csv")
mod2005<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2005.csv")
mod2006<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2006.csv")
mod2007<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2007.csv")
mod2008<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2008.csv")
mod2009<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2009.csv")
mod2010<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2010.csv")
mod2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod1.2011.csv")


mod1<-rbindlist(list(mod2000,mod2001,mod2002,mod2003,mod2004,mod2005,mod2006,mod2007,mod2008,mod2009,mod2010,mod2011))
mod1[, day:=as.Date(strptime(date, "%d%b%Y"))]
#create aodid
mod1$lstid<-paste(mod1$Longitude,mod1$Latitude,sep="-")
#day dataset
mod1$DTckin <-NULL
mod1$T_Day<-NULL
mod1<-na.omit(mod1)
mod1[, c := as.numeric(format(day, "%Y")) ]

#year 2000

mod1.2000 <-filter(mod1,c == 2000)
m1.formula <- as.formula(tm ~ NTckin+elev_m+pcturb+NDVI +(1+NTckin|date)) 




#---------------->>>> CV
#s1
splits_s1 <- splitdf(mod1.2000 )
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(mod1.2000 )
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(mod1.2000 )
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(mod1.2000 )
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(mod1.2000 )
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(mod1.2000 )
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(mod1.2000 )
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(mod1.2000 )
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(mod1.2000 )
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(mod1.2000 )
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
mod1.2000.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))

#2000
m1.fit.all.cv<-lm(tm~pred.m1.cv,data=mod1.2000.cv)
res[res$type=="2000", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=mod1.2000.cv))$r.squared)
res[res$type=="2000", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=mod1.2000.cv))$coef[2,1])
#RMSPE
res[res$type=="2000", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-mod1.2000.cv %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="2000", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="2000", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(mod1.2000.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="2000", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


