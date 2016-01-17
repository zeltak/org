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
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#create CV table
res <- data.frame(type=character(10), R2=numeric(10),Bias=numeric(10),RMSPE=numeric(10),s.r2=numeric(10),s.RMSPE=numeric(10),t.r2=numeric(10))
#name columns
res$type <- c("overall" , "mountain climate", "ocianic climate", "degraded ocianic climate", "mediterranean climate", "continental climate", "winter","summer","urban","rural")

#1-mountain climate
#2-ocianic climate
#3-degraded ocieanic climate
#4-medeteranian climate
#5-continnetal climate

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
#create full grid
m1.all.cv <-mod1 %>%
    group_by(num_insee) %>%
    summarise(X = mean(Longitude , na.rm=TRUE),  Y = mean(Latitude  , na.rm=TRUE))
#write
write.csv(m1.all.cv,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metnum_inseeXY.csv")
#import back climate zone
climz<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/metnum_inseeXY._czone.csv")
climz$id<-NULL
setkey(mod1,num_insee)
setkey(climz,num_insee)
mod1 <- merge(mod1,climz[,list(num_insee,cid)], all.x = T)
mod1<-mod1[, c := as.numeric(format(day, "%Y")) ]
m1.all<-mod1

m1.formula <- as.formula(tm ~ T_Night+elev_m+pcturb+NDVI +(1+NTckin|date)) 


#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
#m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s8,test_s9))
saveRDS(m1.all.cv,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/Xmod1cv.ALL.rds")
# cleanup (remove from WS) objects from CV
#rm(list = ls(pattern = "train_|test_"))




#overall
m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv)
res[res$type=="overall", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="overall", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv))$coef[2,1])
#RMSPE
res[res$type=="overall", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="overall", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="overall", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="overall", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#mountain climate

m1.all.cv.c1<-filter(m1.all.cv,cid==1)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c1)
res[res$type=="mountain climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c1))$r.squared)
res[res$type=="mountain climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c1))$coef[2,1])
#RMSPE
res[res$type=="mountain climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c1 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="mountain climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="mountain climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c1,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="mountain climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


#ocianic climate

m1.all.cv.c2<-filter(m1.all.cv,cid==2)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c2)
res[res$type=="ocianic climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c2))$r.squared)
res[res$type=="ocianic climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c2))$coef[2,1])
#RMSPE
res[res$type=="ocianic climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c2 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="ocianic climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="ocianic climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c2,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="ocianic climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)




#degraded ocianic climate

m1.all.cv.c3<-filter(m1.all.cv,cid==3)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c3)
res[res$type=="degraded ocianic climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c3))$r.squared)
res[res$type=="degraded ocianic climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c3))$coef[2,1])
#RMSPE
res[res$type=="degraded ocianic climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c3 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="degraded ocianic climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="degraded ocianic climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c3,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="degraded ocianic climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#mediterranean climate

m1.all.cv.c4<-filter(m1.all.cv,cid==4)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c4)
res[res$type=="mediterranean climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c4))$r.squared)
res[res$type=="mediterranean climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c4))$coef[2,1])
#RMSPE
res[res$type=="mediterranean climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c4 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="mediterranean climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="mediterranean climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c4,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="mediterranean climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#continental climate

m1.all.cv.c5<-filter(m1.all.cv,cid==5)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.c5)
res[res$type=="continental climate", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c5))$r.squared)
res[res$type=="continental climate", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.c5))$coef[2,1])
#RMSPE
res[res$type=="continental climate", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.c5 %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="continental climate", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="continental climate", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.c5,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="continental climate", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


#urban

m1.all.cv.curb <-filter(m1.all.cv, pcturb > 22)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.curb)
res[res$type=="urban", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.curb))$r.squared)
res[res$type=="urban", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.curb))$coef[2,1])
#RMSPE
res[res$type=="urban", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.curb %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="urban", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="urban", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.curb,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="urban", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


#rural

m1.all.cv.crur <-filter(m1.all.cv, pcturb < 22)

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.crur)
res[res$type=="rural", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.crur))$r.squared)
res[res$type=="rural", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.crur))$coef[2,1])
#RMSPE
res[res$type=="rural", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.crur %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="rural", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="rural", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.crur,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="rural", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)



#Seasons
library(car)
m1.all.cv$month <- as.numeric(format(m1.all.cv$day, "%m"))
#1-winter, 2-spring,3-summer,4-autum
m1.all.cv$season<-recode(m1.all.cv$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m1.all.cv$seasonSW<-as.character(recode(m1.all.cv$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))



#winter

m1.all.cv.wint <-filter(m1.all.cv, seasonSW ==1 )

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.wint)
res[res$type=="winter", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.wint))$r.squared)
res[res$type=="winter", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.wint))$coef[2,1])
#RMSPE
res[res$type=="winter", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.wint %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="winter", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="winter", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.wint,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="winter", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

#summer

m1.all.cv.sum <-filter(m1.all.cv, seasonSW ==2 )

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.sum)
res[res$type=="summer", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.sum))$r.squared)
res[res$type=="summer", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.sum))$coef[2,1])
#RMSPE
res[res$type=="summer", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.sum %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="summer", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="summer", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.sum,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="summer", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)

####paris

#create CV table
resx <- data.frame(type=character(1), R2=numeric(1),Bias=numeric(1),RMSPE=numeric(1),s.r2=numeric(1),s.RMSPE=numeric(1),t.r2=numeric(1))
#name columns
resx$type <- c("paris" )
res<-rbind(res,resx)

#paris analysis
#use overall fit to predict for only oston stations
paris<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/fn007_keytables/parisXY.csv")
#subset
m1.all.cv.paris<- m1.all.cv[m1.all.cv$num_insee %in% paris$num_insee ,]

m1.fit.all.cv<-lm(tm~pred.m1.cv,data=m1.all.cv.paris)
res[res$type=="paris", 'R2'] <- print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.paris))$r.squared)
res[res$type=="paris", 'Bias'] <-print(summary(lm(tm~pred.m1.cv,data=m1.all.cv.paris))$coef[2,1])
#RMSPE
res[res$type=="paris", 'RMSPE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv.paris %>%
    group_by(num_insee) %>%
    parismarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.m1.cv, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="paris", 's.r2'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="paris", 's.RMSPE'] <- print(rmse(residuals(m1.fit.all.cv.s)))

#temporal
tempoall.cv<-left_join(m1.all.cv.paris,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$tm-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="paris", 't.r2'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


write.csv(res,"/media/NAS/Uni/Projects/P022_Temprature_France/4.results/review/reviewres.csv")


#AIC/BIC analysis
#just LST
m1.formula <- as.formula(tm ~ NTckin +(1+NTckin|date)) 
b1 <- lmer(m1.formula,data =  m1.all.cv)
bl1 <- lme(tm ~ NTckin,random = ~1 + NTckin| date,data =  m1.all.cv)
AIC(b1)
BIC(b1)
m1.all.cv$pred.b1<- predict(b1)
summary(lm(tm~pred.b1,data=m1.all.cv))


# m1.formula <- as.formula(tm ~ +elev_m+pcturb+NDVI +(1|date)) 
# b2 <- lmer(m1.formula,data =  m1.all.cv)
# summary(b2)
# BIC(b1)
# m1.all.cv$pred.b2<- predict(b2)
# summary(lm(tm~pred.b2,data=m1.all.cv))

m1.formula <- as.formula(tm ~  NTckin +elev_m +(1+NTckin|date)) 
b3 <- lmer(m1.formula,data =  m1.all.cv)
summary(b3)
AIC(b3)
BIC(b3)
m1.all.cv$pred.b3<- predict(b3)
summary(lm(tm~pred.b3,data=m1.all.cv))

m1.formula <- as.formula(tm ~  NTckin +elev_m+pcturb +(1+NTckin|date)) 
b4 <- lmer(m1.formula,data =  m1.all.cv)
summary(b4)
AIC(b4)
BIC(b4)
m1.all.cv$pred.b4<- predict(b4)
summary(lm(tm~pred.b4,data=m1.all.cv))


m1.formula <- as.formula(tm ~  NTckin +elev_m+pcturb+NDVI +(1+NTckin|date)) 
b5 <- lmer(m1.formula,data =  m1.all.cv)
summary(b5)
AIC(b5)
BIC(b5)
m1.all.cv$pred.b5<- predict(b5)
summary(lm(tm~pred.b5,data=m1.all.cv))

### krig analysis
library(mgcv)
# met<- as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN003_WUNCDC yearly/met2000.dbf") )
# met<-met[,c(1:5),with=FALSE]
# met<-na.omit(met)
# met$month <- as.numeric(format(met$Date, "%m"))

#run "kriging" analysis
#need to convert dat to numeric
m1.all.cv$dd <- as.numeric(m1.all.cv$day)
fi1_gam <- gam( tm ~ s(Longitude,Latitude+elev_m+pcturb+NDVI,by=dd), data=m1.all.cv)
#fi1_gam <- gam( tm ~ s(Longitude,Latitude)+elev_m+pcturb, data=m1.all.cv)
summary(fi1_gam)
m1.all.cv$pred.gam <- predict(fi1_gam)
#calc long term means
#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.gam, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv.s)))




summary(lm(tm~pred,data=m1.all.cv))

add5 <- met[Date == "2000-01-01"]

#run "kriging" analysis
fi1_gam <- gam( tempc ~ s(x,y), data=add5)
add5$pred<- predict(fi1_gam)
summary(lm(tempc~pred,data=add5))

#run "kriging" analysis per day
t1<-met %.% group_by(Date) %.% do(function(df){gam(tempc~s(x,y),data=df)})
#t2<-sapply(t1,function(x) predict(x))
#get R2
tst<-sapply(t1,function(x) summary(x)$r.sq)
mean(tst)







