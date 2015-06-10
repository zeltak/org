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


m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM10.rds")
#describe(m1.all$pm25)
m1.all<-na.omit(m1.all)
summary(m1.all)
m1.all<-filter(m1.all,stn != 40001 & stn != 39007 & stn != 38008)
#delete water flags
m1.all<-filter(m1.all,wflag != 1)
#for data
#m1.formula <- as.formula(pm25~ aod+(1+aod|day)) 
#m1.formula <- as.formula(pm25~ aod) 
#try logs

m1.formula <- as.formula(pm25~ aod+tempa.s+Raina.s+WSa.s+elev.s+tden.s+pden.s+ndvi.s +p_urb.s+dist2A1.s+(1+aod|day/region)) 
m1.formula <- as.formula(pm10~ aod+tempa.s+Raina.s+WSa.s+elev.s+tden.s+pden.s+ndvi.s +p_urb.s+dist2A1.s+(1+aod|day/region)) 

#m1.formula <- as.formula(pm25~ aod+tempa.s+Raina.s+elev.s+tden.s+pden.s+ndvi.s +(1+aod|day/region)) 

m1.formula <- as.formula(pm25~ aod+tempa.s+Raina.s+WSa.s+elev.s+tden.s+pden.s+ndvi.s +p_urb.s+dist2A1.s+(1+aod|day)) 



#m1_sc <- lm(m1.formula,data=m1.all)
m1_sc <- lmer(m1.formula,data=m1.all,weights=normwt)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
#print(summary(lm(pm25~pred.m1,data=m1.all))$r.squared)
print(summary(lm(pm10~pred.m1,data=m1.all))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))

#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))

m1.fit.all.cv<-lm(pm25~pred.m1.cv,data=m1.all.cv)
print(summary(lm(pm25~pred.m1.cv,data=m1.all.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv)))

# m1.fit.all.cv<-lm(pm10~pred.m1.cv,data=m1.all.cv)
# print(summary(lm(pm10~pred.m1.cv,data=m1.all.cv))$r.squared)
# print(rmse(residuals(m1.fit.all.cv)))


