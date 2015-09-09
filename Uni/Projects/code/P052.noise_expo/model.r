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
#source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")
source("/home/zeltak/org/files/Uni/Projects/code/P052.noise_expo/CV_splits30.r")


#using fread from data .table
m1bsv <-fread("/media/NAS/Uni/Projects/P052_noise_LUR/raw/calibration_bsv.csv")
#setname (data.table)
setnames(m1bsv,"Distance","d.busstop")
summary(m1bsv)
#clear missing values
hist(m1bsv$Distance_1)
hist(m1bsv$Mor_LAT__DB_)


#for linear model
#formula
m1.formula <- as.formula(Mor_LAT__DB_~ d.busstop+TOTAL_LANE+Distance_1+Distance_2+Distance_4+Distance_5+Distance_6+Distance_7+Vol_AM+ Pop_weigh+Sum_Shape_Length)  
#simple linear regression
r.bsv <- lm(m1.formula,data=m1bsv)                           
summary(r.bsv)

#check validation
#this prepares the data for spliting
splits_s1 <- splitdf(m1bsv)
#split 10%
test_s1 <- splits_s1$testset
#split 90%
train_s1 <- splits_s1$trainset
#run 
out_train_s1 <- lm(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
#R2
print(summary(lm(Mor_LAT__DB_~pred.m1.cv,data=test_s1))$r.squared)
#RMSPE
print(rmse(residuals(out_train_s1)))

#for linear mixed model
#formula
m1.formula <- as.formula(NO2~ street_dens+Distance_sea+percent_mivne+persent_open+MEAN_tsfifut+Temp+RH+WS+(1+Temp|day))  
m1nl.formula <- as.formula(Mor_LAT__DB_~ d.busstop+TOTAL_LANE+Distance_1+Distance_2+Distance_4+Distance_5+Distance_6+Distance_7+Vol_AM+ Pop_weigh+Sum_Shape_Length) 




#regression
m1_sc <- lmer(m1.formula,data=m1bsv)                         
summary(m1_sc)
m1bsv$pred.m1 <- predict(m1_sc)
print(summary(lm(NO2~pred.m1,data=m1bsv))$r.squared)
#RMSPE
print(rmse(residuals(m1_sc)))
plot(m1bsv$Temp,m1bsv$)

#---------------->>>> CV
#s1
#this prepares the data for spliting
splits_s1 <- splitdf(m1bsv)
#split 90%
test_s1 <- splits_s1$testset
#split 10%
train_s1 <- splits_s1$trainset
#run 
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1bsv)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1bsv)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1bsv)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1bsv)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1bsv)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1bsv)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1bsv)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1bsv)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1bsv)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1bsv.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))

m1.fit.all.cv<-lm(NO2~pred.m1.cv,data=m1bsv.cv)
print(summary(lm(NO2~pred.m1.cv,data=m1bsv.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv)))


#crete mod 2 predictions
m2.all <-readRDS("/media/NAS/Uni/Projects/P000_TMP_PROJECTS/larano2/mod2.rds")
m2.all<-na.omit(m2.all)
#generate predictions
m2.all$pred.m2<-predict(object=m1_sc,newdata=m2.all,allow.new.levels=TRUE,re.form=NULL)
head(m2.all)
summary(m2.all$pred.m2)


#bestprmap
m3d_agg <- (m2.all[, list(LTPM =mean(pred.m2, na.rm = TRUE), 
                        X = X[1], #use the first long and lat (by aodid)
                        Y = Y[1]),by = gid])  
P1 <- ggplot(m3d_agg, aes(X, Y, color = LTPM)) + 
  geom_point(size = 4, shape = 15) + 
  xlab("longitude in utm (meters)") + ylab("latitude in utm (meters)") + 
  scale_colour_gradientn("long term PM2.5 prediction", colours = rainbow(10)) + #c("purple", "blue", "white", "red", "orange")) + 
  theme_bw() + 
  ggtitle("Long term predictions")
P1

