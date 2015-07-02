library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
#library(reshape2)
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



#Read temp/lst data
mod1<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/italy.raw.db/stage_one_2010.sas7bdat",debug=FALSE)
mod1<-it.2010
#convert to data.table
mod1<-data.table(mod1)

#-------------------->> RES TABLE
res <- matrix(nrow=1, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("tempc")


#mod1 clean
summary(mod1)
mod1<-filter(mod1,!is.na(temp_c))
mod1<-filter(mod1,!is.na(NTckin))

#run regression (simple linear)                     
m1.raw.formula <- as.formula(temp_c ~ NTckin)
m1sum <- lm(m1.raw.formula,data=mod1)
summary(m1sum)
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1sum)
print(summary(lm(temp_c~pred.m1,data=mod1))$r.squared)
print(rmse(residuals(m1sum)))

#run mixed model
mod1$day<-as.character(mod1$DATE)
m1.formula <- as.formula(temp_c ~NTckin +(1+NTckin|day))
m1sum <- lmer(m1.formula,data=mod1)
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1sum)
print(summary(lm(temp_c~pred.m1,data=mod1))$r.squared)
#RMSPE
print(rmse(residuals(m1sum)))

summary(mod1)
#run mixed model
names(mod1)
m1.formula <- as.formula(temp_c ~NTckin+NDVI+ELEVATION+RESTOT+Emis +(1+NTckin|day/zona_clima))
m1sum <- lmer(m1.formula,data=mod1)
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1sum)
print(summary(lm(temp_c~pred.m1,data=mod1))$r.squared)
#RMSPE
print(rmse(residuals(m1sum)))
summary(m1sum)

#spatial
spatialall<-mod1 %>%
    group_by(STN) %>%
    summarise(barpm = mean(temp_c, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.s <- lm(barpm ~ barpred, data=spatialall)
 print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
print(rmse(residuals(m1.fit.all.s)))
    
#temporal
tempoall<-left_join(mod1,spatialall)
tempoall$delpm <-tempoall$temp_c-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)
#save
#saveRDS(mod1....




#---------------->>>> CV
#s1
splits_s1 <- splitdf(mod1)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(mod1)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(mod1)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(mod1)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(mod1)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"


#BIND 1 dataset
mod1.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5))
#save
#table updates
m1.fit.all.cv<-lm(temp_c~pred.m1.cv,data=mod1.cv)
 print(summary(lm(temp_c~pred.m1.cv,data=mod1.cv))$r.squared)
print(summary(lm(temp_c~pred.m1.cv,data=mod1.cv))$coef[1,1])
print(summary(lm(temp_c~pred.m1.cv,data=mod1.cv))$coef[1,2])
print(summary(lm(temp_c~pred.m1.cv,data=mod1.cv))$coef[2,1])
print(summary(lm(temp_c~pred.m1.cv,data=mod1.cv))$coef[2,2])
#RMSPE
print(rmse(residuals(m1.fit.all.cv)))
#spatial
spatialall.cv<-mod1.cv %>%
    group_by(STN) %>%
    summarise(barpm = mean(temp_c, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
 print(rmse(residuals(m1.fit.all.cv.s)))
#temporal
tempoall.cv<-left_join(mod1.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$temp_c-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)
