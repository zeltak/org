###############
#LIBS
###############
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(lme4)
library(bit64)


splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}




#create CV table
mod1table <- data.frame(type=character(10), r2000=numeric(10),r2005=numeric(10),r2011=numeric(10))

#name columns
mod1table$type <- c("R2","RMSPE","N_int_se","N_slope","N_slope_se","RMSPE","spatial","temporal","RMSPE_spatial","RMSPE_temporal")



##2005

mod1 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2005.dbf"))
mod1d <- mod1[!is.na(NTckin),]
mod1d<-na.omit(mod1d)


#2.SPLIT DATASETS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#s1

splits_s1 <- splitdf(mod1d)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s1) 
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1 )

#s2
splits_s2 <- splitdf(mod1d)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s2) 
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2 )

#s3
splits_s3 <- splitdf(mod1d)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s3) 
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3 )

#s4
splits_s4 <- splitdf(mod1d)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s4) 
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4 )

#s5
splits_s5 <- splitdf(mod1d)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s5) 
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5 )

#s6
splits_s6 <- splitdf(mod1d)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s6) 
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6 )

#s7
splits_s7 <- splitdf(mod1d)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s7) 
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7 )

#s8
splits_s8 <- splitdf(mod1d)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s8) 
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8 )


#s9
splits_s9 <- splitdf(mod1d)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s9) 
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9 )


#s10
splits_s10 <- splitdf(mod1d)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 = lme( tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_90_s10) 
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10 )


####BIND ALL 10% into 1 dataset

mod1d_all <- rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10)
mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)
summary(mod1d_reg)$r.squared
sqrt(mean(mod1d_reg$residual^2))

# 
# 
# Mod.tbl<-mod1d_all[, list(mod=list(lm(tempc ~ NTckin+elev+purban+NDVI))), by=list(STATE_ABBR)]
# # save out brand-specific predictions
# Mod.tbl[ , predict(mod[[1]]), by= STATE_ABBR]
# DT[, summary(lm(Rev~predbrand))$r.squared]

describe(mod1d_all$STATE_ABBR)

#NY
out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[STATE_ABBR=="NY",]) 
mod1d_all[STATE_ABBR=="NY",]$predicted <- predict(object=out )
summary(lm(mod1d_all[STATE_ABBR=="NY",]$tempc~mod1d_all[STATE_ABBR=="NY",]$predicted))
out2<-summary(lm(mod1d_all[STATE_ABBR=="NY",]$tempc~mod1d_all[STATE_ABBR=="NY",]$predicted))
sqrt(mean(out2$residual^2))
0.9639
1.782
#MA
out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[STATE_ABBR=="MA",]) 
mod1d_all[STATE_ABBR=="MA",]$predicted <- predict(object=out )
out2<-summary(lm(mod1d_all[STATE_ABBR=="MA",]$tempc~mod1d_all[STATE_ABBR=="MA",]$predicted))
sqrt(mean(out2$residual^2))
0.974
1.561796

#PA
out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[STATE_ABBR=="PA",]) 
mod1d_all[STATE_ABBR=="PA",]$predicted <- predict(object=out )
out2<-summary(lm(mod1d_all[STATE_ABBR=="PA",]$tempc~mod1d_all[STATE_ABBR=="PA",]$predicted))
sqrt(mean(out2$residual^2))
0.972 
1.605246

#MD
out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[STATE_ABBR=="MD",]) 
mod1d_all[STATE_ABBR=="MD",]$predicted <- predict(object=out )
out2<-summary(lm(mod1d_all[STATE_ABBR=="MD",]$tempc~mod1d_all[STATE_ABBR=="MD",]$predicted))
sqrt(mean(out2$residual^2))
0.9773 







mod1d <-  read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/Mod1_2005.dbf")
str(mod1d)
#Seasons
library(car)
mod1d_all$month <- as.numeric(format(mod1d_all$date, "%m"))
#1-winter, 2-spring,3-summer,4-autum
mod1d_all$season<-recode(mod1d_all$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
mod1d_all$seasonSW<-as.character(recode(mod1d_all$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))
describe(mod1d_all)


out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[season=="1",]) 
mod1d_all[season=="1",]$predicted <- predict(object=out )
summary(lm(mod1d_all[season=="1",]$tempc~mod1d_all[season=="1",]$predicted))


out = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d_all[season=="2",]) 
mod1d_all[season=="2",]$predicted <- predict(object=out )
summary(lm(mod1d_all[season=="2",]$tempc~mod1d_all[season=="2",]$predicted))


