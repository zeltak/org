library(foreign) 
library(nlme)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)



#create CV table
mod1table <- data.frame(type=character(10), r2000=numeric(10),r2001=numeric(10),r2002=numeric(10),r2003=numeric(10),r2003=numeric(10),r2003=numeric(10),r2006=numeric(10),r2007=numeric(10),r2008=numeric(10),r20010=numeric(10),r2010=numeric(10),r2011=numeric(10),mean=numeric(10))

#name columns
mod1table$type <- c("N_R2","N_int","N_int_se","N_slope","N_slope_se","RMSPE","spatial","temporal","RMSPE_spatial","RMSPE_temporal")




splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}



mod1 <-  read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2003.dbf") 
summary(mod1)
#day dataset
DELLIST <-  names(mod1) %in% c("DTckin", "humidity")
mod1d <- mod1[!DELLIST]
mod1d<-na.omit(mod1d)
mod1d$predicted<-NA


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


#extract logan
mon <-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/amar_temp_qt/X2003.csv")
mon<-na.omit(mon)
mon <- mon[, date:=as.Date(strptime(Date, "%m/%d/%y"))]
#stations within boston
bosmon <-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/amar_temp_qt/relevant150kstations.csv")
monboston <- mon [mon$station %in% bosmon$station, ] 
#logan is Kbos
monlogan <- mon[station %in% c("KBOS"), ]
monlogan <-monlogan [,c(3,18),with=FALSE]
setnames(monlogan,"tempc","tlogan")

mon1<-as.data.table(mod1d_all)
mon1<-na.omit(mon1)
# mon <- mon[, day:=as.Date(strptime(Date, "%m/%d/%y"))]
mon1boston <- mon1 [mon1$station %in% bosmon$station, ] 
m1<-summary(lm(tempc~predicted,data=mon1boston))
#RMSPE
sqrt(mean(m1$residual^2))


setkey(mon1boston,date)
setkey(monlogan,date)
dat <- merge(mon1boston,monlogan, all.x = T)

m2<-summary(lm(tlogan~predicted,data=dat))
#RMSPE
sqrt(mean(m2$residual^2))








