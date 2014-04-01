library(foreign) 
library(nlme)



#create CV table
mod1table <- data.frame(type=character(10), r2000=numeric(10),r2001=numeric(10),r2002=numeric(10),r2003=numeric(10),r2004=numeric(10),r2005=numeric(10),r2006=numeric(10),r2007=numeric(10),r2008=numeric(10),r20010=numeric(10),r2010=numeric(10),r2011=numeric(10),mean=numeric(10))

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



#YEAR 2000

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2000.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2000[1] <-summary(mod1d_reg)$r.squared
mod1table$r2000[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2000[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2000[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2000[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2000[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2000[7] <-summary(mod_spatial)$r.squared
mod1table$r2000[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2000[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2000[9]<- sqrt(mean(mod1allst$spatresid^2))

mod1allst$tempresid<-mod1allst$delpm-mod1allst$delpred
mod1allstTT<-na.omit(mod1allst)
mod1table$r2000[10]<-sqrt(mean(mod1allstTT$tempresid^2))





#YEAR 2001

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2001.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2001[1] <-summary(mod1d_reg)$r.squared
mod1table$r2001[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2001[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2001[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2001[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2001[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2001[7] <-summary(mod_spatial)$r.squared
mod1table$r2001[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2001[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2001[9]<- sqrt(mean(mod1allst$spatresid^2))

mod1allst$tempresid<-mod1allst$delpm-mod1allst$delpred
mod1allstTT<-na.omit(mod1allst)
mod1table$r2001[10]<-sqrt(mean(mod1allstTT$tempresid^2))


#YEAR 2002

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2002.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2002[1] <-summary(mod1d_reg)$r.squared
mod1table$r2002[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2002[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2002[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2002[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2002[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2002[7] <-summary(mod_spatial)$r.squared
mod1table$r2002[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2002[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2002[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2003

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2003.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2003[1] <-summary(mod1d_reg)$r.squared
mod1table$r2003[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2003[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2003[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2003[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2003[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2003[7] <-summary(mod_spatial)$r.squared
mod1table$r2003[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2003[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2003[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2004

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2004.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2004[1] <-summary(mod1d_reg)$r.squared
mod1table$r2004[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2004[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2004[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2004[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2004[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2004[7] <-summary(mod_spatial)$r.squared
mod1table$r2004[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2004[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2004[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2005

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2005.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2005[1] <-summary(mod1d_reg)$r.squared
mod1table$r2005[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2005[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2005[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2005[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2005[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2005[7] <-summary(mod_spatial)$r.squared
mod1table$r2005[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2005[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2005[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2006

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2006.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2006[1] <-summary(mod1d_reg)$r.squared
mod1table$r2006[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2006[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2006[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2006[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2006[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2006[7] <-summary(mod_spatial)$r.squared
mod1table$r2006[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2006[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2006[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2007

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2007.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2007[1] <-summary(mod1d_reg)$r.squared
mod1table$r2007[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2007[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2007[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2007[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2007[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2007[7] <-summary(mod_spatial)$r.squared
mod1table$r2007[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2007[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2007[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2008

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2008.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2008[1] <-summary(mod1d_reg)$r.squared
mod1table$r2008[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2008[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2008[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2008[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2008[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2008[7] <-summary(mod_spatial)$r.squared
mod1table$r2008[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2008[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2008[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2009

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2009.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2009[1] <-summary(mod1d_reg)$r.squared
mod1table$r2009[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2009[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2009[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2009[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2009[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2009[7] <-summary(mod_spatial)$r.squared
mod1table$r2009[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2009[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2009[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2010

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2010.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2010[1] <-summary(mod1d_reg)$r.squared
mod1table$r2010[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2010[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2010[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2010[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2010[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2010[7] <-summary(mod_spatial)$r.squared
mod1table$r2010[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2010[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2010[9]<- sqrt(mean(mod1allst$spatresid^2))
#YEAR 2011

mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2011.dbf") 
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


mod1d_reg <- lm(mod1d_all$tempc~mod1d_all$predicted)

mod1table$r2011[1] <-summary(mod1d_reg)$r.squared
mod1table$r2011[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2011[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2011[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2011[5] <-summary(mod1d_reg)$coef[2,2]


#rmspe
mod1table$r2011[6]<- sqrt(mean(mod1d_reg$residual^2))


#spatial R2
names(mod1d_all)

#create barpm and barpred
attach(mod1d_all)
agg1<- aggregate(tempc ~ station,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ station,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="station")
detach(mod1d_all)
names(aggf) <- c("station", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2011[7] <-summary(mod_spatial)$r.squared
mod1table$r2011[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$station),]  #sort by station
mod1d_all <- mod1d_all[order(mod1d_all$station),] #sort by station
mod1allst <- merge(mod1d_all,aggf,by="station") #merge by station
mod1allst$delpm <- mod1allst$tempc-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2011[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2011[9]<- sqrt(mean(mod1allst$spatresid^2))






###################################FINAL########################################
###means and export
mod1table$mean<-rowMeans(mod1table[,2:13])


write.csv(mod1table, "f:/Uni/Projects/P020_Temprature_NE_MIA/4.Results/mod1/NST.csv") 
 



