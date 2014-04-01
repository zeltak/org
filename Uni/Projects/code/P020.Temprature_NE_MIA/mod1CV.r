library(nlme) 
library(foreign) 
library(nlme) 
library (splines)
library (MASS)
library(ggplot2)
library(plyr)
library(Hmisc)
library (mgcv)
library (splines)

#create CV table
mod1table <- data.frame(type=character(9), r2000=numeric(9),r2001=numeric(9),r2002=numeric(9),r2003=numeric(9),r2004=numeric(9),r2005=numeric(9),r2006=numeric(9),r2007=numeric(9),r2008=numeric(9),r2009=numeric(9),r2010=numeric(9),r2011=numeric(9),mean=numeric(9))

#name columns
mod1table$type <- c("N_R2","N_int","N_int_se","N_slope","N_slope_se","RMSPE","spatial","temporal","RMSPE_spatial")




raw1 <-  read.csv("f:/Uni/Projects/P031_MIAC_MEXICO/3.Work/2.Gather_data/FN009_mod1_files/mod1.csv", header=T)  
names(raw1)
summary(raw1)

hist(raw1$daymean)
hist(raw1$meanpbl)

# drop everything that is missing
mod1 <- raw1[!is.na(raw1$meanpbl+raw1$avewsp) & !is.na(raw1$rhmean), ]

# drop Jan 1 2005, Jan 1 2006
mod1$day <- as.Date(strptime(mod1$DATE, "%m/%d/%y"))
mod1$dayofyr <- as.numeric(format(mod1$day, "%j"))
mod1 <- mod1[mod1$dayofyr > 1, ]

#pbl-elev > interact with AOD
mod1$pblelev<-mod1$meanpbl-mod1$elev_1

model.formula <- as.formula(daymean ~ AOD+pblelev+tden_1+tden_1*season+tden_1+day+elev_1+ns(avewsp,df=3)+tempmean+season+avewdr+meanampbl*elev_1+meanpmpbl*elev_1)

res.mod1 <- lme(model.formula, random = ~1 + AOD| DATE,data =  mod1, na.action = na.omit) 
summary(res.mod1) 
look <- mod1[mod1$daymean >= 60,]

summary(look)
summary(mod1)



#model.gamm <- as.formula(daymean ~ AOD+s(pm_y, fx = T, k = 3))
#res.mod1 <- gamm(model.gamm, random=list(DATE=~1),data =  mod1, na.action = na.omit) 



mod1$predicted <- predict(object=res.mod1)
summary(lm(daymean ~ predicted, data=mod1))

plot(mod1$predicted,mod1$daymean)

mod1$resid<-residuals(res.mod1)
submod1 <- subset(mod1, resid < 2 * sd(mod1$resid) & resid > -2 * sd(mod1$resid))



splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}




splits_s1 <- splitdf(submod1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s1, na.action = na.omit) 
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1 )

splits_s2 <- splitdf(submod1)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s2, na.action = na.omit) 
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2 )

splits_s3 <- splitdf(submod1)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s3, na.action = na.omit) 
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3 )


splits_s4 <- splitdf(submod1)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s4, na.action = na.omit) 
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4 )

splits_s5 <- splitdf(submod1)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s5, na.action = na.omit) 
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5 )

splits_s6 <- splitdf(submod1)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s6, na.action = na.omit) 
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6 )

splits_s7 <- splitdf(submod1)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s7, na.action = na.omit) 
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7 )

splits_s8 <- splitdf(submod1)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s8, na.action = na.omit) 
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8 )

splits_s9 <- splitdf(submod1)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 = lme(model.formula,
                random = ~1 + AOD| DATE,data =  mod1d_90_s9, na.action = na.omit) 
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9 )

splits_s10 <- splitdf(submod1)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 = lme(model.formula,
                 random = ~1 + AOD| DATE,data =  mod1d_90_s10, na.action = na.omit) 
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10 )





####BIND ALL 10% into 1 dataset

mod1d_all <- rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10)


mod1d_reg <- lm(mod1d_all$daymean~mod1d_all$predicted)
summary(mod1d_reg)
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
agg1<- aggregate(daymean ~ mon,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ mon,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="mon")
detach(mod1d_all)
names(aggf) <- c("mon", "barpm", "barpred")

#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2000[7] <-summary(mod_spatial)$r.squared
mod1table$r2000[7] <-summary(mod_spatial)$r.squared

#temporal
aggf <- aggf[order(aggf$mon),]  #sort by mon
mod1d_all <- mod1d_all[order(mod1d_all$mon),] #sort by mon
mod1allst <- merge(mod1d_all,aggf,by="mon") #merge by mon
mod1allst$delpm <- mod1allst$daymean-mod1allst$barpm
mod1allst$delpred <-mod1allst$pred-mod1allst$barpred
#temporal
mod_temporal <- lm(delpm ~ delpred, data=mod1allst)
summary(mod_temporal)
mod1table$r2000[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
mod1allst$spatresid<-mod1allst$barpm-mod1allst$barpred
mod1table$r2000[9]<- sqrt(mean(mod1allst$spatresid^2))


