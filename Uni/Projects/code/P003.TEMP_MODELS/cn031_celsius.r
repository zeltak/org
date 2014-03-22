library(foreign) 
library(nlme)

cvtable <- data.frame(type=character(13), r2003=numeric(13))

cvtable$type <- c("it_1_r2", "it_2_r2","it_3_r2","it_4_r2","it_5_r2","it_6_r2","it_7_r2","it_8_r2","it_9_r2","it_10_r2","Mean_R2","spatial_r2","temporal_r2")




splitdf <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/10))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}

F_T2003_All <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod1/T2003.csv", header=T) 




#emisivity offset correction
F_T2003_All$em2 <- F_T2003_All$emis_scale+0.49

#kinetic temprature
F_T2003_All$temp_kin <- F_T2003_All$st_faren/F_T2003_All$em2^0.25

#convert both to c
F_T2003_All$tsc=(5/9)*(F_T2003_All$temp_kin-32)
F_T2003_All$tac=(5/9)*(F_T2003_All$TMIN-32)





#2.SPLIT DATASETS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#s1

splits_s1 <- splitdf(F_T2003_All)

mod1_T2003_10_s1 <- splits_s1$trainset
mod1_T2003_90_s1 <- splits_s1$testset




out.model_T2003_90_s1 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
	data =  mod1_T2003_90_s1) 

summary(out.model_T2003_90_s1) 

mod1_T2003_10_s1$predicted <- predict(object=out.model_T2003_90_s1,newdata=mod1_T2003_10_s1 )

cvtable$r2003[1] <- cor(mod1_T2003_10_s1$tac,mod1_T2003_10_s1$predicted,use = "complete.obs")*cor(mod1_T2003_10_s1$tac,mod1_T2003_10_s1$predicted,use = "complete.obs")


#s2

splits_s2 <- splitdf(F_T2003_All)

mod1_T2003_10_s2 <- splits_s2$trainset
mod1_T2003_90_s2 <- splits_s2$testset




out.model_T2003_90_s2 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s2) 

summary(out.model_T2003_90_s2) 

mod1_T2003_10_s2$predicted <- predict(object=out.model_T2003_90_s2,newdata=mod1_T2003_10_s2 )

cvtable$r2003[2] <- cor(mod1_T2003_10_s2$tac,mod1_T2003_10_s2$predicted,use = "complete.obs")*cor(mod1_T2003_10_s2$tac,mod1_T2003_10_s2$predicted,use = "complete.obs")



#s3

splits_s3 <- splitdf(F_T2003_All)

mod1_T2003_10_s3 <- splits_s3$trainset
mod1_T2003_90_s3 <- splits_s3$testset




out.model_T2003_90_s3 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s3) 

summary(out.model_T2003_90_s3) 

mod1_T2003_10_s3$predicted <- predict(object=out.model_T2003_90_s3,newdata=mod1_T2003_10_s3 )

cvtable$r2003[3] <- cor(mod1_T2003_10_s3$tac,mod1_T2003_10_s3$predicted,use = "complete.obs")*cor(mod1_T2003_10_s3$tac,mod1_T2003_10_s3$predicted,use = "complete.obs")


#s4

splits_s4 <- splitdf(F_T2003_All)

mod1_T2003_10_s4 <- splits_s4$trainset
mod1_T2003_90_s4 <- splits_s4$testset




out.model_T2003_90_s4 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s4) 

summary(out.model_T2003_90_s4) 

mod1_T2003_10_s4$predicted <- predict(object=out.model_T2003_90_s4,newdata=mod1_T2003_10_s4 )

cvtable$r2003[4] <- cor(mod1_T2003_10_s4$tac,mod1_T2003_10_s4$predicted,use = "complete.obs")*cor(mod1_T2003_10_s4$tac,mod1_T2003_10_s4$predicted,use = "complete.obs")


#s5

splits_s5 <- splitdf(F_T2003_All)

mod1_T2003_10_s5 <- splits_s5$trainset
mod1_T2003_90_s5 <- splits_s5$testset




out.model_T2003_90_s5 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s5) 

summary(out.model_T2003_90_s5) 

mod1_T2003_10_s5$predicted <- predict(object=out.model_T2003_90_s5,newdata=mod1_T2003_10_s5 )

cvtable$r2003[5] <- cor(mod1_T2003_10_s5$tac,mod1_T2003_10_s5$predicted,use = "complete.obs")*cor(mod1_T2003_10_s5$tac,mod1_T2003_10_s5$predicted,use = "complete.obs")



#s6

splits_s6 <- splitdf(F_T2003_All)

mod1_T2003_10_s6 <- splits_s6$trainset
mod1_T2003_90_s6 <- splits_s6$testset




out.model_T2003_90_s6 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s6) 

summary(out.model_T2003_90_s6) 

mod1_T2003_10_s6$predicted <- predict(object=out.model_T2003_90_s6,newdata=mod1_T2003_10_s6 )

cvtable$r2003[6] <- cor(mod1_T2003_10_s6$tac,mod1_T2003_10_s6$predicted,use = "complete.obs")*cor(mod1_T2003_10_s6$tac,mod1_T2003_10_s6$predicted,use = "complete.obs")



#s7

splits_s7 <- splitdf(F_T2003_All)

mod1_T2003_10_s7 <- splits_s7$trainset
mod1_T2003_90_s7 <- splits_s7$testset




out.model_T2003_90_s7 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s7) 

summary(out.model_T2003_90_s7) 

mod1_T2003_10_s7$predicted <- predict(object=out.model_T2003_90_s7,newdata=mod1_T2003_10_s7 )

cvtable$r2003[7] <- cor(mod1_T2003_10_s7$tac,mod1_T2003_10_s7$predicted,use = "complete.obs")*cor(mod1_T2003_10_s7$tac,mod1_T2003_10_s7$predicted,use = "complete.obs")


#s8

splits_s8 <- splitdf(F_T2003_All)

mod1_T2003_10_s8 <- splits_s8$trainset
mod1_T2003_90_s8 <- splits_s8$testset




out.model_T2003_90_s8 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s8) 

summary(out.model_T2003_90_s8) 

mod1_T2003_10_s8$predicted <- predict(object=out.model_T2003_90_s8,newdata=mod1_T2003_10_s8 )

cvtable$r2003[8] <- cor(mod1_T2003_10_s8$tac,mod1_T2003_10_s8$predicted,use = "complete.obs")*cor(mod1_T2003_10_s8$tac,mod1_T2003_10_s8$predicted,use = "complete.obs")


#s9

splits_s9 <- splitdf(F_T2003_All)

mod1_T2003_10_s9 <- splits_s9$trainset
mod1_T2003_90_s9 <- splits_s9$testset




out.model_T2003_90_s9 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s9) 

summary(out.model_T2003_90_s9) 

mod1_T2003_10_s9$predicted <- predict(object=out.model_T2003_90_s9,newdata=mod1_T2003_10_s9 )

cvtable$r2003[9] <- cor(mod1_T2003_10_s9$tac,mod1_T2003_10_s9$predicted,use = "complete.obs")*cor(mod1_T2003_10_s9$tac,mod1_T2003_10_s9$predicted,use = "complete.obs")



#s10

splits_s10 <- splitdf(F_T2003_All)

mod1_T2003_10_s10 <- splits_s10$trainset
mod1_T2003_90_s10 <- splits_s10$testset




out.model_T2003_90_s10 = lme( tac ~ tsc+windsp_krig*tsc+elevation+ndvi+per_built,
  random = ~1 + tsc| DATE,  
  data =  mod1_T2003_90_s10) 

summary(out.model_T2003_90_s10) 

mod1_T2003_10_s10$predicted <- predict(object=out.model_T2003_90_s10,newdata=mod1_T2003_10_s10 )

cvtable$r2003[10] <- cor(mod1_T2003_10_s10$tac,mod1_T2003_10_s10$predicted,use = "complete.obs")*cor(mod1_T2003_10_s10$tac,mod1_T2003_10_s10$predicted,use = "complete.obs")





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FINAL STEPS



####BIND ALL 10% into 1 dataset

all_2003_10p <- rbind(mod1_T2003_10_s1,mod1_T2003_10_s2,mod1_T2003_10_s3,mod1_T2003_10_s4,mod1_T2003_10_s5,mod1_T2003_10_s6,mod1_T2003_10_s7,mod1_T2003_10_s8,mod1_T2003_10_s9, mod1_T2003_10_s10)


#calculate mean Temp for all 10 iterations
cvtable$r2003[11]<- mean(cvtable$r2003[1:10])

plot(all_2003_10p$predicted,all_2003_10p$tac)

########################
#calculate R2 pre local pm and spatial vs temporal
########################


#create barpm and barpred
attach(all_2003_10p)
agg1<- aggregate(tac ~ SID,FUN=mean, na.rm=TRUE)
agg2<- aggregate(predicted ~ SID,FUN=mean, na.rm=TRUE)
aggf<- merge(agg1,agg2,by="SID")
detach(all_2003_10p)


names(aggf) <- c("SID", "barpm", "barpred")


#merge back site mean
aggf <- aggf[order(aggf$SID),]  #sort by SID
all_2003_10p <- all_2003_10p[order(all_2003_10p$SID),] #sort by SID
t2003m <- merge(all_2003_10p,aggf,by="SID") #merge by SID


t2003m$delpm <- t2003m$tac-t2003m$barpm
t2003m$delpred <-t2003m$pred-t2003m$barpred


######################################################################
#get R2 for the spatial and temporal:

# Spatial R2   was calculated by regressing the annual mean  against the mean OApred pm at place i.


# Temporal R2           Delta PM~ Delpred
# Delta PM: is the difference between the actual temp in place i at time t and the annual mean temp at that location
# Delta OApred: is the difference between the OApred temp in place i at time t and the annual OApred mean temp at that location





######################################################################

#spatial
mod_spatial <- lm(barpm ~ barpred, data=t2003m)
summary(mod_spatial)
cvtable$r2003[12] <-summary(mod_spatial)$r.squared

#temporal
mod_temporal <- lm(delpm ~ delpred, data=t2003m)
summary(mod_temporal)
cvtable$r2003[13] <-summary(mod_temporal)$r.squared




write.csv(cvtable,"c:/Users/ekloog/Documents/My Dropbox/uni/~code/3.1.3.3.Code/4.results/t2003.csv")



