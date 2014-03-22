library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2000###########
######2000###########
######2000###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2000_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2000.csv", header=T)
summary(T2000_weight)
T2000_weight<-na.omit(T2000_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2000_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2000_weight$prob <- predict(w1,type = c("response"))

T2000_weight$wt <- 1/T2000_weight$prob

T2000_weight$normwt <- T2000_weight$wt/mean(T2000_weight$wt)
T2000_weight <- rename(T2000_weight, c(date="Date"))
summary(T2000_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2000_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2000.csv", header=T) 
names(F_T2000_Allp)


# sort 
F_T2000_Allp <- F_T2000_Allp[order(F_T2000_Allp$guid,F_T2000_Allp$Date),]
T2000_weight <- T2000_weight[order(T2000_weight$guid,T2000_weight$Date),]

# merge two dataframes by ID
F_T2000_All <- merge(F_T2000_Allp,T2000_weight,by=c("guid","Date"))
names(F_T2000_All)



#transfer all to Z-scores
F_T2000_All$zelev.x<-(F_T2000_All$elev.x-mean(F_T2000_All$elev.x))/sd(F_T2000_All$elev.x) 
F_T2000_All$zpopden<-(F_T2000_All$pop_sqkm-mean(F_T2000_All$pop_sqkm))/sd(F_T2000_All$pop_sqkm)
F_T2000_All$zospace<-(F_T2000_All$p_open-mean(F_T2000_All$p_open))/sd(F_T2000_All$p_open)
F_T2000_All$ztden<- (F_T2000_All$tden-mean(F_T2000_All$tden))/sd(F_T2000_All$tden)
F_T2000_All$zforest<-(F_T2000_All$per_fore_1-mean(F_T2000_All$per_fore_1))/sd(F_T2000_All$per_fore_1)
F_T2000_All$zpsource<-(F_T2000_All$point_pm-mean(F_T2000_All$point_pm))/sd(F_T2000_All$point_pm)
F_T2000_All$zasource<- (F_T2000_All$area_pm-mean(F_T2000_All$area_pm))/sd(F_T2000_All$area_pm)
F_T2000_All$zvisib<- (F_T2000_All$visib_F-mean(F_T2000_All$visib_F))/sd(F_T2000_All$visib_F)
F_T2000_All$zwdsp<-(F_T2000_All$wdsp_F-mean(F_T2000_All$wdsp_F))/sd(F_T2000_All$wdsp_F)
F_T2000_All$zhumid<-(F_T2000_All$ah_gm3_F-mean(F_T2000_All$ah_gm3_F))/sd(F_T2000_All$ah_gm3_F)

F_T2000_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2000_All$zelev.x <-F_T2000_All$zelev.x*-1
F_T2000_All$zospace <- F_T2000_All$zospace*-1
F_T2000_All$zforest<- F_T2000_All$zforest*-1
F_T2000_All$zvisib <- F_T2000_All$zvisib*-1
F_T2000_All$zwdsp <- F_T2000_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2000_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2000_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2000_all_st_table <- data.frame(F_T2000_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2000_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2000.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s1 <- splits_s1$trainset
mod1_T2000_90_s1 <- splits_s1$testset

write.dbf(mod1_T2000_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s1.dbf")

write.dbf(mod1_T2000_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s2 <- splits_s2$trainset
mod1_T2000_90_s2 <- splits_s2$testset

write.dbf(mod1_T2000_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s2.dbf")

write.dbf(mod1_T2000_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s3 <- splits_s3$trainset
mod1_T2000_90_s3 <- splits_s3$testset

write.dbf(mod1_T2000_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s3.dbf")

write.dbf(mod1_T2000_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s4 <- splits_s4$trainset
mod1_T2000_90_s4 <- splits_s4$testset

write.dbf(mod1_T2000_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s4.dbf")

write.dbf(mod1_T2000_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s5 <- splits_s5$trainset
mod1_T2000_90_s5 <- splits_s5$testset

write.dbf(mod1_T2000_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s5.dbf")

write.dbf(mod1_T2000_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s6 <- splits_s6$trainset
mod1_T2000_90_s6 <- splits_s6$testset

write.dbf(mod1_T2000_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s6.dbf")

write.dbf(mod1_T2000_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s7 <- splits_s7$trainset
mod1_T2000_90_s7 <- splits_s7$testset

write.dbf(mod1_T2000_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s7.dbf")

write.dbf(mod1_T2000_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s8 <- splits_s8$trainset
mod1_T2000_90_s8 <- splits_s8$testset

write.dbf(mod1_T2000_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s8.dbf")

write.dbf(mod1_T2000_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s9 <- splits_s9$trainset
mod1_T2000_90_s9 <- splits_s9$testset

write.dbf(mod1_T2000_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s9.dbf")

write.dbf(mod1_T2000_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s10 <- splits_s10$trainset
mod1_T2000_90_s10 <- splits_s10$testset

write.dbf(mod1_T2000_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s10.dbf")

write.dbf(mod1_T2000_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2000_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2000.csv", header=T) 
names(F_T2000_mod2p)
F_T2000_mod2p <- rename(F_T2000_mod2p, c(date="Date"))




# sort 
F_T2000_mod2p <- F_T2000_mod2p[order(F_T2000_mod2p$guid,F_T2000_mod2p$Date),]
T2000_weight <- T2000_weight[order(T2000_weight$guid,T2000_weight$Date),]

# merge two dataframes by ID
F_T2000_mod2 <- merge(F_T2000_mod2p,T2000_weight,by=c("guid","Date"))
names(F_T2000_mod2)




#transfer all to Z-scores
F_T2000_mod2$zelev.x<-(F_T2000_mod2$elev.x-mean(F_T2000_All$elev.x))/sd(F_T2000_All$elev.x) 
F_T2000_mod2$zpopden<-(F_T2000_mod2$pop_sqkm-mean(F_T2000_All$pop_sqkm))/sd(F_T2000_All$pop_sqkm)
F_T2000_mod2$zospace<-(F_T2000_mod2$p_open-mean(F_T2000_All$p_open))/sd(F_T2000_All$p_open)
F_T2000_mod2$ztden<- (F_T2000_mod2$tden-mean(F_T2000_All$tden))/sd(F_T2000_All$tden)
F_T2000_mod2$zforest<-(F_T2000_mod2$per_fore_1-mean(F_T2000_mod2$per_fore_1))/sd(F_T2000_mod2$per_fore_1)
F_T2000_mod2$zpsource<-(F_T2000_mod2$point_pm-mean(F_T2000_All$point_pm))/sd(F_T2000_All$point_pm)
F_T2000_mod2$zasource<- (F_T2000_mod2$area_pm -mean(F_T2000_All$area_pm))/sd(F_T2000_All$area_pm)
F_T2000_mod2$zvisib <- (F_T2000_mod2$visib_F-mean(F_T2000_All$visib_F))/sd(F_T2000_All$visib_F)
F_T2000_mod2$zwdsp<-(F_T2000_mod2$wdsp_F-mean(F_T2000_All$wdsp_F))/sd(F_T2000_All$wdsp_F)
F_T2000_mod2$zhumid<-(F_T2000_mod2$ah_gm3_F-mean(F_T2000_All$ah_gm3_F))/sd(F_T2000_All$ah_gm3_F)

F_T2000_mod2$zid<-1 #create id variable




F_T2000_mod2$zelev.x <-F_T2000_mod2$zelev.x*-1
F_T2000_mod2$zospace <- F_T2000_mod2$zospace*-1
F_T2000_mod2$zforest <- F_T2000_mod2$zforest*-1
F_T2000_mod2$zvisib <- F_T2000_mod2$zvisib*-1
F_T2000_mod2$zwdsp <- F_T2000_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2000_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2000_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2000_all_st_table_mod2 <- data.frame(F_T2000_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2000_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2000.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2001###########
######2001###########
######2001###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2001_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2001.csv", header=T)
summary(T2001_weight)
T2001_weight<-na.omit(T2001_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2001_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2001_weight$prob <- predict(w1,type = c("response"))

T2001_weight$wt <- 1/T2001_weight$prob

T2001_weight$normwt <- T2001_weight$wt/mean(T2001_weight$wt)
T2001_weight <- rename(T2001_weight, c(date="Date"))
summary(T2001_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2001_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2001.csv", header=T) 
names(F_T2001_Allp)


# sort 
F_T2001_Allp <- F_T2001_Allp[order(F_T2001_Allp$guid,F_T2001_Allp$Date),]
T2001_weight <- T2001_weight[order(T2001_weight$guid,T2001_weight$Date),]

# merge two dataframes by ID
F_T2001_All <- merge(F_T2001_Allp,T2001_weight,by=c("guid","Date"))
names(F_T2001_All)



#transfer all to Z-scores
F_T2001_All$zelev.x<-(F_T2001_All$elev.x-mean(F_T2001_All$elev.x))/sd(F_T2001_All$elev.x) 
F_T2001_All$zpopden<-(F_T2001_All$pop_sqkm-mean(F_T2001_All$pop_sqkm))/sd(F_T2001_All$pop_sqkm)
F_T2001_All$zospace<-(F_T2001_All$p_open-mean(F_T2001_All$p_open))/sd(F_T2001_All$p_open)
F_T2001_All$ztden<- (F_T2001_All$tden-mean(F_T2001_All$tden))/sd(F_T2001_All$tden)
F_T2001_All$zforest<-(F_T2001_All$per_fore_1-mean(F_T2001_All$per_fore_1))/sd(F_T2001_All$per_fore_1)
F_T2001_All$zpsource<-(F_T2001_All$point_pm-mean(F_T2001_All$point_pm))/sd(F_T2001_All$point_pm)
F_T2001_All$zasource<- (F_T2001_All$area_pm-mean(F_T2001_All$area_pm))/sd(F_T2001_All$area_pm)
F_T2001_All$zvisib<- (F_T2001_All$visib_F-mean(F_T2001_All$visib_F))/sd(F_T2001_All$visib_F)
F_T2001_All$zwdsp<-(F_T2001_All$wdsp_F-mean(F_T2001_All$wdsp_F))/sd(F_T2001_All$wdsp_F)
F_T2001_All$zhumid<-(F_T2001_All$ah_gm3_F-mean(F_T2001_All$ah_gm3_F))/sd(F_T2001_All$ah_gm3_F)

F_T2001_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2001_All$zelev.x <-F_T2001_All$zelev.x*-1
F_T2001_All$zospace <- F_T2001_All$zospace*-1
F_T2001_All$zforest<- F_T2001_All$zforest*-1
F_T2001_All$zvisib <- F_T2001_All$zvisib*-1
F_T2001_All$zwdsp <- F_T2001_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2001_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2001_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2001_all_st_table <- data.frame(F_T2001_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2001_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2001.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s1 <- splits_s1$trainset
mod1_T2001_90_s1 <- splits_s1$testset

write.dbf(mod1_T2001_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s1.dbf")

write.dbf(mod1_T2001_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s2 <- splits_s2$trainset
mod1_T2001_90_s2 <- splits_s2$testset

write.dbf(mod1_T2001_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s2.dbf")

write.dbf(mod1_T2001_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s3 <- splits_s3$trainset
mod1_T2001_90_s3 <- splits_s3$testset

write.dbf(mod1_T2001_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s3.dbf")

write.dbf(mod1_T2001_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s4 <- splits_s4$trainset
mod1_T2001_90_s4 <- splits_s4$testset

write.dbf(mod1_T2001_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s4.dbf")

write.dbf(mod1_T2001_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s5 <- splits_s5$trainset
mod1_T2001_90_s5 <- splits_s5$testset

write.dbf(mod1_T2001_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s5.dbf")

write.dbf(mod1_T2001_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s6 <- splits_s6$trainset
mod1_T2001_90_s6 <- splits_s6$testset

write.dbf(mod1_T2001_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s6.dbf")

write.dbf(mod1_T2001_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s7 <- splits_s7$trainset
mod1_T2001_90_s7 <- splits_s7$testset

write.dbf(mod1_T2001_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s7.dbf")

write.dbf(mod1_T2001_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s8 <- splits_s8$trainset
mod1_T2001_90_s8 <- splits_s8$testset

write.dbf(mod1_T2001_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s8.dbf")

write.dbf(mod1_T2001_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s9 <- splits_s9$trainset
mod1_T2001_90_s9 <- splits_s9$testset

write.dbf(mod1_T2001_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s9.dbf")

write.dbf(mod1_T2001_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s10 <- splits_s10$trainset
mod1_T2001_90_s10 <- splits_s10$testset

write.dbf(mod1_T2001_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s10.dbf")

write.dbf(mod1_T2001_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2001_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2001.csv", header=T) 
names(F_T2001_mod2p)
F_T2001_mod2p <- rename(F_T2001_mod2p, c(date="Date"))




# sort 
F_T2001_mod2p <- F_T2001_mod2p[order(F_T2001_mod2p$guid,F_T2001_mod2p$Date),]
T2001_weight <- T2001_weight[order(T2001_weight$guid,T2001_weight$Date),]

# merge two dataframes by ID
F_T2001_mod2 <- merge(F_T2001_mod2p,T2001_weight,by=c("guid","Date"))
names(F_T2001_mod2)




#transfer all to Z-scores
F_T2001_mod2$zelev.x<-(F_T2001_mod2$elev.x-mean(F_T2001_All$elev.x))/sd(F_T2001_All$elev.x) 
F_T2001_mod2$zpopden<-(F_T2001_mod2$pop_sqkm-mean(F_T2001_All$pop_sqkm))/sd(F_T2001_All$pop_sqkm)
F_T2001_mod2$zospace<-(F_T2001_mod2$p_open-mean(F_T2001_All$p_open))/sd(F_T2001_All$p_open)
F_T2001_mod2$ztden<- (F_T2001_mod2$tden-mean(F_T2001_All$tden))/sd(F_T2001_All$tden)
F_T2001_mod2$zforest<-(F_T2001_mod2$per_fore_1-mean(F_T2001_mod2$per_fore_1))/sd(F_T2001_mod2$per_fore_1)
F_T2001_mod2$zpsource<-(F_T2001_mod2$point_pm-mean(F_T2001_All$point_pm))/sd(F_T2001_All$point_pm)
F_T2001_mod2$zasource<- (F_T2001_mod2$area_pm -mean(F_T2001_All$area_pm))/sd(F_T2001_All$area_pm)
F_T2001_mod2$zvisib <- (F_T2001_mod2$visib_F-mean(F_T2001_All$visib_F))/sd(F_T2001_All$visib_F)
F_T2001_mod2$zwdsp<-(F_T2001_mod2$wdsp_F-mean(F_T2001_All$wdsp_F))/sd(F_T2001_All$wdsp_F)
F_T2001_mod2$zhumid<-(F_T2001_mod2$ah_gm3_F-mean(F_T2001_All$ah_gm3_F))/sd(F_T2001_All$ah_gm3_F)

F_T2001_mod2$zid<-1 #create id variable




F_T2001_mod2$zelev.x <-F_T2001_mod2$zelev.x*-1
F_T2001_mod2$zospace <- F_T2001_mod2$zospace*-1
F_T2001_mod2$zforest <- F_T2001_mod2$zforest*-1
F_T2001_mod2$zvisib <- F_T2001_mod2$zvisib*-1
F_T2001_mod2$zwdsp <- F_T2001_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2001_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2001_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2001_all_st_table_mod2 <- data.frame(F_T2001_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2001_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2001.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2002###########
######2002###########
######2002###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2002_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2002.csv", header=T)
summary(T2002_weight)
T2002_weight<-na.omit(T2002_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2002_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2002_weight$prob <- predict(w1,type = c("response"))

T2002_weight$wt <- 1/T2002_weight$prob

T2002_weight$normwt <- T2002_weight$wt/mean(T2002_weight$wt)
T2002_weight <- rename(T2002_weight, c(date="Date"))
summary(T2002_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2002_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2002.csv", header=T) 
names(F_T2002_Allp)


# sort 
F_T2002_Allp <- F_T2002_Allp[order(F_T2002_Allp$guid,F_T2002_Allp$Date),]
T2002_weight <- T2002_weight[order(T2002_weight$guid,T2002_weight$Date),]

# merge two dataframes by ID
F_T2002_All <- merge(F_T2002_Allp,T2002_weight,by=c("guid","Date"))
names(F_T2002_All)



#transfer all to Z-scores
F_T2002_All$zelev.x<-(F_T2002_All$elev.x-mean(F_T2002_All$elev.x))/sd(F_T2002_All$elev.x) 
F_T2002_All$zpopden<-(F_T2002_All$pop_sqkm-mean(F_T2002_All$pop_sqkm))/sd(F_T2002_All$pop_sqkm)
F_T2002_All$zospace<-(F_T2002_All$p_open-mean(F_T2002_All$p_open))/sd(F_T2002_All$p_open)
F_T2002_All$ztden<- (F_T2002_All$tden-mean(F_T2002_All$tden))/sd(F_T2002_All$tden)
F_T2002_All$zforest<-(F_T2002_All$per_fore_1-mean(F_T2002_All$per_fore_1))/sd(F_T2002_All$per_fore_1)
F_T2002_All$zpsource<-(F_T2002_All$point_pm-mean(F_T2002_All$point_pm))/sd(F_T2002_All$point_pm)
F_T2002_All$zasource<- (F_T2002_All$area_pm-mean(F_T2002_All$area_pm))/sd(F_T2002_All$area_pm)
F_T2002_All$zvisib<- (F_T2002_All$visib_F-mean(F_T2002_All$visib_F))/sd(F_T2002_All$visib_F)
F_T2002_All$zwdsp<-(F_T2002_All$wdsp_F-mean(F_T2002_All$wdsp_F))/sd(F_T2002_All$wdsp_F)
F_T2002_All$zhumid<-(F_T2002_All$ah_gm3_F-mean(F_T2002_All$ah_gm3_F))/sd(F_T2002_All$ah_gm3_F)

F_T2002_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2002_All$zelev.x <-F_T2002_All$zelev.x*-1
F_T2002_All$zospace <- F_T2002_All$zospace*-1
F_T2002_All$zforest<- F_T2002_All$zforest*-1
F_T2002_All$zvisib <- F_T2002_All$zvisib*-1
F_T2002_All$zwdsp <- F_T2002_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2002_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2002_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2002_all_st_table <- data.frame(F_T2002_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2002_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2002.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s1 <- splits_s1$trainset
mod1_T2002_90_s1 <- splits_s1$testset

write.dbf(mod1_T2002_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s1.dbf")

write.dbf(mod1_T2002_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s2 <- splits_s2$trainset
mod1_T2002_90_s2 <- splits_s2$testset

write.dbf(mod1_T2002_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s2.dbf")

write.dbf(mod1_T2002_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s3 <- splits_s3$trainset
mod1_T2002_90_s3 <- splits_s3$testset

write.dbf(mod1_T2002_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s3.dbf")

write.dbf(mod1_T2002_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s4 <- splits_s4$trainset
mod1_T2002_90_s4 <- splits_s4$testset

write.dbf(mod1_T2002_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s4.dbf")

write.dbf(mod1_T2002_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s5 <- splits_s5$trainset
mod1_T2002_90_s5 <- splits_s5$testset

write.dbf(mod1_T2002_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s5.dbf")

write.dbf(mod1_T2002_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s6 <- splits_s6$trainset
mod1_T2002_90_s6 <- splits_s6$testset

write.dbf(mod1_T2002_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s6.dbf")

write.dbf(mod1_T2002_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s7 <- splits_s7$trainset
mod1_T2002_90_s7 <- splits_s7$testset

write.dbf(mod1_T2002_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s7.dbf")

write.dbf(mod1_T2002_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s8 <- splits_s8$trainset
mod1_T2002_90_s8 <- splits_s8$testset

write.dbf(mod1_T2002_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s8.dbf")

write.dbf(mod1_T2002_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s9 <- splits_s9$trainset
mod1_T2002_90_s9 <- splits_s9$testset

write.dbf(mod1_T2002_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s9.dbf")

write.dbf(mod1_T2002_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s10 <- splits_s10$trainset
mod1_T2002_90_s10 <- splits_s10$testset

write.dbf(mod1_T2002_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s10.dbf")

write.dbf(mod1_T2002_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2002_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2002.csv", header=T) 
names(F_T2002_mod2p)
F_T2002_mod2p <- rename(F_T2002_mod2p, c(date="Date"))




# sort 
F_T2002_mod2p <- F_T2002_mod2p[order(F_T2002_mod2p$guid,F_T2002_mod2p$Date),]
T2002_weight <- T2002_weight[order(T2002_weight$guid,T2002_weight$Date),]

# merge two dataframes by ID
F_T2002_mod2 <- merge(F_T2002_mod2p,T2002_weight,by=c("guid","Date"))
names(F_T2002_mod2)




#transfer all to Z-scores
F_T2002_mod2$zelev.x<-(F_T2002_mod2$elev.x-mean(F_T2002_All$elev.x))/sd(F_T2002_All$elev.x) 
F_T2002_mod2$zpopden<-(F_T2002_mod2$pop_sqkm-mean(F_T2002_All$pop_sqkm))/sd(F_T2002_All$pop_sqkm)
F_T2002_mod2$zospace<-(F_T2002_mod2$p_open-mean(F_T2002_All$p_open))/sd(F_T2002_All$p_open)
F_T2002_mod2$ztden<- (F_T2002_mod2$tden-mean(F_T2002_All$tden))/sd(F_T2002_All$tden)
F_T2002_mod2$zforest<-(F_T2002_mod2$per_fore_1-mean(F_T2002_mod2$per_fore_1))/sd(F_T2002_mod2$per_fore_1)
F_T2002_mod2$zpsource<-(F_T2002_mod2$point_pm-mean(F_T2002_All$point_pm))/sd(F_T2002_All$point_pm)
F_T2002_mod2$zasource<- (F_T2002_mod2$area_pm -mean(F_T2002_All$area_pm))/sd(F_T2002_All$area_pm)
F_T2002_mod2$zvisib <- (F_T2002_mod2$visib_F-mean(F_T2002_All$visib_F))/sd(F_T2002_All$visib_F)
F_T2002_mod2$zwdsp<-(F_T2002_mod2$wdsp_F-mean(F_T2002_All$wdsp_F))/sd(F_T2002_All$wdsp_F)
F_T2002_mod2$zhumid<-(F_T2002_mod2$ah_gm3_F-mean(F_T2002_All$ah_gm3_F))/sd(F_T2002_All$ah_gm3_F)

F_T2002_mod2$zid<-1 #create id variable




F_T2002_mod2$zelev.x <-F_T2002_mod2$zelev.x*-1
F_T2002_mod2$zospace <- F_T2002_mod2$zospace*-1
F_T2002_mod2$zforest <- F_T2002_mod2$zforest*-1
F_T2002_mod2$zvisib <- F_T2002_mod2$zvisib*-1
F_T2002_mod2$zwdsp <- F_T2002_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2002_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2002_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2002_all_st_table_mod2 <- data.frame(F_T2002_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2002_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2002.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2003###########
######2003###########
######2003###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2003_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2003.csv", header=T)
summary(T2003_weight)
T2003_weight<-na.omit(T2003_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2003_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2003_weight$prob <- predict(w1,type = c("response"))

T2003_weight$wt <- 1/T2003_weight$prob

T2003_weight$normwt <- T2003_weight$wt/mean(T2003_weight$wt)
T2003_weight <- rename(T2003_weight, c(date="Date"))
summary(T2003_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2003_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2003.csv", header=T) 
names(F_T2003_Allp)


# sort 
F_T2003_Allp <- F_T2003_Allp[order(F_T2003_Allp$guid,F_T2003_Allp$Date),]
T2003_weight <- T2003_weight[order(T2003_weight$guid,T2003_weight$Date),]

# merge two dataframes by ID
F_T2003_All <- merge(F_T2003_Allp,T2003_weight,by=c("guid","Date"))
names(F_T2003_All)



#transfer all to Z-scores
F_T2003_All$zelev.x<-(F_T2003_All$elev.x-mean(F_T2003_All$elev.x))/sd(F_T2003_All$elev.x) 
F_T2003_All$zpopden<-(F_T2003_All$pop_sqkm-mean(F_T2003_All$pop_sqkm))/sd(F_T2003_All$pop_sqkm)
F_T2003_All$zospace<-(F_T2003_All$p_open-mean(F_T2003_All$p_open))/sd(F_T2003_All$p_open)
F_T2003_All$ztden<- (F_T2003_All$tden-mean(F_T2003_All$tden))/sd(F_T2003_All$tden)
F_T2003_All$zforest<-(F_T2003_All$per_fore_1-mean(F_T2003_All$per_fore_1))/sd(F_T2003_All$per_fore_1)
F_T2003_All$zpsource<-(F_T2003_All$point_pm-mean(F_T2003_All$point_pm))/sd(F_T2003_All$point_pm)
F_T2003_All$zasource<- (F_T2003_All$area_pm-mean(F_T2003_All$area_pm))/sd(F_T2003_All$area_pm)
F_T2003_All$zvisib<- (F_T2003_All$visib_F-mean(F_T2003_All$visib_F))/sd(F_T2003_All$visib_F)
F_T2003_All$zwdsp<-(F_T2003_All$wdsp_F-mean(F_T2003_All$wdsp_F))/sd(F_T2003_All$wdsp_F)
F_T2003_All$zhumid<-(F_T2003_All$ah_gm3_F-mean(F_T2003_All$ah_gm3_F))/sd(F_T2003_All$ah_gm3_F)

F_T2003_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2003_All$zelev.x <-F_T2003_All$zelev.x*-1
F_T2003_All$zospace <- F_T2003_All$zospace*-1
F_T2003_All$zforest<- F_T2003_All$zforest*-1
F_T2003_All$zvisib <- F_T2003_All$zvisib*-1
F_T2003_All$zwdsp <- F_T2003_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2003_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2003_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2003_all_st_table <- data.frame(F_T2003_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2003_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2003.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s1 <- splits_s1$trainset
mod1_T2003_90_s1 <- splits_s1$testset

write.dbf(mod1_T2003_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s1.dbf")

write.dbf(mod1_T2003_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s2 <- splits_s2$trainset
mod1_T2003_90_s2 <- splits_s2$testset

write.dbf(mod1_T2003_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s2.dbf")

write.dbf(mod1_T2003_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s3 <- splits_s3$trainset
mod1_T2003_90_s3 <- splits_s3$testset

write.dbf(mod1_T2003_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s3.dbf")

write.dbf(mod1_T2003_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s4 <- splits_s4$trainset
mod1_T2003_90_s4 <- splits_s4$testset

write.dbf(mod1_T2003_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s4.dbf")

write.dbf(mod1_T2003_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s5 <- splits_s5$trainset
mod1_T2003_90_s5 <- splits_s5$testset

write.dbf(mod1_T2003_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s5.dbf")

write.dbf(mod1_T2003_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s6 <- splits_s6$trainset
mod1_T2003_90_s6 <- splits_s6$testset

write.dbf(mod1_T2003_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s6.dbf")

write.dbf(mod1_T2003_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s7 <- splits_s7$trainset
mod1_T2003_90_s7 <- splits_s7$testset

write.dbf(mod1_T2003_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s7.dbf")

write.dbf(mod1_T2003_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s8 <- splits_s8$trainset
mod1_T2003_90_s8 <- splits_s8$testset

write.dbf(mod1_T2003_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s8.dbf")

write.dbf(mod1_T2003_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s9 <- splits_s9$trainset
mod1_T2003_90_s9 <- splits_s9$testset

write.dbf(mod1_T2003_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s9.dbf")

write.dbf(mod1_T2003_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s10 <- splits_s10$trainset
mod1_T2003_90_s10 <- splits_s10$testset

write.dbf(mod1_T2003_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s10.dbf")

write.dbf(mod1_T2003_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2003_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2003.csv", header=T) 
names(F_T2003_mod2p)
F_T2003_mod2p <- rename(F_T2003_mod2p, c(date="Date"))




# sort 
F_T2003_mod2p <- F_T2003_mod2p[order(F_T2003_mod2p$guid,F_T2003_mod2p$Date),]
T2003_weight <- T2003_weight[order(T2003_weight$guid,T2003_weight$Date),]

# merge two dataframes by ID
F_T2003_mod2 <- merge(F_T2003_mod2p,T2003_weight,by=c("guid","Date"))
names(F_T2003_mod2)




#transfer all to Z-scores
F_T2003_mod2$zelev.x<-(F_T2003_mod2$elev.x-mean(F_T2003_All$elev.x))/sd(F_T2003_All$elev.x) 
F_T2003_mod2$zpopden<-(F_T2003_mod2$pop_sqkm-mean(F_T2003_All$pop_sqkm))/sd(F_T2003_All$pop_sqkm)
F_T2003_mod2$zospace<-(F_T2003_mod2$p_open-mean(F_T2003_All$p_open))/sd(F_T2003_All$p_open)
F_T2003_mod2$ztden<- (F_T2003_mod2$tden-mean(F_T2003_All$tden))/sd(F_T2003_All$tden)
F_T2003_mod2$zforest<-(F_T2003_mod2$per_fore_1-mean(F_T2003_mod2$per_fore_1))/sd(F_T2003_mod2$per_fore_1)
F_T2003_mod2$zpsource<-(F_T2003_mod2$point_pm-mean(F_T2003_All$point_pm))/sd(F_T2003_All$point_pm)
F_T2003_mod2$zasource<- (F_T2003_mod2$area_pm -mean(F_T2003_All$area_pm))/sd(F_T2003_All$area_pm)
F_T2003_mod2$zvisib <- (F_T2003_mod2$visib_F-mean(F_T2003_All$visib_F))/sd(F_T2003_All$visib_F)
F_T2003_mod2$zwdsp<-(F_T2003_mod2$wdsp_F-mean(F_T2003_All$wdsp_F))/sd(F_T2003_All$wdsp_F)
F_T2003_mod2$zhumid<-(F_T2003_mod2$ah_gm3_F-mean(F_T2003_All$ah_gm3_F))/sd(F_T2003_All$ah_gm3_F)

F_T2003_mod2$zid<-1 #create id variable




F_T2003_mod2$zelev.x <-F_T2003_mod2$zelev.x*-1
F_T2003_mod2$zospace <- F_T2003_mod2$zospace*-1
F_T2003_mod2$zforest <- F_T2003_mod2$zforest*-1
F_T2003_mod2$zvisib <- F_T2003_mod2$zvisib*-1
F_T2003_mod2$zwdsp <- F_T2003_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2003_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2003_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2003_all_st_table_mod2 <- data.frame(F_T2003_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2003_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2003.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2004###########
######2004###########
######2004###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2004_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2004.csv", header=T)
summary(T2004_weight)
T2004_weight<-na.omit(T2004_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2004_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2004_weight$prob <- predict(w1,type = c("response"))

T2004_weight$wt <- 1/T2004_weight$prob

T2004_weight$normwt <- T2004_weight$wt/mean(T2004_weight$wt)
T2004_weight <- rename(T2004_weight, c(date="Date"))
summary(T2004_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2004_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2004.csv", header=T) 
names(F_T2004_Allp)


# sort 
F_T2004_Allp <- F_T2004_Allp[order(F_T2004_Allp$guid,F_T2004_Allp$Date),]
T2004_weight <- T2004_weight[order(T2004_weight$guid,T2004_weight$Date),]

# merge two dataframes by ID
F_T2004_All <- merge(F_T2004_Allp,T2004_weight,by=c("guid","Date"))
names(F_T2004_All)



#transfer all to Z-scores
F_T2004_All$zelev.x<-(F_T2004_All$elev.x-mean(F_T2004_All$elev.x))/sd(F_T2004_All$elev.x) 
F_T2004_All$zpopden<-(F_T2004_All$pop_sqkm-mean(F_T2004_All$pop_sqkm))/sd(F_T2004_All$pop_sqkm)
F_T2004_All$zospace<-(F_T2004_All$p_open-mean(F_T2004_All$p_open))/sd(F_T2004_All$p_open)
F_T2004_All$ztden<- (F_T2004_All$tden-mean(F_T2004_All$tden))/sd(F_T2004_All$tden)
F_T2004_All$zforest<-(F_T2004_All$per_fore_1-mean(F_T2004_All$per_fore_1))/sd(F_T2004_All$per_fore_1)
F_T2004_All$zpsource<-(F_T2004_All$point_pm-mean(F_T2004_All$point_pm))/sd(F_T2004_All$point_pm)
F_T2004_All$zasource<- (F_T2004_All$area_pm-mean(F_T2004_All$area_pm))/sd(F_T2004_All$area_pm)
F_T2004_All$zvisib<- (F_T2004_All$visib_F-mean(F_T2004_All$visib_F))/sd(F_T2004_All$visib_F)
F_T2004_All$zwdsp<-(F_T2004_All$wdsp_F-mean(F_T2004_All$wdsp_F))/sd(F_T2004_All$wdsp_F)
F_T2004_All$zhumid<-(F_T2004_All$ah_gm3_F-mean(F_T2004_All$ah_gm3_F))/sd(F_T2004_All$ah_gm3_F)

F_T2004_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2004_All$zelev.x <-F_T2004_All$zelev.x*-1
F_T2004_All$zospace <- F_T2004_All$zospace*-1
F_T2004_All$zforest<- F_T2004_All$zforest*-1
F_T2004_All$zvisib <- F_T2004_All$zvisib*-1
F_T2004_All$zwdsp <- F_T2004_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2004_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2004_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2004_all_st_table <- data.frame(F_T2004_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2004_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2004.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s1 <- splits_s1$trainset
mod1_T2004_90_s1 <- splits_s1$testset

write.dbf(mod1_T2004_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s1.dbf")

write.dbf(mod1_T2004_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s2 <- splits_s2$trainset
mod1_T2004_90_s2 <- splits_s2$testset

write.dbf(mod1_T2004_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s2.dbf")

write.dbf(mod1_T2004_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s3 <- splits_s3$trainset
mod1_T2004_90_s3 <- splits_s3$testset

write.dbf(mod1_T2004_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s3.dbf")

write.dbf(mod1_T2004_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s4 <- splits_s4$trainset
mod1_T2004_90_s4 <- splits_s4$testset

write.dbf(mod1_T2004_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s4.dbf")

write.dbf(mod1_T2004_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s5 <- splits_s5$trainset
mod1_T2004_90_s5 <- splits_s5$testset

write.dbf(mod1_T2004_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s5.dbf")

write.dbf(mod1_T2004_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s6 <- splits_s6$trainset
mod1_T2004_90_s6 <- splits_s6$testset

write.dbf(mod1_T2004_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s6.dbf")

write.dbf(mod1_T2004_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s7 <- splits_s7$trainset
mod1_T2004_90_s7 <- splits_s7$testset

write.dbf(mod1_T2004_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s7.dbf")

write.dbf(mod1_T2004_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s8 <- splits_s8$trainset
mod1_T2004_90_s8 <- splits_s8$testset

write.dbf(mod1_T2004_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s8.dbf")

write.dbf(mod1_T2004_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s9 <- splits_s9$trainset
mod1_T2004_90_s9 <- splits_s9$testset

write.dbf(mod1_T2004_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s9.dbf")

write.dbf(mod1_T2004_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s10 <- splits_s10$trainset
mod1_T2004_90_s10 <- splits_s10$testset

write.dbf(mod1_T2004_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s10.dbf")

write.dbf(mod1_T2004_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2004_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2004.csv", header=T) 
names(F_T2004_mod2p)
F_T2004_mod2p <- rename(F_T2004_mod2p, c(date="Date"))




# sort 
F_T2004_mod2p <- F_T2004_mod2p[order(F_T2004_mod2p$guid,F_T2004_mod2p$Date),]
T2004_weight <- T2004_weight[order(T2004_weight$guid,T2004_weight$Date),]

# merge two dataframes by ID
F_T2004_mod2 <- merge(F_T2004_mod2p,T2004_weight,by=c("guid","Date"))
names(F_T2004_mod2)




#transfer all to Z-scores
F_T2004_mod2$zelev.x<-(F_T2004_mod2$elev.x-mean(F_T2004_All$elev.x))/sd(F_T2004_All$elev.x) 
F_T2004_mod2$zpopden<-(F_T2004_mod2$pop_sqkm-mean(F_T2004_All$pop_sqkm))/sd(F_T2004_All$pop_sqkm)
F_T2004_mod2$zospace<-(F_T2004_mod2$p_open-mean(F_T2004_All$p_open))/sd(F_T2004_All$p_open)
F_T2004_mod2$ztden<- (F_T2004_mod2$tden-mean(F_T2004_All$tden))/sd(F_T2004_All$tden)
F_T2004_mod2$zforest<-(F_T2004_mod2$per_fore_1-mean(F_T2004_mod2$per_fore_1))/sd(F_T2004_mod2$per_fore_1)
F_T2004_mod2$zpsource<-(F_T2004_mod2$point_pm-mean(F_T2004_All$point_pm))/sd(F_T2004_All$point_pm)
F_T2004_mod2$zasource<- (F_T2004_mod2$area_pm -mean(F_T2004_All$area_pm))/sd(F_T2004_All$area_pm)
F_T2004_mod2$zvisib <- (F_T2004_mod2$visib_F-mean(F_T2004_All$visib_F))/sd(F_T2004_All$visib_F)
F_T2004_mod2$zwdsp<-(F_T2004_mod2$wdsp_F-mean(F_T2004_All$wdsp_F))/sd(F_T2004_All$wdsp_F)
F_T2004_mod2$zhumid<-(F_T2004_mod2$ah_gm3_F-mean(F_T2004_All$ah_gm3_F))/sd(F_T2004_All$ah_gm3_F)

F_T2004_mod2$zid<-1 #create id variable




F_T2004_mod2$zelev.x <-F_T2004_mod2$zelev.x*-1
F_T2004_mod2$zospace <- F_T2004_mod2$zospace*-1
F_T2004_mod2$zforest <- F_T2004_mod2$zforest*-1
F_T2004_mod2$zvisib <- F_T2004_mod2$zvisib*-1
F_T2004_mod2$zwdsp <- F_T2004_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2004_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2004_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2004_all_st_table_mod2 <- data.frame(F_T2004_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2004_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2004.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2005###########
######2005###########
######2005###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2005_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2005.csv", header=T)
summary(T2005_weight)
T2005_weight<-na.omit(T2005_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2005_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2005_weight$prob <- predict(w1,type = c("response"))

T2005_weight$wt <- 1/T2005_weight$prob

T2005_weight$normwt <- T2005_weight$wt/mean(T2005_weight$wt)
T2005_weight <- rename(T2005_weight, c(date="Date"))
summary(T2005_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2005_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2005.csv", header=T) 
names(F_T2005_Allp)


# sort 
F_T2005_Allp <- F_T2005_Allp[order(F_T2005_Allp$guid,F_T2005_Allp$Date),]
T2005_weight <- T2005_weight[order(T2005_weight$guid,T2005_weight$Date),]

# merge two dataframes by ID
F_T2005_All <- merge(F_T2005_Allp,T2005_weight,by=c("guid","Date"))
names(F_T2005_All)



#transfer all to Z-scores
F_T2005_All$zelev.x<-(F_T2005_All$elev.x-mean(F_T2005_All$elev.x))/sd(F_T2005_All$elev.x) 
F_T2005_All$zpopden<-(F_T2005_All$pop_sqkm-mean(F_T2005_All$pop_sqkm))/sd(F_T2005_All$pop_sqkm)
F_T2005_All$zospace<-(F_T2005_All$p_open-mean(F_T2005_All$p_open))/sd(F_T2005_All$p_open)
F_T2005_All$ztden<- (F_T2005_All$tden-mean(F_T2005_All$tden))/sd(F_T2005_All$tden)
F_T2005_All$zforest<-(F_T2005_All$per_fore_1-mean(F_T2005_All$per_fore_1))/sd(F_T2005_All$per_fore_1)
F_T2005_All$zpsource<-(F_T2005_All$point_pm-mean(F_T2005_All$point_pm))/sd(F_T2005_All$point_pm)
F_T2005_All$zasource<- (F_T2005_All$area_pm-mean(F_T2005_All$area_pm))/sd(F_T2005_All$area_pm)
F_T2005_All$zvisib<- (F_T2005_All$visib_F-mean(F_T2005_All$visib_F))/sd(F_T2005_All$visib_F)
F_T2005_All$zwdsp<-(F_T2005_All$wdsp_F-mean(F_T2005_All$wdsp_F))/sd(F_T2005_All$wdsp_F)
F_T2005_All$zhumid<-(F_T2005_All$ah_gm3_F-mean(F_T2005_All$ah_gm3_F))/sd(F_T2005_All$ah_gm3_F)

F_T2005_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2005_All$zelev.x <-F_T2005_All$zelev.x*-1
F_T2005_All$zospace <- F_T2005_All$zospace*-1
F_T2005_All$zforest<- F_T2005_All$zforest*-1
F_T2005_All$zvisib <- F_T2005_All$zvisib*-1
F_T2005_All$zwdsp <- F_T2005_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2005_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2005_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2005_all_st_table <- data.frame(F_T2005_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2005_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2005.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s1 <- splits_s1$trainset
mod1_T2005_90_s1 <- splits_s1$testset

write.dbf(mod1_T2005_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s1.dbf")

write.dbf(mod1_T2005_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s2 <- splits_s2$trainset
mod1_T2005_90_s2 <- splits_s2$testset

write.dbf(mod1_T2005_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s2.dbf")

write.dbf(mod1_T2005_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s3 <- splits_s3$trainset
mod1_T2005_90_s3 <- splits_s3$testset

write.dbf(mod1_T2005_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s3.dbf")

write.dbf(mod1_T2005_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s4 <- splits_s4$trainset
mod1_T2005_90_s4 <- splits_s4$testset

write.dbf(mod1_T2005_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s4.dbf")

write.dbf(mod1_T2005_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s5 <- splits_s5$trainset
mod1_T2005_90_s5 <- splits_s5$testset

write.dbf(mod1_T2005_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s5.dbf")

write.dbf(mod1_T2005_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s6 <- splits_s6$trainset
mod1_T2005_90_s6 <- splits_s6$testset

write.dbf(mod1_T2005_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s6.dbf")

write.dbf(mod1_T2005_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s7 <- splits_s7$trainset
mod1_T2005_90_s7 <- splits_s7$testset

write.dbf(mod1_T2005_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s7.dbf")

write.dbf(mod1_T2005_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s8 <- splits_s8$trainset
mod1_T2005_90_s8 <- splits_s8$testset

write.dbf(mod1_T2005_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s8.dbf")

write.dbf(mod1_T2005_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s9 <- splits_s9$trainset
mod1_T2005_90_s9 <- splits_s9$testset

write.dbf(mod1_T2005_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s9.dbf")

write.dbf(mod1_T2005_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s10 <- splits_s10$trainset
mod1_T2005_90_s10 <- splits_s10$testset

write.dbf(mod1_T2005_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s10.dbf")

write.dbf(mod1_T2005_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2005_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2005.csv", header=T) 
names(F_T2005_mod2p)
F_T2005_mod2p <- rename(F_T2005_mod2p, c(date="Date"))




# sort 
F_T2005_mod2p <- F_T2005_mod2p[order(F_T2005_mod2p$guid,F_T2005_mod2p$Date),]
T2005_weight <- T2005_weight[order(T2005_weight$guid,T2005_weight$Date),]

# merge two dataframes by ID
F_T2005_mod2 <- merge(F_T2005_mod2p,T2005_weight,by=c("guid","Date"))
names(F_T2005_mod2)




#transfer all to Z-scores
F_T2005_mod2$zelev.x<-(F_T2005_mod2$elev.x-mean(F_T2005_All$elev.x))/sd(F_T2005_All$elev.x) 
F_T2005_mod2$zpopden<-(F_T2005_mod2$pop_sqkm-mean(F_T2005_All$pop_sqkm))/sd(F_T2005_All$pop_sqkm)
F_T2005_mod2$zospace<-(F_T2005_mod2$p_open-mean(F_T2005_All$p_open))/sd(F_T2005_All$p_open)
F_T2005_mod2$ztden<- (F_T2005_mod2$tden-mean(F_T2005_All$tden))/sd(F_T2005_All$tden)
F_T2005_mod2$zforest<-(F_T2005_mod2$per_fore_1-mean(F_T2005_mod2$per_fore_1))/sd(F_T2005_mod2$per_fore_1)
F_T2005_mod2$zpsource<-(F_T2005_mod2$point_pm-mean(F_T2005_All$point_pm))/sd(F_T2005_All$point_pm)
F_T2005_mod2$zasource<- (F_T2005_mod2$area_pm -mean(F_T2005_All$area_pm))/sd(F_T2005_All$area_pm)
F_T2005_mod2$zvisib <- (F_T2005_mod2$visib_F-mean(F_T2005_All$visib_F))/sd(F_T2005_All$visib_F)
F_T2005_mod2$zwdsp<-(F_T2005_mod2$wdsp_F-mean(F_T2005_All$wdsp_F))/sd(F_T2005_All$wdsp_F)
F_T2005_mod2$zhumid<-(F_T2005_mod2$ah_gm3_F-mean(F_T2005_All$ah_gm3_F))/sd(F_T2005_All$ah_gm3_F)

F_T2005_mod2$zid<-1 #create id variable




F_T2005_mod2$zelev.x <-F_T2005_mod2$zelev.x*-1
F_T2005_mod2$zospace <- F_T2005_mod2$zospace*-1
F_T2005_mod2$zforest <- F_T2005_mod2$zforest*-1
F_T2005_mod2$zvisib <- F_T2005_mod2$zvisib*-1
F_T2005_mod2$zwdsp <- F_T2005_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2005_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2005_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2005_all_st_table_mod2 <- data.frame(F_T2005_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2005_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2005.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2006###########
######2006###########
######2006###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2006_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2006.csv", header=T)
summary(T2006_weight)
T2006_weight<-na.omit(T2006_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2006_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2006_weight$prob <- predict(w1,type = c("response"))

T2006_weight$wt <- 1/T2006_weight$prob

T2006_weight$normwt <- T2006_weight$wt/mean(T2006_weight$wt)
T2006_weight <- rename(T2006_weight, c(date="Date"))
summary(T2006_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2006_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2006.csv", header=T) 
names(F_T2006_Allp)


# sort 
F_T2006_Allp <- F_T2006_Allp[order(F_T2006_Allp$guid,F_T2006_Allp$Date),]
T2006_weight <- T2006_weight[order(T2006_weight$guid,T2006_weight$Date),]

# merge two dataframes by ID
F_T2006_All <- merge(F_T2006_Allp,T2006_weight,by=c("guid","Date"))
names(F_T2006_All)



#transfer all to Z-scores
F_T2006_All$zelev.x<-(F_T2006_All$elev.x-mean(F_T2006_All$elev.x))/sd(F_T2006_All$elev.x) 
F_T2006_All$zpopden<-(F_T2006_All$pop_sqkm-mean(F_T2006_All$pop_sqkm))/sd(F_T2006_All$pop_sqkm)
F_T2006_All$zospace<-(F_T2006_All$p_open-mean(F_T2006_All$p_open))/sd(F_T2006_All$p_open)
F_T2006_All$ztden<- (F_T2006_All$tden-mean(F_T2006_All$tden))/sd(F_T2006_All$tden)
F_T2006_All$zforest<-(F_T2006_All$per_fore_1-mean(F_T2006_All$per_fore_1))/sd(F_T2006_All$per_fore_1)
F_T2006_All$zpsource<-(F_T2006_All$point_pm-mean(F_T2006_All$point_pm))/sd(F_T2006_All$point_pm)
F_T2006_All$zasource<- (F_T2006_All$area_pm-mean(F_T2006_All$area_pm))/sd(F_T2006_All$area_pm)
F_T2006_All$zvisib<- (F_T2006_All$visib_F-mean(F_T2006_All$visib_F))/sd(F_T2006_All$visib_F)
F_T2006_All$zwdsp<-(F_T2006_All$wdsp_F-mean(F_T2006_All$wdsp_F))/sd(F_T2006_All$wdsp_F)
F_T2006_All$zhumid<-(F_T2006_All$ah_gm3_F-mean(F_T2006_All$ah_gm3_F))/sd(F_T2006_All$ah_gm3_F)

F_T2006_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2006_All$zelev.x <-F_T2006_All$zelev.x*-1
F_T2006_All$zospace <- F_T2006_All$zospace*-1
F_T2006_All$zforest<- F_T2006_All$zforest*-1
F_T2006_All$zvisib <- F_T2006_All$zvisib*-1
F_T2006_All$zwdsp <- F_T2006_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2006_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2006_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2006_all_st_table <- data.frame(F_T2006_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2006_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2006.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s1 <- splits_s1$trainset
mod1_T2006_90_s1 <- splits_s1$testset

write.dbf(mod1_T2006_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s1.dbf")

write.dbf(mod1_T2006_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s2 <- splits_s2$trainset
mod1_T2006_90_s2 <- splits_s2$testset

write.dbf(mod1_T2006_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s2.dbf")

write.dbf(mod1_T2006_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s3 <- splits_s3$trainset
mod1_T2006_90_s3 <- splits_s3$testset

write.dbf(mod1_T2006_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s3.dbf")

write.dbf(mod1_T2006_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s4 <- splits_s4$trainset
mod1_T2006_90_s4 <- splits_s4$testset

write.dbf(mod1_T2006_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s4.dbf")

write.dbf(mod1_T2006_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s5 <- splits_s5$trainset
mod1_T2006_90_s5 <- splits_s5$testset

write.dbf(mod1_T2006_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s5.dbf")

write.dbf(mod1_T2006_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s6 <- splits_s6$trainset
mod1_T2006_90_s6 <- splits_s6$testset

write.dbf(mod1_T2006_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s6.dbf")

write.dbf(mod1_T2006_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s7 <- splits_s7$trainset
mod1_T2006_90_s7 <- splits_s7$testset

write.dbf(mod1_T2006_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s7.dbf")

write.dbf(mod1_T2006_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s8 <- splits_s8$trainset
mod1_T2006_90_s8 <- splits_s8$testset

write.dbf(mod1_T2006_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s8.dbf")

write.dbf(mod1_T2006_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s9 <- splits_s9$trainset
mod1_T2006_90_s9 <- splits_s9$testset

write.dbf(mod1_T2006_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s9.dbf")

write.dbf(mod1_T2006_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s10 <- splits_s10$trainset
mod1_T2006_90_s10 <- splits_s10$testset

write.dbf(mod1_T2006_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s10.dbf")

write.dbf(mod1_T2006_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2006_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2006.csv", header=T) 
names(F_T2006_mod2p)
F_T2006_mod2p <- rename(F_T2006_mod2p, c(date="Date"))




# sort 
F_T2006_mod2p <- F_T2006_mod2p[order(F_T2006_mod2p$guid,F_T2006_mod2p$Date),]
T2006_weight <- T2006_weight[order(T2006_weight$guid,T2006_weight$Date),]

# merge two dataframes by ID
F_T2006_mod2 <- merge(F_T2006_mod2p,T2006_weight,by=c("guid","Date"))
names(F_T2006_mod2)




#transfer all to Z-scores
F_T2006_mod2$zelev.x<-(F_T2006_mod2$elev.x-mean(F_T2006_All$elev.x))/sd(F_T2006_All$elev.x) 
F_T2006_mod2$zpopden<-(F_T2006_mod2$pop_sqkm-mean(F_T2006_All$pop_sqkm))/sd(F_T2006_All$pop_sqkm)
F_T2006_mod2$zospace<-(F_T2006_mod2$p_open-mean(F_T2006_All$p_open))/sd(F_T2006_All$p_open)
F_T2006_mod2$ztden<- (F_T2006_mod2$tden-mean(F_T2006_All$tden))/sd(F_T2006_All$tden)
F_T2006_mod2$zforest<-(F_T2006_mod2$per_fore_1-mean(F_T2006_mod2$per_fore_1))/sd(F_T2006_mod2$per_fore_1)
F_T2006_mod2$zpsource<-(F_T2006_mod2$point_pm-mean(F_T2006_All$point_pm))/sd(F_T2006_All$point_pm)
F_T2006_mod2$zasource<- (F_T2006_mod2$area_pm -mean(F_T2006_All$area_pm))/sd(F_T2006_All$area_pm)
F_T2006_mod2$zvisib <- (F_T2006_mod2$visib_F-mean(F_T2006_All$visib_F))/sd(F_T2006_All$visib_F)
F_T2006_mod2$zwdsp<-(F_T2006_mod2$wdsp_F-mean(F_T2006_All$wdsp_F))/sd(F_T2006_All$wdsp_F)
F_T2006_mod2$zhumid<-(F_T2006_mod2$ah_gm3_F-mean(F_T2006_All$ah_gm3_F))/sd(F_T2006_All$ah_gm3_F)

F_T2006_mod2$zid<-1 #create id variable




F_T2006_mod2$zelev.x <-F_T2006_mod2$zelev.x*-1
F_T2006_mod2$zospace <- F_T2006_mod2$zospace*-1
F_T2006_mod2$zforest <- F_T2006_mod2$zforest*-1
F_T2006_mod2$zvisib <- F_T2006_mod2$zvisib*-1
F_T2006_mod2$zwdsp <- F_T2006_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2006_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2006_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2006_all_st_table_mod2 <- data.frame(F_T2006_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2006_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2006.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2007###########
######2007###########
######2007###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2007_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2007.csv", header=T)
summary(T2007_weight)
T2007_weight<-na.omit(T2007_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2007_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2007_weight$prob <- predict(w1,type = c("response"))

T2007_weight$wt <- 1/T2007_weight$prob

T2007_weight$normwt <- T2007_weight$wt/mean(T2007_weight$wt)
T2007_weight <- rename(T2007_weight, c(date="Date"))
summary(T2007_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2007_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2007.csv", header=T) 
names(F_T2007_Allp)


# sort 
F_T2007_Allp <- F_T2007_Allp[order(F_T2007_Allp$guid,F_T2007_Allp$Date),]
T2007_weight <- T2007_weight[order(T2007_weight$guid,T2007_weight$Date),]

# merge two dataframes by ID
F_T2007_All <- merge(F_T2007_Allp,T2007_weight,by=c("guid","Date"))
names(F_T2007_All)



#transfer all to Z-scores
F_T2007_All$zelev.x<-(F_T2007_All$elev.x-mean(F_T2007_All$elev.x))/sd(F_T2007_All$elev.x) 
F_T2007_All$zpopden<-(F_T2007_All$pop_sqkm-mean(F_T2007_All$pop_sqkm))/sd(F_T2007_All$pop_sqkm)
F_T2007_All$zospace<-(F_T2007_All$p_open-mean(F_T2007_All$p_open))/sd(F_T2007_All$p_open)
F_T2007_All$ztden<- (F_T2007_All$tden-mean(F_T2007_All$tden))/sd(F_T2007_All$tden)
F_T2007_All$zforest<-(F_T2007_All$per_fore_1-mean(F_T2007_All$per_fore_1))/sd(F_T2007_All$per_fore_1)
F_T2007_All$zpsource<-(F_T2007_All$point_pm-mean(F_T2007_All$point_pm))/sd(F_T2007_All$point_pm)
F_T2007_All$zasource<- (F_T2007_All$area_pm-mean(F_T2007_All$area_pm))/sd(F_T2007_All$area_pm)
F_T2007_All$zvisib<- (F_T2007_All$visib_F-mean(F_T2007_All$visib_F))/sd(F_T2007_All$visib_F)
F_T2007_All$zwdsp<-(F_T2007_All$wdsp_F-mean(F_T2007_All$wdsp_F))/sd(F_T2007_All$wdsp_F)
F_T2007_All$zhumid<-(F_T2007_All$ah_gm3_F-mean(F_T2007_All$ah_gm3_F))/sd(F_T2007_All$ah_gm3_F)

F_T2007_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2007_All$zelev.x <-F_T2007_All$zelev.x*-1
F_T2007_All$zospace <- F_T2007_All$zospace*-1
F_T2007_All$zforest<- F_T2007_All$zforest*-1
F_T2007_All$zvisib <- F_T2007_All$zvisib*-1
F_T2007_All$zwdsp <- F_T2007_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2007_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2007_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2007_all_st_table <- data.frame(F_T2007_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2007_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2007.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s1 <- splits_s1$trainset
mod1_T2007_90_s1 <- splits_s1$testset

write.dbf(mod1_T2007_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s1.dbf")

write.dbf(mod1_T2007_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s2 <- splits_s2$trainset
mod1_T2007_90_s2 <- splits_s2$testset

write.dbf(mod1_T2007_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s2.dbf")

write.dbf(mod1_T2007_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s3 <- splits_s3$trainset
mod1_T2007_90_s3 <- splits_s3$testset

write.dbf(mod1_T2007_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s3.dbf")

write.dbf(mod1_T2007_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s4 <- splits_s4$trainset
mod1_T2007_90_s4 <- splits_s4$testset

write.dbf(mod1_T2007_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s4.dbf")

write.dbf(mod1_T2007_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s5 <- splits_s5$trainset
mod1_T2007_90_s5 <- splits_s5$testset

write.dbf(mod1_T2007_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s5.dbf")

write.dbf(mod1_T2007_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s6 <- splits_s6$trainset
mod1_T2007_90_s6 <- splits_s6$testset

write.dbf(mod1_T2007_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s6.dbf")

write.dbf(mod1_T2007_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s7 <- splits_s7$trainset
mod1_T2007_90_s7 <- splits_s7$testset

write.dbf(mod1_T2007_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s7.dbf")

write.dbf(mod1_T2007_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s8 <- splits_s8$trainset
mod1_T2007_90_s8 <- splits_s8$testset

write.dbf(mod1_T2007_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s8.dbf")

write.dbf(mod1_T2007_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s9 <- splits_s9$trainset
mod1_T2007_90_s9 <- splits_s9$testset

write.dbf(mod1_T2007_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s9.dbf")

write.dbf(mod1_T2007_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s10 <- splits_s10$trainset
mod1_T2007_90_s10 <- splits_s10$testset

write.dbf(mod1_T2007_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s10.dbf")

write.dbf(mod1_T2007_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2007_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2007.csv", header=T) 
names(F_T2007_mod2p)
F_T2007_mod2p <- rename(F_T2007_mod2p, c(date="Date"))




# sort 
F_T2007_mod2p <- F_T2007_mod2p[order(F_T2007_mod2p$guid,F_T2007_mod2p$Date),]
T2007_weight <- T2007_weight[order(T2007_weight$guid,T2007_weight$Date),]

# merge two dataframes by ID
F_T2007_mod2 <- merge(F_T2007_mod2p,T2007_weight,by=c("guid","Date"))
names(F_T2007_mod2)




#transfer all to Z-scores
F_T2007_mod2$zelev.x<-(F_T2007_mod2$elev.x-mean(F_T2007_All$elev.x))/sd(F_T2007_All$elev.x) 
F_T2007_mod2$zpopden<-(F_T2007_mod2$pop_sqkm-mean(F_T2007_All$pop_sqkm))/sd(F_T2007_All$pop_sqkm)
F_T2007_mod2$zospace<-(F_T2007_mod2$p_open-mean(F_T2007_All$p_open))/sd(F_T2007_All$p_open)
F_T2007_mod2$ztden<- (F_T2007_mod2$tden-mean(F_T2007_All$tden))/sd(F_T2007_All$tden)
F_T2007_mod2$zforest<-(F_T2007_mod2$per_fore_1-mean(F_T2007_mod2$per_fore_1))/sd(F_T2007_mod2$per_fore_1)
F_T2007_mod2$zpsource<-(F_T2007_mod2$point_pm-mean(F_T2007_All$point_pm))/sd(F_T2007_All$point_pm)
F_T2007_mod2$zasource<- (F_T2007_mod2$area_pm -mean(F_T2007_All$area_pm))/sd(F_T2007_All$area_pm)
F_T2007_mod2$zvisib <- (F_T2007_mod2$visib_F-mean(F_T2007_All$visib_F))/sd(F_T2007_All$visib_F)
F_T2007_mod2$zwdsp<-(F_T2007_mod2$wdsp_F-mean(F_T2007_All$wdsp_F))/sd(F_T2007_All$wdsp_F)
F_T2007_mod2$zhumid<-(F_T2007_mod2$ah_gm3_F-mean(F_T2007_All$ah_gm3_F))/sd(F_T2007_All$ah_gm3_F)

F_T2007_mod2$zid<-1 #create id variable




F_T2007_mod2$zelev.x <-F_T2007_mod2$zelev.x*-1
F_T2007_mod2$zospace <- F_T2007_mod2$zospace*-1
F_T2007_mod2$zforest <- F_T2007_mod2$zforest*-1
F_T2007_mod2$zvisib <- F_T2007_mod2$zvisib*-1
F_T2007_mod2$zwdsp <- F_T2007_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2007_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2007_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2007_all_st_table_mod2 <- data.frame(F_T2007_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2007_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2007.dbf") 



library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)




######2008###########
######2008###########
######2008###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2008_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2008.csv", header=T)
summary(T2008_weight)
T2008_weight<-na.omit(T2008_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2008_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2008_weight$prob <- predict(w1,type = c("response"))

T2008_weight$wt <- 1/T2008_weight$prob

T2008_weight$normwt <- T2008_weight$wt/mean(T2008_weight$wt)
T2008_weight <- rename(T2008_weight, c(date="Date"))
summary(T2008_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2008_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2008.csv", header=T) 
summary(F_T2008_Allp)


# sort 
F_T2008_Allp <- F_T2008_Allp[order(F_T2008_Allp$guid,F_T2008_Allp$Date),]
T2008_weight <- T2008_weight[order(T2008_weight$guid,T2008_weight$Date),]

# merge two dataframes by ID
F_T2008_All <- merge(F_T2008_Allp,T2008_weight,by=c("guid","Date"))
names(F_T2008_All)



#transfer all to Z-scores
F_T2008_All$zelev.x<-(F_T2008_All$elev.x-mean(F_T2008_All$elev.x))/sd(F_T2008_All$elev.x) 
F_T2008_All$zpopden<-(F_T2008_All$pop_sqkm-mean(F_T2008_All$pop_sqkm))/sd(F_T2008_All$pop_sqkm)
F_T2008_All$zospace<-(F_T2008_All$p_open-mean(F_T2008_All$p_open))/sd(F_T2008_All$p_open)
F_T2008_All$ztden<- (F_T2008_All$tden-mean(F_T2008_All$tden))/sd(F_T2008_All$tden)
F_T2008_All$zforest<-(F_T2008_All$per_fore_1-mean(F_T2008_All$per_fore_1))/sd(F_T2008_All$per_fore_1)
F_T2008_All$zpsource<-(F_T2008_All$point_pm-mean(F_T2008_All$point_pm))/sd(F_T2008_All$point_pm)
F_T2008_All$zasource<- (F_T2008_All$area_pm-mean(F_T2008_All$area_pm))/sd(F_T2008_All$area_pm)
F_T2008_All$zvisib<- (F_T2008_All$visib_F-mean(F_T2008_All$visib_F))/sd(F_T2008_All$visib_F)
F_T2008_All$zwdsp<-(F_T2008_All$wdsp_F-mean(F_T2008_All$wdsp_F))/sd(F_T2008_All$wdsp_F)
F_T2008_All$zhumid<-(F_T2008_All$ah_gm3_F-mean(F_T2008_All$ah_gm3_F))/sd(F_T2008_All$ah_gm3_F)

F_T2008_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2008_All$zelev.x <-F_T2008_All$zelev.x*-1
F_T2008_All$zospace <- F_T2008_All$zospace*-1
F_T2008_All$zforest<- F_T2008_All$zforest*-1
F_T2008_All$zvisib <- F_T2008_All$zvisib*-1
F_T2008_All$zwdsp <- F_T2008_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2008_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2008_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2008_all_st_table <- data.frame(F_T2008_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2008_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2008.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s1 <- splits_s1$trainset
mod1_T2008_90_s1 <- splits_s1$testset

write.dbf(mod1_T2008_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s1.dbf")

write.dbf(mod1_T2008_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s2 <- splits_s2$trainset
mod1_T2008_90_s2 <- splits_s2$testset

write.dbf(mod1_T2008_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s2.dbf")

write.dbf(mod1_T2008_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s3 <- splits_s3$trainset
mod1_T2008_90_s3 <- splits_s3$testset

write.dbf(mod1_T2008_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s3.dbf")

write.dbf(mod1_T2008_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s4 <- splits_s4$trainset
mod1_T2008_90_s4 <- splits_s4$testset

write.dbf(mod1_T2008_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s4.dbf")

write.dbf(mod1_T2008_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s5 <- splits_s5$trainset
mod1_T2008_90_s5 <- splits_s5$testset

write.dbf(mod1_T2008_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s5.dbf")

write.dbf(mod1_T2008_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s6 <- splits_s6$trainset
mod1_T2008_90_s6 <- splits_s6$testset

write.dbf(mod1_T2008_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s6.dbf")

write.dbf(mod1_T2008_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s7 <- splits_s7$trainset
mod1_T2008_90_s7 <- splits_s7$testset

write.dbf(mod1_T2008_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s7.dbf")

write.dbf(mod1_T2008_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s8 <- splits_s8$trainset
mod1_T2008_90_s8 <- splits_s8$testset

write.dbf(mod1_T2008_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s8.dbf")

write.dbf(mod1_T2008_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s9 <- splits_s9$trainset
mod1_T2008_90_s9 <- splits_s9$testset

write.dbf(mod1_T2008_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s9.dbf")

write.dbf(mod1_T2008_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s10 <- splits_s10$trainset
mod1_T2008_90_s10 <- splits_s10$testset

write.dbf(mod1_T2008_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s10.dbf")

write.dbf(mod1_T2008_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2008_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2008.csv", header=T) 
names(F_T2008_mod2p)
F_T2008_mod2p <- rename(F_T2008_mod2p, c(date="Date"))




# sort 
F_T2008_mod2p <- F_T2008_mod2p[order(F_T2008_mod2p$guid,F_T2008_mod2p$Date),]
T2008_weight <- T2008_weight[order(T2008_weight$guid,T2008_weight$Date),]

# merge two dataframes by ID
F_T2008_mod2 <- merge(F_T2008_mod2p,T2008_weight,by=c("guid","Date"))
names(F_T2008_mod2)




#transfer all to Z-scores
F_T2008_mod2$zelev.x<-(F_T2008_mod2$elev.x-mean(F_T2008_All$elev.x))/sd(F_T2008_All$elev.x) 
F_T2008_mod2$zpopden<-(F_T2008_mod2$pop_sqkm-mean(F_T2008_All$pop_sqkm))/sd(F_T2008_All$pop_sqkm)
F_T2008_mod2$zospace<-(F_T2008_mod2$p_open-mean(F_T2008_All$p_open))/sd(F_T2008_All$p_open)
F_T2008_mod2$ztden<- (F_T2008_mod2$tden-mean(F_T2008_All$tden))/sd(F_T2008_All$tden)
F_T2008_mod2$zforest<-(F_T2008_mod2$per_fore_1-mean(F_T2008_mod2$per_fore_1))/sd(F_T2008_mod2$per_fore_1)
F_T2008_mod2$zpsource<-(F_T2008_mod2$point_pm-mean(F_T2008_All$point_pm))/sd(F_T2008_All$point_pm)
F_T2008_mod2$zasource<- (F_T2008_mod2$area_pm -mean(F_T2008_All$area_pm))/sd(F_T2008_All$area_pm)
F_T2008_mod2$zvisib <- (F_T2008_mod2$visib_F-mean(F_T2008_All$visib_F))/sd(F_T2008_All$visib_F)
F_T2008_mod2$zwdsp<-(F_T2008_mod2$wdsp_F-mean(F_T2008_All$wdsp_F))/sd(F_T2008_All$wdsp_F)
F_T2008_mod2$zhumid<-(F_T2008_mod2$ah_gm3_F-mean(F_T2008_All$ah_gm3_F))/sd(F_T2008_All$ah_gm3_F)

F_T2008_mod2$zid<-1 #create id variable




F_T2008_mod2$zelev.x <-F_T2008_mod2$zelev.x*-1
F_T2008_mod2$zospace <- F_T2008_mod2$zospace*-1
F_T2008_mod2$zforest <- F_T2008_mod2$zforest*-1
F_T2008_mod2$zvisib <- F_T2008_mod2$zvisib*-1
F_T2008_mod2$zwdsp <- F_T2008_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2008_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2008_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2008_all_st_table_mod2 <- data.frame(F_T2008_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2008_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2008.dbf") 
















######2009###########
######2009###########
######2009###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2009_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2009.csv", header=T)
summary(T2009_weight)
T2009_weight<-na.omit(T2009_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2009_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2009_weight$prob <- predict(w1,type = c("response"))

T2009_weight$wt <- 1/T2009_weight$prob

T2009_weight$normwt <- T2009_weight$wt/mean(T2009_weight$wt)
T2009_weight <- rename(T2009_weight, c(date="Date"))
summary(T2009_weight$normwt)


T2009_weight$Date <- as.Date(T2009_weight$Date, format = "%d%b%Y")
tail(T2009_weight)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2009_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2009.csv", header=T) 


summary(F_T2009_Allp)


F_T2009_Allp$Date <- as.Date(F_T2009_Allp$date, format = "%m/%d/%y")
head(F_T2009_Allp)


# sort 
F_T2009_Allp <- F_T2009_Allp[order(F_T2009_Allp$guid,F_T2009_Allp$Date),]
T2009_weight <- T2009_weight[order(T2009_weight$guid,T2009_weight$Date),]

# merge two dataframes by ID
F_T2009_All <- merge(F_T2009_Allp,T2009_weight,by=c("guid","Date"))
names(F_T2009_All)

tden<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/tden.dbf") 
names(tden)
tden$Long_AOD <- NULL 
tden$Lat_AOD<- NULL 
 
forest<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/forest.dbf") 
names(forest)
DELLIST <- names(forest) %in% c("guid", "per_fore_1")
forest <- forest[DELLIST]





#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2009_All<-F_T2009_All[order(F_T2009_All$guid),] 
#merge 
F_T2009_All<-merge(F_T2009_All,tden,by="guid")
#merge 
F_T2009_All<-merge(F_T2009_All,forest,by="guid")




#transfer all to Z-scores
F_T2009_All$zelev.x<-(F_T2009_All$elev.x-mean(F_T2009_All$elev.x))/sd(F_T2009_All$elev.x) 
F_T2009_All$zpopden<-(F_T2009_All$pop_sqkm-mean(F_T2009_All$pop_sqkm))/sd(F_T2009_All$pop_sqkm)
F_T2009_All$zospace<-(F_T2009_All$p_open-mean(F_T2009_All$p_open))/sd(F_T2009_All$p_open)
F_T2009_All$ztden<- (F_T2009_All$tden-mean(F_T2009_All$tden))/sd(F_T2009_All$tden)
F_T2009_All$zforest<-(F_T2009_All$per_fore_1-mean(F_T2009_All$per_fore_1))/sd(F_T2009_All$per_fore_1)
F_T2009_All$zpsource<-(F_T2009_All$point_pm-mean(F_T2009_All$point_pm))/sd(F_T2009_All$point_pm)
F_T2009_All$zasource<- (F_T2009_All$area_pm-mean(F_T2009_All$area_pm))/sd(F_T2009_All$area_pm)
F_T2009_All$zvisib<- (F_T2009_All$visib_F-mean(F_T2009_All$visib_F))/sd(F_T2009_All$visib_F)
F_T2009_All$zwdsp<-(F_T2009_All$wdsp_F-mean(F_T2009_All$wdsp_F))/sd(F_T2009_All$wdsp_F)
F_T2009_All$zhumid<-(F_T2009_All$ah_gm3_F-mean(F_T2009_All$ah_gm3_F))/sd(F_T2009_All$ah_gm3_F)

F_T2009_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2009_All$zelev.x <-F_T2009_All$zelev.x*-1
F_T2009_All$zospace <- F_T2009_All$zospace*-1
F_T2009_All$zforest<- F_T2009_All$zforest*-1
F_T2009_All$zvisib <- F_T2009_All$zvisib*-1
F_T2009_All$zwdsp <- F_T2009_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2009_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2009_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2009_all_st_table <- data.frame(F_T2009_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2009_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2009.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s1 <- splits_s1$trainset
mod1_T2009_90_s1 <- splits_s1$testset

write.dbf(mod1_T2009_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s1.dbf")

write.dbf(mod1_T2009_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s2 <- splits_s2$trainset
mod1_T2009_90_s2 <- splits_s2$testset

write.dbf(mod1_T2009_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s2.dbf")

write.dbf(mod1_T2009_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s3 <- splits_s3$trainset
mod1_T2009_90_s3 <- splits_s3$testset

write.dbf(mod1_T2009_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s3.dbf")

write.dbf(mod1_T2009_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s4 <- splits_s4$trainset
mod1_T2009_90_s4 <- splits_s4$testset

write.dbf(mod1_T2009_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s4.dbf")

write.dbf(mod1_T2009_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s5 <- splits_s5$trainset
mod1_T2009_90_s5 <- splits_s5$testset

write.dbf(mod1_T2009_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s5.dbf")

write.dbf(mod1_T2009_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s6 <- splits_s6$trainset
mod1_T2009_90_s6 <- splits_s6$testset

write.dbf(mod1_T2009_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s6.dbf")

write.dbf(mod1_T2009_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s7 <- splits_s7$trainset
mod1_T2009_90_s7 <- splits_s7$testset

write.dbf(mod1_T2009_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s7.dbf")

write.dbf(mod1_T2009_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s8 <- splits_s8$trainset
mod1_T2009_90_s8 <- splits_s8$testset

write.dbf(mod1_T2009_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s8.dbf")

write.dbf(mod1_T2009_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s9 <- splits_s9$trainset
mod1_T2009_90_s9 <- splits_s9$testset

write.dbf(mod1_T2009_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s9.dbf")

write.dbf(mod1_T2009_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2009_all_st_table)

mod1_T2009_10_s10 <- splits_s10$trainset
mod1_T2009_90_s10 <- splits_s10$testset

write.dbf(mod1_T2009_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_10_s10.dbf")

write.dbf(mod1_T2009_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2009_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2009_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2009.csv", header=T) 
names(F_T2009_mod2p)

F_T2009_mod2p$Date <- as.Date(F_T2009_mod2p$date, format = "%d%b%Y")
head(F_T2009_mod2p)



# sort 
F_T2009_mod2p <- F_T2009_mod2p[order(F_T2009_mod2p$guid,F_T2009_mod2p$Date),]
T2009_weight <- T2009_weight[order(T2009_weight$guid,T2009_weight$Date),]

# merge two dataframes by ID
F_T2009_mod2 <- merge(F_T2009_mod2p,T2009_weight,by=c("guid","Date"))
names(F_T2009_mod2)







#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2009_mod2<-F_T2009_mod2[order(F_T2009_mod2$guid),] 
#merge 
F_T2009_mod2<-merge(F_T2009_mod2,tden,by="guid")
#merge 
F_T2009_mod2<-merge(F_T2009_mod2,forest,by="guid")








#transfer all to Z-scores
F_T2009_mod2$zelev.x<-(F_T2009_mod2$elev.x-mean(F_T2009_All$elev.x))/sd(F_T2009_All$elev.x) 
F_T2009_mod2$zpopden<-(F_T2009_mod2$pop_sqkm-mean(F_T2009_All$pop_sqkm))/sd(F_T2009_All$pop_sqkm)
F_T2009_mod2$zospace<-(F_T2009_mod2$p_open-mean(F_T2009_All$p_open))/sd(F_T2009_All$p_open)
F_T2009_mod2$ztden<- (F_T2009_mod2$tden-mean(F_T2009_All$tden))/sd(F_T2009_All$tden)
F_T2009_mod2$zforest<-(F_T2009_mod2$per_fore_1-mean(F_T2009_mod2$per_fore_1))/sd(F_T2009_mod2$per_fore_1)
F_T2009_mod2$zpsource<-(F_T2009_mod2$point_pm-mean(F_T2009_All$point_pm))/sd(F_T2009_All$point_pm)
F_T2009_mod2$zasource<- (F_T2009_mod2$area_pm -mean(F_T2009_All$area_pm))/sd(F_T2009_All$area_pm)
F_T2009_mod2$zvisib <- (F_T2009_mod2$visib_F-mean(F_T2009_All$visib_F))/sd(F_T2009_All$visib_F)
F_T2009_mod2$zwdsp<-(F_T2009_mod2$wdsp_F-mean(F_T2009_All$wdsp_F))/sd(F_T2009_All$wdsp_F)
F_T2009_mod2$zhumid<-(F_T2009_mod2$ah_gm3_F-mean(F_T2009_All$ah_gm3_F))/sd(F_T2009_All$ah_gm3_F)

F_T2009_mod2$zid<-1 #create id variable




F_T2009_mod2$zelev.x <-F_T2009_mod2$zelev.x*-1
F_T2009_mod2$zospace <- F_T2009_mod2$zospace*-1
F_T2009_mod2$zforest <- F_T2009_mod2$zforest*-1
F_T2009_mod2$zvisib <- F_T2009_mod2$zvisib*-1
F_T2009_mod2$zwdsp <- F_T2009_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2009_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2009_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2009_all_st_table_mod2 <- data.frame(F_T2009_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2009_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2009.dbf") 



















######2010###########
######2010###########
######2010###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2010_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2010.csv", header=T)
summary(T2010_weight)
T2010_weight<-na.omit(T2010_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2010_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2010_weight$prob <- predict(w1,type = c("response"))

T2010_weight$wt <- 1/T2010_weight$prob

T2010_weight$normwt <- T2010_weight$wt/mean(T2010_weight$wt)
T2010_weight <- rename(T2010_weight, c(date="Date"))
summary(T2010_weight$normwt)



T2010_weight$Date <- as.Date(T2010_weight$Date, format = "%d%b%Y")
tail(T2010_weight)
#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2010_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2010.csv", header=T) 

F_T2010_Allp<-na.omit(F_T2010_Allp)

F_T2010_Allp$Date <- as.Date(F_T2010_Allp$date, format = "%m/%d/%y")
head(F_T2010_Allp)


# sort 
F_T2010_Allp <- F_T2010_Allp[order(F_T2010_Allp$guid,F_T2010_Allp$Date),]
T2010_weight <- T2010_weight[order(T2010_weight$guid,T2010_weight$Date),]

# merge two dataframes by ID
F_T2010_All <- merge(F_T2010_Allp,T2010_weight,by=c("guid","Date"))
names(F_T2010_All)

tden<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/tden.dbf") 
names(tden)
tden$Long_AOD <- NULL 
tden$Lat_AOD<- NULL 

forest<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/forest.dbf") 
names(forest)
DELLIST <- names(forest) %in% c("guid", "per_fore_1")
forest <- forest[DELLIST]





#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2010_All<-F_T2010_All[order(F_T2010_All$guid),] 
#merge 
F_T2010_All<-merge(F_T2010_All,tden,by="guid")
#merge 
F_T2010_All<-merge(F_T2010_All,forest,by="guid")




#transfer all to Z-scores
F_T2010_All$zelev.x<-(F_T2010_All$elev.x-mean(F_T2010_All$elev.x))/sd(F_T2010_All$elev.x) 
F_T2010_All$zpopden<-(F_T2010_All$pop_sqkm-mean(F_T2010_All$pop_sqkm))/sd(F_T2010_All$pop_sqkm)
F_T2010_All$zospace<-(F_T2010_All$p_open-mean(F_T2010_All$p_open))/sd(F_T2010_All$p_open)
F_T2010_All$ztden<- (F_T2010_All$tden-mean(F_T2010_All$tden))/sd(F_T2010_All$tden)
F_T2010_All$zforest<-(F_T2010_All$per_fore_1-mean(F_T2010_All$per_fore_1))/sd(F_T2010_All$per_fore_1)
F_T2010_All$zpsource<-(F_T2010_All$point_pm-mean(F_T2010_All$point_pm))/sd(F_T2010_All$point_pm)
F_T2010_All$zasource<- (F_T2010_All$area_pm-mean(F_T2010_All$area_pm))/sd(F_T2010_All$area_pm)
F_T2010_All$zvisib<- (F_T2010_All$visib_F-mean(F_T2010_All$visib_F))/sd(F_T2010_All$visib_F)
F_T2010_All$zwdsp<-(F_T2010_All$wdsp_F-mean(F_T2010_All$wdsp_F))/sd(F_T2010_All$wdsp_F)
F_T2010_All$zhumid<-(F_T2010_All$ah_gm3_F-mean(F_T2010_All$ah_gm3_F))/sd(F_T2010_All$ah_gm3_F)

F_T2010_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2010_All$zelev.x <-F_T2010_All$zelev.x*-1
F_T2010_All$zospace <- F_T2010_All$zospace*-1
F_T2010_All$zforest<- F_T2010_All$zforest*-1
F_T2010_All$zvisib <- F_T2010_All$zvisib*-1
F_T2010_All$zwdsp <- F_T2010_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2010_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2010_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2010_all_st_table <- data.frame(F_T2010_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2010_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2010.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s1 <- splits_s1$trainset
mod1_T2010_90_s1 <- splits_s1$testset

write.dbf(mod1_T2010_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s1.dbf")

write.dbf(mod1_T2010_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s2 <- splits_s2$trainset
mod1_T2010_90_s2 <- splits_s2$testset

write.dbf(mod1_T2010_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s2.dbf")

write.dbf(mod1_T2010_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s3 <- splits_s3$trainset
mod1_T2010_90_s3 <- splits_s3$testset

write.dbf(mod1_T2010_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s3.dbf")

write.dbf(mod1_T2010_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s4 <- splits_s4$trainset
mod1_T2010_90_s4 <- splits_s4$testset

write.dbf(mod1_T2010_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s4.dbf")

write.dbf(mod1_T2010_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s5 <- splits_s5$trainset
mod1_T2010_90_s5 <- splits_s5$testset

write.dbf(mod1_T2010_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s5.dbf")

write.dbf(mod1_T2010_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s6 <- splits_s6$trainset
mod1_T2010_90_s6 <- splits_s6$testset

write.dbf(mod1_T2010_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s6.dbf")

write.dbf(mod1_T2010_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s7 <- splits_s7$trainset
mod1_T2010_90_s7 <- splits_s7$testset

write.dbf(mod1_T2010_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s7.dbf")

write.dbf(mod1_T2010_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s8 <- splits_s8$trainset
mod1_T2010_90_s8 <- splits_s8$testset

write.dbf(mod1_T2010_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s8.dbf")

write.dbf(mod1_T2010_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s9 <- splits_s9$trainset
mod1_T2010_90_s9 <- splits_s9$testset

write.dbf(mod1_T2010_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s9.dbf")

write.dbf(mod1_T2010_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2010_all_st_table)

mod1_T2010_10_s10 <- splits_s10$trainset
mod1_T2010_90_s10 <- splits_s10$testset

write.dbf(mod1_T2010_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_10_s10.dbf")

write.dbf(mod1_T2010_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2010_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2010_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2010.csv", header=T) 
names(F_T2010_mod2p)

F_T2010_mod2p$Date <- as.Date(F_T2010_mod2p$date, format = "%d%b%Y")
head(F_T2010_mod2p)



# sort 
F_T2010_mod2p <- F_T2010_mod2p[order(F_T2010_mod2p$guid,F_T2010_mod2p$Date),]
T2010_weight <- T2010_weight[order(T2010_weight$guid,T2010_weight$Date),]

# merge two dataframes by ID
F_T2010_mod2 <- merge(F_T2010_mod2p,T2010_weight,by=c("guid","Date"))
names(F_T2010_mod2)







#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2010_mod2<-F_T2010_mod2[order(F_T2010_mod2$guid),] 
#merge 
F_T2010_mod2<-merge(F_T2010_mod2,tden,by="guid")
#merge 
F_T2010_mod2<-merge(F_T2010_mod2,forest,by="guid")








#transfer all to Z-scores
F_T2010_mod2$zelev.x<-(F_T2010_mod2$elev.x-mean(F_T2010_All$elev.x))/sd(F_T2010_All$elev.x) 
F_T2010_mod2$zpopden<-(F_T2010_mod2$pop_sqkm-mean(F_T2010_All$pop_sqkm))/sd(F_T2010_All$pop_sqkm)
F_T2010_mod2$zospace<-(F_T2010_mod2$p_open-mean(F_T2010_All$p_open))/sd(F_T2010_All$p_open)
F_T2010_mod2$ztden<- (F_T2010_mod2$tden-mean(F_T2010_All$tden))/sd(F_T2010_All$tden)
F_T2010_mod2$zforest<-(F_T2010_mod2$per_fore_1-mean(F_T2010_mod2$per_fore_1))/sd(F_T2010_mod2$per_fore_1)
F_T2010_mod2$zpsource<-(F_T2010_mod2$point_pm-mean(F_T2010_All$point_pm))/sd(F_T2010_All$point_pm)
F_T2010_mod2$zasource<- (F_T2010_mod2$area_pm -mean(F_T2010_All$area_pm))/sd(F_T2010_All$area_pm)
F_T2010_mod2$zvisib <- (F_T2010_mod2$visib_F-mean(F_T2010_All$visib_F))/sd(F_T2010_All$visib_F)
F_T2010_mod2$zwdsp<-(F_T2010_mod2$wdsp_F-mean(F_T2010_All$wdsp_F))/sd(F_T2010_All$wdsp_F)
F_T2010_mod2$zhumid<-(F_T2010_mod2$ah_gm3_F-mean(F_T2010_All$ah_gm3_F))/sd(F_T2010_All$ah_gm3_F)

F_T2010_mod2$zid<-1 #create id variable




F_T2010_mod2$zelev.x <-F_T2010_mod2$zelev.x*-1
F_T2010_mod2$zospace <- F_T2010_mod2$zospace*-1
F_T2010_mod2$zforest <- F_T2010_mod2$zforest*-1
F_T2010_mod2$zvisib <- F_T2010_mod2$zvisib*-1
F_T2010_mod2$zwdsp <- F_T2010_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2010_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2010_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2010_all_st_table_mod2 <- data.frame(F_T2010_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2010_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2010.dbf") 





######2011###########
######2011###########
######2011###########




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file


T2011_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2011.csv", header=T)
summary(T2011_weight)
T2011_weight<-na.omit(T2011_weight)


w1<- glm(obs ~ elev+Temp_F+as.factor(m),family=binomial,data=T2011_weight) #run modle
summary(w1)

#get probability prediction , note that its a binary logisitc and thus the type-repsonse option
T2011_weight$prob <- predict(w1,type = c("response"))

T2011_weight$wt <- 1/T2011_weight$prob

T2011_weight$normwt <- T2011_weight$wt/mean(T2011_weight$wt)
T2011_weight <- rename(T2011_weight, c(date="Date"))
summary(T2011_weight$normwt)
hist(T2011_weight$normwt)


T2011_weight$Date <- as.Date(T2011_weight$Date, format = "%d%b%Y")
tail(T2011_weight)
#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IMPORTS

#import mod1 DB (PM-AOD)
F_T2011_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN001_mod1/t2011.csv", header=T) 
summary(F_T2011_Allp)

F_T2011_Allp$Date <- as.Date(F_T2011_Allp$date, format = "%m/%d/%y")
head(F_T2011_Allp)


# sort 
F_T2011_Allp <- F_T2011_Allp[order(F_T2011_Allp$guid,F_T2011_Allp$Date),]
T2011_weight <- T2011_weight[order(T2011_weight$guid,T2011_weight$Date),]

# merge two dataframes by ID
F_T2011_All <- merge(F_T2011_Allp,T2011_weight,by=c("guid","Date"))
names(F_T2011_All)

tden<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/tden.dbf") 
names(tden)
tden$Long_AOD <- NULL 
tden$Lat_AOD<- NULL 

forest<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/2.Gather_data/FN004_LU_full_dataset/forest.dbf") 
names(forest)
DELLIST <- names(forest) %in% c("guid", "per_fore_1")
forest <- forest[DELLIST]





#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2011_All<-F_T2011_All[order(F_T2011_All$guid),] 
#merge 
F_T2011_All<-merge(F_T2011_All,tden,by="guid")
#merge 
F_T2011_All<-merge(F_T2011_All,forest,by="guid")
F_T2011_All<-na.omit(F_T2011_All)


summary(F_T2011_All)
F_T2011_All <- subset(F_T2011_All, F_T2011_All$wdsp_F  <= 20)




#transfer all to Z-scores
F_T2011_All$zelev.x<-(F_T2011_All$elev.x-mean(F_T2011_All$elev.x))/sd(F_T2011_All$elev.x) 
F_T2011_All$zpopden<-(F_T2011_All$pop_sqkm-mean(F_T2011_All$pop_sqkm))/sd(F_T2011_All$pop_sqkm)
F_T2011_All$zospace<-(F_T2011_All$p_open-mean(F_T2011_All$p_open))/sd(F_T2011_All$p_open)
F_T2011_All$ztden<- (F_T2011_All$tden-mean(F_T2011_All$tden))/sd(F_T2011_All$tden)
F_T2011_All$zforest<-(F_T2011_All$per_fore_1-mean(F_T2011_All$per_fore_1))/sd(F_T2011_All$per_fore_1)
F_T2011_All$zpsource<-(F_T2011_All$point_pm-mean(F_T2011_All$point_pm))/sd(F_T2011_All$point_pm)
F_T2011_All$zasource<- (F_T2011_All$area_pm-mean(F_T2011_All$area_pm))/sd(F_T2011_All$area_pm)
F_T2011_All$zvisib<- (F_T2011_All$visib_F-mean(F_T2011_All$visib_F))/sd(F_T2011_All$visib_F)
F_T2011_All$zwdsp<-(F_T2011_All$wdsp_F-mean(F_T2011_All$wdsp_F))/sd(F_T2011_All$wdsp_F)
F_T2011_All$zhumid<-(F_T2011_All$ah_gm3_F-mean(F_T2011_All$ah_gm3_F))/sd(F_T2011_All$ah_gm3_F)

F_T2011_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2011_All$zelev.x <-F_T2011_All$zelev.x*-1
F_T2011_All$zospace <- F_T2011_All$zospace*-1
F_T2011_All$zforest<- F_T2011_All$zforest*-1
F_T2011_All$zvisib <- F_T2011_All$zvisib*-1
F_T2011_All$zwdsp <- F_T2011_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2011_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2011_All)





#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2011_all_st_table <- data.frame(F_T2011_All,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)


write.dbf(F_2011_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2011.dbf") 





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#s1
splits_s1 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s1 <- splits_s1$trainset
mod1_T2011_90_s1 <- splits_s1$testset

write.dbf(mod1_T2011_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s1.dbf")

write.dbf(mod1_T2011_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s2 <- splits_s2$trainset
mod1_T2011_90_s2 <- splits_s2$testset

write.dbf(mod1_T2011_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s2.dbf")

write.dbf(mod1_T2011_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s3 <- splits_s3$trainset
mod1_T2011_90_s3 <- splits_s3$testset

write.dbf(mod1_T2011_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s3.dbf")

write.dbf(mod1_T2011_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s4 <- splits_s4$trainset
mod1_T2011_90_s4 <- splits_s4$testset

write.dbf(mod1_T2011_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s4.dbf")

write.dbf(mod1_T2011_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s5 <- splits_s5$trainset
mod1_T2011_90_s5 <- splits_s5$testset

write.dbf(mod1_T2011_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s5.dbf")

write.dbf(mod1_T2011_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s6 <- splits_s6$trainset
mod1_T2011_90_s6 <- splits_s6$testset

write.dbf(mod1_T2011_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s6.dbf")

write.dbf(mod1_T2011_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s7 <- splits_s7$trainset
mod1_T2011_90_s7 <- splits_s7$testset

write.dbf(mod1_T2011_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s7.dbf")

write.dbf(mod1_T2011_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s8 <- splits_s8$trainset
mod1_T2011_90_s8 <- splits_s8$testset

write.dbf(mod1_T2011_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s8.dbf")

write.dbf(mod1_T2011_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s9 <- splits_s9$trainset
mod1_T2011_90_s9 <- splits_s9$testset

write.dbf(mod1_T2011_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s9.dbf")

write.dbf(mod1_T2011_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2011_all_st_table)

mod1_T2011_10_s10 <- splits_s10$trainset
mod1_T2011_90_s10 <- splits_s10$testset

write.dbf(mod1_T2011_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_10_s10.dbf")

write.dbf(mod1_T2011_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2011_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2011_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/y2011.csv", header=T) 
names(F_T2011_mod2p)

F_T2011_mod2p$Date <- as.Date(F_T2011_mod2p$date, format = "%d%b%Y")
head(F_T2011_mod2p)



# sort 
F_T2011_mod2p <- F_T2011_mod2p[order(F_T2011_mod2p$guid,F_T2011_mod2p$Date),]
T2011_weight <- T2011_weight[order(T2011_weight$guid,T2011_weight$Date),]

# merge two dataframes by ID
F_T2011_mod2 <- merge(F_T2011_mod2p,T2011_weight,by=c("guid","Date"))
names(F_T2011_mod2)







#sort
tden<-tden[order(tden$guid),]
forest<-forest[order(tden$guid),]
F_T2011_mod2<-F_T2011_mod2[order(F_T2011_mod2$guid),] 
#merge 
F_T2011_mod2<-merge(F_T2011_mod2,tden,by="guid")
#merge 
F_T2011_mod2<-merge(F_T2011_mod2,forest,by="guid")








#transfer all to Z-scores
F_T2011_mod2$zelev.x<-(F_T2011_mod2$elev.x-mean(F_T2011_All$elev.x))/sd(F_T2011_All$elev.x) 
F_T2011_mod2$zpopden<-(F_T2011_mod2$pop_sqkm-mean(F_T2011_All$pop_sqkm))/sd(F_T2011_All$pop_sqkm)
F_T2011_mod2$zospace<-(F_T2011_mod2$p_open-mean(F_T2011_All$p_open))/sd(F_T2011_All$p_open)
F_T2011_mod2$ztden<- (F_T2011_mod2$tden-mean(F_T2011_All$tden))/sd(F_T2011_All$tden)
F_T2011_mod2$zforest<-(F_T2011_mod2$per_fore_1-mean(F_T2011_mod2$per_fore_1))/sd(F_T2011_mod2$per_fore_1)
F_T2011_mod2$zpsource<-(F_T2011_mod2$point_pm-mean(F_T2011_All$point_pm))/sd(F_T2011_All$point_pm)
F_T2011_mod2$zasource<- (F_T2011_mod2$area_pm -mean(F_T2011_All$area_pm))/sd(F_T2011_All$area_pm)
F_T2011_mod2$zvisib <- (F_T2011_mod2$visib_F-mean(F_T2011_All$visib_F))/sd(F_T2011_All$visib_F)
F_T2011_mod2$zwdsp<-(F_T2011_mod2$wdsp_F-mean(F_T2011_All$wdsp_F))/sd(F_T2011_All$wdsp_F)
F_T2011_mod2$zhumid<-(F_T2011_mod2$ah_gm3_F-mean(F_T2011_All$ah_gm3_F))/sd(F_T2011_All$ah_gm3_F)

F_T2011_mod2$zid<-1 #create id variable




F_T2011_mod2$zelev.x <-F_T2011_mod2$zelev.x*-1
F_T2011_mod2$zospace <- F_T2011_mod2$zospace*-1
F_T2011_mod2$zforest <- F_T2011_mod2$zforest*-1
F_T2011_mod2$zvisib <- F_T2011_mod2$zvisib*-1
F_T2011_mod2$zwdsp <- F_T2011_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2011_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,ztden,zasource,zforest,zforest)
t.mat <- cbind(zvisib,zwdsp,zhumid)
detach(F_T2011_mod2)



#function to create the matrix (st.mat) of interaction between the space variables and time variables

st.mat <-NULL
count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
    st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
  }
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

F_2011_all_st_table_mod2 <- data.frame(F_T2011_mod2,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)

write.dbf(F_2011_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.1.NE_PM_MODELS/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2011.dbf") 


