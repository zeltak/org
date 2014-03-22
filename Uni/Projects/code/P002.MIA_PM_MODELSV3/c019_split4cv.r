library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)


#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2000_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2000.csv", header=T)


T2000_weight <- na.omit(T2000_weight)

names(T2000_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2000_weight) #run modle
summary(w1)
T2000_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2000_weight$wt <- 1/T2000_weight$prob

T2000_weight$normwt <- T2000_weight$wt/mean(T2000_weight$wt)
T2000_weight <- rename(T2000_weight, c(date="Date"))

summary(T2000_weight$normwt)



#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2000_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2000.csv", header=T) 
names(F_T2000_Allp)

F_T2000_Allp <- subset(F_T2000_Allp,F_T2000_Allp$population >= "10")


# sort 
F_T2000_Allp <- F_T2000_Allp[order(F_T2000_Allp$guid,F_T2000_Allp$Date),]
T2000_weight <- T2000_weight[order(T2000_weight$guid,T2000_weight$Date),]

# merge two dataframes by ID

F_T2000_All <- merge(F_T2000_Allp,T2000_weight,by=c("guid","Date"))
summary(F_T2000_All)




#transfer all to Z-scores
F_T2000_All$zelev.x<-(F_T2000_All$elev.x-mean(F_T2000_All$elev.x))/sd(F_T2000_All$elev.x) 
F_T2000_All$zpopden<-(F_T2000_All$pop_sqkm-mean(F_T2000_All$pop_sqkm))/sd(F_T2000_All$pop_sqkm)
F_T2000_All$zospace<-(F_T2000_All$p_open-mean(F_T2000_All$p_open))/sd(F_T2000_All$p_open)
F_T2000_All$zdista1<- (F_T2000_All$A1_dist_km-mean(F_T2000_All$A1_dist_km))/sd(F_T2000_All$A1_dist_km)
F_T2000_All$zpsource<-(F_T2000_All$point_pm-mean(F_T2000_All$point_pm))/sd(F_T2000_All$point_pm)
F_T2000_All$zasource<- (F_T2000_All$area_pm-mean(F_T2000_All$area_pm))/sd(F_T2000_All$area_pm)
F_T2000_All$zvisib<- (F_T2000_All$visib_F-mean(F_T2000_All$visib_F))/sd(F_T2000_All$visib_F)
F_T2000_All$zwdsp<-(F_T2000_All$wdsp_F-mean(F_T2000_All$wdsp_F))/sd(F_T2000_All$wdsp_F)
F_T2000_All$zhumid<-(F_T2000_All$ah_gm3_F-mean(F_T2000_All$ah_gm3_F))/sd(F_T2000_All$ah_gm3_F)

F_T2000_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2000_All$zelev.x <-F_T2000_All$zelev.x*-1
F_T2000_All$zospace <- F_T2000_All$zospace*-1
F_T2000_All$zdista1 <- F_T2000_All$zdista1*-1
F_T2000_All$zvisib <- F_T2000_All$zvisib*-1
F_T2000_All$zwdsp <- F_T2000_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2000_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2000_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2000.dbf") 





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

write.dbf(mod1_T2000_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s1.dbf")

write.dbf(mod1_T2000_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s2 <- splits_s2$trainset
mod1_T2000_90_s2 <- splits_s2$testset

write.dbf(mod1_T2000_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s2.dbf")

write.dbf(mod1_T2000_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s3 <- splits_s3$trainset
mod1_T2000_90_s3 <- splits_s3$testset

write.dbf(mod1_T2000_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s3.dbf")

write.dbf(mod1_T2000_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s4 <- splits_s4$trainset
mod1_T2000_90_s4 <- splits_s4$testset

write.dbf(mod1_T2000_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s4.dbf")

write.dbf(mod1_T2000_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s5 <- splits_s5$trainset
mod1_T2000_90_s5 <- splits_s5$testset

write.dbf(mod1_T2000_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s5.dbf")

write.dbf(mod1_T2000_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s6 <- splits_s6$trainset
mod1_T2000_90_s6 <- splits_s6$testset

write.dbf(mod1_T2000_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s6.dbf")

write.dbf(mod1_T2000_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s7 <- splits_s7$trainset
mod1_T2000_90_s7 <- splits_s7$testset

write.dbf(mod1_T2000_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s7.dbf")

write.dbf(mod1_T2000_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s8 <- splits_s8$trainset
mod1_T2000_90_s8 <- splits_s8$testset

write.dbf(mod1_T2000_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s8.dbf")

write.dbf(mod1_T2000_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s9 <- splits_s9$trainset
mod1_T2000_90_s9 <- splits_s9$testset

write.dbf(mod1_T2000_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s9.dbf")

write.dbf(mod1_T2000_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2000_all_st_table)

mod1_T2000_10_s10 <- splits_s10$trainset
mod1_T2000_90_s10 <- splits_s10$testset

write.dbf(mod1_T2000_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s10.dbf")

write.dbf(mod1_T2000_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2000_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2000.csv", header=T) 
names(F_T2000_mod2p)
F_T2000_mod2p <- rename(F_T2000_mod2p, c(date="Date"))

F_T2000_mod2p <- subset(F_T2000_mod2p,F_T2000_mod2p$population >= "10")



# sort 
F_T2000_mod2p <- F_T2000_mod2p[order(F_T2000_mod2p$guid,F_T2000_mod2p$Date),]
T2000_weight <- T2000_weight[order(T2000_weight$guid,T2000_weight$Date),]

# merge two dataframes by ID
F_T2000_mod2 <- merge(F_T2000_mod2p,T2000_weight,by=c("guid","Date"))
names(F_T2000_mod2)
# mT2000n <-subset(mT2000,!duplicated(mT2000$guid))





#transfer all to Z-scores
F_T2000_mod2$zelev.x<-(F_T2000_mod2$elev.x-mean(F_T2000_All$elev.x))/sd(F_T2000_All$elev.x) 
F_T2000_mod2$zpopden<-(F_T2000_mod2$pop_sqkm-mean(F_T2000_All$pop_sqkm))/sd(F_T2000_All$pop_sqkm)
F_T2000_mod2$zospace<-(F_T2000_mod2$p_open-mean(F_T2000_All$p_open))/sd(F_T2000_All$p_open)
F_T2000_mod2$zdista1<- (F_T2000_mod2$A1_dist_km-mean(F_T2000_All$A1_dist_km))/sd(F_T2000_All$A1_dist_km)
F_T2000_mod2$zpsource<-(F_T2000_mod2$point_pm-mean(F_T2000_All$point_pm))/sd(F_T2000_All$point_pm)
F_T2000_mod2$zasource<- (F_T2000_mod2$area_pm -mean(F_T2000_All$area_pm))/sd(F_T2000_All$area_pm)
F_T2000_mod2$zvisib <- (F_T2000_mod2$visib_F-mean(F_T2000_All$visib_F))/sd(F_T2000_All$visib_F)
F_T2000_mod2$zwdsp<-(F_T2000_mod2$wdsp_F-mean(F_T2000_All$wdsp_F))/sd(F_T2000_All$wdsp_F)
F_T2000_mod2$zhumid<-(F_T2000_mod2$ah_gm3_F-mean(F_T2000_All$ah_gm3_F))/sd(F_T2000_All$ah_gm3_F)

F_T2000_mod2$zid<-1 #create id variable




F_T2000_mod2$zelev.x <-F_T2000_mod2$zelev.x*-1
F_T2000_mod2$zospace <- F_T2000_mod2$zospace*-1
F_T2000_mod2$zdista1 <- F_T2000_mod2$zdista1*-1
F_T2000_mod2$zvisib <- F_T2000_mod2$zvisib*-1
F_T2000_mod2$zwdsp <- F_T2000_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2000_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2000_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2000.dbf") 



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2001
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2001_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2001.csv", header=T)


T2001_weight <- na.omit(T2001_weight)

names(T2001_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2001_weight) #run modle
summary(w1)
T2001_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2001_weight$wt <- 1/T2001_weight$prob

T2001_weight$normwt <- T2001_weight$wt/mean(T2001_weight$wt)
T2001_weight <- rename(T2001_weight, c(date="Date"))

summary(T2001_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2001_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2001.csv", header=T) 
names(F_T2001_Allp)

F_T2001_Allp <- subset(F_T2001_Allp,F_T2001_Allp$population >= "10")



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
F_T2001_All$zdista1<- (F_T2001_All$A1_dist_km-mean(F_T2001_All$A1_dist_km))/sd(F_T2001_All$A1_dist_km)
F_T2001_All$zpsource<-(F_T2001_All$point_pm-mean(F_T2001_All$point_pm))/sd(F_T2001_All$point_pm)
F_T2001_All$zasource<- (F_T2001_All$area_pm-mean(F_T2001_All$area_pm))/sd(F_T2001_All$area_pm)
F_T2001_All$zvisib<- (F_T2001_All$visib_F-mean(F_T2001_All$visib_F))/sd(F_T2001_All$visib_F)
F_T2001_All$zwdsp<-(F_T2001_All$wdsp_F-mean(F_T2001_All$wdsp_F))/sd(F_T2001_All$wdsp_F)
F_T2001_All$zhumid<-(F_T2001_All$ah_gm3_F-mean(F_T2001_All$ah_gm3_F))/sd(F_T2001_All$ah_gm3_F)

F_T2001_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2001_All$zelev.x <-F_T2001_All$zelev.x*-1
F_T2001_All$zospace <- F_T2001_All$zospace*-1
F_T2001_All$zdista1 <- F_T2001_All$zdista1*-1
F_T2001_All$zvisib <- F_T2001_All$zvisib*-1
F_T2001_All$zwdsp <- F_T2001_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2001_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2001_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2001.dbf") 





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

write.dbf(mod1_T2001_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s1.dbf")

write.dbf(mod1_T2001_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s2 <- splits_s2$trainset
mod1_T2001_90_s2 <- splits_s2$testset

write.dbf(mod1_T2001_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s2.dbf")

write.dbf(mod1_T2001_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s3 <- splits_s3$trainset
mod1_T2001_90_s3 <- splits_s3$testset

write.dbf(mod1_T2001_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s3.dbf")

write.dbf(mod1_T2001_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s4 <- splits_s4$trainset
mod1_T2001_90_s4 <- splits_s4$testset

write.dbf(mod1_T2001_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s4.dbf")

write.dbf(mod1_T2001_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s5 <- splits_s5$trainset
mod1_T2001_90_s5 <- splits_s5$testset

write.dbf(mod1_T2001_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s5.dbf")

write.dbf(mod1_T2001_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s6 <- splits_s6$trainset
mod1_T2001_90_s6 <- splits_s6$testset

write.dbf(mod1_T2001_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s6.dbf")

write.dbf(mod1_T2001_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s7 <- splits_s7$trainset
mod1_T2001_90_s7 <- splits_s7$testset

write.dbf(mod1_T2001_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s7.dbf")

write.dbf(mod1_T2001_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s8 <- splits_s8$trainset
mod1_T2001_90_s8 <- splits_s8$testset

write.dbf(mod1_T2001_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s8.dbf")

write.dbf(mod1_T2001_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s9 <- splits_s9$trainset
mod1_T2001_90_s9 <- splits_s9$testset

write.dbf(mod1_T2001_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s9.dbf")

write.dbf(mod1_T2001_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2001_all_st_table)

mod1_T2001_10_s10 <- splits_s10$trainset
mod1_T2001_90_s10 <- splits_s10$testset

write.dbf(mod1_T2001_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s10.dbf")

write.dbf(mod1_T2001_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2001_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2001.csv", header=T) 
names(F_T2001_mod2p)
F_T2001_mod2p <- rename(F_T2001_mod2p, c(date="Date"))

F_T2001_mod2p <- subset(F_T2001_mod2p,F_T2001_mod2p$population >= "10")



# sort 
F_T2001_mod2p <- F_T2001_mod2p[order(F_T2001_mod2p$guid,F_T2001_mod2p$Date),]
T2001_weight <- T2001_weight[order(T2001_weight$guid,T2001_weight$Date),]

# merge two dataframes by ID
F_T2001_mod2 <- merge(F_T2001_mod2p,T2001_weight,by=c("guid","Date"))
names(F_T2001_mod2)
# mT2001n <-subset(mT2001,!duplicated(mT2001$guid))





#transfer all to Z-scores
F_T2001_mod2$zelev.x<-(F_T2001_mod2$elev.x-mean(F_T2001_All$elev.x))/sd(F_T2001_All$elev.x) 
F_T2001_mod2$zpopden<-(F_T2001_mod2$pop_sqkm-mean(F_T2001_All$pop_sqkm))/sd(F_T2001_All$pop_sqkm)
F_T2001_mod2$zospace<-(F_T2001_mod2$p_open-mean(F_T2001_All$p_open))/sd(F_T2001_All$p_open)
F_T2001_mod2$zdista1<- (F_T2001_mod2$A1_dist_km-mean(F_T2001_All$A1_dist_km))/sd(F_T2001_All$A1_dist_km)
F_T2001_mod2$zpsource<-(F_T2001_mod2$point_pm-mean(F_T2001_All$point_pm))/sd(F_T2001_All$point_pm)
F_T2001_mod2$zasource<- (F_T2001_mod2$area_pm -mean(F_T2001_All$area_pm))/sd(F_T2001_All$area_pm)
F_T2001_mod2$zvisib <- (F_T2001_mod2$visib_F-mean(F_T2001_All$visib_F))/sd(F_T2001_All$visib_F)
F_T2001_mod2$zwdsp<-(F_T2001_mod2$wdsp_F-mean(F_T2001_All$wdsp_F))/sd(F_T2001_All$wdsp_F)
F_T2001_mod2$zhumid<-(F_T2001_mod2$ah_gm3_F-mean(F_T2001_All$ah_gm3_F))/sd(F_T2001_All$ah_gm3_F)

F_T2001_mod2$zid<-1 #create id variable




F_T2001_mod2$zelev.x <-F_T2001_mod2$zelev.x*-1
F_T2001_mod2$zospace <- F_T2001_mod2$zospace*-1
F_T2001_mod2$zdista1 <- F_T2001_mod2$zdista1*-1
F_T2001_mod2$zvisib <- F_T2001_mod2$zvisib*-1
F_T2001_mod2$zwdsp <- F_T2001_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2001_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2001_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2001.dbf") 





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2002
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2002_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2002.csv", header=T)

T2002_weight <- na.omit(T2002_weight)

names(T2002_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2002_weight) #run modle
summary(w1)
T2002_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2002_weight$wt <- 1/T2002_weight$prob

T2002_weight$normwt <- T2002_weight$wt/mean(T2002_weight$wt)
T2002_weight <- rename(T2002_weight, c(date="Date"))

summary(T2002_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2002_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2002.csv", header=T) 
names(F_T2002_Allp)

F_T2002_Allp <- subset(F_T2002_Allp,F_T2002_Allp$population >= "10")



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
F_T2002_All$zdista1<- (F_T2002_All$A1_dist_km-mean(F_T2002_All$A1_dist_km))/sd(F_T2002_All$A1_dist_km)
F_T2002_All$zpsource<-(F_T2002_All$point_pm-mean(F_T2002_All$point_pm))/sd(F_T2002_All$point_pm)
F_T2002_All$zasource<- (F_T2002_All$area_pm-mean(F_T2002_All$area_pm))/sd(F_T2002_All$area_pm)
F_T2002_All$zvisib<- (F_T2002_All$visib_F-mean(F_T2002_All$visib_F))/sd(F_T2002_All$visib_F)
F_T2002_All$zwdsp<-(F_T2002_All$wdsp_F-mean(F_T2002_All$wdsp_F))/sd(F_T2002_All$wdsp_F)
F_T2002_All$zhumid<-(F_T2002_All$ah_gm3_F-mean(F_T2002_All$ah_gm3_F))/sd(F_T2002_All$ah_gm3_F)

F_T2002_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2002_All$zelev.x <-F_T2002_All$zelev.x*-1
F_T2002_All$zospace <- F_T2002_All$zospace*-1
F_T2002_All$zdista1 <- F_T2002_All$zdista1*-1
F_T2002_All$zvisib <- F_T2002_All$zvisib*-1
F_T2002_All$zwdsp <- F_T2002_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2002_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2002_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2002.dbf") 





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

write.dbf(mod1_T2002_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s1.dbf")

write.dbf(mod1_T2002_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s2 <- splits_s2$trainset
mod1_T2002_90_s2 <- splits_s2$testset

write.dbf(mod1_T2002_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s2.dbf")

write.dbf(mod1_T2002_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s3 <- splits_s3$trainset
mod1_T2002_90_s3 <- splits_s3$testset

write.dbf(mod1_T2002_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s3.dbf")

write.dbf(mod1_T2002_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s4 <- splits_s4$trainset
mod1_T2002_90_s4 <- splits_s4$testset

write.dbf(mod1_T2002_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s4.dbf")

write.dbf(mod1_T2002_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s5 <- splits_s5$trainset
mod1_T2002_90_s5 <- splits_s5$testset

write.dbf(mod1_T2002_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s5.dbf")

write.dbf(mod1_T2002_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s6 <- splits_s6$trainset
mod1_T2002_90_s6 <- splits_s6$testset

write.dbf(mod1_T2002_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s6.dbf")

write.dbf(mod1_T2002_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s7 <- splits_s7$trainset
mod1_T2002_90_s7 <- splits_s7$testset

write.dbf(mod1_T2002_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s7.dbf")

write.dbf(mod1_T2002_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s8 <- splits_s8$trainset
mod1_T2002_90_s8 <- splits_s8$testset

write.dbf(mod1_T2002_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s8.dbf")

write.dbf(mod1_T2002_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s9 <- splits_s9$trainset
mod1_T2002_90_s9 <- splits_s9$testset

write.dbf(mod1_T2002_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s9.dbf")

write.dbf(mod1_T2002_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2002_all_st_table)

mod1_T2002_10_s10 <- splits_s10$trainset
mod1_T2002_90_s10 <- splits_s10$testset

write.dbf(mod1_T2002_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s10.dbf")

write.dbf(mod1_T2002_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2002_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2002.csv", header=T) 
names(F_T2002_mod2p)
F_T2002_mod2p <- rename(F_T2002_mod2p, c(date="Date"))

F_T2002_mod2p <- subset(F_T2002_mod2p,F_T2002_mod2p$population >= "10")



# sort 
F_T2002_mod2p <- F_T2002_mod2p[order(F_T2002_mod2p$guid,F_T2002_mod2p$Date),]
T2002_weight <- T2002_weight[order(T2002_weight$guid,T2002_weight$Date),]

# merge two dataframes by ID
F_T2002_mod2 <- merge(F_T2002_mod2p,T2002_weight,by=c("guid","Date"))
names(F_T2002_mod2)
# mT2002n <-subset(mT2002,!duplicated(mT2002$guid))





#transfer all to Z-scores
F_T2002_mod2$zelev.x<-(F_T2002_mod2$elev.x-mean(F_T2002_All$elev.x))/sd(F_T2002_All$elev.x) 
F_T2002_mod2$zpopden<-(F_T2002_mod2$pop_sqkm-mean(F_T2002_All$pop_sqkm))/sd(F_T2002_All$pop_sqkm)
F_T2002_mod2$zospace<-(F_T2002_mod2$p_open-mean(F_T2002_All$p_open))/sd(F_T2002_All$p_open)
F_T2002_mod2$zdista1<- (F_T2002_mod2$A1_dist_km-mean(F_T2002_All$A1_dist_km))/sd(F_T2002_All$A1_dist_km)
F_T2002_mod2$zpsource<-(F_T2002_mod2$point_pm-mean(F_T2002_All$point_pm))/sd(F_T2002_All$point_pm)
F_T2002_mod2$zasource<- (F_T2002_mod2$area_pm -mean(F_T2002_All$area_pm))/sd(F_T2002_All$area_pm)
F_T2002_mod2$zvisib <- (F_T2002_mod2$visib_F-mean(F_T2002_All$visib_F))/sd(F_T2002_All$visib_F)
F_T2002_mod2$zwdsp<-(F_T2002_mod2$wdsp_F-mean(F_T2002_All$wdsp_F))/sd(F_T2002_All$wdsp_F)
F_T2002_mod2$zhumid<-(F_T2002_mod2$ah_gm3_F-mean(F_T2002_All$ah_gm3_F))/sd(F_T2002_All$ah_gm3_F)

F_T2002_mod2$zid<-1 #create id variable




F_T2002_mod2$zelev.x <-F_T2002_mod2$zelev.x*-1
F_T2002_mod2$zospace <- F_T2002_mod2$zospace*-1
F_T2002_mod2$zdista1 <- F_T2002_mod2$zdista1*-1
F_T2002_mod2$zvisib <- F_T2002_mod2$zvisib*-1
F_T2002_mod2$zwdsp <- F_T2002_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2002_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2002_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2002.dbf") 







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2003
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2003_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2003.csv", header=T)

names(T2003_weight)

T2003_weight <- na.omit(T2003_weight)


w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2003_weight) #run modle
summary(w1)
T2003_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2003_weight$wt <- 1/T2003_weight$prob

T2003_weight$normwt <- T2003_weight$wt/mean(T2003_weight$wt)
T2003_weight <- rename(T2003_weight, c(date="Date"))

summary(T2003_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2003_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2003.csv", header=T) 
names(F_T2003_Allp)

F_T2003_Allp <- subset(F_T2003_Allp,F_T2003_Allp$population >= "10")



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
F_T2003_All$zdista1<- (F_T2003_All$A1_dist_km-mean(F_T2003_All$A1_dist_km))/sd(F_T2003_All$A1_dist_km)
F_T2003_All$zpsource<-(F_T2003_All$point_pm-mean(F_T2003_All$point_pm))/sd(F_T2003_All$point_pm)
F_T2003_All$zasource<- (F_T2003_All$area_pm-mean(F_T2003_All$area_pm))/sd(F_T2003_All$area_pm)
F_T2003_All$zvisib<- (F_T2003_All$visib_F-mean(F_T2003_All$visib_F))/sd(F_T2003_All$visib_F)
F_T2003_All$zwdsp<-(F_T2003_All$wdsp_F-mean(F_T2003_All$wdsp_F))/sd(F_T2003_All$wdsp_F)
F_T2003_All$zhumid<-(F_T2003_All$ah_gm3_F-mean(F_T2003_All$ah_gm3_F))/sd(F_T2003_All$ah_gm3_F)

F_T2003_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2003_All$zelev.x <-F_T2003_All$zelev.x*-1
F_T2003_All$zospace <- F_T2003_All$zospace*-1
F_T2003_All$zdista1 <- F_T2003_All$zdista1*-1
F_T2003_All$zvisib <- F_T2003_All$zvisib*-1
F_T2003_All$zwdsp <- F_T2003_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2003_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2003_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2003.dbf") 





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

write.dbf(mod1_T2003_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s1.dbf")

write.dbf(mod1_T2003_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s2 <- splits_s2$trainset
mod1_T2003_90_s2 <- splits_s2$testset

write.dbf(mod1_T2003_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s2.dbf")

write.dbf(mod1_T2003_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s3 <- splits_s3$trainset
mod1_T2003_90_s3 <- splits_s3$testset

write.dbf(mod1_T2003_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s3.dbf")

write.dbf(mod1_T2003_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s4 <- splits_s4$trainset
mod1_T2003_90_s4 <- splits_s4$testset

write.dbf(mod1_T2003_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s4.dbf")

write.dbf(mod1_T2003_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s5 <- splits_s5$trainset
mod1_T2003_90_s5 <- splits_s5$testset

write.dbf(mod1_T2003_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s5.dbf")

write.dbf(mod1_T2003_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s6 <- splits_s6$trainset
mod1_T2003_90_s6 <- splits_s6$testset

write.dbf(mod1_T2003_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s6.dbf")

write.dbf(mod1_T2003_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s7 <- splits_s7$trainset
mod1_T2003_90_s7 <- splits_s7$testset

write.dbf(mod1_T2003_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s7.dbf")

write.dbf(mod1_T2003_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s8 <- splits_s8$trainset
mod1_T2003_90_s8 <- splits_s8$testset

write.dbf(mod1_T2003_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s8.dbf")

write.dbf(mod1_T2003_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s9 <- splits_s9$trainset
mod1_T2003_90_s9 <- splits_s9$testset

write.dbf(mod1_T2003_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s9.dbf")

write.dbf(mod1_T2003_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2003_all_st_table)

mod1_T2003_10_s10 <- splits_s10$trainset
mod1_T2003_90_s10 <- splits_s10$testset

write.dbf(mod1_T2003_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s10.dbf")

write.dbf(mod1_T2003_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2003_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2003.csv", header=T) 
names(F_T2003_mod2p)
F_T2003_mod2p <- rename(F_T2003_mod2p, c(date="Date"))

F_T2003_mod2p <- subset(F_T2003_mod2p,F_T2003_mod2p$population >= "10")



# sort 
F_T2003_mod2p <- F_T2003_mod2p[order(F_T2003_mod2p$guid,F_T2003_mod2p$Date),]
T2003_weight <- T2003_weight[order(T2003_weight$guid,T2003_weight$Date),]

# merge two dataframes by ID
F_T2003_mod2 <- merge(F_T2003_mod2p,T2003_weight,by=c("guid","Date"))
names(F_T2003_mod2)
# mT2003n <-subset(mT2003,!duplicated(mT2003$guid))





#transfer all to Z-scores
F_T2003_mod2$zelev.x<-(F_T2003_mod2$elev.x-mean(F_T2003_All$elev.x))/sd(F_T2003_All$elev.x) 
F_T2003_mod2$zpopden<-(F_T2003_mod2$pop_sqkm-mean(F_T2003_All$pop_sqkm))/sd(F_T2003_All$pop_sqkm)
F_T2003_mod2$zospace<-(F_T2003_mod2$p_open-mean(F_T2003_All$p_open))/sd(F_T2003_All$p_open)
F_T2003_mod2$zdista1<- (F_T2003_mod2$A1_dist_km-mean(F_T2003_All$A1_dist_km))/sd(F_T2003_All$A1_dist_km)
F_T2003_mod2$zpsource<-(F_T2003_mod2$point_pm-mean(F_T2003_All$point_pm))/sd(F_T2003_All$point_pm)
F_T2003_mod2$zasource<- (F_T2003_mod2$area_pm -mean(F_T2003_All$area_pm))/sd(F_T2003_All$area_pm)
F_T2003_mod2$zvisib <- (F_T2003_mod2$visib_F-mean(F_T2003_All$visib_F))/sd(F_T2003_All$visib_F)
F_T2003_mod2$zwdsp<-(F_T2003_mod2$wdsp_F-mean(F_T2003_All$wdsp_F))/sd(F_T2003_All$wdsp_F)
F_T2003_mod2$zhumid<-(F_T2003_mod2$ah_gm3_F-mean(F_T2003_All$ah_gm3_F))/sd(F_T2003_All$ah_gm3_F)

F_T2003_mod2$zid<-1 #create id variable




F_T2003_mod2$zelev.x <-F_T2003_mod2$zelev.x*-1
F_T2003_mod2$zospace <- F_T2003_mod2$zospace*-1
F_T2003_mod2$zdista1 <- F_T2003_mod2$zdista1*-1
F_T2003_mod2$zvisib <- F_T2003_mod2$zvisib*-1
F_T2003_mod2$zwdsp <- F_T2003_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2003_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2003_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2003.dbf") 







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2004
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2004_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2004.csv", header=T)

names(T2004_weight)

T2004_weight <- na.omit(T2004_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2004_weight) #run modle
summary(w1)
T2004_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2004_weight$wt <- 1/T2004_weight$prob

T2004_weight$normwt <- T2004_weight$wt/mean(T2004_weight$wt)
T2004_weight <- rename(T2004_weight, c(date="Date"))

summary(T2004_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2004_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2004.csv", header=T) 
names(F_T2004_Allp)

F_T2004_Allp <- subset(F_T2004_Allp,F_T2004_Allp$population >= "10")



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
F_T2004_All$zdista1<- (F_T2004_All$A1_dist_km-mean(F_T2004_All$A1_dist_km))/sd(F_T2004_All$A1_dist_km)
F_T2004_All$zpsource<-(F_T2004_All$point_pm-mean(F_T2004_All$point_pm))/sd(F_T2004_All$point_pm)
F_T2004_All$zasource<- (F_T2004_All$area_pm-mean(F_T2004_All$area_pm))/sd(F_T2004_All$area_pm)
F_T2004_All$zvisib<- (F_T2004_All$visib_F-mean(F_T2004_All$visib_F))/sd(F_T2004_All$visib_F)
F_T2004_All$zwdsp<-(F_T2004_All$wdsp_F-mean(F_T2004_All$wdsp_F))/sd(F_T2004_All$wdsp_F)
F_T2004_All$zhumid<-(F_T2004_All$ah_gm3_F-mean(F_T2004_All$ah_gm3_F))/sd(F_T2004_All$ah_gm3_F)

F_T2004_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2004_All$zelev.x <-F_T2004_All$zelev.x*-1
F_T2004_All$zospace <- F_T2004_All$zospace*-1
F_T2004_All$zdista1 <- F_T2004_All$zdista1*-1
F_T2004_All$zvisib <- F_T2004_All$zvisib*-1
F_T2004_All$zwdsp <- F_T2004_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2004_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2004_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2004.dbf") 





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

write.dbf(mod1_T2004_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s1.dbf")

write.dbf(mod1_T2004_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s2 <- splits_s2$trainset
mod1_T2004_90_s2 <- splits_s2$testset

write.dbf(mod1_T2004_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s2.dbf")

write.dbf(mod1_T2004_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s3 <- splits_s3$trainset
mod1_T2004_90_s3 <- splits_s3$testset

write.dbf(mod1_T2004_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s3.dbf")

write.dbf(mod1_T2004_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s4 <- splits_s4$trainset
mod1_T2004_90_s4 <- splits_s4$testset

write.dbf(mod1_T2004_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s4.dbf")

write.dbf(mod1_T2004_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s5 <- splits_s5$trainset
mod1_T2004_90_s5 <- splits_s5$testset

write.dbf(mod1_T2004_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s5.dbf")

write.dbf(mod1_T2004_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s6 <- splits_s6$trainset
mod1_T2004_90_s6 <- splits_s6$testset

write.dbf(mod1_T2004_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s6.dbf")

write.dbf(mod1_T2004_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s7 <- splits_s7$trainset
mod1_T2004_90_s7 <- splits_s7$testset

write.dbf(mod1_T2004_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s7.dbf")

write.dbf(mod1_T2004_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s8 <- splits_s8$trainset
mod1_T2004_90_s8 <- splits_s8$testset

write.dbf(mod1_T2004_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s8.dbf")

write.dbf(mod1_T2004_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s9 <- splits_s9$trainset
mod1_T2004_90_s9 <- splits_s9$testset

write.dbf(mod1_T2004_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s9.dbf")

write.dbf(mod1_T2004_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2004_all_st_table)

mod1_T2004_10_s10 <- splits_s10$trainset
mod1_T2004_90_s10 <- splits_s10$testset

write.dbf(mod1_T2004_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s10.dbf")

write.dbf(mod1_T2004_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2004_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2004.csv", header=T) 
names(F_T2004_mod2p)
F_T2004_mod2p <- rename(F_T2004_mod2p, c(date="Date"))

F_T2004_mod2p <- subset(F_T2004_mod2p,F_T2004_mod2p$population >= "10")



# sort 
F_T2004_mod2p <- F_T2004_mod2p[order(F_T2004_mod2p$guid,F_T2004_mod2p$Date),]
T2004_weight <- T2004_weight[order(T2004_weight$guid,T2004_weight$Date),]

# merge two dataframes by ID
F_T2004_mod2 <- merge(F_T2004_mod2p,T2004_weight,by=c("guid","Date"))
names(F_T2004_mod2)
# mT2004n <-subset(mT2004,!duplicated(mT2004$guid))





#transfer all to Z-scores
F_T2004_mod2$zelev.x<-(F_T2004_mod2$elev.x-mean(F_T2004_All$elev.x))/sd(F_T2004_All$elev.x) 
F_T2004_mod2$zpopden<-(F_T2004_mod2$pop_sqkm-mean(F_T2004_All$pop_sqkm))/sd(F_T2004_All$pop_sqkm)
F_T2004_mod2$zospace<-(F_T2004_mod2$p_open-mean(F_T2004_All$p_open))/sd(F_T2004_All$p_open)
F_T2004_mod2$zdista1<- (F_T2004_mod2$A1_dist_km-mean(F_T2004_All$A1_dist_km))/sd(F_T2004_All$A1_dist_km)
F_T2004_mod2$zpsource<-(F_T2004_mod2$point_pm-mean(F_T2004_All$point_pm))/sd(F_T2004_All$point_pm)
F_T2004_mod2$zasource<- (F_T2004_mod2$area_pm -mean(F_T2004_All$area_pm))/sd(F_T2004_All$area_pm)
F_T2004_mod2$zvisib <- (F_T2004_mod2$visib_F-mean(F_T2004_All$visib_F))/sd(F_T2004_All$visib_F)
F_T2004_mod2$zwdsp<-(F_T2004_mod2$wdsp_F-mean(F_T2004_All$wdsp_F))/sd(F_T2004_All$wdsp_F)
F_T2004_mod2$zhumid<-(F_T2004_mod2$ah_gm3_F-mean(F_T2004_All$ah_gm3_F))/sd(F_T2004_All$ah_gm3_F)

F_T2004_mod2$zid<-1 #create id variable




F_T2004_mod2$zelev.x <-F_T2004_mod2$zelev.x*-1
F_T2004_mod2$zospace <- F_T2004_mod2$zospace*-1
F_T2004_mod2$zdista1 <- F_T2004_mod2$zdista1*-1
F_T2004_mod2$zvisib <- F_T2004_mod2$zvisib*-1
F_T2004_mod2$zwdsp <- F_T2004_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2004_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2004_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2004.dbf") 







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2005
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2005_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2005.csv", header=T)

names(T2005_weight)

T2005_weight <- na.omit(T2005_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2005_weight) #run modle
summary(w1)
T2005_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2005_weight$wt <- 1/T2005_weight$prob

T2005_weight$normwt <- T2005_weight$wt/mean(T2005_weight$wt)
T2005_weight <- rename(T2005_weight, c(date="Date"))

summary(T2005_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2005_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2005.csv", header=T) 
names(F_T2005_Allp)

F_T2005_Allp <- na.omit(F_T2005_Allp)

F_T2005_Allp <- subset(F_T2005_Allp,F_T2005_Allp$population >= "10")



# sort 
F_T2005_Allp <- F_T2005_Allp[order(F_T2005_Allp$guid,F_T2005_Allp$Date),]
T2005_weight <- T2005_weight[order(T2005_weight$guid,T2005_weight$Date),]

# merge two dataframes by ID
F_T2005_All <- merge(F_T2005_Allp,T2005_weight,by=c("guid","Date"))
names(F_T2005_All)

summary(F_T2005_All)


#transfer all to Z-scores
F_T2005_All$zelev.x<-(F_T2005_All$elev.x-mean(F_T2005_All$elev.x))/sd(F_T2005_All$elev.x) 
F_T2005_All$zpopden<-(F_T2005_All$pop_sqkm-mean(F_T2005_All$pop_sqkm))/sd(F_T2005_All$pop_sqkm)
F_T2005_All$zospace<-(F_T2005_All$p_open-mean(F_T2005_All$p_open))/sd(F_T2005_All$p_open)
F_T2005_All$zdista1<- (F_T2005_All$A1_dist_km-mean(F_T2005_All$A1_dist_km))/sd(F_T2005_All$A1_dist_km)
F_T2005_All$zpsource<-(F_T2005_All$point_pm-mean(F_T2005_All$point_pm))/sd(F_T2005_All$point_pm)
F_T2005_All$zasource<- (F_T2005_All$area_pm-mean(F_T2005_All$area_pm))/sd(F_T2005_All$area_pm)

F_T2005_All$zvisib<- (F_T2005_All$visib_F-mean(F_T2005_All$visib_F))/sd(F_T2005_All$visib_F)
F_T2005_All$zwdsp<-(F_T2005_All$wdsp_F-mean(F_T2005_All$wdsp_F))/sd(F_T2005_All$wdsp_F)
F_T2005_All$zhumid<-(F_T2005_All$ah_gm3_F-mean(F_T2005_All$ah_gm3_F))/sd(F_T2005_All$ah_gm3_F)

F_T2005_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2005_All$zelev.x <-F_T2005_All$zelev.x*-1
F_T2005_All$zospace <- F_T2005_All$zospace*-1
F_T2005_All$zdista1 <- F_T2005_All$zdista1*-1
F_T2005_All$zvisib <- F_T2005_All$zvisib*-1
F_T2005_All$zwdsp <- F_T2005_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2005_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2005_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2005.dbf") 





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

write.dbf(mod1_T2005_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s1.dbf")

write.dbf(mod1_T2005_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s2 <- splits_s2$trainset
mod1_T2005_90_s2 <- splits_s2$testset

write.dbf(mod1_T2005_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s2.dbf")

write.dbf(mod1_T2005_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s3 <- splits_s3$trainset
mod1_T2005_90_s3 <- splits_s3$testset

write.dbf(mod1_T2005_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s3.dbf")

write.dbf(mod1_T2005_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s4 <- splits_s4$trainset
mod1_T2005_90_s4 <- splits_s4$testset

write.dbf(mod1_T2005_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s4.dbf")

write.dbf(mod1_T2005_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s5 <- splits_s5$trainset
mod1_T2005_90_s5 <- splits_s5$testset

write.dbf(mod1_T2005_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s5.dbf")

write.dbf(mod1_T2005_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s6 <- splits_s6$trainset
mod1_T2005_90_s6 <- splits_s6$testset

write.dbf(mod1_T2005_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s6.dbf")

write.dbf(mod1_T2005_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s7 <- splits_s7$trainset
mod1_T2005_90_s7 <- splits_s7$testset

write.dbf(mod1_T2005_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s7.dbf")

write.dbf(mod1_T2005_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s8 <- splits_s8$trainset
mod1_T2005_90_s8 <- splits_s8$testset

write.dbf(mod1_T2005_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s8.dbf")

write.dbf(mod1_T2005_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s9 <- splits_s9$trainset
mod1_T2005_90_s9 <- splits_s9$testset

write.dbf(mod1_T2005_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s9.dbf")

write.dbf(mod1_T2005_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2005_all_st_table)

mod1_T2005_10_s10 <- splits_s10$trainset
mod1_T2005_90_s10 <- splits_s10$testset

write.dbf(mod1_T2005_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s10.dbf")

write.dbf(mod1_T2005_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2005_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2005.csv", header=T) 
names(F_T2005_mod2p)
F_T2005_mod2p <- rename(F_T2005_mod2p, c(date="Date"))

F_T2005_mod2p <- subset(F_T2005_mod2p,F_T2005_mod2p$population >= "10")



# sort 
F_T2005_mod2p <- F_T2005_mod2p[order(F_T2005_mod2p$guid,F_T2005_mod2p$Date),]
T2005_weight <- T2005_weight[order(T2005_weight$guid,T2005_weight$Date),]

# merge two dataframes by ID
F_T2005_mod2 <- merge(F_T2005_mod2p,T2005_weight,by=c("guid","Date"))
names(F_T2005_mod2)
# mT2005n <-subset(mT2005,!duplicated(mT2005$guid))





#transfer all to Z-scores
F_T2005_mod2$zelev.x<-(F_T2005_mod2$elev.x-mean(F_T2005_All$elev.x))/sd(F_T2005_All$elev.x) 
F_T2005_mod2$zpopden<-(F_T2005_mod2$pop_sqkm-mean(F_T2005_All$pop_sqkm))/sd(F_T2005_All$pop_sqkm)
F_T2005_mod2$zospace<-(F_T2005_mod2$p_open-mean(F_T2005_All$p_open))/sd(F_T2005_All$p_open)
F_T2005_mod2$zdista1<- (F_T2005_mod2$A1_dist_km-mean(F_T2005_All$A1_dist_km))/sd(F_T2005_All$A1_dist_km)
F_T2005_mod2$zpsource<-(F_T2005_mod2$point_pm-mean(F_T2005_All$point_pm))/sd(F_T2005_All$point_pm)
F_T2005_mod2$zasource<- (F_T2005_mod2$area_pm -mean(F_T2005_All$area_pm))/sd(F_T2005_All$area_pm)
F_T2005_mod2$zvisib <- (F_T2005_mod2$visib_F-mean(F_T2005_All$visib_F))/sd(F_T2005_All$visib_F)
F_T2005_mod2$zwdsp<-(F_T2005_mod2$wdsp_F-mean(F_T2005_All$wdsp_F))/sd(F_T2005_All$wdsp_F)
F_T2005_mod2$zhumid<-(F_T2005_mod2$ah_gm3_F-mean(F_T2005_All$ah_gm3_F))/sd(F_T2005_All$ah_gm3_F)

F_T2005_mod2$zid<-1 #create id variable




F_T2005_mod2$zelev.x <-F_T2005_mod2$zelev.x*-1
F_T2005_mod2$zospace <- F_T2005_mod2$zospace*-1
F_T2005_mod2$zdista1 <- F_T2005_mod2$zdista1*-1
F_T2005_mod2$zvisib <- F_T2005_mod2$zvisib*-1
F_T2005_mod2$zwdsp <- F_T2005_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2005_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2005_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2005.dbf") 







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2006
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2006_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2006.csv", header=T)

F_T2006_Allp <- na.omit(F_T2006_Allp)

names(T2006_weight)


T2006_weight <- na.omit(T2006_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2006_weight) #run modle
summary(w1)
T2006_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2006_weight$wt <- 1/T2006_weight$prob

T2006_weight$normwt <- T2006_weight$wt/mean(T2006_weight$wt)
T2006_weight <- rename(T2006_weight, c(date="Date"))

summary(T2006_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2006_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2006.csv", header=T) 
names(F_T2006_Allp)


F_T2006_Allp <- na.omit(F_T2006_Allp)

F_T2006_Allp <- subset(F_T2006_Allp,F_T2006_Allp$population >= "10")



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
F_T2006_All$zdista1<- (F_T2006_All$A1_dist_km-mean(F_T2006_All$A1_dist_km))/sd(F_T2006_All$A1_dist_km)
F_T2006_All$zpsource<-(F_T2006_All$point_pm-mean(F_T2006_All$point_pm))/sd(F_T2006_All$point_pm)
F_T2006_All$zasource<- (F_T2006_All$area_pm-mean(F_T2006_All$area_pm))/sd(F_T2006_All$area_pm)
F_T2006_All$zvisib<- (F_T2006_All$visib_F-mean(F_T2006_All$visib_F))/sd(F_T2006_All$visib_F)
F_T2006_All$zwdsp<-(F_T2006_All$wdsp_F-mean(F_T2006_All$wdsp_F))/sd(F_T2006_All$wdsp_F)
F_T2006_All$zhumid<-(F_T2006_All$ah_gm3_F-mean(F_T2006_All$ah_gm3_F))/sd(F_T2006_All$ah_gm3_F)

F_T2006_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2006_All$zelev.x <-F_T2006_All$zelev.x*-1
F_T2006_All$zospace <- F_T2006_All$zospace*-1
F_T2006_All$zdista1 <- F_T2006_All$zdista1*-1
F_T2006_All$zvisib <- F_T2006_All$zvisib*-1
F_T2006_All$zwdsp <- F_T2006_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2006_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2006_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2006.dbf") 





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

write.dbf(mod1_T2006_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s1.dbf")

write.dbf(mod1_T2006_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s2 <- splits_s2$trainset
mod1_T2006_90_s2 <- splits_s2$testset

write.dbf(mod1_T2006_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s2.dbf")

write.dbf(mod1_T2006_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s3 <- splits_s3$trainset
mod1_T2006_90_s3 <- splits_s3$testset

write.dbf(mod1_T2006_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s3.dbf")

write.dbf(mod1_T2006_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s4 <- splits_s4$trainset
mod1_T2006_90_s4 <- splits_s4$testset

write.dbf(mod1_T2006_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s4.dbf")

write.dbf(mod1_T2006_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s5 <- splits_s5$trainset
mod1_T2006_90_s5 <- splits_s5$testset

write.dbf(mod1_T2006_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s5.dbf")

write.dbf(mod1_T2006_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s6 <- splits_s6$trainset
mod1_T2006_90_s6 <- splits_s6$testset

write.dbf(mod1_T2006_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s6.dbf")

write.dbf(mod1_T2006_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s7 <- splits_s7$trainset
mod1_T2006_90_s7 <- splits_s7$testset

write.dbf(mod1_T2006_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s7.dbf")

write.dbf(mod1_T2006_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s8 <- splits_s8$trainset
mod1_T2006_90_s8 <- splits_s8$testset

write.dbf(mod1_T2006_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s8.dbf")

write.dbf(mod1_T2006_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s9 <- splits_s9$trainset
mod1_T2006_90_s9 <- splits_s9$testset

write.dbf(mod1_T2006_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s9.dbf")

write.dbf(mod1_T2006_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2006_all_st_table)

mod1_T2006_10_s10 <- splits_s10$trainset
mod1_T2006_90_s10 <- splits_s10$testset

write.dbf(mod1_T2006_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s10.dbf")

write.dbf(mod1_T2006_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2006_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2006.csv", header=T) 
names(F_T2006_mod2p)
F_T2006_mod2p <- rename(F_T2006_mod2p, c(date="Date"))

F_T2006_mod2p <- subset(F_T2006_mod2p,F_T2006_mod2p$population >= "10")



# sort 
F_T2006_mod2p <- F_T2006_mod2p[order(F_T2006_mod2p$guid,F_T2006_mod2p$Date),]
T2006_weight <- T2006_weight[order(T2006_weight$guid,T2006_weight$Date),]

# merge two dataframes by ID
F_T2006_mod2 <- merge(F_T2006_mod2p,T2006_weight,by=c("guid","Date"))
names(F_T2006_mod2)
# mT2006n <-subset(mT2006,!duplicated(mT2006$guid))





#transfer all to Z-scores
F_T2006_mod2$zelev.x<-(F_T2006_mod2$elev.x-mean(F_T2006_All$elev.x))/sd(F_T2006_All$elev.x) 
F_T2006_mod2$zpopden<-(F_T2006_mod2$pop_sqkm-mean(F_T2006_All$pop_sqkm))/sd(F_T2006_All$pop_sqkm)
F_T2006_mod2$zospace<-(F_T2006_mod2$p_open-mean(F_T2006_All$p_open))/sd(F_T2006_All$p_open)
F_T2006_mod2$zdista1<- (F_T2006_mod2$A1_dist_km-mean(F_T2006_All$A1_dist_km))/sd(F_T2006_All$A1_dist_km)
F_T2006_mod2$zpsource<-(F_T2006_mod2$point_pm-mean(F_T2006_All$point_pm))/sd(F_T2006_All$point_pm)
F_T2006_mod2$zasource<- (F_T2006_mod2$area_pm -mean(F_T2006_All$area_pm))/sd(F_T2006_All$area_pm)
F_T2006_mod2$zvisib <- (F_T2006_mod2$visib_F-mean(F_T2006_All$visib_F))/sd(F_T2006_All$visib_F)
F_T2006_mod2$zwdsp<-(F_T2006_mod2$wdsp_F-mean(F_T2006_All$wdsp_F))/sd(F_T2006_All$wdsp_F)
F_T2006_mod2$zhumid<-(F_T2006_mod2$ah_gm3_F-mean(F_T2006_All$ah_gm3_F))/sd(F_T2006_All$ah_gm3_F)

F_T2006_mod2$zid<-1 #create id variable




F_T2006_mod2$zelev.x <-F_T2006_mod2$zelev.x*-1
F_T2006_mod2$zospace <- F_T2006_mod2$zospace*-1
F_T2006_mod2$zdista1 <- F_T2006_mod2$zdista1*-1
F_T2006_mod2$zvisib <- F_T2006_mod2$zvisib*-1
F_T2006_mod2$zwdsp <- F_T2006_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2006_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2006_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2006.dbf") 







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2007
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2007_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2007.csv", header=T)

T2007_weight <- na.omit(T2007_weight)

names(T2007_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2007_weight) #run modle
summary(w1)
T2007_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2007_weight$wt <- 1/T2007_weight$prob

T2007_weight$normwt <- T2007_weight$wt/mean(T2007_weight$wt)
T2007_weight <- rename(T2007_weight, c(date="Date"))

summary(T2007_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2007_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2007.csv", header=T) 
names(F_T2007_Allp)

F_T2007_Allp <- na.omit(F_T2007_Allp)

F_T2007_Allp <- subset(F_T2007_Allp,F_T2007_Allp$population >= "10")



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
F_T2007_All$zdista1<- (F_T2007_All$A1_dist_km-mean(F_T2007_All$A1_dist_km))/sd(F_T2007_All$A1_dist_km)
F_T2007_All$zpsource<-(F_T2007_All$point_pm-mean(F_T2007_All$point_pm))/sd(F_T2007_All$point_pm)
F_T2007_All$zasource<- (F_T2007_All$area_pm-mean(F_T2007_All$area_pm))/sd(F_T2007_All$area_pm)
F_T2007_All$zvisib<- (F_T2007_All$visib_F-mean(F_T2007_All$visib_F))/sd(F_T2007_All$visib_F)
F_T2007_All$zwdsp<-(F_T2007_All$wdsp_F-mean(F_T2007_All$wdsp_F))/sd(F_T2007_All$wdsp_F)
F_T2007_All$zhumid<-(F_T2007_All$ah_gm3_F-mean(F_T2007_All$ah_gm3_F))/sd(F_T2007_All$ah_gm3_F)

F_T2007_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2007_All$zelev.x <-F_T2007_All$zelev.x*-1
F_T2007_All$zospace <- F_T2007_All$zospace*-1
F_T2007_All$zdista1 <- F_T2007_All$zdista1*-1
F_T2007_All$zvisib <- F_T2007_All$zvisib*-1
F_T2007_All$zwdsp <- F_T2007_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2007_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2007_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2007.dbf") 





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

write.dbf(mod1_T2007_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s1.dbf")

write.dbf(mod1_T2007_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s2 <- splits_s2$trainset
mod1_T2007_90_s2 <- splits_s2$testset

write.dbf(mod1_T2007_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s2.dbf")

write.dbf(mod1_T2007_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s3 <- splits_s3$trainset
mod1_T2007_90_s3 <- splits_s3$testset

write.dbf(mod1_T2007_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s3.dbf")

write.dbf(mod1_T2007_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s4 <- splits_s4$trainset
mod1_T2007_90_s4 <- splits_s4$testset

write.dbf(mod1_T2007_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s4.dbf")

write.dbf(mod1_T2007_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s5 <- splits_s5$trainset
mod1_T2007_90_s5 <- splits_s5$testset

write.dbf(mod1_T2007_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s5.dbf")

write.dbf(mod1_T2007_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s6 <- splits_s6$trainset
mod1_T2007_90_s6 <- splits_s6$testset

write.dbf(mod1_T2007_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s6.dbf")

write.dbf(mod1_T2007_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s7 <- splits_s7$trainset
mod1_T2007_90_s7 <- splits_s7$testset

write.dbf(mod1_T2007_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s7.dbf")

write.dbf(mod1_T2007_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s8 <- splits_s8$trainset
mod1_T2007_90_s8 <- splits_s8$testset

write.dbf(mod1_T2007_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s8.dbf")

write.dbf(mod1_T2007_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s9 <- splits_s9$trainset
mod1_T2007_90_s9 <- splits_s9$testset

write.dbf(mod1_T2007_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s9.dbf")

write.dbf(mod1_T2007_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2007_all_st_table)

mod1_T2007_10_s10 <- splits_s10$trainset
mod1_T2007_90_s10 <- splits_s10$testset

write.dbf(mod1_T2007_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s10.dbf")

write.dbf(mod1_T2007_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2007_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2007.csv", header=T) 
names(F_T2007_mod2p)
F_T2007_mod2p <- rename(F_T2007_mod2p, c(date="Date"))

F_T2007_mod2p <- subset(F_T2007_mod2p,F_T2007_mod2p$population >= "10")



# sort 
F_T2007_mod2p <- F_T2007_mod2p[order(F_T2007_mod2p$guid,F_T2007_mod2p$Date),]
T2007_weight <- T2007_weight[order(T2007_weight$guid,T2007_weight$Date),]

# merge two dataframes by ID
F_T2007_mod2 <- merge(F_T2007_mod2p,T2007_weight,by=c("guid","Date"))
names(F_T2007_mod2)
# mT2007n <-subset(mT2007,!duplicated(mT2007$guid))





#transfer all to Z-scores
F_T2007_mod2$zelev.x<-(F_T2007_mod2$elev.x-mean(F_T2007_All$elev.x))/sd(F_T2007_All$elev.x) 
F_T2007_mod2$zpopden<-(F_T2007_mod2$pop_sqkm-mean(F_T2007_All$pop_sqkm))/sd(F_T2007_All$pop_sqkm)
F_T2007_mod2$zospace<-(F_T2007_mod2$p_open-mean(F_T2007_All$p_open))/sd(F_T2007_All$p_open)
F_T2007_mod2$zdista1<- (F_T2007_mod2$A1_dist_km-mean(F_T2007_All$A1_dist_km))/sd(F_T2007_All$A1_dist_km)
F_T2007_mod2$zpsource<-(F_T2007_mod2$point_pm-mean(F_T2007_All$point_pm))/sd(F_T2007_All$point_pm)
F_T2007_mod2$zasource<- (F_T2007_mod2$area_pm -mean(F_T2007_All$area_pm))/sd(F_T2007_All$area_pm)
F_T2007_mod2$zvisib <- (F_T2007_mod2$visib_F-mean(F_T2007_All$visib_F))/sd(F_T2007_All$visib_F)
F_T2007_mod2$zwdsp<-(F_T2007_mod2$wdsp_F-mean(F_T2007_All$wdsp_F))/sd(F_T2007_All$wdsp_F)
F_T2007_mod2$zhumid<-(F_T2007_mod2$ah_gm3_F-mean(F_T2007_All$ah_gm3_F))/sd(F_T2007_All$ah_gm3_F)

F_T2007_mod2$zid<-1 #create id variable




F_T2007_mod2$zelev.x <-F_T2007_mod2$zelev.x*-1
F_T2007_mod2$zospace <- F_T2007_mod2$zospace*-1
F_T2007_mod2$zdista1 <- F_T2007_mod2$zdista1*-1
F_T2007_mod2$zvisib <- F_T2007_mod2$zvisib*-1
F_T2007_mod2$zwdsp <- F_T2007_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2007_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2007_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2007.dbf") 






#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#2008
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

#import WEIGHT file
T2008_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2008.csv", header=T)

F_T2008_Allp <- na.omit(F_T2008_Allp)

names(T2008_weight)


T2008_weight <- na.omit(T2008_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2008_weight) #run modle
summary(w1)
T2008_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2008_weight$wt <- 1/T2008_weight$prob

T2008_weight$normwt <- T2008_weight$wt/mean(T2008_weight$wt)
T2008_weight <- rename(T2008_weight, c(date="Date"))

summary(T2008_weight$normwt)


#1.<<<<<<<<<<<<<<<<<<<<<<<<<PREPARE DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




# IMPORTS

#import mod1 DB (PM-AOD)
F_T2008_Allp <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN001_mod1/t2008.csv", header=T) 
names(F_T2008_Allp)


F_T2008_Allp <- na.omit(F_T2008_Allp)

F_T2008_Allp <- subset(F_T2008_Allp,F_T2008_Allp$population >= "10")



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
F_T2008_All$zdista1<- (F_T2008_All$A1_dist_km-mean(F_T2008_All$A1_dist_km))/sd(F_T2008_All$A1_dist_km)
F_T2008_All$zpsource<-(F_T2008_All$point_pm-mean(F_T2008_All$point_pm))/sd(F_T2008_All$point_pm)
F_T2008_All$zasource<- (F_T2008_All$area_pm-mean(F_T2008_All$area_pm))/sd(F_T2008_All$area_pm)
F_T2008_All$zvisib<- (F_T2008_All$visib_F-mean(F_T2008_All$visib_F))/sd(F_T2008_All$visib_F)
F_T2008_All$zwdsp<-(F_T2008_All$wdsp_F-mean(F_T2008_All$wdsp_F))/sd(F_T2008_All$wdsp_F)
F_T2008_All$zhumid<-(F_T2008_All$ah_gm3_F-mean(F_T2008_All$ah_gm3_F))/sd(F_T2008_All$ah_gm3_F)

F_T2008_All$zid<-1 #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
F_T2008_All$zelev.x <-F_T2008_All$zelev.x*-1
F_T2008_All$zospace <- F_T2008_All$zospace*-1
F_T2008_All$zdista1 <- F_T2008_All$zdista1*-1
F_T2008_All$zvisib <- F_T2008_All$zvisib*-1
F_T2008_All$zwdsp <- F_T2008_All$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2008_All)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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


write.dbf(F_2008_all_st_table,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2008.dbf") 





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

write.dbf(mod1_T2008_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s1.dbf")

write.dbf(mod1_T2008_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s1.dbf")


#s2
splits_s2 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s2 <- splits_s2$trainset
mod1_T2008_90_s2 <- splits_s2$testset

write.dbf(mod1_T2008_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s2.dbf")

write.dbf(mod1_T2008_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s2.dbf")


#s3
splits_s3 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s3 <- splits_s3$trainset
mod1_T2008_90_s3 <- splits_s3$testset

write.dbf(mod1_T2008_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s3.dbf")

write.dbf(mod1_T2008_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s3.dbf")


#s4
splits_s4 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s4 <- splits_s4$trainset
mod1_T2008_90_s4 <- splits_s4$testset

write.dbf(mod1_T2008_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s4.dbf")

write.dbf(mod1_T2008_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s4.dbf")


#s5
splits_s5 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s5 <- splits_s5$trainset
mod1_T2008_90_s5 <- splits_s5$testset

write.dbf(mod1_T2008_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s5.dbf")

write.dbf(mod1_T2008_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s5.dbf")


#s6
splits_s6 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s6 <- splits_s6$trainset
mod1_T2008_90_s6 <- splits_s6$testset

write.dbf(mod1_T2008_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s6.dbf")

write.dbf(mod1_T2008_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s6.dbf")


#s7
splits_s7 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s7 <- splits_s7$trainset
mod1_T2008_90_s7 <- splits_s7$testset

write.dbf(mod1_T2008_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s7.dbf")

write.dbf(mod1_T2008_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s7.dbf")


#s8
splits_s8 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s8 <- splits_s8$trainset
mod1_T2008_90_s8 <- splits_s8$testset

write.dbf(mod1_T2008_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s8.dbf")

write.dbf(mod1_T2008_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s8.dbf")


#s9
splits_s9 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s9 <- splits_s9$trainset
mod1_T2008_90_s9 <- splits_s9$testset

write.dbf(mod1_T2008_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s9.dbf")

write.dbf(mod1_T2008_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s9.dbf")


#s10
splits_s10 <- splitdf(F_2008_all_st_table)

mod1_T2008_10_s10 <- splits_s10$trainset
mod1_T2008_90_s10 <- splits_s10$testset

write.dbf(mod1_T2008_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s10.dbf")

write.dbf(mod1_T2008_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s10.dbf")




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> export larger mod2 dataset




#import mod2 DB (Just AOD)
F_T2008_mod2p <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/y2008.csv", header=T) 
names(F_T2008_mod2p)
F_T2008_mod2p <- rename(F_T2008_mod2p, c(date="Date"))

F_T2008_mod2p <- subset(F_T2008_mod2p,F_T2008_mod2p$population >= "10")



# sort 
F_T2008_mod2p <- F_T2008_mod2p[order(F_T2008_mod2p$guid,F_T2008_mod2p$Date),]
T2008_weight <- T2008_weight[order(T2008_weight$guid,T2008_weight$Date),]

# merge two dataframes by ID
F_T2008_mod2 <- merge(F_T2008_mod2p,T2008_weight,by=c("guid","Date"))
names(F_T2008_mod2)
# mT2008n <-subset(mT2008,!duplicated(mT2008$guid))





#transfer all to Z-scores
F_T2008_mod2$zelev.x<-(F_T2008_mod2$elev.x-mean(F_T2008_All$elev.x))/sd(F_T2008_All$elev.x) 
F_T2008_mod2$zpopden<-(F_T2008_mod2$pop_sqkm-mean(F_T2008_All$pop_sqkm))/sd(F_T2008_All$pop_sqkm)
F_T2008_mod2$zospace<-(F_T2008_mod2$p_open-mean(F_T2008_All$p_open))/sd(F_T2008_All$p_open)
F_T2008_mod2$zdista1<- (F_T2008_mod2$A1_dist_km-mean(F_T2008_All$A1_dist_km))/sd(F_T2008_All$A1_dist_km)
F_T2008_mod2$zpsource<-(F_T2008_mod2$point_pm-mean(F_T2008_All$point_pm))/sd(F_T2008_All$point_pm)
F_T2008_mod2$zasource<- (F_T2008_mod2$area_pm -mean(F_T2008_All$area_pm))/sd(F_T2008_All$area_pm)
F_T2008_mod2$zvisib <- (F_T2008_mod2$visib_F-mean(F_T2008_All$visib_F))/sd(F_T2008_All$visib_F)
F_T2008_mod2$zwdsp<-(F_T2008_mod2$wdsp_F-mean(F_T2008_All$wdsp_F))/sd(F_T2008_All$wdsp_F)
F_T2008_mod2$zhumid<-(F_T2008_mod2$ah_gm3_F-mean(F_T2008_All$ah_gm3_F))/sd(F_T2008_All$ah_gm3_F)

F_T2008_mod2$zid<-1 #create id variable




F_T2008_mod2$zelev.x <-F_T2008_mod2$zelev.x*-1
F_T2008_mod2$zospace <- F_T2008_mod2$zospace*-1
F_T2008_mod2$zdista1 <- F_T2008_mod2$zdista1*-1
F_T2008_mod2$zvisib <- F_T2008_mod2$zvisib*-1
F_T2008_mod2$zwdsp <- F_T2008_mod2$zwdsp*-1




#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(F_T2008_mod2)
s.mat <- cbind(zelev.x,zpopden,zospace,zpsource,zdista1,zasource)
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

write.dbf(F_2008_all_st_table_mod2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN003_mod2/sas/out2008.dbf") 

