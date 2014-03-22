
library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)



#load split function

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}



#load 2000

#import mod1 DB (PM-AOD)

F_T2000_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2000.dbf") 
names(F_T2000_Allp)



pm2000 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2000.csv", header=T) 
names(F_T2000_Allp)


#sort
F_T2000_Allp<-F_T2000_Allp[order(F_T2000_Allp$SiteCode,F_T2000_Allp$Date),]
pm2000<-pm2000[order(pm2000$SiteCode,pm2000$Date),] 
#merge 
MERGED_DATA2000<-merge(pm2000,F_T2000_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2000$EPACode.y <- NULL 
MERGED_DATA2000$PM25.y <- NULL
MERGED_DATA2000$Lat_PM.y <- NULL
MERGED_DATA2000$Long_PM.y <- NULL
MERGED_DATA2000$guid.y <- NULL
library(reshape)
MERGED_DATA2000<-rename(MERGED_DATA2000,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s1 <- splits_s1$trainset
mod1_T2000_90_s1 <- splits_s1$testset
names(mod1_T2000_90_s1)

mod1_T2000_90_s1 <- subset(mod1_T2000_90_s1, mod1_T2000_90_s1$AOD != "NA")
names(mod1_T2000_90_s1)


write.dbf(mod1_T2000_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s1.dbf")

mod1_T2000_10_s1 <- subset(mod1_T2000_10_s1, mod1_T2000_10_s1$AOD != "NA")


write.dbf(mod1_T2000_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s1_short.dbf")


write.dbf(mod1_T2000_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s2 <- splits_s2$trainset
mod1_T2000_90_s2 <- splits_s2$testset
names(mod1_T2000_90_s2)

mod1_T2000_90_s2 <- subset(mod1_T2000_90_s2, mod1_T2000_90_s2$AOD != "NA")
names(mod1_T2000_90_s2)
mod1_T2000_90_s2$EPACode.y <- NULL 
mod1_T2000_90_s2$PM25.y <- NULL
mod1_T2000_90_s2$Lat_PM.y <- NULL
mod1_T2000_90_s2$Long_PM.y <- NULL
mod1_T2000_90_s2$guid.y <- NULL
library(reshape)
mod1_T2000_90_s2<-rename(mod1_T2000_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s2.dbf")

mod1_T2000_10_s2 <- subset(mod1_T2000_10_s2, mod1_T2000_10_s2$AOD != "NA")


write.dbf(mod1_T2000_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s2_short.dbf")


write.dbf(mod1_T2000_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s3 <- splits_s3$trainset
mod1_T2000_90_s3 <- splits_s3$testset
names(mod1_T2000_90_s3)

mod1_T2000_90_s3 <- subset(mod1_T2000_90_s3, mod1_T2000_90_s3$AOD != "NA")
names(mod1_T2000_90_s3)
mod1_T2000_90_s3$EPACode.y <- NULL 
mod1_T2000_90_s3$PM25.y <- NULL
mod1_T2000_90_s3$Lat_PM.y <- NULL
mod1_T2000_90_s3$Long_PM.y <- NULL
mod1_T2000_90_s3$guid.y <- NULL
library(reshape)
mod1_T2000_90_s3<-rename(mod1_T2000_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s3.dbf")

mod1_T2000_10_s3 <- subset(mod1_T2000_10_s3, mod1_T2000_10_s3$AOD != "NA")


write.dbf(mod1_T2000_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s3_short.dbf")


write.dbf(mod1_T2000_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s4 <- splits_s4$trainset
mod1_T2000_90_s4 <- splits_s4$testset
names(mod1_T2000_90_s4)

mod1_T2000_90_s4 <- subset(mod1_T2000_90_s4, mod1_T2000_90_s4$AOD != "NA")
names(mod1_T2000_90_s4)
mod1_T2000_90_s4$EPACode.y <- NULL 
mod1_T2000_90_s4$PM25.y <- NULL
mod1_T2000_90_s4$Lat_PM.y <- NULL
mod1_T2000_90_s4$Long_PM.y <- NULL
mod1_T2000_90_s4$guid.y <- NULL
library(reshape)
mod1_T2000_90_s4<-rename(mod1_T2000_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s4.dbf")

mod1_T2000_10_s4 <- subset(mod1_T2000_10_s4, mod1_T2000_10_s4$AOD != "NA")


write.dbf(mod1_T2000_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s4_short.dbf")


write.dbf(mod1_T2000_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s5 <- splits_s5$trainset
mod1_T2000_90_s5 <- splits_s5$testset
names(mod1_T2000_90_s5)

mod1_T2000_90_s5 <- subset(mod1_T2000_90_s5, mod1_T2000_90_s5$AOD != "NA")
names(mod1_T2000_90_s5)
mod1_T2000_90_s5$EPACode.y <- NULL 
mod1_T2000_90_s5$PM25.y <- NULL
mod1_T2000_90_s5$Lat_PM.y <- NULL
mod1_T2000_90_s5$Long_PM.y <- NULL
mod1_T2000_90_s5$guid.y <- NULL
library(reshape)
mod1_T2000_90_s5<-rename(mod1_T2000_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s5.dbf")

mod1_T2000_10_s5 <- subset(mod1_T2000_10_s5, mod1_T2000_10_s5$AOD != "NA")


write.dbf(mod1_T2000_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s5_short.dbf")


write.dbf(mod1_T2000_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s6 <- splits_s6$trainset
mod1_T2000_90_s6 <- splits_s6$testset
names(mod1_T2000_90_s6)

mod1_T2000_90_s6 <- subset(mod1_T2000_90_s6, mod1_T2000_90_s6$AOD != "NA")
names(mod1_T2000_90_s6)
mod1_T2000_90_s6$EPACode.y <- NULL 
mod1_T2000_90_s6$PM25.y <- NULL
mod1_T2000_90_s6$Lat_PM.y <- NULL
mod1_T2000_90_s6$Long_PM.y <- NULL
mod1_T2000_90_s6$guid.y <- NULL
library(reshape)
mod1_T2000_90_s6<-rename(mod1_T2000_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s6.dbf")

mod1_T2000_10_s6 <- subset(mod1_T2000_10_s6, mod1_T2000_10_s6$AOD != "NA")


write.dbf(mod1_T2000_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s6_short.dbf")


write.dbf(mod1_T2000_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s7 <- splits_s7$trainset
mod1_T2000_90_s7 <- splits_s7$testset
names(mod1_T2000_90_s7)

mod1_T2000_90_s7 <- subset(mod1_T2000_90_s7, mod1_T2000_90_s7$AOD != "NA")
names(mod1_T2000_90_s7)
mod1_T2000_90_s7$EPACode.y <- NULL 
mod1_T2000_90_s7$PM25.y <- NULL
mod1_T2000_90_s7$Lat_PM.y <- NULL
mod1_T2000_90_s7$Long_PM.y <- NULL
mod1_T2000_90_s7$guid.y <- NULL
library(reshape)
mod1_T2000_90_s7<-rename(mod1_T2000_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s7.dbf")

mod1_T2000_10_s7 <- subset(mod1_T2000_10_s7, mod1_T2000_10_s7$AOD != "NA")


write.dbf(mod1_T2000_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s7_short.dbf")


write.dbf(mod1_T2000_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s8 <- splits_s8$trainset
mod1_T2000_90_s8 <- splits_s8$testset
names(mod1_T2000_90_s8)

mod1_T2000_90_s8 <- subset(mod1_T2000_90_s8, mod1_T2000_90_s8$AOD != "NA")
names(mod1_T2000_90_s8)
mod1_T2000_90_s8$EPACode.y <- NULL 
mod1_T2000_90_s8$PM25.y <- NULL
mod1_T2000_90_s8$Lat_PM.y <- NULL
mod1_T2000_90_s8$Long_PM.y <- NULL
mod1_T2000_90_s8$guid.y <- NULL
library(reshape)
mod1_T2000_90_s8<-rename(mod1_T2000_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s8.dbf")

mod1_T2000_10_s8 <- subset(mod1_T2000_10_s8, mod1_T2000_10_s8$AOD != "NA")


write.dbf(mod1_T2000_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s8_short.dbf")


write.dbf(mod1_T2000_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s9 <- splits_s9$trainset
mod1_T2000_90_s9 <- splits_s9$testset
names(mod1_T2000_90_s9)

mod1_T2000_90_s9 <- subset(mod1_T2000_90_s9, mod1_T2000_90_s9$AOD != "NA")
names(mod1_T2000_90_s9)
mod1_T2000_90_s9$EPACode.y <- NULL 
mod1_T2000_90_s9$PM25.y <- NULL
mod1_T2000_90_s9$Lat_PM.y <- NULL
mod1_T2000_90_s9$Long_PM.y <- NULL
mod1_T2000_90_s9$guid.y <- NULL
library(reshape)
mod1_T2000_90_s9<-rename(mod1_T2000_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s9.dbf")

mod1_T2000_10_s9 <- subset(mod1_T2000_10_s9, mod1_T2000_10_s9$AOD != "NA")


write.dbf(mod1_T2000_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s9_short.dbf")


write.dbf(mod1_T2000_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2000)

mod1_T2000_10_s10 <- splits_s10$trainset
mod1_T2000_90_s10 <- splits_s10$testset
names(mod1_T2000_90_s10)

mod1_T2000_90_s10 <- subset(mod1_T2000_90_s10, mod1_T2000_90_s10$AOD != "NA")
names(mod1_T2000_90_s10)
mod1_T2000_90_s10$EPACode.y <- NULL 
mod1_T2000_90_s10$PM25.y <- NULL
mod1_T2000_90_s10$Lat_PM.y <- NULL
mod1_T2000_90_s10$Long_PM.y <- NULL
mod1_T2000_90_s10$guid.y <- NULL
library(reshape)
mod1_T2000_90_s10<-rename(mod1_T2000_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2000_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s10.dbf")

mod1_T2000_10_s10 <- subset(mod1_T2000_10_s10, mod1_T2000_10_s10$AOD != "NA")


write.dbf(mod1_T2000_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_10_s10_short.dbf")


write.dbf(mod1_T2000_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2000_90_s10.dbf")







#load 2001

#import mod1 DB (PM-AOD)

F_T2001_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2001.dbf") 
names(F_T2001_Allp)



pm2001 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2001.csv", header=T) 
names(F_T2001_Allp)


#sort
F_T2001_Allp<-F_T2001_Allp[order(F_T2001_Allp$SiteCode,F_T2001_Allp$Date),]
pm2001<-pm2001[order(pm2001$SiteCode,pm2001$Date),] 
#merge 
MERGED_DATA2001<-merge(pm2001,F_T2001_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2001$EPACode.y <- NULL 
MERGED_DATA2001$PM25.y <- NULL
MERGED_DATA2001$Lat_PM.y <- NULL
MERGED_DATA2001$Long_PM.y <- NULL
MERGED_DATA2001$guid.y <- NULL
library(reshape)
MERGED_DATA2001<-rename(MERGED_DATA2001,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s1 <- splits_s1$trainset
mod1_T2001_90_s1 <- splits_s1$testset
names(mod1_T2001_90_s1)

mod1_T2001_90_s1 <- subset(mod1_T2001_90_s1, mod1_T2001_90_s1$AOD != "NA")
names(mod1_T2001_90_s1)


write.dbf(mod1_T2001_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s1.dbf")

mod1_T2001_10_s1 <- subset(mod1_T2001_10_s1, mod1_T2001_10_s1$AOD != "NA")


write.dbf(mod1_T2001_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s1_short.dbf")


write.dbf(mod1_T2001_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s2 <- splits_s2$trainset
mod1_T2001_90_s2 <- splits_s2$testset
names(mod1_T2001_90_s2)

mod1_T2001_90_s2 <- subset(mod1_T2001_90_s2, mod1_T2001_90_s2$AOD != "NA")
names(mod1_T2001_90_s2)
mod1_T2001_90_s2$EPACode.y <- NULL 
mod1_T2001_90_s2$PM25.y <- NULL
mod1_T2001_90_s2$Lat_PM.y <- NULL
mod1_T2001_90_s2$Long_PM.y <- NULL
mod1_T2001_90_s2$guid.y <- NULL
library(reshape)
mod1_T2001_90_s2<-rename(mod1_T2001_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s2.dbf")

mod1_T2001_10_s2 <- subset(mod1_T2001_10_s2, mod1_T2001_10_s2$AOD != "NA")


write.dbf(mod1_T2001_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s2_short.dbf")


write.dbf(mod1_T2001_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s3 <- splits_s3$trainset
mod1_T2001_90_s3 <- splits_s3$testset
names(mod1_T2001_90_s3)

mod1_T2001_90_s3 <- subset(mod1_T2001_90_s3, mod1_T2001_90_s3$AOD != "NA")
names(mod1_T2001_90_s3)
mod1_T2001_90_s3$EPACode.y <- NULL 
mod1_T2001_90_s3$PM25.y <- NULL
mod1_T2001_90_s3$Lat_PM.y <- NULL
mod1_T2001_90_s3$Long_PM.y <- NULL
mod1_T2001_90_s3$guid.y <- NULL
library(reshape)
mod1_T2001_90_s3<-rename(mod1_T2001_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s3.dbf")

mod1_T2001_10_s3 <- subset(mod1_T2001_10_s3, mod1_T2001_10_s3$AOD != "NA")


write.dbf(mod1_T2001_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s3_short.dbf")


write.dbf(mod1_T2001_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s4 <- splits_s4$trainset
mod1_T2001_90_s4 <- splits_s4$testset
names(mod1_T2001_90_s4)

mod1_T2001_90_s4 <- subset(mod1_T2001_90_s4, mod1_T2001_90_s4$AOD != "NA")
names(mod1_T2001_90_s4)
mod1_T2001_90_s4$EPACode.y <- NULL 
mod1_T2001_90_s4$PM25.y <- NULL
mod1_T2001_90_s4$Lat_PM.y <- NULL
mod1_T2001_90_s4$Long_PM.y <- NULL
mod1_T2001_90_s4$guid.y <- NULL
library(reshape)
mod1_T2001_90_s4<-rename(mod1_T2001_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s4.dbf")

mod1_T2001_10_s4 <- subset(mod1_T2001_10_s4, mod1_T2001_10_s4$AOD != "NA")


write.dbf(mod1_T2001_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s4_short.dbf")


write.dbf(mod1_T2001_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s5 <- splits_s5$trainset
mod1_T2001_90_s5 <- splits_s5$testset
names(mod1_T2001_90_s5)

mod1_T2001_90_s5 <- subset(mod1_T2001_90_s5, mod1_T2001_90_s5$AOD != "NA")
names(mod1_T2001_90_s5)
mod1_T2001_90_s5$EPACode.y <- NULL 
mod1_T2001_90_s5$PM25.y <- NULL
mod1_T2001_90_s5$Lat_PM.y <- NULL
mod1_T2001_90_s5$Long_PM.y <- NULL
mod1_T2001_90_s5$guid.y <- NULL
library(reshape)
mod1_T2001_90_s5<-rename(mod1_T2001_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s5.dbf")

mod1_T2001_10_s5 <- subset(mod1_T2001_10_s5, mod1_T2001_10_s5$AOD != "NA")


write.dbf(mod1_T2001_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s5_short.dbf")


write.dbf(mod1_T2001_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s6 <- splits_s6$trainset
mod1_T2001_90_s6 <- splits_s6$testset
names(mod1_T2001_90_s6)

mod1_T2001_90_s6 <- subset(mod1_T2001_90_s6, mod1_T2001_90_s6$AOD != "NA")
names(mod1_T2001_90_s6)
mod1_T2001_90_s6$EPACode.y <- NULL 
mod1_T2001_90_s6$PM25.y <- NULL
mod1_T2001_90_s6$Lat_PM.y <- NULL
mod1_T2001_90_s6$Long_PM.y <- NULL
mod1_T2001_90_s6$guid.y <- NULL
library(reshape)
mod1_T2001_90_s6<-rename(mod1_T2001_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s6.dbf")

mod1_T2001_10_s6 <- subset(mod1_T2001_10_s6, mod1_T2001_10_s6$AOD != "NA")


write.dbf(mod1_T2001_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s6_short.dbf")


write.dbf(mod1_T2001_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s7 <- splits_s7$trainset
mod1_T2001_90_s7 <- splits_s7$testset
names(mod1_T2001_90_s7)

mod1_T2001_90_s7 <- subset(mod1_T2001_90_s7, mod1_T2001_90_s7$AOD != "NA")
names(mod1_T2001_90_s7)
mod1_T2001_90_s7$EPACode.y <- NULL 
mod1_T2001_90_s7$PM25.y <- NULL
mod1_T2001_90_s7$Lat_PM.y <- NULL
mod1_T2001_90_s7$Long_PM.y <- NULL
mod1_T2001_90_s7$guid.y <- NULL
library(reshape)
mod1_T2001_90_s7<-rename(mod1_T2001_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s7.dbf")

mod1_T2001_10_s7 <- subset(mod1_T2001_10_s7, mod1_T2001_10_s7$AOD != "NA")


write.dbf(mod1_T2001_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s7_short.dbf")


write.dbf(mod1_T2001_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s8 <- splits_s8$trainset
mod1_T2001_90_s8 <- splits_s8$testset
names(mod1_T2001_90_s8)

mod1_T2001_90_s8 <- subset(mod1_T2001_90_s8, mod1_T2001_90_s8$AOD != "NA")
names(mod1_T2001_90_s8)
mod1_T2001_90_s8$EPACode.y <- NULL 
mod1_T2001_90_s8$PM25.y <- NULL
mod1_T2001_90_s8$Lat_PM.y <- NULL
mod1_T2001_90_s8$Long_PM.y <- NULL
mod1_T2001_90_s8$guid.y <- NULL
library(reshape)
mod1_T2001_90_s8<-rename(mod1_T2001_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s8.dbf")

mod1_T2001_10_s8 <- subset(mod1_T2001_10_s8, mod1_T2001_10_s8$AOD != "NA")


write.dbf(mod1_T2001_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s8_short.dbf")


write.dbf(mod1_T2001_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s9 <- splits_s9$trainset
mod1_T2001_90_s9 <- splits_s9$testset
names(mod1_T2001_90_s9)

mod1_T2001_90_s9 <- subset(mod1_T2001_90_s9, mod1_T2001_90_s9$AOD != "NA")
names(mod1_T2001_90_s9)
mod1_T2001_90_s9$EPACode.y <- NULL 
mod1_T2001_90_s9$PM25.y <- NULL
mod1_T2001_90_s9$Lat_PM.y <- NULL
mod1_T2001_90_s9$Long_PM.y <- NULL
mod1_T2001_90_s9$guid.y <- NULL
library(reshape)
mod1_T2001_90_s9<-rename(mod1_T2001_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s9.dbf")

mod1_T2001_10_s9 <- subset(mod1_T2001_10_s9, mod1_T2001_10_s9$AOD != "NA")


write.dbf(mod1_T2001_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s9_short.dbf")


write.dbf(mod1_T2001_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2001)

mod1_T2001_10_s10 <- splits_s10$trainset
mod1_T2001_90_s10 <- splits_s10$testset
names(mod1_T2001_90_s10)

mod1_T2001_90_s10 <- subset(mod1_T2001_90_s10, mod1_T2001_90_s10$AOD != "NA")
names(mod1_T2001_90_s10)
mod1_T2001_90_s10$EPACode.y <- NULL 
mod1_T2001_90_s10$PM25.y <- NULL
mod1_T2001_90_s10$Lat_PM.y <- NULL
mod1_T2001_90_s10$Long_PM.y <- NULL
mod1_T2001_90_s10$guid.y <- NULL
library(reshape)
mod1_T2001_90_s10<-rename(mod1_T2001_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2001_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s10.dbf")

mod1_T2001_10_s10 <- subset(mod1_T2001_10_s10, mod1_T2001_10_s10$AOD != "NA")


write.dbf(mod1_T2001_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_10_s10_short.dbf")


write.dbf(mod1_T2001_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2001_90_s10.dbf")







#load 2002

#import mod1 DB (PM-AOD)

F_T2002_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2002.dbf") 
names(F_T2002_Allp)



pm2002 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2002.csv", header=T) 
names(F_T2002_Allp)


#sort
F_T2002_Allp<-F_T2002_Allp[order(F_T2002_Allp$SiteCode,F_T2002_Allp$Date),]
pm2002<-pm2002[order(pm2002$SiteCode,pm2002$Date),] 
#merge 
MERGED_DATA2002<-merge(pm2002,F_T2002_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2002$EPACode.y <- NULL 
MERGED_DATA2002$PM25.y <- NULL
MERGED_DATA2002$Lat_PM.y <- NULL
MERGED_DATA2002$Long_PM.y <- NULL
MERGED_DATA2002$guid.y <- NULL
library(reshape)
MERGED_DATA2002<-rename(MERGED_DATA2002,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s1 <- splits_s1$trainset
mod1_T2002_90_s1 <- splits_s1$testset
names(mod1_T2002_90_s1)

mod1_T2002_90_s1 <- subset(mod1_T2002_90_s1, mod1_T2002_90_s1$AOD != "NA")
names(mod1_T2002_90_s1)


write.dbf(mod1_T2002_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s1.dbf")

mod1_T2002_10_s1 <- subset(mod1_T2002_10_s1, mod1_T2002_10_s1$AOD != "NA")


write.dbf(mod1_T2002_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s1_short.dbf")


write.dbf(mod1_T2002_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s2 <- splits_s2$trainset
mod1_T2002_90_s2 <- splits_s2$testset
names(mod1_T2002_90_s2)

mod1_T2002_90_s2 <- subset(mod1_T2002_90_s2, mod1_T2002_90_s2$AOD != "NA")
names(mod1_T2002_90_s2)
mod1_T2002_90_s2$EPACode.y <- NULL 
mod1_T2002_90_s2$PM25.y <- NULL
mod1_T2002_90_s2$Lat_PM.y <- NULL
mod1_T2002_90_s2$Long_PM.y <- NULL
mod1_T2002_90_s2$guid.y <- NULL
library(reshape)
mod1_T2002_90_s2<-rename(mod1_T2002_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s2.dbf")

mod1_T2002_10_s2 <- subset(mod1_T2002_10_s2, mod1_T2002_10_s2$AOD != "NA")


write.dbf(mod1_T2002_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s2_short.dbf")


write.dbf(mod1_T2002_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s3 <- splits_s3$trainset
mod1_T2002_90_s3 <- splits_s3$testset
names(mod1_T2002_90_s3)

mod1_T2002_90_s3 <- subset(mod1_T2002_90_s3, mod1_T2002_90_s3$AOD != "NA")
names(mod1_T2002_90_s3)
mod1_T2002_90_s3$EPACode.y <- NULL 
mod1_T2002_90_s3$PM25.y <- NULL
mod1_T2002_90_s3$Lat_PM.y <- NULL
mod1_T2002_90_s3$Long_PM.y <- NULL
mod1_T2002_90_s3$guid.y <- NULL
library(reshape)
mod1_T2002_90_s3<-rename(mod1_T2002_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s3.dbf")

mod1_T2002_10_s3 <- subset(mod1_T2002_10_s3, mod1_T2002_10_s3$AOD != "NA")


write.dbf(mod1_T2002_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s3_short.dbf")


write.dbf(mod1_T2002_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s4 <- splits_s4$trainset
mod1_T2002_90_s4 <- splits_s4$testset
names(mod1_T2002_90_s4)

mod1_T2002_90_s4 <- subset(mod1_T2002_90_s4, mod1_T2002_90_s4$AOD != "NA")
names(mod1_T2002_90_s4)
mod1_T2002_90_s4$EPACode.y <- NULL 
mod1_T2002_90_s4$PM25.y <- NULL
mod1_T2002_90_s4$Lat_PM.y <- NULL
mod1_T2002_90_s4$Long_PM.y <- NULL
mod1_T2002_90_s4$guid.y <- NULL
library(reshape)
mod1_T2002_90_s4<-rename(mod1_T2002_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s4.dbf")

mod1_T2002_10_s4 <- subset(mod1_T2002_10_s4, mod1_T2002_10_s4$AOD != "NA")


write.dbf(mod1_T2002_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s4_short.dbf")


write.dbf(mod1_T2002_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s5 <- splits_s5$trainset
mod1_T2002_90_s5 <- splits_s5$testset
names(mod1_T2002_90_s5)

mod1_T2002_90_s5 <- subset(mod1_T2002_90_s5, mod1_T2002_90_s5$AOD != "NA")
names(mod1_T2002_90_s5)
mod1_T2002_90_s5$EPACode.y <- NULL 
mod1_T2002_90_s5$PM25.y <- NULL
mod1_T2002_90_s5$Lat_PM.y <- NULL
mod1_T2002_90_s5$Long_PM.y <- NULL
mod1_T2002_90_s5$guid.y <- NULL
library(reshape)
mod1_T2002_90_s5<-rename(mod1_T2002_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s5.dbf")

mod1_T2002_10_s5 <- subset(mod1_T2002_10_s5, mod1_T2002_10_s5$AOD != "NA")


write.dbf(mod1_T2002_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s5_short.dbf")


write.dbf(mod1_T2002_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s6 <- splits_s6$trainset
mod1_T2002_90_s6 <- splits_s6$testset
names(mod1_T2002_90_s6)

mod1_T2002_90_s6 <- subset(mod1_T2002_90_s6, mod1_T2002_90_s6$AOD != "NA")
names(mod1_T2002_90_s6)
mod1_T2002_90_s6$EPACode.y <- NULL 
mod1_T2002_90_s6$PM25.y <- NULL
mod1_T2002_90_s6$Lat_PM.y <- NULL
mod1_T2002_90_s6$Long_PM.y <- NULL
mod1_T2002_90_s6$guid.y <- NULL
library(reshape)
mod1_T2002_90_s6<-rename(mod1_T2002_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s6.dbf")

mod1_T2002_10_s6 <- subset(mod1_T2002_10_s6, mod1_T2002_10_s6$AOD != "NA")


write.dbf(mod1_T2002_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s6_short.dbf")


write.dbf(mod1_T2002_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s7 <- splits_s7$trainset
mod1_T2002_90_s7 <- splits_s7$testset
names(mod1_T2002_90_s7)

mod1_T2002_90_s7 <- subset(mod1_T2002_90_s7, mod1_T2002_90_s7$AOD != "NA")
names(mod1_T2002_90_s7)
mod1_T2002_90_s7$EPACode.y <- NULL 
mod1_T2002_90_s7$PM25.y <- NULL
mod1_T2002_90_s7$Lat_PM.y <- NULL
mod1_T2002_90_s7$Long_PM.y <- NULL
mod1_T2002_90_s7$guid.y <- NULL
library(reshape)
mod1_T2002_90_s7<-rename(mod1_T2002_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s7.dbf")

mod1_T2002_10_s7 <- subset(mod1_T2002_10_s7, mod1_T2002_10_s7$AOD != "NA")


write.dbf(mod1_T2002_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s7_short.dbf")


write.dbf(mod1_T2002_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s8 <- splits_s8$trainset
mod1_T2002_90_s8 <- splits_s8$testset
names(mod1_T2002_90_s8)

mod1_T2002_90_s8 <- subset(mod1_T2002_90_s8, mod1_T2002_90_s8$AOD != "NA")
names(mod1_T2002_90_s8)
mod1_T2002_90_s8$EPACode.y <- NULL 
mod1_T2002_90_s8$PM25.y <- NULL
mod1_T2002_90_s8$Lat_PM.y <- NULL
mod1_T2002_90_s8$Long_PM.y <- NULL
mod1_T2002_90_s8$guid.y <- NULL
library(reshape)
mod1_T2002_90_s8<-rename(mod1_T2002_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s8.dbf")

mod1_T2002_10_s8 <- subset(mod1_T2002_10_s8, mod1_T2002_10_s8$AOD != "NA")


write.dbf(mod1_T2002_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s8_short.dbf")


write.dbf(mod1_T2002_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s9 <- splits_s9$trainset
mod1_T2002_90_s9 <- splits_s9$testset
names(mod1_T2002_90_s9)

mod1_T2002_90_s9 <- subset(mod1_T2002_90_s9, mod1_T2002_90_s9$AOD != "NA")
names(mod1_T2002_90_s9)
mod1_T2002_90_s9$EPACode.y <- NULL 
mod1_T2002_90_s9$PM25.y <- NULL
mod1_T2002_90_s9$Lat_PM.y <- NULL
mod1_T2002_90_s9$Long_PM.y <- NULL
mod1_T2002_90_s9$guid.y <- NULL
library(reshape)
mod1_T2002_90_s9<-rename(mod1_T2002_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s9.dbf")

mod1_T2002_10_s9 <- subset(mod1_T2002_10_s9, mod1_T2002_10_s9$AOD != "NA")


write.dbf(mod1_T2002_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s9_short.dbf")


write.dbf(mod1_T2002_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2002)

mod1_T2002_10_s10 <- splits_s10$trainset
mod1_T2002_90_s10 <- splits_s10$testset
names(mod1_T2002_90_s10)

mod1_T2002_90_s10 <- subset(mod1_T2002_90_s10, mod1_T2002_90_s10$AOD != "NA")
names(mod1_T2002_90_s10)
mod1_T2002_90_s10$EPACode.y <- NULL 
mod1_T2002_90_s10$PM25.y <- NULL
mod1_T2002_90_s10$Lat_PM.y <- NULL
mod1_T2002_90_s10$Long_PM.y <- NULL
mod1_T2002_90_s10$guid.y <- NULL
library(reshape)
mod1_T2002_90_s10<-rename(mod1_T2002_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2002_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s10.dbf")

mod1_T2002_10_s10 <- subset(mod1_T2002_10_s10, mod1_T2002_10_s10$AOD != "NA")


write.dbf(mod1_T2002_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_10_s10_short.dbf")


write.dbf(mod1_T2002_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2002_90_s10.dbf")






#load 2003

#import mod1 DB (PM-AOD)

F_T2003_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2003.dbf") 
names(F_T2003_Allp)



pm2003 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2003.csv", header=T) 
names(F_T2003_Allp)


#sort
F_T2003_Allp<-F_T2003_Allp[order(F_T2003_Allp$SiteCode,F_T2003_Allp$Date),]
pm2003<-pm2003[order(pm2003$SiteCode,pm2003$Date),] 
#merge 
MERGED_DATA2003<-merge(pm2003,F_T2003_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2003$EPACode.y <- NULL 
MERGED_DATA2003$PM25.y <- NULL
MERGED_DATA2003$Lat_PM.y <- NULL
MERGED_DATA2003$Long_PM.y <- NULL
MERGED_DATA2003$guid.y <- NULL
library(reshape)
MERGED_DATA2003<-rename(MERGED_DATA2003,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s1 <- splits_s1$trainset
mod1_T2003_90_s1 <- splits_s1$testset
names(mod1_T2003_90_s1)

mod1_T2003_90_s1 <- subset(mod1_T2003_90_s1, mod1_T2003_90_s1$AOD != "NA")
names(mod1_T2003_90_s1)


write.dbf(mod1_T2003_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s1.dbf")

mod1_T2003_10_s1 <- subset(mod1_T2003_10_s1, mod1_T2003_10_s1$AOD != "NA")


write.dbf(mod1_T2003_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s1_short.dbf")


write.dbf(mod1_T2003_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s2 <- splits_s2$trainset
mod1_T2003_90_s2 <- splits_s2$testset
names(mod1_T2003_90_s2)

mod1_T2003_90_s2 <- subset(mod1_T2003_90_s2, mod1_T2003_90_s2$AOD != "NA")
names(mod1_T2003_90_s2)
mod1_T2003_90_s2$EPACode.y <- NULL 
mod1_T2003_90_s2$PM25.y <- NULL
mod1_T2003_90_s2$Lat_PM.y <- NULL
mod1_T2003_90_s2$Long_PM.y <- NULL
mod1_T2003_90_s2$guid.y <- NULL
library(reshape)
mod1_T2003_90_s2<-rename(mod1_T2003_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s2.dbf")

mod1_T2003_10_s2 <- subset(mod1_T2003_10_s2, mod1_T2003_10_s2$AOD != "NA")


write.dbf(mod1_T2003_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s2_short.dbf")


write.dbf(mod1_T2003_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s3 <- splits_s3$trainset
mod1_T2003_90_s3 <- splits_s3$testset
names(mod1_T2003_90_s3)

mod1_T2003_90_s3 <- subset(mod1_T2003_90_s3, mod1_T2003_90_s3$AOD != "NA")
names(mod1_T2003_90_s3)
mod1_T2003_90_s3$EPACode.y <- NULL 
mod1_T2003_90_s3$PM25.y <- NULL
mod1_T2003_90_s3$Lat_PM.y <- NULL
mod1_T2003_90_s3$Long_PM.y <- NULL
mod1_T2003_90_s3$guid.y <- NULL
library(reshape)
mod1_T2003_90_s3<-rename(mod1_T2003_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s3.dbf")

mod1_T2003_10_s3 <- subset(mod1_T2003_10_s3, mod1_T2003_10_s3$AOD != "NA")


write.dbf(mod1_T2003_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s3_short.dbf")


write.dbf(mod1_T2003_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s4 <- splits_s4$trainset
mod1_T2003_90_s4 <- splits_s4$testset
names(mod1_T2003_90_s4)

mod1_T2003_90_s4 <- subset(mod1_T2003_90_s4, mod1_T2003_90_s4$AOD != "NA")
names(mod1_T2003_90_s4)
mod1_T2003_90_s4$EPACode.y <- NULL 
mod1_T2003_90_s4$PM25.y <- NULL
mod1_T2003_90_s4$Lat_PM.y <- NULL
mod1_T2003_90_s4$Long_PM.y <- NULL
mod1_T2003_90_s4$guid.y <- NULL
library(reshape)
mod1_T2003_90_s4<-rename(mod1_T2003_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s4.dbf")

mod1_T2003_10_s4 <- subset(mod1_T2003_10_s4, mod1_T2003_10_s4$AOD != "NA")


write.dbf(mod1_T2003_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s4_short.dbf")


write.dbf(mod1_T2003_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s5 <- splits_s5$trainset
mod1_T2003_90_s5 <- splits_s5$testset
names(mod1_T2003_90_s5)

mod1_T2003_90_s5 <- subset(mod1_T2003_90_s5, mod1_T2003_90_s5$AOD != "NA")
names(mod1_T2003_90_s5)
mod1_T2003_90_s5$EPACode.y <- NULL 
mod1_T2003_90_s5$PM25.y <- NULL
mod1_T2003_90_s5$Lat_PM.y <- NULL
mod1_T2003_90_s5$Long_PM.y <- NULL
mod1_T2003_90_s5$guid.y <- NULL
library(reshape)
mod1_T2003_90_s5<-rename(mod1_T2003_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s5.dbf")

mod1_T2003_10_s5 <- subset(mod1_T2003_10_s5, mod1_T2003_10_s5$AOD != "NA")


write.dbf(mod1_T2003_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s5_short.dbf")


write.dbf(mod1_T2003_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s6 <- splits_s6$trainset
mod1_T2003_90_s6 <- splits_s6$testset
names(mod1_T2003_90_s6)

mod1_T2003_90_s6 <- subset(mod1_T2003_90_s6, mod1_T2003_90_s6$AOD != "NA")
names(mod1_T2003_90_s6)
mod1_T2003_90_s6$EPACode.y <- NULL 
mod1_T2003_90_s6$PM25.y <- NULL
mod1_T2003_90_s6$Lat_PM.y <- NULL
mod1_T2003_90_s6$Long_PM.y <- NULL
mod1_T2003_90_s6$guid.y <- NULL
library(reshape)
mod1_T2003_90_s6<-rename(mod1_T2003_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s6.dbf")

mod1_T2003_10_s6 <- subset(mod1_T2003_10_s6, mod1_T2003_10_s6$AOD != "NA")


write.dbf(mod1_T2003_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s6_short.dbf")


write.dbf(mod1_T2003_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s7 <- splits_s7$trainset
mod1_T2003_90_s7 <- splits_s7$testset
names(mod1_T2003_90_s7)

mod1_T2003_90_s7 <- subset(mod1_T2003_90_s7, mod1_T2003_90_s7$AOD != "NA")
names(mod1_T2003_90_s7)
mod1_T2003_90_s7$EPACode.y <- NULL 
mod1_T2003_90_s7$PM25.y <- NULL
mod1_T2003_90_s7$Lat_PM.y <- NULL
mod1_T2003_90_s7$Long_PM.y <- NULL
mod1_T2003_90_s7$guid.y <- NULL
library(reshape)
mod1_T2003_90_s7<-rename(mod1_T2003_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s7.dbf")

mod1_T2003_10_s7 <- subset(mod1_T2003_10_s7, mod1_T2003_10_s7$AOD != "NA")


write.dbf(mod1_T2003_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s7_short.dbf")


write.dbf(mod1_T2003_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s8 <- splits_s8$trainset
mod1_T2003_90_s8 <- splits_s8$testset
names(mod1_T2003_90_s8)

mod1_T2003_90_s8 <- subset(mod1_T2003_90_s8, mod1_T2003_90_s8$AOD != "NA")
names(mod1_T2003_90_s8)
mod1_T2003_90_s8$EPACode.y <- NULL 
mod1_T2003_90_s8$PM25.y <- NULL
mod1_T2003_90_s8$Lat_PM.y <- NULL
mod1_T2003_90_s8$Long_PM.y <- NULL
mod1_T2003_90_s8$guid.y <- NULL
library(reshape)
mod1_T2003_90_s8<-rename(mod1_T2003_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s8.dbf")

mod1_T2003_10_s8 <- subset(mod1_T2003_10_s8, mod1_T2003_10_s8$AOD != "NA")


write.dbf(mod1_T2003_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s8_short.dbf")


write.dbf(mod1_T2003_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s9 <- splits_s9$trainset
mod1_T2003_90_s9 <- splits_s9$testset
names(mod1_T2003_90_s9)

mod1_T2003_90_s9 <- subset(mod1_T2003_90_s9, mod1_T2003_90_s9$AOD != "NA")
names(mod1_T2003_90_s9)
mod1_T2003_90_s9$EPACode.y <- NULL 
mod1_T2003_90_s9$PM25.y <- NULL
mod1_T2003_90_s9$Lat_PM.y <- NULL
mod1_T2003_90_s9$Long_PM.y <- NULL
mod1_T2003_90_s9$guid.y <- NULL
library(reshape)
mod1_T2003_90_s9<-rename(mod1_T2003_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s9.dbf")

mod1_T2003_10_s9 <- subset(mod1_T2003_10_s9, mod1_T2003_10_s9$AOD != "NA")


write.dbf(mod1_T2003_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s9_short.dbf")


write.dbf(mod1_T2003_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2003)

mod1_T2003_10_s10 <- splits_s10$trainset
mod1_T2003_90_s10 <- splits_s10$testset
names(mod1_T2003_90_s10)

mod1_T2003_90_s10 <- subset(mod1_T2003_90_s10, mod1_T2003_90_s10$AOD != "NA")
names(mod1_T2003_90_s10)
mod1_T2003_90_s10$EPACode.y <- NULL 
mod1_T2003_90_s10$PM25.y <- NULL
mod1_T2003_90_s10$Lat_PM.y <- NULL
mod1_T2003_90_s10$Long_PM.y <- NULL
mod1_T2003_90_s10$guid.y <- NULL
library(reshape)
mod1_T2003_90_s10<-rename(mod1_T2003_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2003_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s10.dbf")

mod1_T2003_10_s10 <- subset(mod1_T2003_10_s10, mod1_T2003_10_s10$AOD != "NA")


write.dbf(mod1_T2003_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_10_s10_short.dbf")


write.dbf(mod1_T2003_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2003_90_s10.dbf")






#load 2004

#import mod1 DB (PM-AOD)

F_T2004_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2004.dbf") 
names(F_T2004_Allp)



pm2004 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2004.csv", header=T) 
names(F_T2004_Allp)


#sort
F_T2004_Allp<-F_T2004_Allp[order(F_T2004_Allp$SiteCode,F_T2004_Allp$Date),]
pm2004<-pm2004[order(pm2004$SiteCode,pm2004$Date),] 
#merge 
MERGED_DATA2004<-merge(pm2004,F_T2004_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2004$EPACode.y <- NULL 
MERGED_DATA2004$PM25.y <- NULL
MERGED_DATA2004$Lat_PM.y <- NULL
MERGED_DATA2004$Long_PM.y <- NULL
MERGED_DATA2004$guid.y <- NULL
library(reshape)
MERGED_DATA2004<-rename(MERGED_DATA2004,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s1 <- splits_s1$trainset
mod1_T2004_90_s1 <- splits_s1$testset
names(mod1_T2004_90_s1)

mod1_T2004_90_s1 <- subset(mod1_T2004_90_s1, mod1_T2004_90_s1$AOD != "NA")
names(mod1_T2004_90_s1)


write.dbf(mod1_T2004_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s1.dbf")

mod1_T2004_10_s1 <- subset(mod1_T2004_10_s1, mod1_T2004_10_s1$AOD != "NA")


write.dbf(mod1_T2004_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s1_short.dbf")


write.dbf(mod1_T2004_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s2 <- splits_s2$trainset
mod1_T2004_90_s2 <- splits_s2$testset
names(mod1_T2004_90_s2)

mod1_T2004_90_s2 <- subset(mod1_T2004_90_s2, mod1_T2004_90_s2$AOD != "NA")
names(mod1_T2004_90_s2)
mod1_T2004_90_s2$EPACode.y <- NULL 
mod1_T2004_90_s2$PM25.y <- NULL
mod1_T2004_90_s2$Lat_PM.y <- NULL
mod1_T2004_90_s2$Long_PM.y <- NULL
mod1_T2004_90_s2$guid.y <- NULL
library(reshape)
mod1_T2004_90_s2<-rename(mod1_T2004_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s2.dbf")

mod1_T2004_10_s2 <- subset(mod1_T2004_10_s2, mod1_T2004_10_s2$AOD != "NA")


write.dbf(mod1_T2004_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s2_short.dbf")


write.dbf(mod1_T2004_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s3 <- splits_s3$trainset
mod1_T2004_90_s3 <- splits_s3$testset
names(mod1_T2004_90_s3)

mod1_T2004_90_s3 <- subset(mod1_T2004_90_s3, mod1_T2004_90_s3$AOD != "NA")
names(mod1_T2004_90_s3)
mod1_T2004_90_s3$EPACode.y <- NULL 
mod1_T2004_90_s3$PM25.y <- NULL
mod1_T2004_90_s3$Lat_PM.y <- NULL
mod1_T2004_90_s3$Long_PM.y <- NULL
mod1_T2004_90_s3$guid.y <- NULL
library(reshape)
mod1_T2004_90_s3<-rename(mod1_T2004_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s3.dbf")

mod1_T2004_10_s3 <- subset(mod1_T2004_10_s3, mod1_T2004_10_s3$AOD != "NA")


write.dbf(mod1_T2004_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s3_short.dbf")


write.dbf(mod1_T2004_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s4 <- splits_s4$trainset
mod1_T2004_90_s4 <- splits_s4$testset
names(mod1_T2004_90_s4)

mod1_T2004_90_s4 <- subset(mod1_T2004_90_s4, mod1_T2004_90_s4$AOD != "NA")
names(mod1_T2004_90_s4)
mod1_T2004_90_s4$EPACode.y <- NULL 
mod1_T2004_90_s4$PM25.y <- NULL
mod1_T2004_90_s4$Lat_PM.y <- NULL
mod1_T2004_90_s4$Long_PM.y <- NULL
mod1_T2004_90_s4$guid.y <- NULL
library(reshape)
mod1_T2004_90_s4<-rename(mod1_T2004_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s4.dbf")

mod1_T2004_10_s4 <- subset(mod1_T2004_10_s4, mod1_T2004_10_s4$AOD != "NA")


write.dbf(mod1_T2004_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s4_short.dbf")


write.dbf(mod1_T2004_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s5 <- splits_s5$trainset
mod1_T2004_90_s5 <- splits_s5$testset
names(mod1_T2004_90_s5)

mod1_T2004_90_s5 <- subset(mod1_T2004_90_s5, mod1_T2004_90_s5$AOD != "NA")
names(mod1_T2004_90_s5)
mod1_T2004_90_s5$EPACode.y <- NULL 
mod1_T2004_90_s5$PM25.y <- NULL
mod1_T2004_90_s5$Lat_PM.y <- NULL
mod1_T2004_90_s5$Long_PM.y <- NULL
mod1_T2004_90_s5$guid.y <- NULL
library(reshape)
mod1_T2004_90_s5<-rename(mod1_T2004_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s5.dbf")

mod1_T2004_10_s5 <- subset(mod1_T2004_10_s5, mod1_T2004_10_s5$AOD != "NA")


write.dbf(mod1_T2004_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s5_short.dbf")


write.dbf(mod1_T2004_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s6 <- splits_s6$trainset
mod1_T2004_90_s6 <- splits_s6$testset
names(mod1_T2004_90_s6)

mod1_T2004_90_s6 <- subset(mod1_T2004_90_s6, mod1_T2004_90_s6$AOD != "NA")
names(mod1_T2004_90_s6)
mod1_T2004_90_s6$EPACode.y <- NULL 
mod1_T2004_90_s6$PM25.y <- NULL
mod1_T2004_90_s6$Lat_PM.y <- NULL
mod1_T2004_90_s6$Long_PM.y <- NULL
mod1_T2004_90_s6$guid.y <- NULL
library(reshape)
mod1_T2004_90_s6<-rename(mod1_T2004_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s6.dbf")

mod1_T2004_10_s6 <- subset(mod1_T2004_10_s6, mod1_T2004_10_s6$AOD != "NA")


write.dbf(mod1_T2004_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s6_short.dbf")


write.dbf(mod1_T2004_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s7 <- splits_s7$trainset
mod1_T2004_90_s7 <- splits_s7$testset
names(mod1_T2004_90_s7)

mod1_T2004_90_s7 <- subset(mod1_T2004_90_s7, mod1_T2004_90_s7$AOD != "NA")
names(mod1_T2004_90_s7)
mod1_T2004_90_s7$EPACode.y <- NULL 
mod1_T2004_90_s7$PM25.y <- NULL
mod1_T2004_90_s7$Lat_PM.y <- NULL
mod1_T2004_90_s7$Long_PM.y <- NULL
mod1_T2004_90_s7$guid.y <- NULL
library(reshape)
mod1_T2004_90_s7<-rename(mod1_T2004_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s7.dbf")

mod1_T2004_10_s7 <- subset(mod1_T2004_10_s7, mod1_T2004_10_s7$AOD != "NA")


write.dbf(mod1_T2004_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s7_short.dbf")


write.dbf(mod1_T2004_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s8 <- splits_s8$trainset
mod1_T2004_90_s8 <- splits_s8$testset
names(mod1_T2004_90_s8)

mod1_T2004_90_s8 <- subset(mod1_T2004_90_s8, mod1_T2004_90_s8$AOD != "NA")
names(mod1_T2004_90_s8)
mod1_T2004_90_s8$EPACode.y <- NULL 
mod1_T2004_90_s8$PM25.y <- NULL
mod1_T2004_90_s8$Lat_PM.y <- NULL
mod1_T2004_90_s8$Long_PM.y <- NULL
mod1_T2004_90_s8$guid.y <- NULL
library(reshape)
mod1_T2004_90_s8<-rename(mod1_T2004_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s8.dbf")

mod1_T2004_10_s8 <- subset(mod1_T2004_10_s8, mod1_T2004_10_s8$AOD != "NA")


write.dbf(mod1_T2004_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s8_short.dbf")


write.dbf(mod1_T2004_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s9 <- splits_s9$trainset
mod1_T2004_90_s9 <- splits_s9$testset
names(mod1_T2004_90_s9)

mod1_T2004_90_s9 <- subset(mod1_T2004_90_s9, mod1_T2004_90_s9$AOD != "NA")
names(mod1_T2004_90_s9)
mod1_T2004_90_s9$EPACode.y <- NULL 
mod1_T2004_90_s9$PM25.y <- NULL
mod1_T2004_90_s9$Lat_PM.y <- NULL
mod1_T2004_90_s9$Long_PM.y <- NULL
mod1_T2004_90_s9$guid.y <- NULL
library(reshape)
mod1_T2004_90_s9<-rename(mod1_T2004_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s9.dbf")

mod1_T2004_10_s9 <- subset(mod1_T2004_10_s9, mod1_T2004_10_s9$AOD != "NA")


write.dbf(mod1_T2004_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s9_short.dbf")


write.dbf(mod1_T2004_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2004)

mod1_T2004_10_s10 <- splits_s10$trainset
mod1_T2004_90_s10 <- splits_s10$testset
names(mod1_T2004_90_s10)

mod1_T2004_90_s10 <- subset(mod1_T2004_90_s10, mod1_T2004_90_s10$AOD != "NA")
names(mod1_T2004_90_s10)
mod1_T2004_90_s10$EPACode.y <- NULL 
mod1_T2004_90_s10$PM25.y <- NULL
mod1_T2004_90_s10$Lat_PM.y <- NULL
mod1_T2004_90_s10$Long_PM.y <- NULL
mod1_T2004_90_s10$guid.y <- NULL
library(reshape)
mod1_T2004_90_s10<-rename(mod1_T2004_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2004_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s10.dbf")

mod1_T2004_10_s10 <- subset(mod1_T2004_10_s10, mod1_T2004_10_s10$AOD != "NA")


write.dbf(mod1_T2004_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_10_s10_short.dbf")


write.dbf(mod1_T2004_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2004_90_s10.dbf")






#load 2005

#import mod1 DB (PM-AOD)

F_T2005_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2005.dbf") 
names(F_T2005_Allp)



pm2005 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2005.csv", header=T) 
names(F_T2005_Allp)


#sort
F_T2005_Allp<-F_T2005_Allp[order(F_T2005_Allp$SiteCode,F_T2005_Allp$Date),]
pm2005<-pm2005[order(pm2005$SiteCode,pm2005$Date),] 
#merge 
MERGED_DATA2005<-merge(pm2005,F_T2005_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2005$EPACode.y <- NULL 
MERGED_DATA2005$PM25.y <- NULL
MERGED_DATA2005$Lat_PM.y <- NULL
MERGED_DATA2005$Long_PM.y <- NULL
MERGED_DATA2005$guid.y <- NULL
library(reshape)
MERGED_DATA2005<-rename(MERGED_DATA2005,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s1 <- splits_s1$trainset
mod1_T2005_90_s1 <- splits_s1$testset
names(mod1_T2005_90_s1)

mod1_T2005_90_s1 <- subset(mod1_T2005_90_s1, mod1_T2005_90_s1$AOD != "NA")
names(mod1_T2005_90_s1)


write.dbf(mod1_T2005_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s1.dbf")

mod1_T2005_10_s1 <- subset(mod1_T2005_10_s1, mod1_T2005_10_s1$AOD != "NA")


write.dbf(mod1_T2005_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s1_short.dbf")


write.dbf(mod1_T2005_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s2 <- splits_s2$trainset
mod1_T2005_90_s2 <- splits_s2$testset
names(mod1_T2005_90_s2)

mod1_T2005_90_s2 <- subset(mod1_T2005_90_s2, mod1_T2005_90_s2$AOD != "NA")
names(mod1_T2005_90_s2)
mod1_T2005_90_s2$EPACode.y <- NULL 
mod1_T2005_90_s2$PM25.y <- NULL
mod1_T2005_90_s2$Lat_PM.y <- NULL
mod1_T2005_90_s2$Long_PM.y <- NULL
mod1_T2005_90_s2$guid.y <- NULL
library(reshape)
mod1_T2005_90_s2<-rename(mod1_T2005_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s2.dbf")

mod1_T2005_10_s2 <- subset(mod1_T2005_10_s2, mod1_T2005_10_s2$AOD != "NA")


write.dbf(mod1_T2005_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s2_short.dbf")


write.dbf(mod1_T2005_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s3 <- splits_s3$trainset
mod1_T2005_90_s3 <- splits_s3$testset
names(mod1_T2005_90_s3)

mod1_T2005_90_s3 <- subset(mod1_T2005_90_s3, mod1_T2005_90_s3$AOD != "NA")
names(mod1_T2005_90_s3)
mod1_T2005_90_s3$EPACode.y <- NULL 
mod1_T2005_90_s3$PM25.y <- NULL
mod1_T2005_90_s3$Lat_PM.y <- NULL
mod1_T2005_90_s3$Long_PM.y <- NULL
mod1_T2005_90_s3$guid.y <- NULL
library(reshape)
mod1_T2005_90_s3<-rename(mod1_T2005_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s3.dbf")

mod1_T2005_10_s3 <- subset(mod1_T2005_10_s3, mod1_T2005_10_s3$AOD != "NA")


write.dbf(mod1_T2005_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s3_short.dbf")


write.dbf(mod1_T2005_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s4 <- splits_s4$trainset
mod1_T2005_90_s4 <- splits_s4$testset
names(mod1_T2005_90_s4)

mod1_T2005_90_s4 <- subset(mod1_T2005_90_s4, mod1_T2005_90_s4$AOD != "NA")
names(mod1_T2005_90_s4)
mod1_T2005_90_s4$EPACode.y <- NULL 
mod1_T2005_90_s4$PM25.y <- NULL
mod1_T2005_90_s4$Lat_PM.y <- NULL
mod1_T2005_90_s4$Long_PM.y <- NULL
mod1_T2005_90_s4$guid.y <- NULL
library(reshape)
mod1_T2005_90_s4<-rename(mod1_T2005_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s4.dbf")

mod1_T2005_10_s4 <- subset(mod1_T2005_10_s4, mod1_T2005_10_s4$AOD != "NA")


write.dbf(mod1_T2005_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s4_short.dbf")


write.dbf(mod1_T2005_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s5 <- splits_s5$trainset
mod1_T2005_90_s5 <- splits_s5$testset
names(mod1_T2005_90_s5)

mod1_T2005_90_s5 <- subset(mod1_T2005_90_s5, mod1_T2005_90_s5$AOD != "NA")
names(mod1_T2005_90_s5)
mod1_T2005_90_s5$EPACode.y <- NULL 
mod1_T2005_90_s5$PM25.y <- NULL
mod1_T2005_90_s5$Lat_PM.y <- NULL
mod1_T2005_90_s5$Long_PM.y <- NULL
mod1_T2005_90_s5$guid.y <- NULL
library(reshape)
mod1_T2005_90_s5<-rename(mod1_T2005_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s5.dbf")

mod1_T2005_10_s5 <- subset(mod1_T2005_10_s5, mod1_T2005_10_s5$AOD != "NA")


write.dbf(mod1_T2005_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s5_short.dbf")


write.dbf(mod1_T2005_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s6 <- splits_s6$trainset
mod1_T2005_90_s6 <- splits_s6$testset
names(mod1_T2005_90_s6)

mod1_T2005_90_s6 <- subset(mod1_T2005_90_s6, mod1_T2005_90_s6$AOD != "NA")
names(mod1_T2005_90_s6)
mod1_T2005_90_s6$EPACode.y <- NULL 
mod1_T2005_90_s6$PM25.y <- NULL
mod1_T2005_90_s6$Lat_PM.y <- NULL
mod1_T2005_90_s6$Long_PM.y <- NULL
mod1_T2005_90_s6$guid.y <- NULL
library(reshape)
mod1_T2005_90_s6<-rename(mod1_T2005_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s6.dbf")

mod1_T2005_10_s6 <- subset(mod1_T2005_10_s6, mod1_T2005_10_s6$AOD != "NA")


write.dbf(mod1_T2005_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s6_short.dbf")


write.dbf(mod1_T2005_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s7 <- splits_s7$trainset
mod1_T2005_90_s7 <- splits_s7$testset
names(mod1_T2005_90_s7)

mod1_T2005_90_s7 <- subset(mod1_T2005_90_s7, mod1_T2005_90_s7$AOD != "NA")
names(mod1_T2005_90_s7)
mod1_T2005_90_s7$EPACode.y <- NULL 
mod1_T2005_90_s7$PM25.y <- NULL
mod1_T2005_90_s7$Lat_PM.y <- NULL
mod1_T2005_90_s7$Long_PM.y <- NULL
mod1_T2005_90_s7$guid.y <- NULL
library(reshape)
mod1_T2005_90_s7<-rename(mod1_T2005_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s7.dbf")

mod1_T2005_10_s7 <- subset(mod1_T2005_10_s7, mod1_T2005_10_s7$AOD != "NA")


write.dbf(mod1_T2005_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s7_short.dbf")


write.dbf(mod1_T2005_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s8 <- splits_s8$trainset
mod1_T2005_90_s8 <- splits_s8$testset
names(mod1_T2005_90_s8)

mod1_T2005_90_s8 <- subset(mod1_T2005_90_s8, mod1_T2005_90_s8$AOD != "NA")
names(mod1_T2005_90_s8)
mod1_T2005_90_s8$EPACode.y <- NULL 
mod1_T2005_90_s8$PM25.y <- NULL
mod1_T2005_90_s8$Lat_PM.y <- NULL
mod1_T2005_90_s8$Long_PM.y <- NULL
mod1_T2005_90_s8$guid.y <- NULL
library(reshape)
mod1_T2005_90_s8<-rename(mod1_T2005_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s8.dbf")

mod1_T2005_10_s8 <- subset(mod1_T2005_10_s8, mod1_T2005_10_s8$AOD != "NA")


write.dbf(mod1_T2005_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s8_short.dbf")


write.dbf(mod1_T2005_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s9 <- splits_s9$trainset
mod1_T2005_90_s9 <- splits_s9$testset
names(mod1_T2005_90_s9)

mod1_T2005_90_s9 <- subset(mod1_T2005_90_s9, mod1_T2005_90_s9$AOD != "NA")
names(mod1_T2005_90_s9)
mod1_T2005_90_s9$EPACode.y <- NULL 
mod1_T2005_90_s9$PM25.y <- NULL
mod1_T2005_90_s9$Lat_PM.y <- NULL
mod1_T2005_90_s9$Long_PM.y <- NULL
mod1_T2005_90_s9$guid.y <- NULL
library(reshape)
mod1_T2005_90_s9<-rename(mod1_T2005_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s9.dbf")

mod1_T2005_10_s9 <- subset(mod1_T2005_10_s9, mod1_T2005_10_s9$AOD != "NA")


write.dbf(mod1_T2005_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s9_short.dbf")


write.dbf(mod1_T2005_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2005)

mod1_T2005_10_s10 <- splits_s10$trainset
mod1_T2005_90_s10 <- splits_s10$testset
names(mod1_T2005_90_s10)

mod1_T2005_90_s10 <- subset(mod1_T2005_90_s10, mod1_T2005_90_s10$AOD != "NA")
names(mod1_T2005_90_s10)
mod1_T2005_90_s10$EPACode.y <- NULL 
mod1_T2005_90_s10$PM25.y <- NULL
mod1_T2005_90_s10$Lat_PM.y <- NULL
mod1_T2005_90_s10$Long_PM.y <- NULL
mod1_T2005_90_s10$guid.y <- NULL
library(reshape)
mod1_T2005_90_s10<-rename(mod1_T2005_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2005_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s10.dbf")

mod1_T2005_10_s10 <- subset(mod1_T2005_10_s10, mod1_T2005_10_s10$AOD != "NA")


write.dbf(mod1_T2005_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_10_s10_short.dbf")


write.dbf(mod1_T2005_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2005_90_s10.dbf")







#load 2006

#import mod1 DB (PM-AOD)

F_T2006_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2006.dbf") 
names(F_T2006_Allp)



pm2006 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2006.csv", header=T) 
names(F_T2006_Allp)


#sort
F_T2006_Allp<-F_T2006_Allp[order(F_T2006_Allp$SiteCode,F_T2006_Allp$Date),]
pm2006<-pm2006[order(pm2006$SiteCode,pm2006$Date),] 
#merge 
MERGED_DATA2006<-merge(pm2006,F_T2006_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2006$EPACode.y <- NULL 
MERGED_DATA2006$PM25.y <- NULL
MERGED_DATA2006$Lat_PM.y <- NULL
MERGED_DATA2006$Long_PM.y <- NULL
MERGED_DATA2006$guid.y <- NULL
library(reshape)
MERGED_DATA2006<-rename(MERGED_DATA2006,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s1 <- splits_s1$trainset
mod1_T2006_90_s1 <- splits_s1$testset
names(mod1_T2006_90_s1)

mod1_T2006_90_s1 <- subset(mod1_T2006_90_s1, mod1_T2006_90_s1$AOD != "NA")
names(mod1_T2006_90_s1)


write.dbf(mod1_T2006_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s1.dbf")

mod1_T2006_10_s1 <- subset(mod1_T2006_10_s1, mod1_T2006_10_s1$AOD != "NA")


write.dbf(mod1_T2006_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s1_short.dbf")


write.dbf(mod1_T2006_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s2 <- splits_s2$trainset
mod1_T2006_90_s2 <- splits_s2$testset
names(mod1_T2006_90_s2)

mod1_T2006_90_s2 <- subset(mod1_T2006_90_s2, mod1_T2006_90_s2$AOD != "NA")
names(mod1_T2006_90_s2)
mod1_T2006_90_s2$EPACode.y <- NULL 
mod1_T2006_90_s2$PM25.y <- NULL
mod1_T2006_90_s2$Lat_PM.y <- NULL
mod1_T2006_90_s2$Long_PM.y <- NULL
mod1_T2006_90_s2$guid.y <- NULL
library(reshape)
mod1_T2006_90_s2<-rename(mod1_T2006_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s2.dbf")

mod1_T2006_10_s2 <- subset(mod1_T2006_10_s2, mod1_T2006_10_s2$AOD != "NA")


write.dbf(mod1_T2006_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s2_short.dbf")


write.dbf(mod1_T2006_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s3 <- splits_s3$trainset
mod1_T2006_90_s3 <- splits_s3$testset
names(mod1_T2006_90_s3)

mod1_T2006_90_s3 <- subset(mod1_T2006_90_s3, mod1_T2006_90_s3$AOD != "NA")
names(mod1_T2006_90_s3)
mod1_T2006_90_s3$EPACode.y <- NULL 
mod1_T2006_90_s3$PM25.y <- NULL
mod1_T2006_90_s3$Lat_PM.y <- NULL
mod1_T2006_90_s3$Long_PM.y <- NULL
mod1_T2006_90_s3$guid.y <- NULL
library(reshape)
mod1_T2006_90_s3<-rename(mod1_T2006_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s3.dbf")

mod1_T2006_10_s3 <- subset(mod1_T2006_10_s3, mod1_T2006_10_s3$AOD != "NA")


write.dbf(mod1_T2006_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s3_short.dbf")


write.dbf(mod1_T2006_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s4 <- splits_s4$trainset
mod1_T2006_90_s4 <- splits_s4$testset
names(mod1_T2006_90_s4)

mod1_T2006_90_s4 <- subset(mod1_T2006_90_s4, mod1_T2006_90_s4$AOD != "NA")
names(mod1_T2006_90_s4)
mod1_T2006_90_s4$EPACode.y <- NULL 
mod1_T2006_90_s4$PM25.y <- NULL
mod1_T2006_90_s4$Lat_PM.y <- NULL
mod1_T2006_90_s4$Long_PM.y <- NULL
mod1_T2006_90_s4$guid.y <- NULL
library(reshape)
mod1_T2006_90_s4<-rename(mod1_T2006_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s4.dbf")

mod1_T2006_10_s4 <- subset(mod1_T2006_10_s4, mod1_T2006_10_s4$AOD != "NA")


write.dbf(mod1_T2006_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s4_short.dbf")


write.dbf(mod1_T2006_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s5 <- splits_s5$trainset
mod1_T2006_90_s5 <- splits_s5$testset
names(mod1_T2006_90_s5)

mod1_T2006_90_s5 <- subset(mod1_T2006_90_s5, mod1_T2006_90_s5$AOD != "NA")
names(mod1_T2006_90_s5)
mod1_T2006_90_s5$EPACode.y <- NULL 
mod1_T2006_90_s5$PM25.y <- NULL
mod1_T2006_90_s5$Lat_PM.y <- NULL
mod1_T2006_90_s5$Long_PM.y <- NULL
mod1_T2006_90_s5$guid.y <- NULL
library(reshape)
mod1_T2006_90_s5<-rename(mod1_T2006_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s5.dbf")

mod1_T2006_10_s5 <- subset(mod1_T2006_10_s5, mod1_T2006_10_s5$AOD != "NA")


write.dbf(mod1_T2006_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s5_short.dbf")


write.dbf(mod1_T2006_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s6 <- splits_s6$trainset
mod1_T2006_90_s6 <- splits_s6$testset
names(mod1_T2006_90_s6)

mod1_T2006_90_s6 <- subset(mod1_T2006_90_s6, mod1_T2006_90_s6$AOD != "NA")
names(mod1_T2006_90_s6)
mod1_T2006_90_s6$EPACode.y <- NULL 
mod1_T2006_90_s6$PM25.y <- NULL
mod1_T2006_90_s6$Lat_PM.y <- NULL
mod1_T2006_90_s6$Long_PM.y <- NULL
mod1_T2006_90_s6$guid.y <- NULL
library(reshape)
mod1_T2006_90_s6<-rename(mod1_T2006_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s6.dbf")

mod1_T2006_10_s6 <- subset(mod1_T2006_10_s6, mod1_T2006_10_s6$AOD != "NA")


write.dbf(mod1_T2006_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s6_short.dbf")


write.dbf(mod1_T2006_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s7 <- splits_s7$trainset
mod1_T2006_90_s7 <- splits_s7$testset
names(mod1_T2006_90_s7)

mod1_T2006_90_s7 <- subset(mod1_T2006_90_s7, mod1_T2006_90_s7$AOD != "NA")
names(mod1_T2006_90_s7)
mod1_T2006_90_s7$EPACode.y <- NULL 
mod1_T2006_90_s7$PM25.y <- NULL
mod1_T2006_90_s7$Lat_PM.y <- NULL
mod1_T2006_90_s7$Long_PM.y <- NULL
mod1_T2006_90_s7$guid.y <- NULL
library(reshape)
mod1_T2006_90_s7<-rename(mod1_T2006_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s7.dbf")

mod1_T2006_10_s7 <- subset(mod1_T2006_10_s7, mod1_T2006_10_s7$AOD != "NA")


write.dbf(mod1_T2006_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s7_short.dbf")


write.dbf(mod1_T2006_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s8 <- splits_s8$trainset
mod1_T2006_90_s8 <- splits_s8$testset
names(mod1_T2006_90_s8)

mod1_T2006_90_s8 <- subset(mod1_T2006_90_s8, mod1_T2006_90_s8$AOD != "NA")
names(mod1_T2006_90_s8)
mod1_T2006_90_s8$EPACode.y <- NULL 
mod1_T2006_90_s8$PM25.y <- NULL
mod1_T2006_90_s8$Lat_PM.y <- NULL
mod1_T2006_90_s8$Long_PM.y <- NULL
mod1_T2006_90_s8$guid.y <- NULL
library(reshape)
mod1_T2006_90_s8<-rename(mod1_T2006_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s8.dbf")

mod1_T2006_10_s8 <- subset(mod1_T2006_10_s8, mod1_T2006_10_s8$AOD != "NA")


write.dbf(mod1_T2006_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s8_short.dbf")


write.dbf(mod1_T2006_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s9 <- splits_s9$trainset
mod1_T2006_90_s9 <- splits_s9$testset
names(mod1_T2006_90_s9)

mod1_T2006_90_s9 <- subset(mod1_T2006_90_s9, mod1_T2006_90_s9$AOD != "NA")
names(mod1_T2006_90_s9)
mod1_T2006_90_s9$EPACode.y <- NULL 
mod1_T2006_90_s9$PM25.y <- NULL
mod1_T2006_90_s9$Lat_PM.y <- NULL
mod1_T2006_90_s9$Long_PM.y <- NULL
mod1_T2006_90_s9$guid.y <- NULL
library(reshape)
mod1_T2006_90_s9<-rename(mod1_T2006_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s9.dbf")

mod1_T2006_10_s9 <- subset(mod1_T2006_10_s9, mod1_T2006_10_s9$AOD != "NA")


write.dbf(mod1_T2006_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s9_short.dbf")


write.dbf(mod1_T2006_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2006)

mod1_T2006_10_s10 <- splits_s10$trainset
mod1_T2006_90_s10 <- splits_s10$testset
names(mod1_T2006_90_s10)

mod1_T2006_90_s10 <- subset(mod1_T2006_90_s10, mod1_T2006_90_s10$AOD != "NA")
names(mod1_T2006_90_s10)
mod1_T2006_90_s10$EPACode.y <- NULL 
mod1_T2006_90_s10$PM25.y <- NULL
mod1_T2006_90_s10$Lat_PM.y <- NULL
mod1_T2006_90_s10$Long_PM.y <- NULL
mod1_T2006_90_s10$guid.y <- NULL
library(reshape)
mod1_T2006_90_s10<-rename(mod1_T2006_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2006_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s10.dbf")

mod1_T2006_10_s10 <- subset(mod1_T2006_10_s10, mod1_T2006_10_s10$AOD != "NA")


write.dbf(mod1_T2006_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_10_s10_short.dbf")


write.dbf(mod1_T2006_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2006_90_s10.dbf")







#load 2007

#import mod1 DB (PM-AOD)

F_T2007_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2007.dbf") 
names(F_T2007_Allp)



pm2007 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2007.csv", header=T) 
names(F_T2007_Allp)


#sort
F_T2007_Allp<-F_T2007_Allp[order(F_T2007_Allp$SiteCode,F_T2007_Allp$Date),]
pm2007<-pm2007[order(pm2007$SiteCode,pm2007$Date),] 
#merge 
MERGED_DATA2007<-merge(pm2007,F_T2007_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2007$EPACode.y <- NULL 
MERGED_DATA2007$PM25.y <- NULL
MERGED_DATA2007$Lat_PM.y <- NULL
MERGED_DATA2007$Long_PM.y <- NULL
MERGED_DATA2007$guid.y <- NULL
library(reshape)
MERGED_DATA2007<-rename(MERGED_DATA2007,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s1 <- splits_s1$trainset
mod1_T2007_90_s1 <- splits_s1$testset
names(mod1_T2007_90_s1)

mod1_T2007_90_s1 <- subset(mod1_T2007_90_s1, mod1_T2007_90_s1$AOD != "NA")
names(mod1_T2007_90_s1)


write.dbf(mod1_T2007_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s1.dbf")

mod1_T2007_10_s1 <- subset(mod1_T2007_10_s1, mod1_T2007_10_s1$AOD != "NA")


write.dbf(mod1_T2007_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s1_short.dbf")


write.dbf(mod1_T2007_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s2 <- splits_s2$trainset
mod1_T2007_90_s2 <- splits_s2$testset
names(mod1_T2007_90_s2)

mod1_T2007_90_s2 <- subset(mod1_T2007_90_s2, mod1_T2007_90_s2$AOD != "NA")
names(mod1_T2007_90_s2)
mod1_T2007_90_s2$EPACode.y <- NULL 
mod1_T2007_90_s2$PM25.y <- NULL
mod1_T2007_90_s2$Lat_PM.y <- NULL
mod1_T2007_90_s2$Long_PM.y <- NULL
mod1_T2007_90_s2$guid.y <- NULL
library(reshape)
mod1_T2007_90_s2<-rename(mod1_T2007_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s2.dbf")

mod1_T2007_10_s2 <- subset(mod1_T2007_10_s2, mod1_T2007_10_s2$AOD != "NA")


write.dbf(mod1_T2007_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s2_short.dbf")


write.dbf(mod1_T2007_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s3 <- splits_s3$trainset
mod1_T2007_90_s3 <- splits_s3$testset
names(mod1_T2007_90_s3)

mod1_T2007_90_s3 <- subset(mod1_T2007_90_s3, mod1_T2007_90_s3$AOD != "NA")
names(mod1_T2007_90_s3)
mod1_T2007_90_s3$EPACode.y <- NULL 
mod1_T2007_90_s3$PM25.y <- NULL
mod1_T2007_90_s3$Lat_PM.y <- NULL
mod1_T2007_90_s3$Long_PM.y <- NULL
mod1_T2007_90_s3$guid.y <- NULL
library(reshape)
mod1_T2007_90_s3<-rename(mod1_T2007_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s3.dbf")

mod1_T2007_10_s3 <- subset(mod1_T2007_10_s3, mod1_T2007_10_s3$AOD != "NA")


write.dbf(mod1_T2007_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s3_short.dbf")


write.dbf(mod1_T2007_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s4 <- splits_s4$trainset
mod1_T2007_90_s4 <- splits_s4$testset
names(mod1_T2007_90_s4)

mod1_T2007_90_s4 <- subset(mod1_T2007_90_s4, mod1_T2007_90_s4$AOD != "NA")
names(mod1_T2007_90_s4)
mod1_T2007_90_s4$EPACode.y <- NULL 
mod1_T2007_90_s4$PM25.y <- NULL
mod1_T2007_90_s4$Lat_PM.y <- NULL
mod1_T2007_90_s4$Long_PM.y <- NULL
mod1_T2007_90_s4$guid.y <- NULL
library(reshape)
mod1_T2007_90_s4<-rename(mod1_T2007_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s4.dbf")

mod1_T2007_10_s4 <- subset(mod1_T2007_10_s4, mod1_T2007_10_s4$AOD != "NA")


write.dbf(mod1_T2007_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s4_short.dbf")


write.dbf(mod1_T2007_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s5 <- splits_s5$trainset
mod1_T2007_90_s5 <- splits_s5$testset
names(mod1_T2007_90_s5)

mod1_T2007_90_s5 <- subset(mod1_T2007_90_s5, mod1_T2007_90_s5$AOD != "NA")
names(mod1_T2007_90_s5)
mod1_T2007_90_s5$EPACode.y <- NULL 
mod1_T2007_90_s5$PM25.y <- NULL
mod1_T2007_90_s5$Lat_PM.y <- NULL
mod1_T2007_90_s5$Long_PM.y <- NULL
mod1_T2007_90_s5$guid.y <- NULL
library(reshape)
mod1_T2007_90_s5<-rename(mod1_T2007_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s5.dbf")

mod1_T2007_10_s5 <- subset(mod1_T2007_10_s5, mod1_T2007_10_s5$AOD != "NA")


write.dbf(mod1_T2007_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s5_short.dbf")


write.dbf(mod1_T2007_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s6 <- splits_s6$trainset
mod1_T2007_90_s6 <- splits_s6$testset
names(mod1_T2007_90_s6)

mod1_T2007_90_s6 <- subset(mod1_T2007_90_s6, mod1_T2007_90_s6$AOD != "NA")
names(mod1_T2007_90_s6)
mod1_T2007_90_s6$EPACode.y <- NULL 
mod1_T2007_90_s6$PM25.y <- NULL
mod1_T2007_90_s6$Lat_PM.y <- NULL
mod1_T2007_90_s6$Long_PM.y <- NULL
mod1_T2007_90_s6$guid.y <- NULL
library(reshape)
mod1_T2007_90_s6<-rename(mod1_T2007_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s6.dbf")

mod1_T2007_10_s6 <- subset(mod1_T2007_10_s6, mod1_T2007_10_s6$AOD != "NA")


write.dbf(mod1_T2007_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s6_short.dbf")


write.dbf(mod1_T2007_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s7 <- splits_s7$trainset
mod1_T2007_90_s7 <- splits_s7$testset
names(mod1_T2007_90_s7)

mod1_T2007_90_s7 <- subset(mod1_T2007_90_s7, mod1_T2007_90_s7$AOD != "NA")
names(mod1_T2007_90_s7)
mod1_T2007_90_s7$EPACode.y <- NULL 
mod1_T2007_90_s7$PM25.y <- NULL
mod1_T2007_90_s7$Lat_PM.y <- NULL
mod1_T2007_90_s7$Long_PM.y <- NULL
mod1_T2007_90_s7$guid.y <- NULL
library(reshape)
mod1_T2007_90_s7<-rename(mod1_T2007_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s7.dbf")

mod1_T2007_10_s7 <- subset(mod1_T2007_10_s7, mod1_T2007_10_s7$AOD != "NA")


write.dbf(mod1_T2007_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s7_short.dbf")


write.dbf(mod1_T2007_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s8 <- splits_s8$trainset
mod1_T2007_90_s8 <- splits_s8$testset
names(mod1_T2007_90_s8)

mod1_T2007_90_s8 <- subset(mod1_T2007_90_s8, mod1_T2007_90_s8$AOD != "NA")
names(mod1_T2007_90_s8)
mod1_T2007_90_s8$EPACode.y <- NULL 
mod1_T2007_90_s8$PM25.y <- NULL
mod1_T2007_90_s8$Lat_PM.y <- NULL
mod1_T2007_90_s8$Long_PM.y <- NULL
mod1_T2007_90_s8$guid.y <- NULL
library(reshape)
mod1_T2007_90_s8<-rename(mod1_T2007_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s8.dbf")

mod1_T2007_10_s8 <- subset(mod1_T2007_10_s8, mod1_T2007_10_s8$AOD != "NA")


write.dbf(mod1_T2007_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s8_short.dbf")


write.dbf(mod1_T2007_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s9 <- splits_s9$trainset
mod1_T2007_90_s9 <- splits_s9$testset
names(mod1_T2007_90_s9)

mod1_T2007_90_s9 <- subset(mod1_T2007_90_s9, mod1_T2007_90_s9$AOD != "NA")
names(mod1_T2007_90_s9)
mod1_T2007_90_s9$EPACode.y <- NULL 
mod1_T2007_90_s9$PM25.y <- NULL
mod1_T2007_90_s9$Lat_PM.y <- NULL
mod1_T2007_90_s9$Long_PM.y <- NULL
mod1_T2007_90_s9$guid.y <- NULL
library(reshape)
mod1_T2007_90_s9<-rename(mod1_T2007_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s9.dbf")

mod1_T2007_10_s9 <- subset(mod1_T2007_10_s9, mod1_T2007_10_s9$AOD != "NA")


write.dbf(mod1_T2007_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s9_short.dbf")


write.dbf(mod1_T2007_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2007)

mod1_T2007_10_s10 <- splits_s10$trainset
mod1_T2007_90_s10 <- splits_s10$testset
names(mod1_T2007_90_s10)

mod1_T2007_90_s10 <- subset(mod1_T2007_90_s10, mod1_T2007_90_s10$AOD != "NA")
names(mod1_T2007_90_s10)
mod1_T2007_90_s10$EPACode.y <- NULL 
mod1_T2007_90_s10$PM25.y <- NULL
mod1_T2007_90_s10$Lat_PM.y <- NULL
mod1_T2007_90_s10$Long_PM.y <- NULL
mod1_T2007_90_s10$guid.y <- NULL
library(reshape)
mod1_T2007_90_s10<-rename(mod1_T2007_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2007_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s10.dbf")

mod1_T2007_10_s10 <- subset(mod1_T2007_10_s10, mod1_T2007_10_s10$AOD != "NA")


write.dbf(mod1_T2007_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_10_s10_short.dbf")


write.dbf(mod1_T2007_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2007_90_s10.dbf")






#load 2008

#import mod1 DB (PM-AOD)

F_T2008_Allp<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/out2008.dbf") 
names(F_T2008_Allp)



pm2008 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN008_mod3_corr/pmguidt2008.csv", header=T) 
names(F_T2008_Allp)


#sort
F_T2008_Allp<-F_T2008_Allp[order(F_T2008_Allp$SiteCode,F_T2008_Allp$Date),]
pm2008<-pm2008[order(pm2008$SiteCode,pm2008$Date),] 
#merge 
MERGED_DATA2008<-merge(pm2008,F_T2008_Allp,by=c("SiteCode","Date"), all.x=TRUE)

MERGED_DATA2008$EPACode.y <- NULL 
MERGED_DATA2008$PM25.y <- NULL
MERGED_DATA2008$Lat_PM.y <- NULL
MERGED_DATA2008$Long_PM.y <- NULL
MERGED_DATA2008$guid.y <- NULL
library(reshape)
MERGED_DATA2008<-rename(MERGED_DATA2008,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM",guid.x="guid"))





#s1
splits_s1 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s1 <- splits_s1$trainset
mod1_T2008_90_s1 <- splits_s1$testset
names(mod1_T2008_90_s1)

mod1_T2008_90_s1 <- subset(mod1_T2008_90_s1, mod1_T2008_90_s1$AOD != "NA")
names(mod1_T2008_90_s1)


write.dbf(mod1_T2008_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s1.dbf")

mod1_T2008_10_s1 <- subset(mod1_T2008_10_s1, mod1_T2008_10_s1$AOD != "NA")


write.dbf(mod1_T2008_10_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s1_short.dbf")


write.dbf(mod1_T2008_90_s1,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s1.dbf")





#s2
splits_s2 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s2 <- splits_s2$trainset
mod1_T2008_90_s2 <- splits_s2$testset
names(mod1_T2008_90_s2)

mod1_T2008_90_s2 <- subset(mod1_T2008_90_s2, mod1_T2008_90_s2$AOD != "NA")
names(mod1_T2008_90_s2)
mod1_T2008_90_s2$EPACode.y <- NULL 
mod1_T2008_90_s2$PM25.y <- NULL
mod1_T2008_90_s2$Lat_PM.y <- NULL
mod1_T2008_90_s2$Long_PM.y <- NULL
mod1_T2008_90_s2$guid.y <- NULL
library(reshape)
mod1_T2008_90_s2<-rename(mod1_T2008_90_s2,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s2.dbf")

mod1_T2008_10_s2 <- subset(mod1_T2008_10_s2, mod1_T2008_10_s2$AOD != "NA")


write.dbf(mod1_T2008_10_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s2_short.dbf")


write.dbf(mod1_T2008_90_s2,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s2.dbf")




#s3
splits_s3 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s3 <- splits_s3$trainset
mod1_T2008_90_s3 <- splits_s3$testset
names(mod1_T2008_90_s3)

mod1_T2008_90_s3 <- subset(mod1_T2008_90_s3, mod1_T2008_90_s3$AOD != "NA")
names(mod1_T2008_90_s3)
mod1_T2008_90_s3$EPACode.y <- NULL 
mod1_T2008_90_s3$PM25.y <- NULL
mod1_T2008_90_s3$Lat_PM.y <- NULL
mod1_T2008_90_s3$Long_PM.y <- NULL
mod1_T2008_90_s3$guid.y <- NULL
library(reshape)
mod1_T2008_90_s3<-rename(mod1_T2008_90_s3,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s3.dbf")

mod1_T2008_10_s3 <- subset(mod1_T2008_10_s3, mod1_T2008_10_s3$AOD != "NA")


write.dbf(mod1_T2008_10_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s3_short.dbf")


write.dbf(mod1_T2008_90_s3,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s3.dbf")




#s4
splits_s4 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s4 <- splits_s4$trainset
mod1_T2008_90_s4 <- splits_s4$testset
names(mod1_T2008_90_s4)

mod1_T2008_90_s4 <- subset(mod1_T2008_90_s4, mod1_T2008_90_s4$AOD != "NA")
names(mod1_T2008_90_s4)
mod1_T2008_90_s4$EPACode.y <- NULL 
mod1_T2008_90_s4$PM25.y <- NULL
mod1_T2008_90_s4$Lat_PM.y <- NULL
mod1_T2008_90_s4$Long_PM.y <- NULL
mod1_T2008_90_s4$guid.y <- NULL
library(reshape)
mod1_T2008_90_s4<-rename(mod1_T2008_90_s4,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s4.dbf")

mod1_T2008_10_s4 <- subset(mod1_T2008_10_s4, mod1_T2008_10_s4$AOD != "NA")


write.dbf(mod1_T2008_10_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s4_short.dbf")


write.dbf(mod1_T2008_90_s4,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s4.dbf")




#s5
splits_s5 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s5 <- splits_s5$trainset
mod1_T2008_90_s5 <- splits_s5$testset
names(mod1_T2008_90_s5)

mod1_T2008_90_s5 <- subset(mod1_T2008_90_s5, mod1_T2008_90_s5$AOD != "NA")
names(mod1_T2008_90_s5)
mod1_T2008_90_s5$EPACode.y <- NULL 
mod1_T2008_90_s5$PM25.y <- NULL
mod1_T2008_90_s5$Lat_PM.y <- NULL
mod1_T2008_90_s5$Long_PM.y <- NULL
mod1_T2008_90_s5$guid.y <- NULL
library(reshape)
mod1_T2008_90_s5<-rename(mod1_T2008_90_s5,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s5.dbf")

mod1_T2008_10_s5 <- subset(mod1_T2008_10_s5, mod1_T2008_10_s5$AOD != "NA")


write.dbf(mod1_T2008_10_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s5_short.dbf")


write.dbf(mod1_T2008_90_s5,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s5.dbf")



#s6
splits_s6 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s6 <- splits_s6$trainset
mod1_T2008_90_s6 <- splits_s6$testset
names(mod1_T2008_90_s6)

mod1_T2008_90_s6 <- subset(mod1_T2008_90_s6, mod1_T2008_90_s6$AOD != "NA")
names(mod1_T2008_90_s6)
mod1_T2008_90_s6$EPACode.y <- NULL 
mod1_T2008_90_s6$PM25.y <- NULL
mod1_T2008_90_s6$Lat_PM.y <- NULL
mod1_T2008_90_s6$Long_PM.y <- NULL
mod1_T2008_90_s6$guid.y <- NULL
library(reshape)
mod1_T2008_90_s6<-rename(mod1_T2008_90_s6,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s6.dbf")

mod1_T2008_10_s6 <- subset(mod1_T2008_10_s6, mod1_T2008_10_s6$AOD != "NA")


write.dbf(mod1_T2008_10_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s6_short.dbf")


write.dbf(mod1_T2008_90_s6,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s6.dbf")



#s7
splits_s7 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s7 <- splits_s7$trainset
mod1_T2008_90_s7 <- splits_s7$testset
names(mod1_T2008_90_s7)

mod1_T2008_90_s7 <- subset(mod1_T2008_90_s7, mod1_T2008_90_s7$AOD != "NA")
names(mod1_T2008_90_s7)
mod1_T2008_90_s7$EPACode.y <- NULL 
mod1_T2008_90_s7$PM25.y <- NULL
mod1_T2008_90_s7$Lat_PM.y <- NULL
mod1_T2008_90_s7$Long_PM.y <- NULL
mod1_T2008_90_s7$guid.y <- NULL
library(reshape)
mod1_T2008_90_s7<-rename(mod1_T2008_90_s7,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s7.dbf")

mod1_T2008_10_s7 <- subset(mod1_T2008_10_s7, mod1_T2008_10_s7$AOD != "NA")


write.dbf(mod1_T2008_10_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s7_short.dbf")


write.dbf(mod1_T2008_90_s7,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s7.dbf")



#s8
splits_s8 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s8 <- splits_s8$trainset
mod1_T2008_90_s8 <- splits_s8$testset
names(mod1_T2008_90_s8)

mod1_T2008_90_s8 <- subset(mod1_T2008_90_s8, mod1_T2008_90_s8$AOD != "NA")
names(mod1_T2008_90_s8)
mod1_T2008_90_s8$EPACode.y <- NULL 
mod1_T2008_90_s8$PM25.y <- NULL
mod1_T2008_90_s8$Lat_PM.y <- NULL
mod1_T2008_90_s8$Long_PM.y <- NULL
mod1_T2008_90_s8$guid.y <- NULL
library(reshape)
mod1_T2008_90_s8<-rename(mod1_T2008_90_s8,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s8.dbf")

mod1_T2008_10_s8 <- subset(mod1_T2008_10_s8, mod1_T2008_10_s8$AOD != "NA")


write.dbf(mod1_T2008_10_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s8_short.dbf")


write.dbf(mod1_T2008_90_s8,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s8.dbf")



#s9
splits_s9 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s9 <- splits_s9$trainset
mod1_T2008_90_s9 <- splits_s9$testset
names(mod1_T2008_90_s9)

mod1_T2008_90_s9 <- subset(mod1_T2008_90_s9, mod1_T2008_90_s9$AOD != "NA")
names(mod1_T2008_90_s9)
mod1_T2008_90_s9$EPACode.y <- NULL 
mod1_T2008_90_s9$PM25.y <- NULL
mod1_T2008_90_s9$Lat_PM.y <- NULL
mod1_T2008_90_s9$Long_PM.y <- NULL
mod1_T2008_90_s9$guid.y <- NULL
library(reshape)
mod1_T2008_90_s9<-rename(mod1_T2008_90_s9,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s9.dbf")

mod1_T2008_10_s9 <- subset(mod1_T2008_10_s9, mod1_T2008_10_s9$AOD != "NA")


write.dbf(mod1_T2008_10_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s9_short.dbf")


write.dbf(mod1_T2008_90_s9,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s9.dbf")




#s10
splits_s10 <- splitdf(MERGED_DATA2008)

mod1_T2008_10_s10 <- splits_s10$trainset
mod1_T2008_90_s10 <- splits_s10$testset
names(mod1_T2008_90_s10)

mod1_T2008_90_s10 <- subset(mod1_T2008_90_s10, mod1_T2008_90_s10$AOD != "NA")
names(mod1_T2008_90_s10)
mod1_T2008_90_s10$EPACode.y <- NULL 
mod1_T2008_90_s10$PM25.y <- NULL
mod1_T2008_90_s10$Lat_PM.y <- NULL
mod1_T2008_90_s10$Long_PM.y <- NULL
mod1_T2008_90_s10$guid.y <- NULL
library(reshape)
mod1_T2008_90_s10<-rename(mod1_T2008_90_s10,c(EPACode.x="EPACode",PM25.x="PM25",Lat_PM.x="Lat_PM",Long_PM.x="Long_PM")) 



write.dbf(mod1_T2008_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s10.dbf")

mod1_T2008_10_s10 <- subset(mod1_T2008_10_s10, mod1_T2008_10_s10$AOD != "NA")


write.dbf(mod1_T2008_10_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_10_s10_short.dbf")


write.dbf(mod1_T2008_90_s10,"c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/mod1_T2008_90_s10.dbf")




