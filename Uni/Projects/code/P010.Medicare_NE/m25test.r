library (MASS)
library (splines)
library(nlme)




allcvd_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/cvd0106.csv", header=T)
names(allcvd_m25)

plot(allcvd_m25$count,allcvd_m25$pmnewmayear)


allcvd_m25_r1<-subset(allcvd_m25,allcvd_m25$reg_id==1) 
allcvd_m25_r2<-subset(allcvd_m25,allcvd_m25$reg_id==2)
allcvd_m25_r3<-subset(allcvd_m25,allcvd_m25$reg_id==3)

allcvdx<- rbind(allcvd_m25_r2,allcvd_m25_r3)

summary(allcvd_m25_r1)


Rcvdm25_r1 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r1))
summary(Rcvdm25_r1)$tTable


Rcvdm25_r2 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r2))
summary(Rcvdm25_r2)$tTable



Rcvdm25_r3 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r3))
summary(Rcvdm25_r3)$tTable

Rcvdm25_ALL <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allcvd_m25))
summary(Rcvdm25_ALL)$tTable

Rcvdm25_rx <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allcvd_m25))
summary(Rcvdm25_rx)$tTable



Rcvd_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25))
summary(Rcvd_m25x)$tTable





Rcvdm25_r1_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r1))
summary(Rcvdm25_r1_full)$tTable


Rcvdm25_r2_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r2))
summary(Rcvdm25_r2_full)$tTable



Rcvdm25_r3_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25_r3))
summary(Rcvdm25_r3_full)$tTable





#REP






allresp_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/resp0106.csv", header=T)
names(allresp_m25)

plot(allresp_m25$count,allresp_m25$pmnewmayear)


allresp_m25_r1<-subset(allresp_m25,allresp_m25$reg_id==1) 
allresp_m25_r2<-subset(allresp_m25,allresp_m25$reg_id==2)
allresp_m25_r3<-subset(allresp_m25,allresp_m25$reg_id==3)

allrespx<- rbind(allresp_m25_r2,allresp_m25_r3)

summary(allresp_m25_r1)


Rrespm25_r1 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allresp_m25_r1))
summary(Rrespm25_r1)$tTable


Rrespm25_r2 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allresp_m25_r2))
summary(Rrespm25_r2)$tTable



Rrespm25_r3 <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allresp_m25_r3))
summary(Rrespm25_r3)$tTable

Rrespm25_ALL <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allresp_m25))
summary(Rrespm25_ALL)$tTable

Rrespm25_rx <- (glmmPQL(count ~ ns(date,df=30)+pmnewmayear, random = ~ 1 | guid, family = poisson, data = allresp_m25))
summary(Rrespm25_rx)$tTable



Rresp_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25))
summary(Rresp_m25x)$tTable





Rrespm25_r1_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25_r1))
summary(Rrespm25_r1_full)$tTable


Rrespm25_r2_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25_r2))
summary(Rrespm25_r2_full)$tTable



Rrespm25_r3_full <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25_r3))
summary(Rrespm25_r3_full)$tTable
