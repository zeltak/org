# resp

allresp = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/FINAL/resp0006h.csv", header=T)
names(allresp)

allresp<- na.omit(allresp)

Rresp <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp)$tTable


Rresp1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den++pctcrowd, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp1)$tTable

Rresp2 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den+pctownocch, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp2)$tTable


Rresp3 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+ah_gm3_F_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp3)$tTable



Rresp4 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+ah_gm3_F_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp4)$tTable


Rresp5 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l2+pmnewmayear+temp_f_l2+ah_gm3_F_l2+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allresp))
summary(Rresp5)$tTable


# MALES:

allrespm = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/FINAL/resp0006m.csv", header=T)
names(allresp)

allresmp<- na.omit(allrespm)

Rresp7 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allrespm))
summary(Rresp7)$tTable




# FEMALES:

allrespf = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/FINAL/resp0006f.csv", header=T)

allrespf<- na.omit(allrespf)

Rresp8 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65+P_Ospace+pop_den, random = ~ 1 | guid, family = poisson, data = allrespf))
summary(Rresp8)$tTable
