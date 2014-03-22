library (MASS)
library (splines)
library(nlme)


#IMPORT LAG0

allresp_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/resp0106.csv", header=T)
names(allresp_m25)

Rresp_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25))
summary(Rresp_m25x)$tTable


allcvd_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/cvd0106.csv", header=T)
names(allcvd_m25)

Rcvd_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25))
summary(Rcvd_m25x)$tTable


allstroke_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/stroke0106.csv", header=T)
names(allstroke_m25)

Rstroke_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstroke_m25))
summary(Rstroke_m25x)$tTable

allstrisc_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/strisc0106.csv", header=T)
names(allstrisc_m25)

Rstrisc_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstrisc_m25))
summary(Rstrisc_m25x)$tTable

allstrhem_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/strhem0106.csv", header=T)
names(allstrhem_m25)

Rstrhem_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstrhem_m25))
summary(Rstrhem_m25x)$tTable


allpneum_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/pneum0106.csv", header=T)
names(allpneum_m25)

Rpneum_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allpneum_m25))
summary(Rpneum_m25x)$tTable


allmi_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/mi0106.csv", header=T)
names(allmi_m25)

Rmi_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allmi_m25))
summary(Rmi_m25x)$tTable


allihd_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/ihd0106.csv", header=T)
names(allihd_m25)

Rihd_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allihd_m25))
summary(Rihd_m25x)$tTable



allgi_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/gi0106.csv", header=T)
names(allgi_m25)

Rgi_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allgi_m25))
summary(Rgi_m25x)$tTable

alldiab_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/diab0106.csv", header=T)
names(alldiab_m25)

Rdiab_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = alldiab_m25))
summary(Rdiab_m25x)$tTable


allcopd_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/copd0106.csv", header=T)
names(allcopd_m25)

Rcopd_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcopd_m25))
summary(Rcopd_m25x)$tTable


allchf_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/chf0106.csv", header=T)
names(allchf_m25)

Rchf_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allchf_m25))
summary(Rchf_m25x)$tTable


allari_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/m25/ari0106.csv", header=T)
names(allari_m25)

Rari_m25x <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l0+pmnewmayear+temp_f_l0+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allari_m25))
summary(Rari_m25x)$tTable







#LAG 1

Rresp_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allresp_m25))
summary(Rresp_m25xl1)$tTable



Rcvd_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcvd_m25))
summary(Rcvd_m25xl1)$tTable


Rstroke_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstroke_m25))
summary(Rstroke_m25xl1)$tTable


Rstrisc_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstrisc_m25))
summary(Rstrisc_m25xl1)$tTable



Rstrhem_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allstrhem_m25))
summary(Rstrhem_m25xl1)$tTable




Rpneum_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allpneum_m25))
summary(Rpneum_m25xl1)$tTable


Rmi_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allmi_m25))
summary(Rmi_m25xl1)$tTable



Rihd_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allihd_m25))
summary(Rihd_m25xl1)$tTable



Rgi_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allgi_m25))
summary(Rgi_m25xl1)$tTable



Rdiab_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = alldiab_m25))
summary(Rdiab_m25xl1)$tTable




Rcopd_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allcopd_m25))
summary(Rcopd_m25xl1)$tTable



Rchf_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allchf_m25))
summary(Rchf_m25xl1)$tTable


Rari_m25xl1 <- (glmmPQL(count ~ ns(date,df=35)+pmnew_l1+pmnewmayear+temp_f_l1+med_inc+Avg_per_mi+Avg_pctnoh+Avg_p_A65, random = ~ 1 | guid, family = poisson, data = allari_m25))
summary(Rari_m25xl1)$tTable


sink("c:\\Users\\ekloog\\Documents\\tmp\\medicare_NE.txt",append=TRUE,split=TRUE) 

#IMPORT LAG0
summary(Rresp_m25x)$tTable
summary(Rcvd_m25x)$tTable
summary(Rstroke_m25x)$tTable
summary(Rstrisc_m25x)$tTable
summary(Rstrhem_m25x)$tTable
summary(Rpneum_m25x)$tTable
summary(Rmi_m25x)$tTable
summary(Rihd_m25x)$tTable
summary(Rgi_m25x)$tTable
summary(Rdiab_m25x)$tTable
summary(Rcopd_m25x)$tTable
summary(Rchf_m25x)$tTable
summary(Rari_m25x)$tTable
#LAG 1
summary(Rresp_m25xl1)$tTable
summary(Rcvd_m25xl1)$tTable
summary(Rstroke_m25xl1)$tTable
summary(Rstrisc_m25xl1)$tTable
summary(Rstrhem_m25xl1)$tTable
summary(Rpneum_m25xl1)$tTable
summary(Rmi_m25xl1)$tTable
summary(Rihd_m25xl1)$tTable
summary(Rgi_m25xl1)$tTable
summary(Rdiab_m25xl1)$tTable
summary(Rcopd_m25xl1)$tTable
summary(Rchf_m25xl1)$tTable
summary(Rari_m25xl1)$tTable



