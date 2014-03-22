library (MASS)
library (splines)
library(nlme)
library(lme4)


sink("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/offset/results.txt",append=TRUE,split=TRUE) 




allresp_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/offset/respmatt0106.csv", header=T)
names(allresp_m25)

glmer(count~pmnewmayear + (1|guid)  , data=allresp_m25, family='poisson')

glmer(count~pmnewmayear+ (1|guid) , offset=log(offset)  , data=allresp_m25, family='poisson')



allcvd_m25 = read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.10.Medicare_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/short/offset/cvdmatt0106.csv", header=T)
names(allcvd_m25)

glmer(count~pmnewmayear + (1|guid)  , data=allcvd_m25, family='poisson')

glmer(count~pmnewmayear+ (1|guid) , offset=log(offset)  , data=allcvd_m25, family='poisson')



