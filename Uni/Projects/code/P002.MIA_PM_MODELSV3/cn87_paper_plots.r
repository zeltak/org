library(nlme)
library(foreign) 
library(ggplot2)

F_T2006_All<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN002_mod1_CV/overall_random/pdataA_2006.dbf") 
names(F_T2006_All)




trend2<-qplot(AOD,PM25,data=F_T2006_All, xlab="AOD", ylab="PM25", )
trend2<-trend2+stat_smooth(method="lm")
trend2



trend1<-qplot(Pred,PM25,data=F_T2006_All, xlab="Predicted PM25", ylab="PM25", )
trend1<-trend1+stat_smooth(method="lm")
trend1





# density plot

# names(F_T2006_All)


ran2001 <-out.model_T2003$coef$random 
ran2<-unlist(out.model_T2003$coef$random)

summary(ran2001)
ran2001$AOD
names(data1)
summary(data1$AOD)
summary(data1$fixran)

ran2001 #copy paste colums from term to editor and save as text

#then reread the file after i manually edit the above step ^^^^^
data1 <- read.table("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/random_effect/ranef2.txt", header = TRUE)

data1$fixran <- data1$AOD+0.321 #add the fixed effect 


#just the random effect (AOD)
d <- density(data1$fixran) # returns the density data
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")


qplot(fixran, data = data1, geom = "density")+geom_density2d()
# 
# 









