library(nlme)
library(foreign) 
library(ggplot2)


mod1 <-  read.dbf("f:/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/mod1_2001.dbf") 
DELLIST <-  names(mod1) %in% c("DTckin", "humidity")
mod1d <- mod1[!DELLIST]
mod1d<-na.omit(mod1d)
mod1d$predicted<-NA


#new model
out.model_T2001 = lme( tempc ~ NTckin+elev+purban+NDVI, random = ~1 + NTckin| date,  data =  mod1d) 
summary(out.model_T2001)
mod1d$resid<-residuals(out.model_T2001)
sub2001 <- subset(mod1d, resid <= 20 & resid <= -20,select=c(guid,purban))
mod1d<- mod1d[!(mod1d$guid %in% sub2001$guid),]
#rerun
out.model_T2001 = lme( tempc ~ NTckin+elev+purban+NDVI, random = ~1 + NTckin| date,  data =  mod1d) 
mod1d$predicted<-predict(out.model_T2001)


#R2-noncal
ggplot(mod1d, aes(NTckin,tempc)) + geom_point() + geom_smooth()
summary(lm(mod1d$tempc~mod1d$NTckin))
#R2-calibrated
ggplot(mod1d, aes(predicted,tempc)) + geom_point() + geom_smooth()
summary(lm(mod1d$tempc~mod1d$predicted))





# density plot

ran2001 <-out.model_T2001$coef$random 
ran2<-unlist(out.model_T2001$coef$random)

summary(ran2001)
ran2001$NTckin
names(mod1d)
summary(mod1d$NTckin)
summary(mod1d$fixran)

ran2001 #copy paste colums from term to editor and save as text

#then reread the file after i manually edit the above step ^^^^^
data1 <- read.table("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/random_effect/ranef2.txt", header = TRUE)

data1$fixran <- data1$NTckin+0.321 #add the fixed effect (9.45) to each random effect (st_faren)


#just the random effect (NTckin)
d <- density(data1$fixran) # returns the density data
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")


qplot(fixran, data = data1, geom = "density")+geom_density2d()











