library(lme4)
library(foreign) 
library(psych)
library(car)
library(reshape)
library(mgcv)

#CHANGE1
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

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

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


#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

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

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2003_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2003.csv", header=T)


T2003_weight <- na.omit(T2003_weight)

names(T2003_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2003_weight) #run modle
summary(w1)
T2003_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2003_weight$wt <- 1/T2003_weight$prob

T2003_weight$normwt <- T2003_weight$wt/mean(T2003_weight$wt)
T2003_weight <- rename(T2003_weight, c(date="Date"))

summary(T2003_weight$normwt)

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2004_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2004.csv", header=T)


T2004_weight <- na.omit(T2004_weight)

names(T2004_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2004_weight) #run modle
summary(w1)
T2004_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2004_weight$wt <- 1/T2004_weight$prob

T2004_weight$normwt <- T2004_weight$wt/mean(T2004_weight$wt)
T2004_weight <- rename(T2004_weight, c(date="Date"))

summary(T2004_weight$normwt)

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2005_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2005.csv", header=T)


T2005_weight <- na.omit(T2005_weight)

names(T2005_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2005_weight) #run modle
summary(w1)
T2005_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2005_weight$wt <- 1/T2005_weight$prob

T2005_weight$normwt <- T2005_weight$wt/mean(T2005_weight$wt)
T2005_weight <- rename(T2005_weight, c(date="Date"))

summary(T2005_weight$normwt)

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2006_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2006.csv", header=T)


T2006_weight <- na.omit(T2006_weight)

names(T2006_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2006_weight) #run modle
summary(w1)
T2006_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2006_weight$wt <- 1/T2006_weight$prob

T2006_weight$normwt <- T2006_weight$wt/mean(T2006_weight$wt)
T2006_weight <- rename(T2006_weight, c(date="Date"))

summary(T2006_weight$normwt)

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

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

#>>>>>>>>>>>>>>>>>>>>create WEIGHTS

T2008_weight <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/2.Gather_data/FN009_Weights/y2008.csv", header=T)


T2008_weight <- na.omit(T2008_weight)

names(T2008_weight)

w1<- glm(obs ~ elev+slp_F+Temp_F+as.factor(m),family=binomial,data=T2008_weight) #run modle
summary(w1)
T2008_weight$prob <- predict(w1,type = c("response"))  #get probability prediction , note that its a binary logisitc and thus the type-repsonse option

T2008_weight$wt <- 1/T2008_weight$prob

T2008_weight$normwt <- T2008_weight$wt/mean(T2008_weight$wt)
T2008_weight <- rename(T2008_weight, c(date="Date"))

summary(T2008_weight$normwt)

allw<-rbind(T2000_weight,T2001_weight,T2002_weight,T2003_weight,T2004_weight,T2005_weight,T2006_weight,T2007_weight,T2008_weight)

summary(allw$normwt)
hist(allw$normwt)

# Kernel Density Plot
d <- density(allw$normwt) # returns the density data
plot(d) # plots the results

