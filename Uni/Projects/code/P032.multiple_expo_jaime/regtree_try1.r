###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
mexpo<-readRDS("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/FN001_datastes/bw_expo_final.rds")

mexpo$outcome<-as.numeric(mexpo$birthw)
library(rpart)
str(mexpo$outcome)
dim(mexpo)
str(mexpo)
hist(mexpo$outcome)
#summary(lm(outcome~temppreg+pmpreg.x+NDVI+lan+med_income,data=mexpo))
fit <-rpart(lbw ~ temppreg+pmpreg.x+NDVI+lan+med_income, method="class",control=rpart.control(minsplit=2,minbucket=1), data=mexpo)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
   main="bw")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(fit, file = "E:/users/jm17/Dropbox/Multi_Exposure_Project/Data/tree.ps",
   title = "Classification Tree for Child Asthma Hospitalizations")
   
# prune the tree
pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[ , "xerror"]), "CP"])

# plot pruned tree
plot(pfit, uniform=TRUE, main= "Pruned Classifcation Tree for Child Asthma Hospitalization")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file="/Users/james/Dropbox/Multi_Exposure_Project/Data/ptree.ps", title="Pruned Classification Tree for Child Asthma Hospitalizations")