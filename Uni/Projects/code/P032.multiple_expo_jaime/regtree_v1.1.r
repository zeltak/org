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
library(party)
mexpo<-readRDS("/media/NAS/Uni/Projects/P032_multiple_expo_jaime/3.Work/FN001_datastes/bw_expo_final.rds")
#library(rpart)


#NOTE- caret package-may be worth while to look into
mexpo<-as.data.table(mexpo)


#fit <-rpart(birthw ~ ges_calc+sex, method="anova", data=mexpo)
?ctree
### regression
#mexpo<-mexpo
test<-mexpo[1:10000,]
# 
# bwctree <- ctree(birthw ~temppreg+pmpreg.x+NDVI+lan+med_income+sex, data = mexpo)
# bwctree
# plot(bwctree)

?cforest
# set up a simple unbiased forest
##increase mtry when you increase ntree // scale ntree 5x of mtry 
bwcforest <- cforest(birthw ~temppreg+pmpreg.x+med_income+sex, data = test,
               control = cforest_unbiased(mtry = 2, ntree = 20))
bwcforest
# biased variable importance
# (not protected for correlated predictrs - but fast)
varimp(bwcforest, conditional = F)
?varimp
# conditional variable importance - SLOW!
#varimp(bwcforest, conditional = T)

# peak under the hood
str(bwcforest, max.level = 2)
# not yet clear how to plot the trees
summary(lm(birthw~temppreg+pmpreg.x+NDVI+lan+med_income,data=mexpo))


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