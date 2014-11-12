library(rpart)

nyc <- read.csv("E:/users/jm17/Dropbox/Multi_Exposure_Project/Data/nyc.csv")
nyc <- read.csv("/Users/james/Dropbox/Multi_Exposure_Project/Data/nyc.csv")
dim(nyc)
names(nyc)

fit <-rpart(ChildHospAst ~ PM25 +roachhome_perc +NO2 +SHS_perc, method="anova", data=nyc)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
   main="Classification Tree for Child Asthma Hospitalizations")
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