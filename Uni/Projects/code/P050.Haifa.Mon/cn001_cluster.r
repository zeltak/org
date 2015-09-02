###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(broom)
library(FNN)
library(zoo)
library(DataCombine)
library(readr)
library(ztable)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha_ex-1.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/geomerge_alpha.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/lsR.r")


ssa<-fread("/media/NAS/Uni/Projects/P050_Haifa/2.work/SSA/ssa_poll.csv")
head(ssa)
#clean for cluster
ssa$SET_NAME <- NULL
ssa$SAM_YASUV <- NULL
# ssa$S_STAT <- NULL
# ssa$S_YASUV <- NULL
# ssa$DIROG <- NULL
# ssa$MADD_SOC <- NULL
# ssa$INCOME <- NULL
# ssa$ESHKOL_NUM <- NULL
# ssa$syscode2  <- NULL
names(ssa)
setnames(ssa,"MEANCONTOU","ssaso2")

#PM
cluspm<-select(ssa,ssapm25)
# cluspm$STAT_Z<-as.character(cluspm$STAT_Z)
# K-Means Cluster Analysis
fit <- kmeans(cluspm, 3) # 3 cluster solution
fit
ssa$pmclus<-fit$cluster
ssa <- ssa[pmclus  == 1 , pmclus_o  := "mid"]
ssa <- ssa[pmclus  == 2 , pmclus_o  := "high"]
ssa <- ssa[pmclus  == 3 , pmclus_o  := "low"]

#co2
cluspm<-select(ssa,ssaso2)
# cluspm$STAT_Z<-as.character(cluspm$STAT_Z)
# K-Means Cluster Analysis
fit <- kmeans(cluspm, 3) # 3 cluster solution
fit
ssa$so2clus<-fit$cluster
ssa <- ssa[so2clus  == 1 , so2clus_o  := "low"]
ssa <- ssa[so2clus  == 2 , so2clus_o  := "high"]
ssa <- ssa[so2clus  == 3 , so2clus_o  := "mid"]

#nox
cluspm<-select(ssa,sasnox)
# cluspm$STAT_Z<-as.character(cluspm$STAT_Z)
# K-Means Cluster Analysis
fit <- kmeans(cluspm, 3) # 3 cluster solution
fit
ssa$noxclus<-fit$cluster
ssa <- ssa[noxclus  == 1 , noxclus_o  := "high"]
ssa <- ssa[noxclus  == 2 , noxclus_o  := "low"]
ssa <- ssa[noxclus  == 3 , noxclus_o  := "mid"]

#export for mapping
write.csv(ssa,"/media/NAS/Uni/Projects/P050_Haifa/2.work/SSA/poll_clusters.csv")




#set ztable opt
options(ztable.type="html")


#PM25 annova 
#head
aov.ex1 = aov(Avg_Head1_~pmclus_o,data=ssa)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell

png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.pm.head.png")
boxplot(Avg_Head1_~pmclus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex1)

#Birth weight
aov.ex2 = aov(Avg_Weight~as.factor(pmclus_o),data=ssa)  #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.pm.bw.png")
boxplot(Avg_Weight~pmclus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex2)

#so2 annova 
#head
aov.ex1 = aov(Avg_Head1_~so2clus_o,data=ssa)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell

png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.so2.head.png")
boxplot(Avg_Head1_~so2clus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex1)

#Birth weight
aov.ex2 = aov(Avg_Weight~as.factor(so2clus_o),data=ssa)  #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.so2.bw.png")
boxplot(Avg_Weight~so2clus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex2)

#nox annova 
#head
aov.ex1 = aov(Avg_Head1_~noxclus_o,data=ssa)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell

png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.nox.head.png")
boxplot(Avg_Head1_~noxclus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex1)

#Birth weight
aov.ex2 = aov(Avg_Weight~as.factor(noxclus_o),data=ssa)  #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
png(filename="/media/NAS/Uni/Projects/P050_Haifa/3.results/images/an.nox.bw.png")
boxplot(Avg_Weight~noxclus,data=ssa)        #graphical summary
dev.off()
TukeyHSD(aov.ex2)


#regressions






