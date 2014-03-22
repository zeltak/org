#Prediction
library(data.table)
library(plyr)
library(reshape2)
library(foreign)
library(Hmisc)
library(mgcv)
#library(rgdal)


cases<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/MCM_bgu_2014/Work/cases.csv")
casesknwon<- cases[SemelYeshuv>90,]
casesshavtim<- cases[SemelYeshuv<=90,]


#unkown
names(unmapped)
unmapped<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/MCM_bgu_2014/Work/nosemeld_cords.csv")
unmapped<-unmapped[,c(11,12,13),with=FALSE]

setkey(casesshavtim,SemelYeshuv)
setnames(unmapped,"semelYeshuv","SemelYeshuv")
setkey(unmapped,SemelYeshuv)
casesshvatimXY<- merge(casesshavtim,unmapped,all.x = T)






#knwon
semelxy<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/MCM_bgu_2014/Work/semelxyXY.csv")
names(semelxy)
semelxy<-semelxy[,c(3,25,26),with=FALSE]
setnames(semelxy,"SEMEL_YISH","SemelYeshuv")

describe(semelxy$SemelYeshuv)
semelxy <- unique(semelxy, by="SemelYeshuv")

setkey(casesknwon,SemelYeshuv)
setkey(semelxy,SemelYeshuv)
casesknownXY<- merge(casesknwon,semelxy)

final<-rbind(casesshvatimXY,casesknownXY)

write.csv(final,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/MCM_bgu_2014/Work/final_cases.csv")



