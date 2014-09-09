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

#######################################
#######################################
#import all grid cells 200m Landuse 
MIA<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/0.raw/gis/l200/MIA.csv")
NE<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/0.raw/gis/l200/NE.csv")

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

NE<-NE[,c(5:12),with=FALSE]
MIA<-MIA[,c(3:7,11,13,14),with=FALSE]

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

MIA<-setcolorder(MIA, c(1,2,6,3,5,4,7,8))

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

flu<-rbind(MIA,NE)

setnames(flu,"popdens","popden")
setnames(flu,"urban","pcturban")
setnames(flu,"x_alb","x_alb_LPMgrid")
setnames(flu,"y_alb","y_alb_LPMgrid")
flu$lpmid<-paste(flu$x_alb_LPMgrid,flu$y_alb_LPMgrid,sep="")
###############################################
saveRDS(flu,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200.rds")

###### export X,Y (x_alb and y_alb)
fluCP<-flu[,c(7,8,9),with=FALSE]
write.dbf(fluCP,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_XY.dbf")

######### Reimport after GIS join of above XY to LU-met IDs

test<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/LPM_GRID_IDS.csv")
setnames(test,"x_alb","x_alb_LPMgrid")
setnames(test,"y_alb","y_alb_LPMgrid")
test$lpmid<-paste(test$x_alb_LPMgrid,test$y_alb_LPMgrid,sep="")


setkey(flu,lpmid)
setkey(test,lpmid)
test2<- merge(flu,test,all.x=TRUE)
test2[,x_alb_LPMgrid.y :=NULL]
test2[,y_alb_LPMgrid.y :=NULL]
setnames(test2,"x_alb_LPMgrid.x","x_alb_LPMgrid")
setnames(test2,"y_alb_LPMgrid.x","y_alb_LPMgrid")

saveRDS(test2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.rds")
write.dbf(test2,"/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/GRID_lu200_IDS.dbf")

