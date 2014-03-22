###############
#LIBS
###############
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
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


mon <-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/amar_temp_qt/X2003.csv")
mon<-na.omit(mon)
mon <- mon[, day:=as.Date(strptime(Date, "%m/%d/%y"))]



bosmon <-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P000_TMP_PROJECTS/amar_temp_qt/relevant150kstations.csv")

monboston <- mon [mon$station %in% bosmon$station, ] 
m1<-summary(lm(tempc~pred_m3,data=monboston))
#RMSPE
sqrt(mean(m1$residual^2))


#logan is Kbos

monlogan <- mon[station %in% c("KBOS"), ]
monlogan <-monlogan [,c(3,18),with=FALSE]
setnames(monlogan,"tempc","tlogan")

setkey(monboston,day)
setkey(monlogan,day)
dat <- merge(monboston,monlogan, all.x = T)


m1<-summary(lm(tempc~tlogan,data=dat))
#RMSPE
sqrt(mean(m1$residual^2))



