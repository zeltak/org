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

pm2004<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2004.rds")
pm2004<-pm2004[,c(1:4,9),with=FALSE]
#pm2004<-as.data.frame(pm2004)

write.csv(pm2004,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2004.csv")



pm2005<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2005.rds")
pm2005<-pm2005[,c(1:4,9),with=FALSE]
#pm2005<-as.data.frame(pm2005)

write.csv(pm2005,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2005.csv")


pm2006<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2006.rds")
pm2006<-pm2006[,c(1:4,9),with=FALSE]
#pm2006<-as.data.frame(pm2006)

write.csv(pm2006,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2006.csv")



pm2007<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2007.rds")
pm2007<-pm2007[,c(1:4,9),with=FALSE]
#pm2007<-as.data.frame(pm2007)

write.csv(pm2007,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2007.csv")
 



pm2008<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2008.rds")
pm2008<-pm2008[,c(1:4,9),with=FALSE]
#pm2008<-as.data.frame(pm2008)

write.csv(pm2008,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2008.csv")
 



pm2009<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2009.rds")
pm2009<-pm2009[,c(1:4,9),with=FALSE]
#pm2009<-as.data.frame(pm2009)

write.csv(pm2009,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2009.csv")
 

pm2010<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2010.rds")
pm2010<-pm2010[,c(1:4,9),with=FALSE]
#pm2010<-as.data.frame(pm2010)

write.csv(pm2010,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2010.csv")
 

pm2011<-readRDS("Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2011.rds")
pm2011<-pm2011[,c(1:4,9),with=FALSE]
#pm2011<-as.data.frame(pm2011)

write.csv(pm2011,"Y:/3.Work/2.Gather_data/FN008_model_prep/mod3best_2011.csv")
 
