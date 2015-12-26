###############
#LIBSy
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
library(broom)
library(splines)
library(DataCombine)
library(spdep)
library(classInt)
library(e1071)


try<-svm(dB_Avg_M~ D_Bustop+D_Building+D_Gastation+D_Railine+D_Mroad+Lanes_AR+D_Aroad+D_Busline+Cars_hour+D_trafficlight+L_Aroads+L_Mraods+Pop+Num_Buslines+L_buslines+Slope+P_residence+P_Publicopenspace+P_Trade_and_Services+P_industry+P_Openspace+oneway,
         type="nu-regression",cross=10,data=TA_T)


TA_T$pred <- predict(object=try) #morning
head(TA_T)
print(summary(lm(dB_Avg_M~ pred ,data=TA_T))$r.squared)
