#add all packages
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
library(e1071)

#load data
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c3.rds")

###base model
m1.formula <- as.formula(pm25~ aod
+dport.s+dtrain.s+daroad.s+dcoast.s+dwb.s    
+NO2.s+SO2.s
+PM10ems.s
+ p.agric.s
+ p.open.s
+p.forest.s        
+p.urban.s
+temp.s+elev_m+tden+pbl.s)

#svm
model <- svm( m1.formula, mod1)
mod1$pred.m1 <- predict(model)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)


#example model
m1.formula <- as.formula(pm25 ~ aod
#temporal
+pbl.s+tempc+wdsp+NDVI
#spatial
poden+purban+ +elev_m+tden
#distances
+dport.s+dtrain.s+daroad.s+dcoast.s+dwb.s    
#emission data
 +NO2.s+SO2.s+PM10ems.s
#land use                
+ p.agric.s+ p.open.s+p.forest.s +p.urban.s
#random component
+(1+aod|day/cid)  )  


