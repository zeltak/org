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
library(stargazer)

ind <-read.csv("/media/NAS/Uni/Projects/P050_Haifa/2.work/Individual/indiv1415.csv")
str(ind)
ind$BDX <- gsub(" 0:00:00", "",ind$Birth_Date)
ind<-as.data.table(ind)
ind[, day:=as.Date(strptime(BDX, "%m/%d/%Y"))]
ind[, month := as.numeric(format(day, "%m")) ]

#########
###regression
m1.formula <- as.formula(Head1_Valu~PregnancyW+as.factor(month)+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+nox)

m2.formula <- as.formula(Head1_Valu~PregnancyW++as.factor(month)+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+pm25)

m3.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+so2)

m4.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+so2+pm25+nox)
 
m1 <- lm(m1.formula,data=ind)
m2 <- lm(m2.formula,data=ind)
m3 <- lm(m3.formula,data=ind)
m4 <- lm(m4.formula,data=ind)
  
summary(m1)
summary(m2)
summary(m4)
summary(m4)

#txt
stargazer(m1, m2, m3, m4,
type="text",
dep.var.labels=c("Head Circumference"),
column.labels=c("PM2.5 model","NOx model","SO2 model","Multi exposure model"),
covariate.labels=c("Afgar","Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"), 
title="SSA level regression results: Head Circumference",
intercept.bottom = FALSE,
omit.stat = c("rsq", "f","adj.rsq","ser"),
out="/media/NAS/Uni/Projects/P050_Haifa/3.results/report_2015/xx.txt")









###
r1.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+nox)
r2.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+pm25)
r3.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+so2)
r4.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+so2+pm25+nox)

r1 <- lm(r1.formula,data=ssa)
r2 <- lm(r2.formula,data=ssa)
r3 <- lm(r3.formula,data=ssa)
r4 <- lm(r4.formula,data=ssa)

summary(r1)

summary(r2)

summary(r4)

summary(r4)



#txt
stargazer(r1, r2, r3, r4,
type="text",
dep.var.labels=c("Head Circumference"),
column.labels=c("PM2.5 model","NOx model","SO2 model","Multi exposure model"),
covariate.labels=c("Afgar","Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"), 
title="SSA level regression results: Head Circumference",
intercept.bottom = FALSE,
omit.stat = c("rsq", "f","adj.rsq","ser"),
out="/media/NAS/Uni/Projects/P050_Haifa/3.results/report_2015/models_bw.txt")



sink("r1.txt")
summary(r1)
sink() 
sink("r2.txt")
summary(r2)
sink()
sink("r3.txt")
summary(r3)
sink()
sink("r4.txt")
summary(r4)
sink()
##near
Nh1.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_BAZAN+I(N_BAZAN^2))
Nh2.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT)
Nh3.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_POWERSTA)
Nh4.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_OIL_L)
Nh5.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_OIL_S)
Nh6.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_ROAD)
Nh7.formula <- as.formula(Head1_Valu~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD)

Nh1 <- lm(Nh1.formula,data=ssa)
Nh2 <- lm(Nh2.formula,data=ssa)
Nh3 <- lm(Nh3.formula,data=ssa)
Nh4 <- lm(Nh4.formula,data=ssa)
Nh5 <- lm(Nh5.formula, data = ssa)
Nh6 <- lm(Nh6.formula, data = ssa)
Nh7 <- lm(Nh7.formula, data = ssa)


Nw1.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_BAZAN+I(N_BAZAN^2))
Nw2.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT)
Nw3.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_POWERSTA)
Nw4.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_OIL_L)
Nw5.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_OIL_S)
Nw6.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_ROAD)
Nw7.formula <- as.formula(Weight1_Va~PregnancyW+Education_+ApgarOneMi+POPULATION+HOUSEHOLDS+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+BA+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD)

Nw1 <- lm(Nw1.formula,data=ssa)
Nw2 <- lm(Nw2.formula,data=ssa)
Nw3 <- lm(Nw3.formula,data=ssa)
Nw4 <- lm(Nw4.formula,data=ssa)
Nw5 <- lm(Nw5.formula, data = ssa)
Nw6 <- lm(Nw6.formula, data = ssa)
Nw7 <- lm(Nw7.formula, data = ssa)


