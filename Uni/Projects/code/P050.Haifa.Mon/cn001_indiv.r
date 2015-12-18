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
library(stargazer)
library(mgcv)
library(splines)

ind <-read.csv("/home/zeltak/ZH_tmp/dat/indiv1415.csv")
str(ind)
head(ind)
ind$BDX <- gsub(" 0:00:00", "",ind$Birth_Date)
ind<-as.data.table(ind)
ind[, day:=as.Date(strptime(BDX, "%m/%d/%Y"))]
ind[, month := as.numeric(format(day, "%m")) ]
names(ind)
#write.dbf(ind,"/media/NAS/Uni/Projects/P050_Haifa/2.work/Individual/indiv1415.dbf")



#subset data
ind<-ind[,c("Head1_Valu","Gender","Weight1_Va","Mother_Nat","PregnancyW","month","TotalSibli","Education_","ApgarOneMi","ApgarFiveM","POPULATION","HOUSEHOLDS","AVERAGE_HH","DENSITY","OWNERSHIP","RENTALS","BAGRUT","BA","INCOME","N_AIRPORT","N_BAZAN","N_POWERSTA","N_OIL_L","N_OIL_S","N_ROAD","nox","day","Postal","Mother_Bir","pm25","so2"),with=FALSE]

#rename
setnames(ind,old=c("Weight1_Va", "Head1_Valu","PregnancyW"),new=c("birthw", "headc","ges"))


#clean data
#delete bad data
ind<-filter(ind,ges >= 20 )
ind<-filter(ind,ges <= 44 )
#clean all crap data
ind[ind == -999] <- NA
ind[ind == -9.99] <- NA
ind<-filter(ind,OWNERSHIP  >= 0 )
ind<-filter(ind,AVERAGE_HH  >= 0 )
ind<-filter(ind,BA  >= 0 )
ind<-filter(ind,birthw >= 2 )
summary(ind)

# ###recoding
# #finer race
# bd<- bd[mother_race  == 1 , mrn.n  := 1] #white
# bd<- bd[mother_race  == 5 , mrn.n  := 2] #hispanic
# bd<- bd[mother_race  == 2 , mrn.n  := 3] #black
# bd<- bd[mother_race  == 3 , mrn.n  := 4] #asian
# bd<- bd[mother_race  == 4 , mrn.n  := 5] #other
# bd<- bd[mother_race  == 8 , mrn.n  := 5] #other
# bd<- bd[mother_race  == 9 , mrn.n  := 5] #other

#########
###regression
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)

m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)


#try mixed model
m1mix.formula <- as.formula(headc ~ges+as.factor(month)+Gender+TotalSibli+Education_+ApgarOneMi+POPULATION+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_POWERSTA+N_OIL_L+N_OIL_S+N_ROAD+nox+(1|Postal))
m1mix <- lmer(m1mix.formula,data=ind)
summary(m1mix)

#splines
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+s(N_AIRPORT)+s(N_BAZAN)+s(N_POWERSTA)+s(N_OIL_L)+s(N_OIL_S)+s(N_ROAD)+nox )
m1 <- gam(m1.formula,data=ind)
summary(m1)


#splines
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+ns(N_AIRPORT,2)+ns(N_BAZAN,4)+ns(N_POWERSTA,2)+ns(N_OIL_L,2)+ns(N_OIL_S,2)+ns(N_ROAD,2)+nox )
m1 <- lm(m1.formula,data=ind)
summary(m1)

#splines
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+ns(N_BAZAN,2)+ns(N_ROAD,2)+nox )
m1 <- lm(m1.formula,data=ind)
summary(m1)


m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+ns(N_ROAD,2)+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)

m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)

m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+pm25)
m1 <- lm(m1.formula,data=ind)
summary(m1)

m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+so2)
m1 <- lm(m1.formula,data=ind)
summary(m1)


m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(Gender)+as.factor(Mother_Nat)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+INCOME+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+so2+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)

#only nox liner, dist linear
#nox was most significance


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


