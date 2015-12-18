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
write.dbf(ind,"/home/zeltak/ZH_tmp/dat/indiv1415.dbf")


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
summary(ind$Mother_Nat)

# ###recoding
# #finer race
ind$mrn.n<-0 #for jews
ind<- ind[Mother_Nat  == "יהודי" , mrn.n  := 1] #jewish
#gender 01
ind$sex<-1 #for male
ind<- ind[Gender  == "נקבה" , sex  := 0] #female




#only nox liner, dist linear, #nox was most significance

##Headc regression nox
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
m1 <- lm(m1.formula,data=ind)
summary(m1)

##Headc regression nox
r1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
r1 <- lm(r1.formula,data=ind)
summary(r1)

#txt
stargazer(m1, r1,
type="text",
dep.var.labels=c("Head Circumference"),
column.labels=c("PM2.5 model","NOx model","SO2 model","Multi exposure model"),
covariate.labels=c("Afgar","Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"), 
title="SSA level regression results: Head Circumference",
intercept.bottom = FALSE,
omit.stat = c("rsq", "f","adj.rsq","ser"),
out="/media/NAS/Uni/Projects/P050_Haifa/3.results/report_2015/xx.txt")





