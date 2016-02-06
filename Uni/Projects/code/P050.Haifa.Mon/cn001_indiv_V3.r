###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(readr)
library(stargazer)
library(splines)
library(sjPlot)

ind <-read.csv("/home/zeltak/ZH_tmp/dat/indiv1415av.csv")
str(ind)
head(ind)
ind$BDX <- gsub(" 0:00:00", "",ind$Birth_Date)
ind<-as.data.table(ind)
ind[, day:=as.Date(strptime(BDX, "%m/%d/%Y"))]
ind[, month := as.numeric(format(day, "%m")) ]
names(ind)
#write.dbf(ind,"/media/NAS/Uni/Projects/P050_Haifa/2.work/Individual/indiv1415.dbf")

#subset data
ind<-ind[,c("Head1_Valu","X","Y","Gender","Weight1_Va","Mother_Nat","PregnancyW","month","TotalSibli","Education_","ApgarOneMi","ApgarFiveM","POPULATION","HOUSEHOLDS","AVERAGE_HH","DENSITY","OWNERSHIP","RENTALS","BAGRUT","BA","INCOME","N_AIRPORT","N_BAZAN","N_POWERSTA","N_OIL_L","N_OIL_S","N_ROAD","nox","day","Postal","Mother_Bir","pm25","so2","nox2014","Elevation","People_est","Pop_arnona"),with=FALSE]

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






#write.dbf(ind,"/home/zeltak/ZH_tmp/dat/indiv1415.dbf")
#ind<-read.dbf("/home/zeltak/ZH_tmp/dat/indiv1415.dbf")
#summary(ind)
#setnames(ind,"mrn_n","mrn.n")
#only nox liner, dist linear, #nox was most significance
ind$N_BAZAN<-ind$N_BAZAN/1000
ind$N_AIRPORT<-ind$N_AIRPORT/1000
ind$N_POWERSTA<-ind$N_POWERSTA/1000
ind$N_OIL_L<-ind$N_OIL_L/1000
ind$N_OIL_S<-ind$N_OIL_S/1000
ind$N_ROAD<-ind$N_ROAD/1000
ind$headcWT<-ind$headc/ind$birthw
ind$birthwHC<-ind$birthw/ind$headc

ind.pre<-filter(ind, ges < 37)
ind.full<-filter(ind, ges >= 37)
#write.dbf(ind,"/home/zeltak/ZH_tmp/dat/indiv1415_clean.dbf")

#normal
##Headc regression nox
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
h1 <- lm(m1.formula,data=ind)
summary(h1)

##birth weight regression nox
r1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
b1 <- lm(r1.formula,data=ind)
summary(b1)

headc<-sjt.lm(h1,b1,
              digits.est=4,
              group.pred = TRUE, #group catagorical variables
              showHeaderStrings = TRUE,
              stringB = "Estimate",
              stringCI = "Conf. Int.",
              stringP = "p-value",
              stringPredictors = "Coefficients",
              stringIntercept = "intercept",
              separateConfColumn = FALSE, # ci in same cell as estimates
              pvaluesAsNumbers = FALSE,
              showAIC = TRUE,
              showFStat = TRUE,
              labelDependentVariables = c("Head", "BirthW"),
              labelPredictors = c("Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"),
              remove.estimates = c("month", "RENTALS","BAGRUT")
)





##Headc regression nox pre
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
h1.pre <- lm(m1.formula,data=ind.pre)
summary(h1.pre)

##Headc regression nox full
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
h1.full <- lm(m1.formula,data=ind.full)
summary(h1.full)

##bw regression nox pre
m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
b1.pre <- lm(m1.formula,data=ind.pre)
summary(b1.pre)

##bw regression nox full
m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_AIRPORT+N_BAZAN+I(N_BAZAN^2)+N_ROAD+nox)
b1.full <- lm(m1.formula,data=ind.full)
summary(b1.full)


headc<-sjt.lm(h1.pre,h1.full,b1.pre,b1.full,
       digits.est=4,
       group.pred = TRUE, #group catagorical variables
       showHeaderStrings = TRUE,
       stringB = "Estimate",
       stringCI = "Conf. Int.",
       stringP = "p-value",
       #stringDependentVariables = "Head circ.",
       stringPredictors = "Coefficients",
       stringIntercept = "intercept",
       separateConfColumn = FALSE, # ci in same cell as estimates
       pvaluesAsNumbers = FALSE,
       showAIC = TRUE,
       showFStat = TRUE,
       labelDependentVariables = c("HC-pre term","HC-full term","BW-pre term","BW-pre term"),
       labelPredictors = c("Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"),
       remove.estimates = c("month", "RENTALS","BAGRUT")
                     )


###### with devided variables


##Headc regression nox pre
m1.formula <- as.formula(headcWT ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT++N_BAZAN+I(N_BAZAN^2)+nox+birthw)
h1.WT <- lm(m1.formula,data=ind)
summary(h1.WT)

##bw regression nox pre
m1.formula <- as.formula(birthwHC ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_BAZAN+I(N_BAZAN^2)+nox)
b1.HC <- lm(m1.formula,data=ind)
summary(b1.HC)



headc<-sjt.lm(h1.WT,b1.HC,
              digits.est=4,
              group.pred = TRUE, #group catagorical variables
              showHeaderStrings = TRUE,
              stringB = "Estimate",
              stringCI = "Conf. Int.",
              stringP = "p-value",
              #stringDependentVariables = "Head circ.",
              stringPredictors = "Coefficients",
              stringIntercept = "intercept",
              separateConfColumn = FALSE, # ci in same cell as estimates
              pvaluesAsNumbers = FALSE,
              showAIC = TRUE,
              showFStat = TRUE,
              labelDependentVariables = c("HC/W","BW/HC"),
              labelPredictors = c("Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"),
              remove.estimates = c("month", "RENTALS","BAGRUT")
)





### only BAZAN




##Headc regression nox pre
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_BAZAN+I(N_BAZAN^2)+nox+birthw)
h1.pre <- lm(m1.formula,data=ind.pre)
summary(h1.pre)

##Headc regression nox full
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_BAZAN+I(N_BAZAN^2)+nox+birthw)
h1.full <- lm(m1.formula,data=ind.full)
summary(h1.full)

##bw regression nox pre
m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_BAZAN+I(N_BAZAN^2)+nox)
b1.pre <- lm(m1.formula,data=ind.pre)
summary(b1.pre)

##bw regression nox full
m1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+as.factor(mrn.n)+TotalSibli+Education_+ApgarOneMi+AVERAGE_HH+DENSITY+OWNERSHIP+RENTALS+BAGRUT+N_BAZAN+I(N_BAZAN^2)+nox)
b1.full <- lm(m1.formula,data=ind.full)
summary(b1.full)


headc<-sjt.lm(h1.pre,h1.full,b1.pre,b1.full,
              digits.est=4,
              group.pred = TRUE, #group catagorical variables
              showHeaderStrings = TRUE,
              stringB = "Estimate",
              stringCI = "Conf. Int.",
              stringP = "p-value",
              #stringDependentVariables = "Head circ.",
              stringPredictors = "Coefficients",
              stringIntercept = "intercept",
              separateConfColumn = FALSE, # ci in same cell as estimates
              pvaluesAsNumbers = FALSE,
              showAIC = TRUE,
              showFStat = TRUE,
              labelDependentVariables = c("HC-pre term","HC-full term","BW-pre term","BW-pre term"),
              labelPredictors = c("Gestational age","mothers education","% Jewish population","Mean Income","mean household income","% of Bagrut","% House owners"),
              remove.estimates = c("month", "RENTALS","BAGRUT")
)


#spatial auto correlation

library(nlme)
m1.formula <- as.formula(headc ~ges)
fit = gls(m1.formula, data=ind, correlation = corSpher(form = ~ X + Y|day, nugget = TRUE))


summary(ind)



#BORIS MODELS
##Headc regression nox
m1.formula <- as.formula(headc ~ges+as.factor(month)+as.factor(sex)+TotalSibli+Education_+DENSITY+nox2014+birthw+N_OIL_L+Elevation+People_est)
h1 <- lm(m1.formula,data=ind)
summary(h1)

##birth weight regression nox
r1.formula <- as.formula(birthw ~ges+as.factor(month)+as.factor(sex)+TotalSibli+Education_+DENSITY+nox+N_OIL_L)
b1 <- lm(r1.formula,data=ind)
summary(b1)












