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

#ind <-read.csv("/home/zeltak/ZH_tmp/dat/indiv1415av.csv")
ind <-read.dbf("/home/zeltak/ZH_tmp/dat/tipot_all_SPSS_6.02.16.dbf")

names(ind)
ind$month = as.numeric(format(ind$BIRTH_DATE,"%m")) 

# #subset data
# ind<-ind[,c("Head1_Valu","X","Y","Gender","Weight1_Va","Mother_Nat","PregnancyW","month","TotalSibli","Education_","ApgarOneMi","ApgarFiveM","POPULATION","HOUSEHOLDS","AVERAGE_HH","DENSITY","OWNERSHIP","RENTALS","BAGRUT","BA","INCOME","N_AIRPORT","N_BAZAN","N_POWERSTA","N_OIL_L","N_OIL_S","N_ROAD","nox","day","Postal","Mother_Bir","pm25","so2","nox2014","Elevation","People_est","Pop_arnona"),with=FALSE]

#rename
setnames(ind,old=c("WEIGHT1_VA", "HEAD1_VA_A","PREGNANCYW","WEIGHT1__A"),new=c("birthw", "headc.bc","ges","birthw.bc"))


# #clean data
# #delete bad data
# ind<-filter(ind,ges >= 20 )
# ind<-filter(ind,ges <= 44 )
# #clean all crap data
# ind[ind == -999] <- NA
# ind[ind == -9.99] <- NA
# ind<-filter(ind,OWNERSHIP  >= 0 )
# ind<-filter(ind,AVERAGE_HH  >= 0 )
# ind<-filter(ind,BA  >= 0 )
# ind<-filter(ind,birthw >= 2 )
# summary(ind$Mother_Nat)

# ###recoding
# #finer race
# ind$mrn.n<-0 #for jews
# ind<- ind[Mother_Nat  == "יהודי" , mrn.n  := 1] #jewish
# #gender 01
# ind$sex<-1 #for male
# ind<- ind[Gender  == "נקבה" , sex  := 0] #female


# #only nox liner, dist linear, #nox was most significance
# ind$N_BAZAN<-ind$N_BAZAN/1000
# ind$N_AIRPORT<-ind$N_AIRPORT/1000
# ind$N_POWERSTA<-ind$N_POWERSTA/1000
# ind$N_OIL_L<-ind$N_OIL_L/1000
# ind$N_OIL_S<-ind$N_OIL_S/1000
# ind$N_ROAD<-ind$N_ROAD/1000
# ind$headcWT<-ind$headc/ind$birthw
# ind$birthwHC<-ind$birthw/ind$headc

ind.pre<-filter(ind, ges < 37)
ind.full<-filter(ind, ges >= 37)
#write.dbf(ind,"/home/zeltak/ZH_tmp/dat/indiv1415_clean.dbf")

#normal
##Headc regression nox
m1.formula <- as.formula(headc.bc ~birthw+ges+as.factor(SEX)+as.factor(month)+MOTHER_COU+LNDIST_ROA+LNDIST_OIL)
h1 <- lm(m1.formula,data=ind)
summary(h1)
##Headc regression nox pre
h1.pre <- lm(m1.formula,data=ind.pre)
summary(h1.pre)

##Headc regression nox full
h1.full <- lm(m1.formula,data=ind.full)
summary(h1.full)


#html
stargazer(h1,h1.full,h1.pre,
          type="html",
          dep.var.labels=c("Head Circumference"),
          column.labels=c("Full model","pre term","Full term"),
          title="Factors affecting head circumference in the Haifa Bay area (dependent variables – head circumference (centimeters), Box-Cox transformed values (λ=1.752); method – OLS regression)",
          intercept.bottom = TRUE,
          omit.stat = c("ser"),
           report=('vct*'),
          model.numbers          = FALSE,
          single.row = TRUE,
          #remove DF
          df = FALSE,
          #which variables to keep
          keep = c("birthw","ges","SEX","MOTHER_COU","LNDIST_ROA","LNDIST_OIL"),
          covariate.labels = c("Birth Weight (kg)", "pregnancy (weeks)","Gender (1=boy, 0=girl)", 
                               "2nd quarter", "3rd quarter", "Fourth quarter"),
          out="~/ZH_tmp/models_HC.htm")


birthw+ges+as.factor(SEX)+as.factor(month)+MOTHER_COU+LNDIST_ROA+LNDIST_OIL





