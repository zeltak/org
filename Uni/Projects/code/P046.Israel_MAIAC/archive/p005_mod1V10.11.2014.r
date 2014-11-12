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
library(gdata)5
library(car)
library(dplyr)
library(ggmap)


######## import mod1
pm10.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM10all_reg.RDS")
pm25.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.PM25all_reg.RDS")

#note terra is '0' and aqua is '1'

system.time(pm25.m1[, MaskCloud := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
system.time(pm25.m1[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(pm25.m1[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
system.time(pm25.m1[, MaskGlint := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}))])
system.time(pm25.m1[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])

#note terra is '0' and aqua is '1'

system.time(pm10.m1[, MaskCloud := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[1:3]), collapse = "")}))])
system.time(pm10.m1[, MaskLandWaterSnow := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[4:5]), collapse = "")}))])
system.time(pm10.m1[, MaskAdjacency := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[6:8]), collapse = "")}))])
system.time(pm10.m1[, MaskGlint := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[13]), collapse = "")}))])
system.time(pm10.m1[, AerosolModel := as.factor(sapply(QA, function(x){paste(rev(as.integer(intToBits(x))[14:15]), collapse = "")}))])


#new seasons
#1-cloudy,#2-noncloud
pm25.m1$cloudIL<-recode(pm25.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")
pm10.m1$cloudIL<-recode(pm10.m1$m,"1=1;2=1;3=2;4=2;5=2;6=2;7=2;8=2;9=2;10=2;11=1;12=1")

pm25.m1<-pm25.m1[aod != 'NA']
pm10.m1<-pm10.m1[aod != 'NA']

### subset to aqua and apply alexei cleaning methods
pm25.m1<-pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & c >= 2003] #0.066
pm10.m1<-pm10.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & c >= 2003] #0.066



#### PM25


#################BAD STN PM25
rawdf <- ddply(pm25.m1, c( "c"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,c,sep="-")]
#################BAD STN
pm25.m1[,badid := paste(stn,c,sep="-")]
####Take out bad stations
pm25.m1 <- pm25.m1[!(pm25.m1$badid %in% bad$badid), ] 


# pm25.m1$aodp<-pm25.m1$aod/pm25.m1$MeanPbl
# summary(lm(PM25~aodp,data=pm25.m1[A_T==1 & UN > 0 & UN < 0.04 ])) #0.083
# summary(gam(PM25~aod+s(MeanPbl),data=pm25.m1[A_T==1 & UN > 0 & UN < 0.04 ]))
#logPM
#pm25.m1$PM25l<-log(pm25.m1$PM25)
#pm25.m1$aodl<-log(pm25.m1$aod)




#lme mixed model
m1.formula <- as.formula(PM25~ aod+Temp+WS+NO2+Dust+elev+tden+pden+dist2rail+dist2A1+Dist2road+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+(1+aod|day/reg_num))
x<-  lmer(m1.formula,data=pm25.m1)
pm25.m1$predicted <- predict(x)
summary(lm(PM25~predicted,data=pm25.m1))



#per year
modelList <- dlply(pm25.m1, "c", function(x) lmer(m1.formula, data=x))
pm25.year<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
pm25.year


#model
mod1PM25<-pm25.m1
mod1PM25_2002 <- mod1PM25[c == "2002"]
mod1PM25_2003 <- mod1PM25[c == "2003"]
mod1PM25_2004 <- mod1PM25[c == "2004"]
mod1PM25_2005 <- mod1PM25[c == "2005"]
mod1PM25_2006 <- mod1PM25[c == "2006"]
mod1PM25_2007 <- mod1PM25[c == "2007"]
mod1PM25_2008 <- mod1PM25[c == "2008"]
mod1PM25_2009 <- mod1PM25[c == "2009"]
mod1PM25_2010 <- mod1PM25[c == "2010"]
mod1PM25_2011 <- mod1PM25[c == "2011"]
mod1PM25_2012 <- mod1PM25[c == "2012"]

# #model
# out.m1 = lmer(m1.formula ,data =  mod1PM25_2002)
# mod1PM25_2002$predicted <- predict(out.m1)
# summary(lm(PM25~predicted,data=mod1PM25_2002))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2003)
mod1PM25_2003$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2003))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2004)
mod1PM25_2004$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2004))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2005)
mod1PM25_2005$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2005))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2006)
mod1PM25_2006$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2006))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2007)
mod1PM25_2007$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2007))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2008)
mod1PM25_2008$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2008))

out.m1 = lmer(m1.formula ,data =  mod1PM25_2009)
mod1PM25_2009$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2009))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2010)
mod1PM25_2010$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2010))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2011)
mod1PM25_2011$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2011))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2012)
mod1PM25_2012$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2012))







