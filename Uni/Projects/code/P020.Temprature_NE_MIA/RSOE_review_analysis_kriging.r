library(foreign) 
library(nlme)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lme4)
library(mgcv)
str(met)
met<- as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN003_WUNCDC yearly/met2000.dbf") )
met<-met[,c(1:5),with=FALSE]
met<-na.omit(met)
met$month <- as.numeric(format(met$Date, "%m"))
met$dd <- as.numeric(met$Date)


#run "kriging" analysis
fi1_gam <- gam( tempc ~ s(x,y,by=dd), data=met)
met$pred<- predict(fi1_gam)
summary(lm(tempc~pred,data=met))

add5 <- met[Date == "2000-01-01"]

#run "kriging" analysis
fi1_gam <- gam( tempc ~ s(x,y), data=add5)
add5$pred<- predict(fi1_gam)
summary(lm(tempc~pred,data=add5))

#run "kriging" analysis per day
t1<-met %.% group_by(Date) %.% do(function(df){gam(tempc~s(x,y),data=df)})

#t2<-sapply(t1,function(x) predict(x))

#get R2
tst<-sapply(t1,function(x) summary(x)$r.sq)
mean(tst)


