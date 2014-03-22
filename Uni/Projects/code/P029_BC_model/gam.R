library(foreign)
library(stats)
library(splines)
library(mgcv)
library(reshape)


summerreg <-  read.csv("f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/sumreg3.csv", header=T)  
names<-names(summerreg)
names <- names[order(names)] 
names
summary(summerreg)

###add a interaction between area 1x1km traffic and below 2003/above 2003
summerreg$year_urbind<-ifelse(summerreg$Year < 2003,summerreg$nlcd_ind,summerreg$nlcd_ind +2)

str(summerreg)
summary(summerreg$Year)

summary(summerreg$bc_pred)


attach(summerreg)
summerreg$y1995[Year == 1995] <- 1
summerreg$y1995[Year != 1995] <- 0
summerreg$y1996[Year == 1996] <- 1
summerreg$y1996[Year != 1996] <- 0
summerreg$y1997[Year == 1997] <- 1
summerreg$y1997[Year != 1997] <- 0
summerreg$y1998[Year == 1998] <- 1
summerreg$y1998[Year != 1998] <- 0
summerreg$y1999[Year == 1999] <- 1
summerreg$y1999[Year != 1999] <- 0
summerreg$y2000[Year == 2000] <- 1
summerreg$y2000[Year != 2000] <- 0
summerreg$y2001[Year == 2001] <- 1
summerreg$y2001[Year != 2001] <- 0
summerreg$y2002[Year == 2002] <- 1
summerreg$y2002[Year != 2002] <- 0
summerreg$y2003[Year == 2003] <- 1
summerreg$y2003[Year != 2003] <- 0
summerreg$y2004[Year == 2004] <- 1
summerreg$y2004[Year != 2004] <- 0
summerreg$y2005[Year == 2005] <- 1
summerreg$y2005[Year != 2005] <- 0
summerreg$y2006[Year == 2006] <- 1
summerreg$y2006[Year != 2006] <- 0
summerreg$y2007[Year == 2007] <- 1
summerreg$y2007[Year != 2007] <- 0
summerreg$y2008[Year == 2008] <- 1
summerreg$y2008[Year != 2008] <- 0
detach(summerreg) 


try<-gam(bc_out~s(bc_pred,by=as.factor(region))+s(yearday) +s(TEMP_C) +s(wind_speed) +pop_sqkm+
           s(RHUM) +as.factor(weekday)+s(nlcd_urb01)+bc_pred:nlcd_ind+w_dir_1 +w_dir_2 +w_dir_3 +
           +s(sfgrid09adtxlth,by=as.factor(year_urbind))+y1995+y1996+y1997+y1998+y1999+y2000+y2001+y2002+y2003+y2004+y2005+y2006+y2007+y2008+otgrid09adtxlth +HPBL:as.factor(pop_sqkm >3500)+s(xinkm,yinkm),data=summerreg)
summary(try)


#plot(try,scale=0,scheme=2,pers=TRUE)


#predict

nas1 <-  read.csv("f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A1.csv", header=T) 
names(nas1)
nas1 <- rename(nas1, c(hpbl="HPBL",rhum="RHUM",temp_c="TEMP_C"))
nas1$year_urbind<-ifelse(nas1$Year < 2003,nas1 $nlcd_ind,nas1$nlcd_ind +2)

attach(nas1)
nas1$y1995[Year == 1995] <- 1
nas1$y1995[Year != 1995] <- 0
nas1$y1996[Year == 1996] <- 1
nas1$y1996[Year != 1996] <- 0
nas1$y1997[Year == 1997] <- 1
nas1$y1997[Year != 1997] <- 0
nas1$y1998[Year == 1998] <- 1
nas1$y1998[Year != 1998] <- 0
nas1$y1999[Year == 1999] <- 1
nas1$y1999[Year != 1999] <- 0
nas1$y2000[Year == 2000] <- 1
nas1$y2000[Year != 2000] <- 0
nas1$y2001[Year == 2001] <- 1
nas1$y2001[Year != 2001] <- 0
nas1$y2002[Year == 2002] <- 1
nas1$y2002[Year != 2002] <- 0
nas1$y2003[Year == 2003] <- 1
nas1$y2003[Year != 2003] <- 0
nas1$y2004[Year == 2004] <- 1
nas1$y2004[Year != 2004] <- 0
nas1$y2005[Year == 2005] <- 1
nas1$y2005[Year != 2005] <- 0
nas1$y2006[Year == 2006] <- 1
nas1$y2006[Year != 2006] <- 0
nas1$y2007[Year == 2007] <- 1
nas1$y2007[Year != 2007] <- 0
nas1$y2008[Year == 2008] <- 1
nas1$y2008[Year != 2008] <- 0
detach(nas1) 

nas1$pred <- predict.gam(try,nas1)
summary(nas1$pred)
names(nas1)
nas_short <- nas1[, c(1:3,10,18,43)] 
summary(nas_short)
nas_short<-na.omit(nas_short)
write.csv(nas_short, "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A1.csv") 





#a2
nas2 <-  read.csv("f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A2.csv", header=T) 
nas2 <- rename(nas2, c(hpbl="HPBL",rhum="RHUM",temp_c="TEMP_C"))
nas2$year_urbind<-ifelse(nas2$Year < 2003,nas2 $nlcd_ind,nas2$nlcd_ind +2)

attach(nas2)
nas2$y1995[Year == 1995] <- 1
nas2$y1995[Year != 1995] <- 0
nas2$y1996[Year == 1996] <- 1
nas2$y1996[Year != 1996] <- 0
nas2$y1997[Year == 1997] <- 1
nas2$y1997[Year != 1997] <- 0
nas2$y1998[Year == 1998] <- 1
nas2$y1998[Year != 1998] <- 0
nas2$y1999[Year == 1999] <- 1
nas2$y1999[Year != 1999] <- 0
nas2$y2000[Year == 2000] <- 1
nas2$y2000[Year != 2000] <- 0
nas2$y2001[Year == 2001] <- 1
nas2$y2001[Year != 2001] <- 0
nas2$y2002[Year == 2002] <- 1
nas2$y2002[Year != 2002] <- 0
nas2$y2003[Year == 2003] <- 1
nas2$y2003[Year != 2003] <- 0
nas2$y2004[Year == 2004] <- 1
nas2$y2004[Year != 2004] <- 0
nas2$y2005[Year == 2005] <- 1
nas2$y2005[Year != 2005] <- 0
nas2$y2006[Year == 2006] <- 1
nas2$y2006[Year != 2006] <- 0
nas2$y2007[Year == 2007] <- 1
nas2$y2007[Year != 2007] <- 0
nas2$y2008[Year == 2008] <- 1
nas2$y2008[Year != 2008] <- 0
detach(nas2) 




#predict
nas2$pred <- predict.gam(try,nas2)
names(nas2)
nas_short2 <- nas2[, c(1:3,10,18,43)] 

##note-----there will be alot of missing since 09/10 are in the dataset and not orig model, thats OK
summary(nas_short2)
nas_short2<-na.omit(nas_short2)



write.csv(nas_short2, "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A2.csv") 


a95 <-  read.csv("f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A95.csv", header=T)

try_ts<-gam(bc_out~s(bc_pred,by=as.factor(region))+s(yearday) +s(TEMP_C) +s(wind_speed) +pop_sqkm+
           s(RHUM) +as.factor(weekday)+s(nlcd_urb01)+bc_pred:nlcd_ind+w_dir_1 +w_dir_2 +w_dir_3 +
           +s(sfgrid09adtxlth,by=as.factor(year_urbind))+y1995+y1996+y1997+y1998+y1999+y2000+y2001+y2002+y2003+y2004+y2005+y2006+y2007+y2008+otgrid09adtxlth +HPBL:as.factor(pop_sqkm >3500)+s(xinkm,yinkm,bs="ts"),data=summerreg)

#see diffrances here with plots

#also preict using terms and see 3d plot of s(xy)
a95 <- rename(a95, c(hpbl="HPBL",rhum="RHUM",temp_c="TEMP_C"))
a95$year_urbind<-ifelse(a95$Year < 2003,a95 $nlcd_ind,a95$nlcd_ind +2)

attach(a95)
a95$y1995[Year == 1995] <- 1
a95$y1995[Year != 1995] <- 0
a95$y1996[Year == 1996] <- 1
a95$y1996[Year != 1996] <- 0
a95$y1997[Year == 1997] <- 1
a95$y1997[Year != 1997] <- 0
a95$y1998[Year == 1998] <- 1
a95$y1998[Year != 1998] <- 0
a95$y1999[Year == 1999] <- 1
a95$y1999[Year != 1999] <- 0
a95$y2000[Year == 2000] <- 1
a95$y2000[Year != 2000] <- 0
a95$y2001[Year == 2001] <- 1
a95$y2001[Year != 2001] <- 0
a95$y2002[Year == 2002] <- 1
a95$y2002[Year != 2002] <- 0
a95$y2003[Year == 2003] <- 1
a95$y2003[Year != 2003] <- 0
a95$y2004[Year == 2004] <- 1
a95$y2004[Year != 2004] <- 0
a95$y2005[Year == 2005] <- 1
a95$y2005[Year != 2005] <- 0
a95$y2006[Year == 2006] <- 1
a95$y2006[Year != 2006] <- 0
a95$y2007[Year == 2007] <- 1
a95$y2007[Year != 2007] <- 0
a95$y2008[Year == 2008] <- 1
a95$y2008[Year != 2008] <- 0
detach(a95) 




#predict
a95$pred <- predict.gam(try_ts,a95)
a95s <- a95[, c(1:3,10,18,43)] 
names(a95s)
write.csv(a95s, "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A95ts.csv") 




#predict
a95_terms<-a95
typeout <- predict.gam(try,a95_terms,type="terms")
names(a95_terms)
a95_terms_short2 <- a95_terms[, c(1:3,10,18,43)] 
head(a95_terms_short2)

plot(typeout,)


plot(try)

