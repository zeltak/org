library(foreign) 
library(stats)
library(mgcv)
library(splines)
library(nlme)
library(car)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

# IMPORTS

#import all pm stations for all years and the stage 3 predictions

F_T2000_All <-read.csv("h:\\$Final\\R\\LPM_all.csv",header=T) 

summary(F_T2000_All)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>ADD LOCAL PM STAGE

#import the 50x50 LU terms for the pred model monitoring stations

lu <-read.dbf("h:\\$Final\\R\\lu_50x50.dbf") 

names(lu)



# sorts
F_T2000_All <- F_T2000_All[order(F_T2000_All$SiteCode),] 
lu <- lu[order(lu$SiteCode),] 



# merge two dataframes by ID
T2000_merged <- merge(F_T2000_All,lu,by="SiteCode")
T2000_merged <- subset(T2000_merged,T2000_merged$pcturban >-0.0000000001)
T2000_merged <- subset(T2000_merged,T2000_merged$pm_mod3 >-0.0000000001)
summary(T2000_merged)

T2000_merged$resm3<-T2000_merged$PM25-T2000_merged$pm_mod3

summary(T2000_merged$pm_mod3)


#The GAM model
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),na.action=na.omit,data=T2000_merged)

summary(bp.model.ps)

test<-predict (bp.model.ps)
summary(test)








#
#import the cases datastes

g3 <-  read.csv("h:\\$Final\\R\\lu_g3.csv", header=T)  
names(g3)
names(T2000_merged)


#make sure var names and units are the same as in the NE pm model 
library(reshape)
g3<-rename(g3,c(dist_a1="dist_A1")) 
g3<-rename(g3,c(purban="pcturban"))
g3$pcturban <- g3$pcturban*100

summary(T2000_merged)
summary(g3)




resid_pred <- predict(bp.model.ps,g3)

g3$localpm <- resid_pred

summary(g3$localpm)
hist(g3$localpm)

write.csv (g3, "h:\\$Final\\localpm\\g3.csv")




#
#import the cases datastes

off <-  read.csv("h:\\$Final\\R\\lu_off.csv", header=T)  
names(off)
names(T2000_merged)


#make sure var names and units are the same as in the NE pm model 
library(reshape)
off<-rename(off,c(dist_a1="dist_A1")) 
off<-rename(off,c(purban="pcturban"))
off$pcturban <- off$pcturban*100

summary(T2000_merged)
summary(off)




resid_pred <- predict(bp.model.ps,off)

off$localpm <- resid_pred

summary(off$localpm)
hist(off$localpm)

write.csv (off, "h:\\$Final\\localpm\\off.csv")







