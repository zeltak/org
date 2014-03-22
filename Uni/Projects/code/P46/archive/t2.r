#clean PM25
pm25<-pm[PM25 <= 100]
pm25<-pm25[PM25 >= 0]
describe(pm25$PM25)

#create year data
pm2003<-pm25[c == 2003]
#load aod data
dat2003<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN003_AOd_allyears/allaod2003.rds")
describe(dat2003)
dat2003<-dat2003[aod <= 2]


###################
#start with mod1
###################
#delete missing values
#pm2003<-na.omit(pm2003)

# import monitor data and spatial merge with nearestbyday()
source("/media/NAS/Uni/org/files/Uni/Projects/code/P31/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm2003, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod matrix
aod.m <- makepointsmatrix(dat2003[dat2003[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm2003, dat2003 [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm2003,stn,day)
setkey(closestaod,stn,day)
mod1 <- merge(pm2003, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]
mod1 <- mod1[Temp != "NA"]




#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+elev+tden+pden+ dist2rail+dist2A1+Dist2road+dist2water+ x_aod_ITM+ y_aod_ITM +(1 +aod|day)+(1 |stn))

#m1.formula <- as.formula(PM25~ aod+ (1+aod|day))


#full model 1
out.m1_2003 = lmer(m1.formula ,data =  mod1)
#out.m1_2003 = lm(PM25~ aod ,data =  mod1)
#generate prediction
mod1$predicted <- predict(out.m1_2003)
#get overall R2
summary(lm(PM25~predicted,data=mod1))




