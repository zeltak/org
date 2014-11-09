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
library(gdata)
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


###################
#PM25
#clean data based on Alexeis and our input
####################
##test one year
#pm25.m1<-pm25.m1[c==2012]

#remove missing
pm25.m1<-pm25.m1[aod != 'NA']


#pm25.check.aod <-pm25.m1[aod > 1]
#pm25.check.pm <-pm25.m1[PM25 > 100]


#clean run
summary(lm(PM25~aod,data=pm25.m1)) #0.053
#aqua
summary(lm(PM25~aod,data=pm25.m1[A_T==1])) #0.066
#terra
summary(lm(PM25~aod,data=pm25.m1[A_T==0])) #0.04
#check availability
describe(pm25.m1[A_T==1]$aod)#53616 aod points in aqua
describe(pm25.m1[A_T==0]$aod)#43271 aod points in terra

##adjusted aod
pm25.m1$aodp<-pm25.m1$aod/pm25.m1$MeanPbl
summary(lm(PM25~aodp,data=pm25.m1[A_T==1 & UN > 0 & UN < 0.04 ])) #0.083
#logPM
#pm25.m1$PM25l<-log(pm25.m1$PM25)
#pm25.m1$aodl<-log(pm25.m1$aod)



##alexei cleaning
#uncert
#aqua
summary(lm(PM25~aod,data=pm25.m1[A_T==1 & UN > 0 & UN < 0.04 ])) #0.076
#terra
summary(lm(PM25~aod,data=pm25.m1[A_T==0 & UN > 0 & UN < 0.04 ])) #0.042
## check only mask
summary(lm(PM25~aod,data=pm25.m1[A_T==1 & MaskAdjacency == "000" ])) #0.072
#from now on look only on aqua
summary(lm(PM25~aod,data=pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04   ])) #0.08

#clearing "outlier" values
#from now on look only on aqua
summary(lm(PM25~aod,data=pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & PM25 < 200 ])) #0.08

summary(lm(PM25~aod+ns(MeanPbl,9),data=pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 & PM25 < 200 ])) #0.08




plot(pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04]$aod,pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04]$PM25)#,xlim=c(-0.1,0.1))

#per year
m1.formula<-PM25~aod
modelList <- dlply(pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04 ], "c", function(x) lm(m1.formula, data=x))
aquaPY<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaPY


#per year
m1.formula<-PM25~aod
modelList <- dlply(pm25.m1[A_T==0 & MaskAdjacency == "000" & UN > 0 & UN < 0.04], "c", function(x) lm(m1.formula, data=x))
terraPY<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
terraPY



#per station
modelList <- dlply(pm25.m1[c == 2004 & A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04], "stn", function(x) lm(m1.formula, data=x))
aquaSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaSTN


#per station
modelList <- dlply(pm25.m1[c == 2004 & A_T==0& & MaskAdjacency == "000" & UN > 0 & UN < 0.04], "stn", function(x) lm(m1.formula, data=x))
terraSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
terraSTN

#compare
c<-cbind(aquaSTN,terraSTN)



#per station
modelList <- dlply(pm25.m1[c == 2005 & A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04], "stn", function(x) lm(m1.formula, data=x))
aquaSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaSTN



#per station overall
modelList <- dlply(pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04], "stn", function(x) lm(m1.formula, data=x))
aquaSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaSTN

##check days lag bomer etc 

# and by year
rawdf <- ddply(pm25.m1[A_T==1 & MaskAdjacency == "000" & UN > 0 & UN < 0.04], c("stn", "c"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 2), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf

rawdf<-as.data.table(rawdf)
rawdf2 <- rawdf[!stn %in% c("ANT","NSH","IRD","TMM","BIN"), ]
ggplot(rawdf2, aes(c, R2, color = stn)) + geom_line() + geom_text(aes(label = paste(stn, nsamps)), size = 3.5) + theme_bw(13)
ggsave(file="/home/zeltak/ZH_tmp/ggplot/stn_per_year_R2.png")





###EXAMINE BAD STATIONS

#MAL
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "MAL" & PM25 < 200], aes(aod, PM25)) + geom_line() + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == MAL", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/MAL.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "MAL" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "MAL" & PM25 < 200]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "MAL" & PM25 > 80]


#HOL
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "HOL" & PM25 < 40 ], aes(aod, PM25)) + geom_line() + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == HOL", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/HOL.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "HOL" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "HOL" & PM25 < 40]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "HOL" & PM25 > 80]

#ANT
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "ANT"  ], aes(aod, PM25)) + geom_line()+ geom_point(aes(colour = "red"))    + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == ANT", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/ANT.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "ANT" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "ANT" & PM25 < 40]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "ANT" & PM25 > 80]

#NSH
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "NSH"  ], aes(aod, PM25)) + geom_line()+ geom_point(aes(colour = "red"))    + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == NSH", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/NSH.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "NSH" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "NSH" & PM25 < 40]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "NSH" & PM25 > 80]

#PTR
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "PTR"  ], aes(aod, PM25)) + geom_line()+ geom_point(aes(colour = "red"))    + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == PTR", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/PTR.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "PTR" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "PTR" & PM25 < 40]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "PTR" & PM25 > 80]


#REM
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & stn == "REM"  ], aes(aod, PM25)) + geom_line()+ geom_point(aes(colour = "red"))    + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == REM", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/REM.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "REM" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1 & MaskAdjacency == "000" & stn == "REM"]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "REM" & PM25 > 80]

#YLB
x<-ggplot(pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & stn == "YLB"  ], aes(aod, PM25)) + geom_line()+ geom_point(aes(colour = "red"))    + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == YLB", parse = TRUE, x=0.5, y=400) 
ggsave(file="/home/zeltak/ZH_tmp/ggplot/YLB.png")
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "YLB" ]))
summary(lm(PM25~aod,data=pm25.m1[c == 2004 & A_T==1 & MaskAdjacency == "000" & stn == "YLB"]))
days<-pm25.m1[c == 2004 & A_T==1& MaskAdjacency == "000" & UN > 0 & UN < 0.04 & stn == "YLB" & PM25 > 80]

#base model for stage 1


summary(lm(PM25~aod,data=pm25.m1.c[A_T==1]))
summary(lm(PM25~aod,data=pm25.m1.c[A_T==1 & PM25 < 200 & aod < 1]))

#run by station
modelList <- dlply(pm25.m1.c[A_T==1], "stn", function(x) lm(m1.formula, data=x))
r2map<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
#aggregate station xy
stnxy<-pm25.m1.c %>%
  group_by(stn) %>%
  dplyr::summarise(x = mean(x_stn_ITM, na.rm=TRUE),y = mean(y_stn_ITM, na.rm=TRUE) )
stnxy$r2<-r2map
write.csv(stnxy,"/home/zeltak/ZH_tmp/tst.csv")




out <- pm25.m1.c %>%
  group_by(stn) %>%
  do(function(df){summary(lm(m1.formula,data=df))})

#dput(pm25.m1.c,"/home/zeltak/ZH_tmp/obj")

x<-pm25.m1.c %>%
group_by(stn) %>%
do(xx=summary(lm(m1.formula, .)))                        



#lme mixed model
pm25.m1.c.aq<-pm25.m1.c[A_T==1]
m1.formula <- as.formula(PM25~ aod+Dust+elev+tden+pden+dist2rail+dist2A1+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))
m1.formula <-as.formula(PM25~ aod+(1+aod|day))
#model
#lme
x<-  lmer(m1.formula,data=pm25.m1.c.aq)
pm25.m1.c.aq$predicted <- predict(x)
summary(lm(PM25~predicted,data=pm25.m1.c.aq))











#mod1 per year

#model
out.m1 = lmer(m1.formula ,data =  mod1_2002)
mod1_2002$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=mod1_2002))

#model
out.m1 = lmer(m1.formula ,data =  mod1_2003)
mod1_2003$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=mod1_2003))

#model
out.m1 = lmer(m1.formula ,data =  mod1_2004)
mod1_2004$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=mod1_2004))





####Dust days
mod1_dust <- mod1[Dust == "1"]
#model
out.m1 = lmer(m1.formula ,data =  mod1_dust)
mod1_dust$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=mod1_dust))

####Dust days
mod1_Ndust <- mod1[Dust == "0"]
#model
out.m1 = lmer(m1.formula ,data =  mod1_Ndust)
mod1_Ndust$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=mod1_Ndust))





###########################
###########################PM25
###########################












#Pm25
pm25all<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm25all.RDS")
summary(pm25all)
pm25all[,DOW:=NULL]
pm25all[,Holiday:=NULL]
pm25all[,PM10:=NULL]
pm25all[,RH:=NULL]
pm25all[,WD:=NULL]
pm25all[,Temp:=NULL]

pm25all<-na.omit(pm25all)
#clean
pm25all<-pm25all[PM25 <= 500]
describe(pm25all$PM25)
summary(pm25all)



# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm25all, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm25all, terra [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm25all,stn,day)
setkey(closestaod,stn,day)
mod1PM25 <- merge(pm25all, closestaod, all.x = T)
#head(mod1)
mod1PM25 <- mod1PM25[aod != "NA"]
summary(mod1PM25)

#base model for stage 1
m1.formula <- as.formula(PM25~ aod+ (1+aod|day))
m1.formula <- as.formula(PM25 ~ aod+elev+tden+pden+dist2rail+dist2A1+dist2water+daytemp+dayRH+season+MeanPbl+(1+aod+daytemp|day)+(1|stn))


#model
out.m1 = lmer(m1.formula ,data =  mod1PM25)
mod1PM25$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25))


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

#base model for stage 1
m1.formula <- as.formula(PM25~ aod+ (1+aod|day))
#mod1PM25 per year

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25_2002)
mod1PM25_2002$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25_2002))

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



##############################
#Aqua
##############################

#import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm25all, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod matrix
aod.m <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm25all, aqua [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge



setkey(pm25all,stn,day)
setkey(closestaod,stn,day)
mod1PM25AQ <- merge(pm25all, closestaod, all.x = T)
#head(mod1)
mod1PM25AQ <- mod1PM25AQ[aod != "NA"]


#base model for stage 1
m1.formula <- as.formula(PM25~ aod+ (1+aod|day))
m1.formula <- as.formula(PM25 ~ aod+elev+tden+pden+dist2rail+dist2A1+dist2water+daytemp+dayRH+season+MeanPbl+(1+aod+daytemp|day)+(1|stn))


#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ)
mod1PM25AQ$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ))

mod1PM25AQ_2002 <- mod1PM25AQ[c == "2002"]
mod1PM25AQ_2003 <- mod1PM25AQ[c == "2003"]
mod1PM25AQ_2004 <- mod1PM25AQ[c == "2004"]
mod1PM25AQ_2005 <- mod1PM25AQ[c == "2005"]
mod1PM25AQ_2006 <- mod1PM25AQ[c == "2006"]
mod1PM25AQ_2007 <- mod1PM25AQ[c == "2007"]
mod1PM25AQ_2008 <- mod1PM25AQ[c == "2008"]
mod1PM25AQ_2009 <- mod1PM25AQ[c == "2009"]
mod1PM25AQ_2010 <- mod1PM25AQ[c == "2010"]
mod1PM25AQ_2011 <- mod1PM25AQ[c == "2011"]
mod1PM25AQ_2012 <- mod1PM25AQ[c == "2012"]

#base model for stage 1
m1.formula <- as.formula(PM25~ aod+ (1+aod|day))
#mod1PM25AQ per year

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2002)
mod1PM25AQ_2002$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2002))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2003)
mod1PM25AQ_2003$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2003))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2004)
mod1PM25AQ_2004$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2004))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2005)
mod1PM25AQ_2005$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2005))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2006)
mod1PM25AQ_2006$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2006))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2007)
mod1PM25AQ_2007$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2007))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2008)
mod1PM25AQ_2008$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2008))

out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2009)
mod1PM25AQ_2009$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2009))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2010)
mod1PM25AQ_2010$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2010))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2011)
mod1PM25AQ_2011$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2011))

#model
out.m1 = lmer(m1.formula ,data =  mod1PM25AQ_2012)
mod1PM25AQ_2012$predicted <- predict(out.m1)
summary(lm(PM25~predicted,data=mod1PM25AQ_2012))








