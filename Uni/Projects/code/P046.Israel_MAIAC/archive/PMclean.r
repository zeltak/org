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
library(broom)
library(splines)
library(DataCombine)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")




#-------------------->> RES TABLE
res <- matrix(nrow=2, ncol=44)
res <- data.frame(res)
colnames(res) <- c(
          "m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
      ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"m2.R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
          ,"XX1" ,"XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX")
          
res$type <- c("PM25","PM10")



m1.all #<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.TR.rds")

m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.AQ.rds")


#add stn elev
estn <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/PMstnASL.csv")
estn<-estn[!is.na(Long)]
setkey(estn,stn)
setkey(m1.all,stn)
m1.all <- merge(m1.all, estn[,list(stn,ASLm)], all.x = T)


# take out stn with co located PM10/25 with very high ratios
#calculate meanPM per grid per day to each station (excluding first station)
PM25 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM25_D.csv")
PM25$date<-paste(PM25$Day,PM25$Month,PM25$Year,sep="/")
PM25[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM25[, c := as.numeric(format(day, "%Y")) ]
PM25[,c("Year","Month","Day","date"):=NULL]
PM25 <- PM25[X != 'NaN']
PM25<-PM25[!is.na(PM25)]
PM25<-PM25[PM25 > 0.000000000001 & PM25 < 900 ]
#clear non continous stations
setnames(PM25,"X","x_stn_ITM")
setnames(PM25,"Y","y_stn_ITM")
#calculate meanPM per grid per day to each station (excluding first station)
PM10 <- fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/PM/PM10_D.csv")
PM10$date<-paste(PM10$Day,PM10$Month,PM10$Year,sep="/")
PM10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
PM10[, c := as.numeric(format(day, "%Y")) ]
PM10[,c("Year","Month","Day","date"):=NULL]
PM10 <- PM10[X != 'NaN']
PM10<-PM10[!is.na(PM10)]
PM10<-PM10[PM10 > 0.000000000001 & PM10 < 20200 ]
#clear non continous stations
setnames(PM10,"X","x_stn_ITM")
setnames(PM10,"Y","y_stn_ITM")
setkey(PM10,stn,day)
setkey(PM25,stn,day)
PM.j=merge(PM10,PM25,by=c("stn","day"))
#leave only stations with both PM2.5 and PM 10 measurements
PM.j=na.omit(PM.j)
PM.j$ratio=PM.j[,PM25]/PM.j[,PM10]
PM.j[,badstn := paste(stn,day,sep="-")]
#################BAD STN
m1.all[,badstn := paste(stn,day,sep="-")]
PM.j<- PM.j[ratio > 0.95]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badstn %in% PM.j$badstn), ] 

#cal prev/post day
#read PM data
setkey(PM25,stn,day)
PM25<-as.data.frame(PM25)

Data1 <- slide(PM25, Var = "PM25", GroupVar = "stn",
               slideBy = 1)
Data2 <- slide(Data1, Var = "PM25", GroupVar = "stn",
               slideBy = -1)

data2<-as.data.table(Data2)
setkey(data2,day,stn)
setnames(data2,"PM251","PM25_pre")
setnames(data2,"PM25-1","PM25_post")
setkey(m1.all,day,stn)
m1.all <- merge(m1.all, data2[,list(stn,day, PM25_pre, PM25_post)], all.x = T)
summary(m1.all)

m1.all[,elev.s:= scale(elev)]
m1.all[,tden.s:= scale(tden)]
m1.all[,pden.s:= scale(pden)]
m1.all[,dist2A1.s:= scale(dist2A1)]
m1.all[,dist2water.s:= scale(dist2water)]
m1.all[,dist2rail.s:= scale(dist2rail)]
m1.all[,Dist2road.s:= scale(Dist2road)]
m1.all[,ndvi.s:= scale(ndvi)]
m1.all[,MeanPbl.s:= scale(MeanPbl)]
m1.all[,p_ind.s:= scale(p_ind)]
m1.all[,p_for.s:= scale(p_for)]
m1.all[,p_farm.s:= scale(p_farm)]
m1.all[,p_dos.s:= scale(p_dos)]
m1.all[,p_dev.s:= scale(p_dev)]
m1.all[,p_os.s:= scale(p_os)]
m1.all[,tempa.s:= scale(Temp.im)]
m1.all[,WDa.s:= scale(WD.im)]
m1.all[,WSa.s:= scale(WS.im)]
m1.all[,RHa.s:= scale(RH.im)]
m1.all[,Raina.s:= scale(Rain.im)]
m1.all[,NOa.s:= scale(NO.im)]
m1.all[,O3a.s:= scale(O3.im)]
m1.all[,SO2a.s:= scale(SO2.im)]
################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.all, c("stn","m"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,m,sep="-")]
#################BAD STN
m1.all[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badid %in% bad$badid), ] 

summary(m1.all)
#clear missings
m1.all<- m1.all[!is.na(ASLm)]
m1.all<- m1.all[!is.na(aodpre)]
m1.all<- m1.all[!is.na(aodpost)]

m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WSa.s
                        +Dust#+MeanPbl.s
                        +RHa.s+O3a.s 
                        +ASLm+tden.s
                        +pden.s
                        +Dist2road.s
                        +ndvi.s 
                        +dist2rail.s +dist2water.s +dist2A1.s
                        +p_os.s+p_dev.s
                        +p_dos.s
                        +p_farm.s
                        +p_for.s
                        +p_ind.s  #land use
                  #       +aodpre
                   #      +aodpost
                        # +meanPM10
                      # + aod:Dust 
                        +(1+aod+|day/reg_num)) #+(1|stn) !!! stn screws up mod3 


#--------->mod1  .76
#full fit
m1.fit.all <-  lmer(m1.formula,data=m1.all,weights=normwt)
summary(m1.fit.all)
m1.all$pred.m1 <- predict(m1.fit.all)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE
res[res$type=="PM25", 'm1.PE'] <- print(rmse(residuals(m1.fit.all)))



## names(m1.all)
## m1.allx<-m1.all[,c(1,2,3,7,30),with=FALSE]
## m1.allDF<-as.data.frame(m1.allx)
## #per season
## #base model for stage 1
## m1.formula <- as.formula(PM25~ aod)
## t1<- m1.allDF  %>% group_by(season) %>% do(function(df){lmer(m1.formula,data=df)})

## seas2007<- m1.allDF  %>% group_by(season) %>% do(function(df){summary(lm(m1.formula,data=df))})


## ################# clean BAD STN PM25 and check if improved model?
## raWDaf <- ddply(m1.all, c("season"), 
##       function(x) {
##         mod1 <- lmer(m1.formula, data=x)
##         data.frame(R2 = round(summary(mod1)$r.squared, 5), 
##                    nsamps = length(summary(mod1)$resid))
## })
## raWDaf
## raWDaf<-as.data.table(raWDaf)





#spatial
###to check
spatialall<-m1.all %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.spat<- lm(barpm ~ barpred, data=spatialall)
res[res$type=="PM25", 'm1.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall))$r.squared)
res[res$type=="PM25", 'm1.PE.s'] <- print(rmse(residuals(m1.fit.all.spat)))
       
#temporal
tempoall<-left_join(m1.all,spatialall)
tempoall$delpm <-tempoall$PM25-tempoall$barpm
tempoall$delpred <-tempoall$pred.m1-tempoall$barpred
mod_temporal <- lm(delpm ~ delpred, data=tempoall)
res[res$type=="PM25", 'm1.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall))$r.squared)

#saveRDS(m1.all,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.allYEARS.pred.rds")


#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
# cleanup (remove from WS) objects from CV
rm(list = ls(pattern = "train_|test_"))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
res[res$type=="PM25", 'm1cv.I'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,1])
res[res$type=="PM25", 'm1cv.I.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[1,2])
res[res$type=="PM25", 'm1cv.S'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,1])
res[res$type=="PM25", 'm1cv.S.se'] <-print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$coef[2,2])
#RMSPE
res[res$type=="PM25", 'm1cv.PE'] <- print(rmse(residuals(m1.fit.all.cv)))

#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(stn) %>%
    summarise(barpm = mean(PM25, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
res[res$type=="PM25", 'm1cv.R2.s'] <-  print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
res[res$type=="PM25", 'm1cv.PE.s'] <- print(rmse(residuals(m1.fit.all.cv.s)))
       
#temporal
tempoall.cv<-left_join(m1.all.cv,spatialall.cv)
tempoall.cv$delpm <-tempoall.cv$PM25-tempoall.cv$barpm
tempoall.cv$delpred <-tempoall.cv$pred.m1.cv-tempoall.cv$barpred
mod_temporal.cv <- lm(delpm ~ delpred, data=tempoall.cv)
res[res$type=="PM25", 'm1cv.R2.t'] <-  print(summary(lm(delpm ~ delpred, data=tempoall.cv))$r.squared)


# LOOCross-validation
# for mod1
library(doRNG)
library(doParallel)


# cross-validation and model building
# repeated leave x monitors out CV
#neveruse <- c("PER")
neveruse <- c("")
mons <- unique(m1.all[!stn %in% neveruse, stn]); length(mons)
xout <- 2 # number of monitors to hold out
# how many combinations if we pull out xout mons
ncol(combn(mons, xout))
n.iter <- 50
# we will compute mean of the other monitors using all monitoring data
setkey(m1.all, stn)

# list to store scheme
cvscheme <- list()
cvout <- list()
# set seed for reproducibility
set.seed(20150112)

# cross-validation in parallel

registerDoParallel(14)
# use a proper reproducible backend RNG
registerDoRNG(1234)
system.time({
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
  #system.time(for(i in 1:n.iter){
  #mons.test <- mons[sample(length(mons), xout)]
  mons.test <- combn(mons, xout)[,i]
  cvscheme[[i]] <- mons.test
  test <- m1.all[stn %in% mons.test, ]
  train<- m1.all[!stn %in% mons.test, ]
  # fit the model
  print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  trainmod <-  lmer(m1.formula, data =  train)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i  
  # export these results
  test[, list(day, stn, PM25, predcv, itercv)]
}# end of cross-validation loop
})
summary(lm(PM25 ~ predcv, data = iter.out))
# compute root mean squared error
iter.out[, sqrt(mean((PM25 - predcv)^2))]
