 library(lme4)
 library(reshape)
 library(foreign) 
 library(ggplot2)
 library(plyr)
 library(data.table)
 #library(reshape2)
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
 

summary(mod2)
#     aodid                day                   m               aod                UN                pop06           pcturb            elev_m           distA1             wflag              tden        
#  Length:60845385    Min.   :2003-01-01   Min.   : 1.000   Min.   : 0.0000   Min.   :-3.240200   Min.   :    0   Min.   :  0.000   Min.   :   0.0   Min.   :0.000000   Min.   :0.00000   Min.   :0.03542  
#  Class :character   1st Qu.:2003-04-08   1st Qu.: 4.000   1st Qu.: 0.0700   1st Qu.: 0.009000   1st Qu.:    4   1st Qu.:  0.000   1st Qu.: 101.0   1st Qu.:0.001808   1st Qu.:0.00000   1st Qu.:0.67591  
#  Mode  :character   Median :2003-07-15   Median : 7.000   Median : 0.1325   Median : 0.013600   Median :   16   Median :  0.000   Median : 186.0   Median :0.004196   Median :0.00000   Median :0.77953  
#                     Mean   :2003-07-05   Mean   : 6.668   Mean   : 0.1654   Mean   : 0.004052   Mean   :  117   Mean   :  5.107   Mean   : 287.4   Mean   :0.005863   Mean   :0.00472   Mean   :0.77005  
#                     3rd Qu.:2003-09-17   3rd Qu.: 9.000   3rd Qu.: 0.2325   3rd Qu.: 0.021100   3rd Qu.:   54   3rd Qu.:  0.000   3rd Qu.: 346.0   3rd Qu.:0.007940   3rd Qu.:0.00000   3rd Qu.:0.86412  
#                     Max.   :2003-12-31   Max.   :12.000   Max.   :10.0000   Max.   : 3.275600   Max.   :42655   Max.   :100.000   Max.   :3862.0   Max.   :0.119706   Max.   :1.00000   Max.   :1.79390  
#                                                                                                                                                                                                          
#       dair              dport           dist_train      dist.aroad        dist.coast        dist.wb           NO2               SO2                PM2.5                PM10             p.agric      
#  Min.   :   32.79   Min.   :6170882   Min.   :    0   Min.   :    0.0   Min.   :     0   Min.   :    0   Min.   :0.00000   Min.   : 0.000000   Min.   :0.000e+00   Min.   :0.000000   Min.   :  0.00  
#  1st Qu.: 8650.86   1st Qu.:6419099   1st Qu.: 2574   1st Qu.:  150.0   1st Qu.: 59304   1st Qu.: 5384   1st Qu.:0.00000   1st Qu.: 0.000000   1st Qu.:0.000e+00   1st Qu.:0.000000   1st Qu.: 29.41  
#  Median :13714.17   Median :6640486   Median : 6042   Median :  424.3   Median :139290   Median : 9917   Median :0.01282   Median : 0.000134   Median :0.000e+00   Median :0.000000   Median : 72.00  
#  Mean   :14612.91   Mean   :6630869   Mean   : 7872   Mean   :  545.3   Mean   :158549   Mean   :11636   Mean   :0.15941   Mean   : 0.135383   Mean   :8.642e-05   Mean   :0.002305   Mean   : 61.60  
#  3rd Qu.:19500.19   3rd Qu.:6823794   3rd Qu.:11336   3rd Qu.:  750.0   3rd Qu.:241980   3rd Qu.:15998   3rd Qu.:0.12796   3rd Qu.: 0.021888   3rd Qu.:0.000e+00   3rd Qu.:0.000000   3rd Qu.: 99.01  
#  Max.   :46902.45   Max.   :7141348   Max.   :45426   Max.   :11245.0   Max.   :429725   Max.   :52443   Max.   :7.38500   Max.   :14.633571   Max.   :1.033e-01   Max.   :0.821600   Max.   :100.00  
#                                                                                                                                                                                       NA's   :23313   
#      p.open          p.urban           p.forest         pmreg                cid           long_aod          lat_aod         tempavg           wsavg              PBL              season     
#  Min.   :  0.00   Min.   :  0.000   Min.   :  0.00   Length:60845385    Min.   :1.000   Min.   :-4.7887   Min.   :42.34   Min.   :-15.8    Min.   : 0        Min.   :  11.53   Min.   :1.000  
#  1st Qu.: 29.41   1st Qu.:  0.000   1st Qu.:  0.00   Class :character   1st Qu.:2.000   1st Qu.: 0.4954   1st Qu.:44.49   1st Qu.:  7.2    1st Qu.: 2        1st Qu.: 843.29   1st Qu.:2.000  
#  Median : 72.00   Median :  0.000   Median : 15.84   Mode  :character   Median :3.000   Median : 2.5275   Median :46.53   Median : 14.3    Median : 2        Median :1335.21   Median :3.000  
#  Mean   : 61.60   Mean   :  4.923   Mean   : 31.74                      Mean   :2.587   Mean   : 2.5173   Mean   :46.42   Mean   : 14.1    Mean   : 3        Mean   :1420.20   Mean   :2.651  
#  3rd Qu.: 99.01   3rd Qu.:  0.000   3rd Qu.: 59.60                      3rd Qu.:3.000   3rd Qu.: 4.6985   3rd Qu.:48.17   3rd Qu.: 21.3    3rd Qu.: 4        3rd Qu.:1879.99   3rd Qu.:3.000  
#  Max.   :100.00   Max.   :100.000   Max.   :100.00                      Max.   :5.000   Max.   : 8.2130   Max.   :51.09   Max.   : 34.4    Max.   :28        Max.   :4391.85   Max.   :4.000  
#  NA's   :23313    NA's   :23313     NA's   :23313                       NA's   :23313                                     NA's   :864474   NA's   :3438626                                    
#     seasonSW        meanPM25         meanPM10         normwt            dair.s           dport.s            dtrain.s          daroad.s          dcoast.s           dwb.s             NO2.s         
#  Min.   :1.000   Min.   : 4.312   Min.   : 2.50   Min.   : 0.2561   Min.   :-1.8571   Min.   :-1.99573   Min.   :-1.1584   Min.   :-0.9415   Min.   :-1.3764   Min.   :-1.4032   Min.   :-0.34575  
#  1st Qu.:1.000   1st Qu.:12.920   1st Qu.:18.70   1st Qu.: 0.5595   1st Qu.:-0.7594   1st Qu.:-0.91880   1st Qu.:-0.7796   1st Qu.:-0.6825   1st Qu.:-0.8616   1st Qu.:-0.7539   1st Qu.:-0.34575  
#  Median :2.000   Median :16.857   Median :24.12   Median : 0.7302   Median :-0.1145   Median : 0.04173   Median :-0.2693   Median :-0.2090   Median :-0.1672   Median :-0.2072   Median :-0.31795  
#  Mean   :1.594   Mean   :17.657   Mean   :25.89   Mean   : 0.8158   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000  
#  3rd Qu.:2.000   3rd Qu.:21.000   3rd Qu.:31.50   3rd Qu.: 1.0212   3rd Qu.: 0.6225   3rd Qu.: 0.83704   3rd Qu.: 0.5098   3rd Qu.: 0.3534   3rd Qu.: 0.7243   3rd Qu.: 0.5260   3rd Qu.:-0.06821  
#  Max.   :2.000   Max.   :51.750   Max.   :91.67   Max.   :12.1091   Max.   : 4.1128   Max.   : 2.21481   Max.   : 5.5264   Max.   :18.4737   Max.   : 2.3541   Max.   : 4.9209   Max.   :15.67233  
#                                   NA's   :8629                                                                                                                                                     
#      SO2.s           PM25ems.s          PM10ems.s         p.agric.s         p.open.s        p.urban.s        p.forest.s         da1.s             pbl.s             temp.s            ws.s        
#  Min.   :-0.2533   Min.   :-0.06224   Min.   :-0.1128   Min.   :-1.674   Min.   :-1.674   Min.   :-0.333   Min.   :-0.875   Min.   :-0.9562   Min.   :-1.8166   Min.   :-3.4     Min.   :-2       
#  1st Qu.:-0.2533   1st Qu.:-0.06224   1st Qu.:-0.1128   1st Qu.:-0.875   1st Qu.:-0.875   1st Qu.:-0.333   1st Qu.:-0.875   1st Qu.:-0.6613   1st Qu.:-0.7440   1st Qu.:-0.8     1st Qu.:-1       
#  Median :-0.2530   Median :-0.06224   Median :-0.1128   Median : 0.283   Median : 0.283   Median :-0.333   Median :-0.438   Median :-0.2718   Median :-0.1096   Median : 0.0     Median : 0       
#  Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0     Mean   : 0       
#  3rd Qu.:-0.2123   3rd Qu.:-0.06224   3rd Qu.:-0.1128   3rd Qu.: 1.017   3rd Qu.: 1.017   3rd Qu.:-0.333   3rd Qu.: 0.768   3rd Qu.: 0.3386   3rd Qu.: 0.5929   3rd Qu.: 0.8     3rd Qu.: 0       
#  Max.   :27.1258   Max.   :74.31755   Max.   :40.0820   Max.   : 1.044   Max.   : 1.044   Max.   : 6.428   Max.   : 1.881   Max.   :18.5652   Max.   : 3.8322   Max.   : 2.3     Max.   :16       
#                                                         NA's   :23313    NA's   :23313    NA's   :23313    NA's   :23313                                        NA's   :864474   NA's   :3438626  

#load mod1 and mod 2 
mod2 <- readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod2.AQ.2003.c.rds")
mod1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.c2.rds")
 
#**** RAW model 
#R2=0.743, AIC: 19268.67 
 m1.formula <- as.formula(pm25 ~aod
+(1+aod|day/cid) )   

m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/RAW.csv")

 
#**** simple model 
#R2=0.746, AIC: 19182.7

 m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden
+(1+aod|day/cid) )   
  
 
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/simple.RAW.csv")


 
 #**** met.lu model 
#R2=0.758, AIC: 19088.38

m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+ distA1  + pop06+           pcturb
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.csv")


#**** met.lu.dist model 
#R2=0.769, AIC: 19037.45

m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+distA1+pop06+pcturb
#distances
+dair.s+dport.s+dtrain.s+dwb.s   
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.dist.csv")

 
     
#**** lu.met.ems model 
#R2=0.763, AIC: 19046.71 

m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+distA1+pop06+pcturb
#emissions
+NO2.s+SO2.s+PM10ems.s
+(1+aod|day/cid)   )  
  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.ems.csv") 
 
  
#**** met.lu model // no da1 


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+ + pcturb  + pop06
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.No.da1.csv")

 

 
 #**** met.lu model // no pop  


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+ distA1  + pcturb
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.No.pop.csv")


 

 
 #**** met.lu model // no pop  


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.WK.csv")


 
 
 #**** met.lu model // no urb 


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+ distA1  + pop06
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.No.urb.csv")

     
#**** lu.met.ems model //no PM10
 


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+distA1+pop06+pcturb
#emissions
+NO2.s+SO2.s
+(1+aod|day/cid)   )  
  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.ems.NO.pm10.csv") 
summary(out) 
   
 
#**** lu.met.ems model //no SO2.s
 


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+distA1+pcturb
#emissions
+NO2.s+PM10ems.s  
+(1+aod|day/cid)   )  
  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.ems.NO.SO2.csv") 
summary(out)  
 
 
 #**** lu.met.ems model //no NO2.s
 


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden+distA1+pcturb
#emissions
+SO2.s+PM10ems.s  
+(1+aod|day/cid)   )  
  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.ems.NO.NO2.csv") 
summary(out)  

 
 
 
 
 
 
 


 
 #**** met.lu model // WK


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden
+(1+aod|day/cid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.WK.csv")
 
 
  
 #**** met.lu model // WK2


m1.formula <- as.formula(pm25 ~aod
#temporal
+pbl.s
#spatial
+elev_m+tden
+(1+aod|day/cid) +(1| aodid)   )  
m1_sc <- lmer(m1.formula,data=mod1)
summary(m1_sc)$AIC
mod1[,pred.m1 := NULL]
mod1$pred.m1 <- predict(m1_sc)
print(summary(lm(pm25~pred.m1,data=mod1))$r.squared)
#pred m2
mod2[, pred.m2 := NULL] 
#predm2 12:00--12:27 
mod2[, pred.m2 := predict(object=m1_sc,newdata=mod2,allow.new.levels=TRUE,re.form=NULL)]
gc()
#check spatial map mod2
out <-mod2 %>%
group_by(aodid) %>%
summarise(x=mean(long_aod, na.rm=TRUE), y =mean(lat_aod, na.rm=TRUE), predm2=mean(pred.m2, na.rm=TRUE), aodm=mean(aod)  )
out<-na.omit(out)
write.csv(out,"~/ZH_tmp/Rtmp/met.lu.WK.rint.csv")
 
 
 