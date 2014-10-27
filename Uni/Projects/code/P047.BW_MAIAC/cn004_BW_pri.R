library(plyr)
library(reshape2)
library(mgcv)
library(FNN)

#imports
bwfull<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bwallv2.csv")
xyguid<-fread("/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN007_Key_tables/locxy0308_guid_lpmid.csv")
xyguid<-xyguid[,c(4,7),with=FALSE]

#merge new guids
setkey(xyguid,uniqueid_y)
setkey(bwfull,uniqueid_y)
bwfull.g <- merge(bwfull,xyguid)

#convert ges_calc to numeric
bwfull.g[,ges_calc:=as.numeric(ges_calc)]
bwfull.g[,clinega:=as.numeric(clinega)]
#get rid of impossible gestational ages other wise the expanding part later wont work
bwfull.g<- bwfull.g[ges_calc > 12 & ges_calc < 48,  ]
 

#subset data (short dataset)
bwfull.s<-bwfull.g[,c("bdob","byob","birthw","ges_calc","uniqueid_y","guid","clinega"),with=FALSE]

#create unique location
# lengthen out to each day of pregnancy
setnames(bwfull.s, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))
bwfull.s[, birthdate := as.Date(strptime(birthdate, format = "%m/%d/%y"))]
# new variable for start of gestation using the best gestational age (in weeks)
bwfull.s[, pregstart := birthdate - 7*ges_calc]
bwfull.s[, prepri := pregstart - 90]


#subset to current expo year range (all pregnancies that start after first day of exposure)
bwfull.s <- bwfull.s[pregstart >= as.Date("2003-01-01") , ]

# create every single day of pregnancy for each pregnancy
gestlong <- bwfull.s[,list(day = seq(.SD$pregstart, .SD$birthdate -1, by = "day")),by=id]
#calculate pre birth exposure
prilong <- bwfull.s[,list(day = seq(.SD$prepri, .SD$pregstart -1, by = "day")),by=id]
 
setkey(bwfull.s,id)
setkey(gestlong,id)
gestlong <- merge(gestlong, bwfull.s, by = "id")


setkey(bwfull.s,id)
setkey(prilong,id)
gestlong.pri <- merge(prilong, bwfull.s, by = "id")


######## import pollution sets

p2003<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM03.csv",select=c(1,2,3))
p2004<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM04.csv",select=c(1,2,3))
p2005<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM05.csv",select=c(1,2,3))
p2006<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM06.csv",select=c(1,2,3))
p2007<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM07.csv",select=c(1,2,3))
p2008<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN40_steve_clean/finalprPM08.csv",select=c(1,2,3))
allbestpred <- rbind(p2003,p2004,p2005,p2006,p2007,p2008)
rm(p2003,p2004,p2005,p2006,p2007,p2008)


gc()
allbestpred$guid<-as.numeric(allbestpred$guid)
#common dates
allbestpred[, day := as.Date(strptime(day, format = "%Y-%m-%d"))]


####merge ges_calc dataset

setkey(gestlong,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gestlong.pm <- merge(gestlong,allbestpred,all.x=TRUE)
summary(gestlong.pm$predpm25)

####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).

gestlong.pm.lags <- gestlong.pm[, list(pmpreg = mean(predpm25), 
                                   pm3rdT = mean(tail(.SD[,predpm25], 90)),
                                   pmlast30 = mean(tail(.SD[,predpm25], 30)),
                                   pm1stT = mean(head(.SD[,predpm25], 90)),
                                   pm1stmonth = mean(head(.SD[,predpm25], 30)),
                                   pmweek12to24 = mean(.SD[84:168,predpm25]),
                                   pm2ndT = mean(.SD[91:175,predpm25]),
                                   pmf20w = mean(.SD[1:140,predpm25]),
                                   guid = guid[1]),by=id]


saveRDS(gestlong.pm.lags,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup.rds")
summary(gestlong.pm.lags)
write.csv(gestlong.pm.lags,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup.csv")



####merge ges_clinical dataset

setkey(gestlong.pri,guid,day)
setkey(allbestpred ,guid, day)
#make sure to allow cartesian
gestlong.pm.pri <- merge(gestlong.pri,allbestpred,all.x=TRUE)
summary(gestlong.pm.pri$predpm25)

####this is where we calculate the exposure per period for each participent-
#pmperg-exposure all pregnancy
#As far as the lags, I met with Emily yesterday, and if we proceed, we are thinking 0-12.99 weeks (1st trimester), 13 weeks-24.99 weeks (2nd trimester), 25 weeks-delivery (3rd trimester), and LMP-20 weeks (which is often considered a relevant exposure window for the outcome of gestational hypertension).



gestlong.pm.lags.pri <- gestlong.pm.pri[, list(pri.90d = mean(predpm25), 
                                   pri.7d = mean(tail(.SD[,predpm25], 7)),
                                   pri.14d = mean(tail(.SD[,predpm25], 14)),
                                   pri.30d = mean(tail(.SD[,predpm25], 30)),
                                   pri.60d = mean(tail(.SD[,predpm25], 60)),
                                   pri.2d = mean(tail(.SD[,predpm25], 2)),
                                   pri.3d = mean(tail(.SD[,predpm25], 3)),
                                   guid.pri = guid[1]),
                                   by=id]


saveRDS(gestlong.pm.lags.pri,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup_pri.rds")
summary(gestlong.pm.lags.pri)
#write.csv(gestlong.pm.lags.clin,"/media/NAS/Uni/Projects/P047_BW_MAIAC/2.Gather_data/FN008_Fin_data/bw_pm1knodup_clin.csv")






