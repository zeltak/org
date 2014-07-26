

#PM10
pm10 <- fread("/media/NAS/Uni/Data/Israel/IPA_stations/PM10Ter_overpass.csv")
pm10$date<-paste(pm10$Year,pm10$Month,pm10$Day,sep="/")
pm10[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
pm10[, c := as.numeric(format(day, "%Y")) ]
pm10[,c("Year","Month","Day","V10","date","StationID","X","Y"):=NULL]
# setnames(pm10,"Y","y_stn_ITM")
# setnames(pm10,"X","x_stn_ITM")

#add full X,Y values
setkey(fullxy , stn)
setkey(pm10, stn)
pm10 <- merge(pm10, fullxy, all.x = T)
pm10 <- pm10[x_stn_ITM != 'NA']
pm10[,length(na.omit(PM10)),by=list(stn,c)]
pm10[, pm10_miss := length(na.omit(PM10)),by=list(stn,c)]
pm10<-pm10[!is.na(PM10)]
pm10<-na.omit(pm10)

# import monitor data and spatial merge with nearestbyday()
source("/home/zeltak/org/files/Uni/Projects/code/P031.MIAC_PM/code_snips/nearestbyday.r")

#create PM matrix
pm.m <- makepointsmatrix(pm10, "x_stn_ITM", "y_stn_ITM", "stn")

#create aod terra matrix
aod.m <- makepointsmatrix(terra[terra[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")
#create aod aqua matrix
aod.m.aq <- makepointsmatrix(aqua[aqua[,unique(aodid)], list(x_aod_ITM, y_aod_ITM, aodid), mult = "first"], "x_aod_ITM", "y_aod_ITM", "aodid")

########### join Terra to pm10
closestaod <- nearestbyday(pm.m, aod.m, 
                           pm10, terra [, list(day, aodid, aod)], 
                           "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)

########### join aqua to pm10
closestaod.aq <- nearestbyday(pm.m, aod.m.aq, 
                              pm10, aqua [, list(day, aodid, aod)], 
                              "stn", "aodid", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has aod even when there is no pm; it gets dropped on the merge

setnames(closestaod.aq,"aod","aod.aq")



setkey(pm10,stn,day)
setkey(closestaod,stn,day)
pm10.m1 <- merge(pm10, closestaod[,list(stn,day,aod)], all.x = T)
#head(mod1)
# pm10.m1 <- pm10.m1[aod != "NA"]

setkey(pm10.m1 ,stn,day)
setkey(closestaod.aq ,stn,day)
pm10.m1 <- merge(pm10.m1 , closestaod.aq[,list(stn,day,aod.aq)] , all.x = T)



#lme formulas
#base raw
m1t.formula <- as.formula(PM10~ aod+(1+aod|day)) #0.80

####################################################
####################################################
#model terra
###Base formula terra
out.m1 = lmer(m1t.formula ,data =  pm10.m1,na.action = na.exclude)
pm10.m1$predicted <- predict(out.m1)
summary(lm(PM10~predicted,data=pm10.m1))
