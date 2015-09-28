#Israel PM collocate with Aeronet

#read data
pm<-readRDS('/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod2.AQ.rds')
setnames(pm,"long_aod.x","Long")
setnames(pm,"lat_aod.x","Lat")
str(pm)
pm.ar<-subset(pm,aodid=="34.792-31.926")
pm.ar<-pm.ar[,c("aodid","aod","day"),with=FALSE]
write.csv(pm.ar,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/pm.ar.NZ.csv')
pm.ar<-subset(pm,aodid=="34.782-30.856")
pm.ar<-pm.ar[,c("aodid","aod","day"),with=FALSE]
write.csv(pm.ar,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/pm.ar.NZ.csv')



####################################################################
ar$Lat<-as.numeric(c( 31.92250,30.85500))
ar$Long<-as.numeric(c(34.78917,34.78222))
ar$stn<-as.character(rbind("NT","SB"))
ar<-as.data.table(ar)

ar$aodid<-paste(ar$Long,ar$Lat,sep="-")

#prepare for collocation
ar.m1 <- makepointsmatrix(ar[1,],"Long", "Lat","aodid" )
ar.m2 <- makepointsmatrix(ar[2,],"Long", "Lat","aodid" )

aod.m <- makepointsmatrix(pm[pm[,unique(aodid)], list(Long, Lat, aodid), mult = "first"], "Long", "Lat", "aodid")


#collocate
closestaod_NT <- nearestbyday(ar.m1, aod.m,ar, pm,"stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)
closestaod_SB <- nearestbyday(ar.m2, aod.m,ar, pm,"stn", "aodid", "closest", "aod", knearest = 9, maxdistance = 1500)

close_NT<-closestaod_NT[,c("closest","day","aod","c","m"),with=FALSE]
close_SB<-closestaod_SB[,c("closest","day","aod","c","m"),with=FALSE]

write.csv(close_NT,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/close_NT.csv')
write.csv(close_SB,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/close_SB.csv')
