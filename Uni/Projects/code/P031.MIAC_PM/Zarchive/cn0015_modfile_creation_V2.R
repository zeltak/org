
###############
#Imports for use of all years
##############





#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, day:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(day, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]


#PM
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/all_pm.csv")
pm <- pm[, day:=as.Date(strptime(Date, "%d%b%Y"))]
pm[, c := as.numeric(format(day, "%Y")) ]
#summary(pm)
pm[,c("Date","POC","State","CountyFIPS","CountyName"):=NULL]


###############################
#year 2003
###############################
#xtract year met
met2003<- met[c==2003]
#xtract year PM
pm2003<- pm[c==2003]



#Full aod mod2 data set
aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2_2003.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, day:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)

########################
#start Joins 
########################


#join ID data
setkey(luxID, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, luxID, all.x = T)
#head(am2.lu)

#clean it
am2.lu <- am2.lu[,c("guid.y") :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]
#create month variable
am2.lu[, m := as.numeric(format(day, "%m")) ]

####clean a bit
gc()


###NDVI
ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
#str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]
setnames(ndvi,"month","m")
setnames(ndvi,"X","long_ndvi")
setnames(ndvi,"Y","lat_ndvi")
#names(ndvi)
ndvi <- ndvi[, c("date","xx","yy") :=NULL]
ndvi <- ndvi[NDVI < 1]


#join NDVI to aod
setkey(ndvi, m, ndviid)
setkey(am2.lu, m, ndviid)
am2.lu.nd <- merge(am2.lu, ndvi, all.x = T)
#summary(am2.lu.nd$NDVI)
am2.lu.nd [, c("long_ndvi", "lat_ndvi") := NULL]


###PBL
pbl <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/HPBL/P2003.dbf"))
#str(pbl)
pbl[, day := paste(V2,V3,V1,sep="/")]
pbl <- pbl [, day:=as.Date(strptime(day, "%m/%d/%Y"))]
pbl[, c("V1", "V2", "V3", "y1", "x1") := NULL]

#join pbl
#str(am2.lu.nd)
#fix pbl levels
am2.lu.nd[, pblid:= as.factor(pblid)]
#Join PBL
setkey(pbl , day, pblid)
setkey(am2.lu.nd, day, pblid)
am2.lu.nd.pb <- merge(am2.lu.nd, pbl, all.x = T)
#head(am2.lu.nd.pb)
am2.lu.nd.pb [, c("Long_pbl", "Lat_pbl") := NULL]


###met
#str(met2003)
#str(am2.lu.nd.pb)
setkey(met2003 , day, stn)
setkey(am2.lu.nd.pb, day, stn)
am2.lu.nd.pb.met <- merge(am2.lu.nd.pb, met2003 , all.x = T)
#summary(am2.lu.nd.pb.met)


##Save current state
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/ZH_tmp/pa.rds")
#am2.lu.nd.pb<- readRDS("/home/zeltak/ZH_tmp/pa.rds")

###################
#start with mod1
###################
# import monitor data and spatial merge with nearestbyday()

#create PM matrix
pm.m <- makepointsmatrix(pm2003, "Long_PM", "Lat_PM", "SiteCode")

#create aod matrix
m2g<-copy(am2.lu.nd.pb.met)
m2g[, guidc := as.character(guid)]
names(m2g)
#need to sort
setkey(m2g, guidc)
mod2.m <- makepointsmatrix(m2g[m2g[,unique(guidc)], list(long_aod, lat_aod, guidc), mult = "first"], "long_aod", "lat_aod", "guidc")

# use the nearestbyday() function
###########
closestaod <- nearestbyday(pm.m, mod2.m, 
                           pm2003, m2g [, list(day, guidc, aod,NDVI,pbl,WDSP,visib,ah_gm3,tempc,guid)], 
                           "SiteCode", "guidc", "closestaod", "aod", knearest = 5, maxdistance = 1500)
# this has AOD even when there is no pm; it gets dropped on the merge



setkey(pm2003,SiteCode,day)
setkey(closestaod,SiteCode,day)
mod1 <- merge(pm2003, closestaod, all.x = T)
#head(mod1)
mod1 <- mod1[aod != "NA"]



###################
#add LU to mod1 and mod2
###################

#add lu mod1
#join Land use
setkey(PLU,guid)
setkey(mod1,guid)
mod1 <- merge(mod1, PLU, all.x = T)
mod1<-na.omit(mod1)

#add lu mod2
#join Land use
setkey(am2.lu.nd.pb.met,guid)
mod2 <- merge(am2.lu.nd.pb.met, PLU, all.x = T)
#names(mod2)
mod2 [, c("lat_aod.y", "long_aod.y","wflag.y","DATE","date.x") := NULL]
setnames(mod2,"wflag.x", "wflag")
setnames(mod2,"long_aod.x", "long_aod")
setnames(mod2,"lat_aod.x", "lat_aod")
mod2<-na.omit(mod2)

###################
###save and clean
###################
saveRDS(mod1, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003.rds")
saveRDS(mod2, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2_2003.rds")
saveRDS(am2.lu.nd.pb.met, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/C15_2003_work.rds")


keep(mod1,mod2,lux,luxID,PLU,met,pm sure=TRUE) 
gc()



