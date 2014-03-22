#################################
#cross valadating mod3
#################################


############
#method 1
###########


#PM
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/all_pm.csv")
pm <- pm[, day:=as.Date(strptime(Date, "%d%b%Y"))]
pm[, c := as.numeric(format(day, "%Y")) ]
#summary(pm)
pm[,c("Date","POC","State","CountyFIPS","CountyName"):=NULL]

dd<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid.csv")

setkey(dd , SiteCode )
setkey(pm, SiteCode )
pmg <- merge(pm, dd , all.x = T)


############################
#2003
############################

#xtract year PM
pmg2003<- pmg[c==2003]



mod1<- readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
mod1<-mod1[,c("guid","day","PM25","predicted","SiteCode"),with=FALSE]
setnames(mod1,"predicted","predicted.m1")


mod3<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/m3_2003.pred3.rds")

#########################
#prepare for m3.R2
#########################
mod3[,lat_aod.y:=NULL]
mod3[,long_aod.y:=NULL]
setnames(mod3,"lat_aod.x","lat_aod")
setnames(mod3,"long_aod.x","long_aod")
#subset mod3
mod3<-mod3[,c("guid","day","lat_aod","long_aod","predicted.m3"),with=FALSE]


monused<-mod1[,unique(guid)]

pmg2003x<- pmg2003[!guid %in% monused, ]



setkey(mod3,day,guid)
setkey(pmg2003x,day,guid)
x1 <- merge(pmg2003x,mod3[, list(day,guid,predicted.m3)], all.x = T)    		
summary(lm(PM25~predicted.m3,data=x1))



############
#method 2
###########

mod1CV_all_2003<-readRDS( "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1CV_all_2003_pred.m1.rds")





