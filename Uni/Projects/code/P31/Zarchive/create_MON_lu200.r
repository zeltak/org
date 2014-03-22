#import 50x50LU terms
lu2 <-read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50.dbf")
lu <-read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/lu_50x50_MIA.dbf")

lu$guid<-as.factor(lu$guid)
lu<-as.data.table(lu)
lu2<-as.data.table(lu2)
lu[,c("long_pm","lat_pm","reg_name","reg_id","tdenden"):=NULL]
lu2[,c("LONGITUDE","LATITUDE_M","reg_name","reg_id","OID_"):=NULL]
lu2<- rename(lu2, c(SITECODE="SiteCode",guid_="guid")) 
setcolorder(lu2, c("SiteCode","guid","popden","pcturban","tden", "dist_pemis","dist_A1","elev"))
luf<-rbindlist(list(lu, lu2))

saveRDS(luf, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/luf_NE_MIA.rds")
