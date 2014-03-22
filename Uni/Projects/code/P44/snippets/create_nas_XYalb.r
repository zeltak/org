#import NAS
nascases<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_guid.csv")
names(nascases)
nascases_albertsXY<-nascases[,c(4,5,6,23,24),with=FALSE]
setnames(nascases_albertsXY,"xalb","xalb_NAS")
setnames(nascases_albertsXY,"yalb","yalb_NAS")
write.csv(nascases_albertsXY,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_albertsXY.csv")
