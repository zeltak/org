MIA<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/gis/l200/MIA.csv")
NE<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/0.raw/gis/l200/NE.csv")

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

NE<-NE[,c(5:12),with=FALSE]
MIA<-MIA[,c(3:7,11,13,14),with=FALSE]

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

MIA<-setcolorder(MIA, c(1,2,6,3,5,4,7,8))

l=seq(names(MIA));names(l)=names(MIA);l
l=seq(names(NE));names(l)=names(NE);l

flu<-rbind(MIA,NE)

saveRDS(flu,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/flu_LPM.rds")


write.csv(flu,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_200_LU.csv")
fluCP<-flu[,c(7,8),with=FALSE]
write.dbf(fluCP,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_200_LU_XY.dbf")





