PBL2000 <- read.table("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.4.Resources/DATA/hpbl/hpbl/hpbl_2000.txt", header=FALSE)

head(PBL2000)

PBL2000$dd2 <- paste(PBL2000$V2,PBL2000$V1,sep="")

PBL2000$Date<- as.Date(PBL2000$dd2,format = "%j%Y")