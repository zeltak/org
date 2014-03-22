###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)



add<-fread("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN007_Key_tables/NAS_addresses_guid.csv")
str(add)
l=seq(names(add));names(l)=names(add);l
add2<-add[,c("guid","nasid", "AddID", "AddStartDate", "AddEndDate" ,"SeasonStartMos", "SeasonEndMos"),with=FALSE]

add2$xstart<-substr(add2$AddStartDate, 1, 10)
add2$xend<-substr(add2$AddEndDate, 1, 10)

add2 <- add2[,daystart:=as.Date(strptime(xstart, "%d/%m/%Y"))]
add2 <- add2[,dayend:=as.Date(strptime(xend, "%d/%m/%Y"))]
add2[,xstart:=NULL]
add2[,xend:=NULL]
add2[,AddStartDate:=NULL]
add2[,AddEndDate:=NULL]

#populate all days
days_x <-seq.Date(from = as.Date("2003-01-01"), to = as.Date("2011-12-31"), 1)
#create date range
add3 <- data.table(expand.grid(AddID = add2[, unique(AddID)], day = days_x))
setkey(add3,AddID)
setkey(add2,AddID)
add4<- merge(add3,add2,all.x=TRUE)

#for each subject id create the dates the subject was at that address
add5 <- add4[daystart <= day & dayend > day, ]
#recode zeros to NA
add5[ SeasonStartMos == 0, SeasonStartMos := NA]
add5[ SeasonEndMos == 0, SeasonEndMos := NA]

# address seasonality
add5$m <- as.numeric(format(add5$day, "%m"))
add5$d <- as.numeric(format(add5$day, "%d"))

add5$ms2<-substr(add5$SeasonEndMos, 1, 2)
add5$ds2<-substr(add5$SeasonEndMos, 3, 2)

try2 <- add5[!is.na(SeasonStartMos),]
try2$ms2<-substr(try2$SeasonEndMos, 1, 2)
try2$ds2<-substr(try2$SeasonEndMos, 3, 4)

ms=substr(SeasonStartMos,1,2);
ms2=ms*1;
ds=substr(SeasonStartMos,3,2);
ds2=ds*1;
me=substr(SeasonEndMos,1,2);
me2=me*1;
de=substr(SeasonEndMos,3,2);
de2=de*1;
drop ms ds me de;





nasPM<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P044_NAS_MAIAC/3.Work/2.Gather_data/FN002_Exposure/nasPM.rds")