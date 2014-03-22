###############
#LIBS
###############
install.packages("Matrix")
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
#library(nlme)
library(lme4)



#met
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
#convert date from 01JAN2000 format
met <- met [, date:=as.Date(strptime(date, "%d%b%Y"))]
met[, c := as.numeric(format(date, "%Y")) ]
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
met [, c := as.numeric(format(date, "%Y")) ]

describe(met)
ts<-data.table(as.data.frame(table(met$stn)))
setnames(ts,"Var1","stn")
str(ts)
ts <- ts[Freq >= 4700]


#met
met[, stn:= as.factor(stn)]
met_agg <- (met[, list(tempc =mean(tempc, na.rm = TRUE), 
                       long_met = long_met[1], #use the first long and lat (by aodid)
                       lat_met = lat_met[1]),by = stn])  
#keep only valid met stations
tsz<-met_agg[(met_agg$stn %in% ts$stn),]

write.csv(tsz,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/metID.csv")

