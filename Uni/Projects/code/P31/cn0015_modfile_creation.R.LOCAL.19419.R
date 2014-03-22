###############
#LIBS
###############
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

###########Paths
# set working directory
path.root <- "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/"
setwd(path.root); list.files()


path.keys <- paste(path.root, "FN007_Key_tables/", sep = "")

# clip to bounding coordinates of road network
#dat <- dat[long_aod > -99.5 & long_aod < -98.5 & lat_aod < 19.9 & lat_aod > 19, ]


###############
Imports
##############

lu<- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN004_LU_full_dataset/full_LU.csv")
met <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN002_NCDC_allyears/ncdc00_12.csv")
str(met)


#convert date from 01JAN2000 format
met <- met [, date:=as.Date(strptime(date, "%d%b%Y"))]

#add temp in celsius
met[, tempc := (5/9)*(TEMP-32)]
met <- met[slp != 9999.9]
met <- met[WDSP != 999.9]
met <- met[visib != 999.9]
met <- met[dewp != 9999.9]
met <- met[tempc != 9999.9]
describe(met)


aodmod2 <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/aod_ne_mod2.csv")
#describe(aodmod2$DATE)
aodmod2 <- aodmod2 [, date:=as.Date(strptime(DATE, "%d%b%Y"))]
#head(aodmod2,n=200)



describe(lu)
# covers all used grid cells
# set -9999 to missing
lu[,reg := NULL]
lu[,FIPS_1 := NULL]

#join Land use
names(lu)
names(aodmod2)
setkey(lu, lat_aod, long_aod)
setkey(aodmod2, lat_aod, long_aod)
am2.lu <- merge(aodmod2, lu, all.x = T)
head(am2.lu)

#clean it
am2.lu <- am2.lu[,guid.y :=NULL]
setnames(am2.lu,"guid.x","guid")
#get rid of water gird cells
am2.lu <- am2.lu[wflag == 0]

mod1$dayofyr <- as.numeric(format(mod1$day, "%j"))
am2.lu <- am2.lu[, m := date, ]


ndvi <-  as.data.table(read.dbf("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Data/USA/NDVI/MODIS MIA_NE/ndvi2003.dbf"))
str(ndvi)
#create ndviID
ndvi[, ndviid := paste(X,Y,sep="")]

####for first run only create a ndvi id keytable by sourcing this file
#####NEED TO CREATE MIMICING SAS LATER

#add closeltst ndviID

#join Land use
names(ndvi)
names(am2.lu)
setkey(ndvi, lat_aod, long_aod)
setkey(am2.lu, lat_aod, long_aod)
am2.lu <- merge(aodmod2, lu, all.x = T)
head(am2.lu)






gc()