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

mod1 <-  as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN003_WUNCDC yearly/met2008.dbf"))
mod1[, day:=as.Date(strptime(Date, "%Y-%m-%d"))]
str(mod1)
mod1 <- mod1[x> -74 & x < -69 & y < 44 & y > 41, ]


ugrid <-mod1 %>%
    group_by(station) %>%
    summarise(tempc = mean(tempc, na.rm=TRUE),x = mean(x, na.rm=TRUE),  y = mean(y, na.rm=TRUE))


dugrid <-mod1[day == "2008-08-01"] %>%
    group_by(station) %>%
    summarise(tempc = mean(tempc, na.rm=TRUE),x = mean(x, na.rm=TRUE),  y = mean(y, na.rm=TRUE))

d2ugrid <-mod1[day == "2003-08-01"] %>%
    group_by(station) %>%
    summarise(tempc = mean(tempc, na.rm=TRUE),x = mean(x, na.rm=TRUE),  y = mean(y, na.rm=TRUE))
describe(d2ugrid$tempc)

write.csv(ugrid,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Joel_temp_grant/ygrid.csv")
write.csv(dugrid,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Joel_temp_grant/dgrid08.csv")
write.csv(d2ugrid,"/media/NAS/Uni/Projects/P000_TMP_PROJECTS/Joel_temp_grant/d4grid.csv")

