###############
#LIBS
###############
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
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


m1_2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")
m1_2004<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2004_st.rds")
m1_2005<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2005_st.rds")
m1_2006<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2006_st.rds")
m1_2007<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2007_st.rds")
m1_2008<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2008_st.rds")
m1_2009<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2009_st.rds")
m1_2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2010_st.rds")
m1_2011<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2011_st.rds")

m1all<-rbindlist(list(m1_2003,m1_2004,m1_2005,m1_2006,m1_2007,m1_2008,m1_2009,m1_2010,m1_2011))
names(m1all)

m1alls<-m1all[,c(1,2,3,4,5,6,12,13,44),with=FALSE]


#map the predictions
#aggregate by guid
m3d_agg <- (m1alls[, list(LTres3 =mean(resm3, na.rm = TRUE),
                          LTPM25 =mean(PM25, na.rm = TRUE),
                        long_pm = long_pm[1], #use the first long and lat (by guid)
                        lat_pm = lat_pm[1]),by = SiteCode])


write.csv(m3d_agg,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN30_paperplots/monrsid.csv")


describe(m3d_agg)

