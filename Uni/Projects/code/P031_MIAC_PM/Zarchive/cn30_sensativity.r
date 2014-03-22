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


m1_2003<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
m1_2004<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2004_pred.m1.rds")
m1_2005<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2005_pred.m1.rds")
m1_2006<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006_pred.m1.rds")
m1_2007<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2007_pred.m1.rds")
m1_2008<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2008_pred.m1.rds")
m1_2009<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2009_pred.m1.rds")
m1_2010<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2010_pred.m1.rds")
m1_2011<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2011_pred.m1.rds")

m1all<-rbindlist(list(m1_2003,m1_2004,m1_2005,m1_2006,m1_2007,m1_2008,m1_2009,m1_2010,m1_2011))


#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))

m2.formula <- as.formula(PM25 ~ aod+ (1 +aod|day/region))

describe(m1all)


#full model 1
out.m1_2003 = lmer(m1.formula ,data =  m1all)
summary(out.m1_2003)
#generate prediction
m1all$predicted.m1.fall <- predict(out.m1_2003)
#get overall R2
summary(lm(m1all$PM25~m1all$predicted.m1.fall))
#m1all$predicted.m1.fall  1.01761
#ultiple R-squared:  0.9216

#full model 1
out.m2_mall = lmer(m2.formula ,data =  m1all)
#generate prediction
m1all$predicted.m1.aodonly <- predict(out.m2_mall)
#get overall R2
summary(lm(m1all$PM25~m1all$predicted.m1.aodonly))
#m1all$predicted.m1.aodonly  1.021926
#Multiple R-squared:  0.9024

#######################
#seaons
#######################

m1all$month <- as.numeric(format(m1all$day, "%m"))


library(car)
#1-winter, 2-spring,3-summer,4-autum
m1all$season<-recode(m1all$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m1all$seasonSW<-recode(m1all$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1")
describe(m1all$seasonSW)



#full model 1 swinter
out.m1_2003 = lmer(m1.formula ,data = m1alls  )
m1alls<- m1all[seasonSW==1]
#generate prediction
m1alls$predicted.m1.summer <- predict(out.m1_2003)
#get overall R2
summary(lm(m1alls$PM25~m1alls$predicted.m1.summer))
#m1alls$predicted.m1.summer  1.025556
#Multiple R-squared:  0.8769

#full model 1 summer
setkey(m1all,seasonSW)
out.m1_2003 = lmer(m1.formula ,data = m1all[seasonSW==2])

m1all[seasonSW==2]$predicted.m1.summer <- predict(out.m1_2003)
#get overall R2
summary(lm(m1all[seasonSW==2]$PM25~m1all[seasonSW==2]$predicted.m1.aodonly))
#m1all[seasonSW == 2]$predicted.m1.aodonly  1.018028
#Multiple R-squared:  0.9247
