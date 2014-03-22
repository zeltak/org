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


# m1_2003<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2003_pred.m1.rds")
m1_2004<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2004_pred.m1.rds")
# m1_2005<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2005_pred.m1.rds")
# m1_2006<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2006_pred.m1.rds")
# m1_2007<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2007_pred.m1.rds")
# m1_2008<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2008_pred.m1.rds")
# m1_2009<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2009_pred.m1.rds")
# m1_2010<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2010_pred.m1.rds")
# m1_2011<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2011_pred.m1.rds")

#m1all<-rbindlist(list(m1_2003,m1_2004,m1_2005,m1_2006,m1_2007,m1_2008,m1_2009,m1_2010,m1_2011))

m1all<-m1_2004
m1all<- m1all[aod< 1.3]
m1all<- m1all[aod > 0.000000000]
m1all<- m1all[PM25 < 80]
m1all<- m1all[predicted > 0.000000000]  
names(m1all)

#full model

#base model for stage 1
m1.formula <- as.formula(PM25 ~ aod+tempc+WDSP+NDVI+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+ah_gm3+visib+aod*pbl+pbl+NOXsum+PM10sum+SO2sum+pctmd_1km + pctld_1km+pctop_1km+  pctdf_1km+pctmf_1km+pctev_1km+  pctcr_1km+pctpa_1km+pctsh_1km+  pctgr_1km+  pm25stge30_15k  +  pm25stlt30_3k+pm10stge30_15k   + pm10stlt30_3k   +noxstge30_15k+noxstlt30_3k+ so2stge30_15k+so2stlt30_3k+ (1 +aod+tempc|day/region))

#full model 1
out.m1_2003 = lmer(m1.formula ,data =  m1all)

str(out.m1_2003)
str(ranef(out.m1_2003))
m1ranef <- data.table(ranef(out.m1_2003)[["day"]], keep.rownames = T)
m1nested <- data.table(ranef(out.m1_2003)[["region:day"]], keep.rownames = T)
setnames(m1ranef, c("day", "ranint", "ranslope", "ranslopetemp"))

#add back fixed coef
m1ranef$ranfixddaod<-m1ranef$ranslope+summary(out.m1_2003)$coef[2,1]
m1ranef$ranfixddtemp<-m1ranef$ranslopetemp+summary(out.m1_2003)$coef[3,1]

# m1ranef[, day := as.Date(day)]
# mod1 <- merge(mod1, m1ranef, by = "day", all.x = T)

# This draws a blue polygon with geom_density(), then adds a line on top
aod<-ggplot(m1ranef, aes(x=ranfixddaod)) + geom_density(fill="black", colour=NA, alpha=.2) +geom_line(stat="density") 
temp<-ggplot(m1ranef, aes(x=ranfixddtemp)) + geom_density(fill="black", colour=NA, alpha=.2) +geom_line(stat="density") 

#for AOD
aod+ theme(axis.title=element_text(size=0, lineheight=.9, face="bold", colour="black"))+theme(plot.title=element_text(size=0, lineheight=.9, face="bold", colour="black"))+theme(axis.text.x = element_text(angle=0,size=20, vjust=1, face="bold", colour="black")) +theme(axis.text.y = element_text(angle=0,size=20, vjust=1, face="bold", colour="black"))

#for temp

temp+ theme(axis.title=element_text(size=0, lineheight=.9, face="bold", colour="black"))+theme(plot.title=element_text(size=0, lineheight=.9, face="bold", colour="black"))+theme(axis.text.x = element_text(angle=0,size=20, vjust=1, face="bold", colour="black")) +theme(axis.text.y = element_text(angle=0,size=20, vjust=1, face="bold", colour="black"))








m1all$month <- as.numeric(format(m1all$day, "%m"))
library(car)
#1-winter, 2-spring,3-summer,4-autum
m1all$season<-recode(m1all$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
m1all$seasonSW<-as.character(recode(m1all$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))
describe(m1all$seasonSW)
str(m1all)


#R2-noncal
sp<-ggplot(m1all, aes(x=aod,y=PM25)) + geom_point(size=2,shape=19)  +xlab("Predicted PM2.5") + ylab("PM2.5")+ ggtitle("(a)")
#add trend line
sp + stat_smooth(method=lm, se=TRUE, colour="white")

sp + annotate("text", label="R^2==0.42",  parse = TRUE, x=0.09, y=69, size = 9, fontface="bold.italic", colour="grey")+ theme(axis.title=element_text(size=16, lineheight=.9, face="bold", colour="black"))+theme(plot.title=element_text(size=16, lineheight=.9, face="bold", colour="black"))


