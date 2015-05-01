library(ztable)
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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")




#mod3


#-------------------->> RES TABLE
res <- matrix(nrow=12, ncol=48)
res <- data.frame(res)
colnames(res) <- c(
"m1.raw","m1.raw.space","m1.raw.time","m1.time","m1.time.space","m1.time.time","m1.space","m1.space.space","m1.space.time","m1.noaod","m1.noaod.space","m1.noaod.time"
,"m1.R2","m1.rmspe","m1.R2.space","m1.R2.time","m1.rmspe.space" #mod1 Full
,"m1cv.R2","m1cv.I","m1cv.Ise","m1cv.slope","m1cv.slopese","m1cv.rmspe","m1cv.R2.space","m1cv.R2.time","m1cv.rmspe.space" #mod1 CV
,"m1cvloc.R2","m1cvloc.I","m1cvloc.Ise","m1cvloc.slope","m1cvloc.slopese","m1cvloc.rmspe","m1cvloc.R2.space","m1cvloc.R2.time","m1cvloc.rmspe.space"#loc m1
,"m2.R2" #mod2
,"m3.t31","m3.t33" #mod3 tests
,"m3.R2","m3.rmspe","m3.R2.space","m3.R2.time","m3.rmspe.space" #mod3
,"m3.I","m3.Ise","m3.slope","m3.slopese")#Extra
res$type <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011")




mod1.2000<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2000.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2000))
res[res$type=="2000", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2000))$r.squared)    
res[res$type=="2000", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2000))$coef[1,1])
res[res$type=="2000", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2000))$coef[1,2])
res[res$type=="2000", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2000))$coef[2,1])
res[res$type=="2000", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2000))$coef[2,2])
#RMSPE
res[res$type=="2000", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2001<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2001.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2001))
res[res$type=="2001", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2001))$r.squared)    
res[res$type=="2001", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2001))$coef[1,1])
res[res$type=="2001", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2001))$coef[1,2])
res[res$type=="2001", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2001))$coef[2,1])
res[res$type=="2001", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2001))$coef[2,2])
#RMSPE
res[res$type=="2001", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2002<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2002.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2002))
res[res$type=="2002", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2002))$r.squared)    
res[res$type=="2002", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2002))$coef[1,1])
res[res$type=="2002", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2002))$coef[1,2])
res[res$type=="2002", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2002))$coef[2,1])
res[res$type=="2002", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2002))$coef[2,2])
#RMSPE
res[res$type=="2002", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2003<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2003.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2003))
res[res$type=="2003", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2003))$r.squared)    
res[res$type=="2003", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2003))$coef[1,1])
res[res$type=="2003", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2003))$coef[1,2])
res[res$type=="2003", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2003))$coef[2,1])
res[res$type=="2003", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2003))$coef[2,2])
#RMSPE
res[res$type=="2003", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2004<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2004.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2004))
res[res$type=="2004", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2004))$r.squared)    
res[res$type=="2004", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2004))$coef[1,1])
res[res$type=="2004", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2004))$coef[1,2])
res[res$type=="2004", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2004))$coef[2,1])
res[res$type=="2004", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2004))$coef[2,2])
#RMSPE
res[res$type=="2004", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2005<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2005.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2005))
res[res$type=="2005", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2005))$r.squared)    
res[res$type=="2005", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2005))$coef[1,1])
res[res$type=="2005", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2005))$coef[1,2])
res[res$type=="2005", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2005))$coef[2,1])
res[res$type=="2005", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2005))$coef[2,2])
#RMSPE
res[res$type=="2005", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2006<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2006.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2006))
res[res$type=="2006", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2006))$r.squared)    
res[res$type=="2006", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2006))$coef[1,1])
res[res$type=="2006", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2006))$coef[1,2])
res[res$type=="2006", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2006))$coef[2,1])
res[res$type=="2006", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2006))$coef[2,2])
#RMSPE
res[res$type=="2006", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2007<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2007.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2007))
res[res$type=="2007", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2007))$r.squared)    
res[res$type=="2007", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2007))$coef[1,1])
res[res$type=="2007", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2007))$coef[1,2])
res[res$type=="2007", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2007))$coef[2,1])
res[res$type=="2007", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2007))$coef[2,2])
#RMSPE
res[res$type=="2007", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2008<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2008.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2008))
res[res$type=="2008", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2008))$r.squared)    
res[res$type=="2008", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2008))$coef[1,1])
res[res$type=="2008", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2008))$coef[1,2])
res[res$type=="2008", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2008))$coef[2,1])
res[res$type=="2008", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2008))$coef[2,2])
#RMSPE
res[res$type=="2008", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2009<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2009.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2009))
res[res$type=="2009", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2009))$r.squared)    
res[res$type=="2009", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2009))$coef[1,1])
res[res$type=="2009", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2009))$coef[1,2])
res[res$type=="2009", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2009))$coef[2,1])
res[res$type=="2009", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2009))$coef[2,2])
#RMSPE
res[res$type=="2009", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2010<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2010.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2010))
res[res$type=="2010", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2010))$r.squared)    
res[res$type=="2010", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2010))$coef[1,1])
res[res$type=="2010", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2010))$coef[1,2])
res[res$type=="2010", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2010))$coef[2,1])
res[res$type=="2010", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2010))$coef[2,2])
#RMSPE
res[res$type=="2010", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))


mod1.2011<-readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.pred.2011.csv")
m3.fit.all<- summary(lm( tm~Final_Pred,data=mod1.2011))
res[res$type=="2011", 'm3.R2'] <- print(summary(lm(tm~Final_Pred,data=mod1.2011))$r.squared)    
res[res$type=="2011", 'm3.I'] <-print(summary(lm(tm~Final_Pred,data=mod1.2011))$coef[1,1])
res[res$type=="2011", 'm3.Ise'] <-print(summary(lm(tm~Final_Pred,data=mod1.2011))$coef[1,2])
res[res$type=="2011", 'm3.slope'] <-print(summary(lm(tm~Final_Pred,data=mod1.2011))$coef[2,1])
res[res$type=="2011", 'm3.slopese'] <-print(summary(lm(tm~Final_Pred,data=mod1.2011))$coef[2,2])
#RMSPE
res[res$type=="2011", 'm3.rmspe'] <- print(rmse(residuals(m3.fit.all)))

res2<-select(res,39:47,49)
#save to csv
write.csv(res2,"/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/res.tm.csv")


#calculate mean Ta across area

all<-rbindlist(list(mod1.2000,mod1.2001,mod1.2002,mod1.2003,mod1.2004,mod1.2005,mod1.2006,mod1.2007,mod1.2008,mod1.2009,mod1.2010,mod1.2011))
summary(all$tm)




out<-mod1.2001 %>%
  group_by(num_insee) %>%
  summarise (n = n()) 
setkey(out,n)
tail(out)

obs<-filter(out,n>300)

m1<-filter(mod1.2001,num_insee=="65413400")

# plot a single year by day- KMR- North
ggplot(mod1.2001], aes(x = day)) + 
#overall line
  geom_line(aes(y = tm), color = "black", alpha = 0.15) + 
  #points
  geom_point(aes(y = tm), color = "blue", alpha = 0.7, size = 0.8) + 
  geom_point(aes(y = Final_Pred), color = "red", alpha = 0.7, size = 0.8) + 
  #smoothers
  geom_smooth(aes(y = Final_Pred), color = "red", linetype = "dashed", width = 1.4, se = F, size=0.9) + 
  geom_smooth(aes(y = tm), color = "blue", se = F, size=0.9) + 
  ylab(expression(paste(PM[2.5], "concentration (ug/", m^3, ")"))) + 
  theme_bw() + theme(axis.title.x = element_blank())


