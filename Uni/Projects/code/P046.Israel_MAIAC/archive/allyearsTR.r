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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")


#-------------------->> RES TABLE
res <- matrix(nrow=2, ncol=45)
res <- data.frame(res)
colnames(res) <- c(
          "year"
          ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
          ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
          ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
          ,"m2.R2" #mod2
          ,"m3.t31","m3.t33" #mod3 tests
          ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
          ,"XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX","XX")
          
res$type <- c("PM25","PM25")


m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2003.rds")
m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2004.rds")
m1.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2005.rds")
m1.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2006.rds")
m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2007.rds")
m1.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2008.rds")
m1.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2009.rds")
m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2010.rds")
m1.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2011.rds")
m1.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2012.rds")
m1.2013 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.TR.2013.rds")


m1.all <- rbindlist(list(m1.2003,m1.2004,m1.2005,m1.2006,m1.2007,m1.2008,m1.2009,m1.2010,m1.2011,m1.2012,m1.2013), fill=TRUE)

m1.all[,elev.s:= scale(elev)]
m1.all[,tden.s:= scale(tden)]
m1.all[,pden.s:= scale(pden)]
m1.all[,dist2A1.s:= scale(dist2A1)]
m1.all[,dist2water.s:= scale(dist2water)]
m1.all[,dist2rail.s:= scale(dist2rail)]
m1.all[,Dist2road.s:= scale(Dist2road)]
m1.all[,ndvi.s:= scale(ndvi)]
m1.all[,MeanPbl.s:= scale(MeanPbl)]
m1.all[,p_ind.s:= scale(p_ind)]
m1.all[,p_for.s:= scale(p_for)]
m1.all[,p_farm.s:= scale(p_farm)]
m1.all[,p_dos.s:= scale(p_dos)]
m1.all[,p_dev.s:= scale(p_dev)]
m1.all[,p_os.s:= scale(p_os)]
m1.all[,tempa.s:= scale(tempa)]
m1.all[,WDa.s:= scale(WDa)]
m1.all[,WSa.s:= scale(WSa)]
m1.all[,RHa.s:= scale(RHa)]
m1.all[,Raina.s:= scale(Raina)]
m1.all[,NO2a.s:= scale(NO2a)]


################# clean BAD STN PM25 and check if improved model?
raWDaf <- ddply(m1.all, c("stn","m"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
raWDaf
raWDaf<-as.data.table(raWDaf)
bad<- raWDaf[R2< 0.05]
bad[,badid := paste(stn,m,sep="-")]
#################BAD STN
m1.all[,badid := paste(stn,m,sep="-")]
####Take out bad stations
m1.all <- m1.all[!(m1.all$badid %in% bad$badid), ] 


m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WDa.s+WSa.s+Dust#+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+Dist2road.s+ndvi.s #spatial
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                        #+aod*Dust #interactions
                        #+ns(day)+ns(m)
                        #+ as.factor(season)
                        + aod:as.factor(elev.s)
                        #+ as.factor(reg_num) + aod:as.factor(reg_num)
                        #+ns(tempa.s)
                        +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 
#[1] 0.7455536

#full fit
m1.fit.all <-  lmer(m1.formula,data=m1.all,weights=normwt)
m1.all$pred.m1 <- predict(m1.fit.all)
res[res$type=="PM25", 'm1.R2'] <- print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#RMSPE