############### HEAD
###############
#LIBS
###############

library(lme4);library(reshape);library(foreign) ;library(ggplot2);library(plyr);library(data.table);library(reshape2);library(Hmisc);library(mgcv);library(gdata);library(car);library(dplyr);library(ggmap);library(broom);library(splines)

#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#-------------------->> RES TABLE
res <- matrix(nrow=10, ncol=45)
res <- data.frame(res)
colnames(res) <- c(
  "year"
  ,"m1.R2","m1.PE","m1.R2.s","m1.R2.t","m1.PE.s" #full model
  ,"m1cv.R2","m1cv.I","m1cv.I.se","m1cv.S","m1cv.S.se","m1cv.PE","m1cv.R2.s","m1cv.R2.t","m1cv.PE.s" #mod1 CV
  ,"m1cv.loc.R2","m1cv.loc.I","m1cv.loc.I.se","m1cv.loc.S","m1cv.loc.S.se","m1cv.loc.PE","m1cv.loc.PE.s","m1cv.loc.R2.s","m1cv.loc.R2.t"#loc m1
  ,"m2.R2" #mod2
  ,"m3.t31","m3.t33" #mod3 tests
  ,"m3.R2","m3.PE","m3.R2.s","m3.R2.t","m3.PE.s"#mod3
  ,"m3.loc.R2","m3.loc.I","m3.loc.I.se","m3.loc.S","m3.loc.S.se","m3.loc.PE","m3.loc.PE.s","m3.loc.R2.s","m3.loc.R2.t"#loc m3
  ,"XX","XX","XX","XX")


res$year <- c(2003:2012) 



################TAIL

t2003<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2003.csv")
t2004<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2004.csv")
t2005<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2005.csv")
t2006<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2006.csv")
t2007<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2007.csv")
t2008<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2008.csv")
t2009<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2009.csv")
t2010<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2010.csv")
t2011<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2011.csv")
t2012<-fread("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.2012.csv")

LTPM.ALL<- rbindlist(list(t2003,t2004,t2005,t2006,t2007,t2008,t2009,t2010,t2011,t2012))

AYLTPM<- LTPM.ALL %>%
    group_by(aodid) %>%
    summarise_each(funs(mean))

write.csv(AYLTPM,"/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/LTPM3.AQ10.ALLY.csv")

