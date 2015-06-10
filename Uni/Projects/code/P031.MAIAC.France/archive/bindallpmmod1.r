m1.all1 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2003.PM25.clean.rds")
m1.all3 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2005.PM25.clean.rds")
m1.all4 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.clean.rds")
m1.all5 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2007.PM25.clean.rds")
m1.all6 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2008.PM25.clean.rds")
m1.all7 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2009.PM25.clean.rds")
m1.all8 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2010.PM25.clean.rds")
m1.all9 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2011.PM25.clean.rds")
m1.all11 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2012.PM25.clean.rds")
m1.all12 <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2013.PM25.clean.rds")

m1.all <-rbindlist(list(m1.all1,m1.all2,m1.all3,m1.all4,m1.all5,m1.all6,m1.all7,m1.all8,m1.all9,m1.all11,m1.all12))

names(m1.all2)
m1.all1$res<-NULL

describe(m1.all$region)

PM=m1.all%>%group_by(c,region)%>%summarise(count=n())
write.csv(PM,"~/ZH_tmp/pmbyyear.csv")

m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.clean.rds")
describe(m1.all$region)



m1.all <-readRDS("/media/NAS/Uni/Projects/P031_MAIAC_France/2.work/WORKDIR/mod1.AQ.2006.PM25.clean.rds")
describe(m1.all$region)