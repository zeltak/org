library(Hmisc)

m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM10.AQ.rds")

x<- m1.all %>%
    group_by(stn) %>%
    summarise(day = n())
x
mean(m1.all$PM10)
IQR(m1.all$PM10)
sd(m1.all$PM10)



m1.all <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/Xmod1.PM25.AQ.rds")

x<- m1.all %>%
    group_by(stn) %>%
    summarise(day = n())
x
mean(m1.all$PM25)
IQR(m1.all$PM25)
sd(m1.all$PM25)ss