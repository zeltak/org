library(sas7bdat)
library(readr)
#Read PM data
t2000<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2000_a.sas7bdat",debug=FALSE)
out2000<-t2000 %>% group_by(num_insee) %>% summarise(mean = mean(tm, na.rm=TRUE))


t2006<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2006_a.sas7bdat",debug=FALSE)
out2006<-t2006 %>% group_by(num_insee) %>% summarise(mean = mean(tm, na.rm=TRUE))

t2011<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2011_a.sas7bdat",debug=FALSE)
out2011<-t2011 %>% group_by(num_insee) %>% summarise(mean = mean(tm, na.rm=TRUE))
head(t2011)
write_csv(t2011,"/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_mon_xy.csv")


system.time(m3.2011<-fread("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/mod3.2011.csv"))
m3.2011$lstid<-paste(m3.2011$Longitude,m3.2011$Latitude,sep="-")
grid <- unique(m3.2011, by="lstid")
write_csv(grid,"/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_fullgrid_xy.csv")


#calc mean

t2000<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2000_a.sas7bdat",debug=FALSE)

t2001<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2001_a.sas7bdat",debug=FALSE)

t2002<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2002_a.sas7bdat",debug=FALSE)

t2003<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2003_a.sas7bdat",debug=FALSE)

t2004<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2004_a.sas7bdat",debug=FALSE)

t2005<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2005_a.sas7bdat",debug=FALSE)

t2006<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2006_a.sas7bdat",debug=FALSE)

t2007<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2007_a.sas7bdat",debug=FALSE)

t2008<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2008_a.sas7bdat",debug=FALSE)

t2009<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2009_a.sas7bdat",debug=FALSE)

t2010<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2010_a.sas7bdat",debug=FALSE)

t2011<-read.sas7bdat ("/media/NAS/Uni/Projects/P022_Temprature_France/1_Raw_data/Ta.monitors/france_2011_a.sas7bdat",debug=FALSE)




tall <- rbindlist(list(t2000,t2001,t2002,t2003,t2004,t2005,t2006,t2007,t2008,t2009,t2010,t2011), fill=TRUE)
summary(tall)
