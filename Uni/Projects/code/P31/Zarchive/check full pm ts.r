#met
pm <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN001_PM_allyears/pm_all_complete.csv")
#check how many stations we have (using unique)
test<-unique(pm$SiteCode)
length(test)
pm <- pm [, day:=as.Date(strptime(Date,"%d%b%Y"))]
pm[, c := as.numeric(format(day, "%Y")) ]
#xtract year PM
pm2011<- pm[c==2011]
#check how many days in each station per year
library("plyr")
data_frame <- data.table(ddply(pm2011, .(SiteCode), transform, n = length(SiteCode)))
try2 <- data_frame[n<365 , ]

pmid <- fread ("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/pmID_guid.csv")
setkey(pm2011 ,SiteCode)
setkey(pmid,SiteCode)
pmall <- merge(pm2011,pmid)
pmall
data_frame <- data.table(ddply(pmall, .(SiteCode), transform, n = length(SiteCode)))
try2 <- data_frame[n<365 , ]


