library(data.table)
library(e1071)
library(Hmisc)

?fread
dat <- fread("f:/Uni/Projects/P031_MIAC_MEXICO/3.Work/Archive/mod2.csv")
datm1 <- fread("f:/Uni/Projects/P031_MIAC_MEXICO/3.Work/2.Gather_data/FN009_mod1_files/mod1.csv")

# setting key before binary join
setkey(datkurt, DATE)
setkey(datm1, DATE)
# binary not join
datm1[!datkurt[kurthigh == T,]]

class(dat)
dat
sapply(dat, class)
dat[, day:=as.Date(strptime(DATE, "%m/%d/%y"))]

#suspicious value
dat[, table(AOD == 0.082)]


setkey(dat,AOD)
dat[,list(freq = .N),by=AOD][which.max(freq), ]
dat[AOD != 0.05, qplot(AOD, binwidth = 0.01)]

setkey(dat,AOD)
datkurt <- dat[, list(kurt = kurtosis(AOD)), by=DATE]
datkurt[,describe(kurt)]

setkey(dat,DATE)
setkey(datkurt,DATE)
dat[datkurt[kurthigh == F,], ][,qplot(AOD, binwidth = 0.01)]

dat[,prop.table(table(AOD == 0.05))]
dat[,prop.table(table(AOD == 0))]

datkurt[, day:=as.Date(strptime(DATE, "%m/%d/%y"))]
datkurt[,yr:=format(day, "%Y")]
datkurt[,dayofyr:=format(day, "%j")]

datkurt[which.max(kurt)]

ggplot(datkurt, aes(kurt)) + geom_histogram() + facet_wrap(~yr)

datkurt[, kurthigh := as.numeric(kurt > quantile(kurt, 0.75, na.rm = T))]

