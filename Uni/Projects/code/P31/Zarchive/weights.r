library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)


wtst <- fread("f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/weight_test.csv",drop=2:3)

wtst <- fread("f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/weight_test.csv",select=c(1,4))




dim(wtst)
tail(wtst)
wtst[, day := as.Date(strptime(DATE, "%d%b%Y"))]


# lengthen aodidlur out for every day in all.met
#extract date range
dayrange <- wtst[, range(day)]
#create date range
alldays <- seq.Date(from = as.Date(dayrange[1]), to = as.Date(dayrange[2]), 1)
#create full grid
mod3grid <- data.table(expand.grid(guid = wtst[, unique(guid)], day = alldays))
