# script to join geolocated BI lat/lon/dates with Itai's PM and temperature predictions
# started 3/24/2016

library(data.table)
library(pryr) # keep track of memory usage
library(ggmap) # to plot participant locations

#### participant info
load("data/FromHeather/BIdeID_2016-03-22.RData")
bideid <- BIdeID
rm(BIdeID)

# This file contains dob and PHI and sits w/ Heather
biid<-fread("data/FromHeather/deid2.csv")
bideid<-merge(bideid, biid[,.(ID,dob)],by='ID')
rm(biid)

#### PM exposures
# import the data.table of exposure series for each grid ID
# we use a relative path (relative to our R project/git repo)
pm <- readRDS("data/FromItai/pmmatrix_2016-03-18.rds")
# import table linking subjectID with gridID
sidlinkpm <- readRDS("data/FromItai/cases_aodguid_2016-03-18.rds")

# #### Temperature exposures
# # import the data.table of exposure series for each grid ID
# # we use a relative path (relative to our R project/git repo)
# tmp <- readRDS("data/FromItai/tmpmatrix_2016-03-18.rds")
# # import table linking subjectID with gridID
# sidlinktmp <- readRDS("data/FromItai/cases_tempguid_2016-03-18.rds")

# looking at PM estimates
class(pm)
dim(pm)
sapply(pm, class)
# fix some variable classes
pm[, GUID := as.integer(GUID)]
pm[, day := as.IDate(day)] # for data.table purposes
pm[1:2,]

# look at exposure link table
class(sidlinkpm)
dim(sidlinkpm)
sidlinkpm[1:2,]

# look at participant info table
bideid[1:2,]
sapply(bideid, class)
bideid[, LMP := as.IDate(LMP)] # for data.table merging purposes
bideid[, dob:=as.IDate(strptime(dob,"%m/%d/%Y"))]
dim(bideid)

# to generate exposure estimates, we need to join tables and extract date ranges
bideid <- merge(bideid, sidlinkpm[, .(ID, GUID)], by = "ID", all.x = T)
# first - are there kids not in the link table?
bideid[is.na(GUID), .N] # 64 kids
# where are these kids?
backgroundmap <- get_map("Boston, MA", zoom = 5)
ggmap(backgroundmap, extent = "normal", darken = c(0.5, "white")) + 
  geom_point(aes(x = X, y = Y), alpha = 1, data = bideid[is.na(GUID),]) +
  theme_bw()
# they are outside of the exposure model region

# where are the 100 most frequent GUIDs (where are most people coming from)
ggmap(backgroundmap, extent = "normal", darken = c(0.5, "white")) + 
  geom_point(aes(x = X, y = Y), alpha = 1, 
             data = bideid[GUID %in% bideid[, .N, by = GUID][order(N, decreasing = T)][1:100, GUID]]) +
  theme_bw()

# for kids with a GUID - make a long data.table with their daily exposure time series 
# (395 days starting 60 days before LMP)
# computing range join with foverlaps
# since we need a start and end for both datasets, we say that each PM exposure ends on the nextday
pm[, nextday := day + 1]
# set the period we are interested in (395 days starting 60 days before LMP)
bideid[, start := LMP-59]
bideid[, end := LMP+335]
# key up both DTs
setkey(pm, GUID, day, nextday)
setkey(bideid, GUID, start, end)
# let's see how long this takes
ptm <- proc.time(); Sys.time() # took 12 minutes on Allan's iMac; 25 min on Heather's BI laptop
bipmlong <- foverlaps(pm[, .(GUID, day, nextday, pm25 = pm25_final)], bideid, by.x = c("GUID", "day", "nextday"), by.y = c("GUID", "start", "end"), type="any", nomatch = 0)
ptm <- proc.time() - ptm; paste(round(ptm[["elapsed"]]/60, 1), "minutes")

dim(bipmlong)
bipmlong
# are the NA only people who didn't have a GUID?
identical(bideid[is.na(GUID), ID], bipmlong[is.na(pm25), ID])
bipmlong[is.na(pm25), .N] # same 64 people
# we drop them here
bipmlong <- bipmlong[!is.na(pm25)]

# create summary variables (trimester specific averages and recent exposures)
# restrict to those who have an LMP more than 335 days before 2013-12-31 (to allow a dlm series that has a 60 day lag after birth)
setkey(bipmlong, ID)
setkey(bideid, ID)
bideid[LMP + 335 <= as.Date("2013-12-31"), .N] # 47971 babies
bideid[,range(LMP)] # All LMPs - 60 days are after start of PM model
#merge in dob from Heather's file
bipmlong[bideid[LMP + 335 <= as.Date("2013-12-31"), .(ID,dob)],dob:=dob]
         
bipmsummary <- bipmlong[bideid[LMP + 335 <= as.Date("2013-12-31"), .(ID)], 
                        list(pmpreg = mean(.SD[day >= LMP & day <= dob, pm25]), 
                               pmtri1 = mean(.SD[day >= LMP & day < LMP + 7*14, pm25], na.rm = T),
                               pmtri2 = mean(.SD[day >= LMP + 7*14 & day < LMP + 7*28 & day <= dob, pm25], na.rm = T),
                               pmtri3 = mean(.SD[day >= LMP + 7*28 & day <= dob, pm25], na.rm = T), # consider the days you did have
                               pmlast02days = mean(.SD[day >= dob - 2 & day <= dob, pm25]), #these variables need to be run by Heather with the real dob
                               pmlast07days = mean(.SD[day >= dob - 7 & day <= dob, pm25]),
                               pmlast14days = mean(.SD[day >= dob - 14 & day <= dob, pm25]),
                               pmlast28days = mean(.SD[day >= dob - 28 & day <= dob, pm25]),
                               dobdow = format(dob, "%a"),
                               LMP = LMP[1], edd = LMP[1] + GA_days[1]),by=ID]
dim(bipmsummary)
# Remove dob for sharing back to Allan/Margherita/Itai
bipmlong[,dob:=NULL]
names(bideid)
bideid[,dob:=NULL]
names(bipmlong)
names(bipmsummary)
# check for missingness
bipmsummary[is.na(pmtri3)]
problemid <- bipmsummary[is.na(pmtri3), ID]
setkey(bideid, ID)
bideid[.(problemid)]
# well some of these may not have a third trimester (delivered too soon)
# <7*28 = 196 days after LMP
bideid[.(problemid), summary(GA_days)]
bideid[.(problemid),][order(GA_days)]
# let's track down someone missing their pmtri3
bideid[.(problemid)][485]# ID 46537
bipmsummary[ID == 46570]
# drop one all missing row created by our summary somehow
bipmsummary <- bipmsummary[!is.na(ID)]

# #check one observation with separate code: ID 1000
# #We calculated thee summary using edd (but actual averages based on dob)
# bideid[ID == 1000]
# pm[GUID == 1202432 & day >= as.Date("2003-06-02") & day <= as.Date("2003-06-02") + 283.5, mean(pm25_final)]
# bipmsummary[ID == 1000]

# save out the derived exposure summary
write.csv(bipmsummary, file = paste0("data/bipmsummary_", Sys.Date(), ".csv"), row.names = F)

# generate the wide exposure dataset for DLMs
setkey(bipmlong, ID, day)
# subsetting to individuals within the model time period
# create a dayindex
# note that LMP will always be dayindex061
bipmlong[bideid[LMP + 335 <= as.Date("2013-12-31"), .(ID)], dayindex := paste0("dayindex", sprintf("%0.3i", 1:.N)), by = "ID"]

# save out the derived long exposure time series
write.csv(bipmlong, file = paste0("data/bipmlongdaily_", Sys.Date(), ".csv"), row.names = F)

bipmwide <- dcast.data.table(bipmlong[bideid[LMP + 335 <= as.Date("2013-12-31"), .(ID)], list(ID, dayindex, pm25)], ID ~ dayindex)
dim(bipmwide)
bipmwide[1:5,1:5,with=FALSE]
# drop the column of NA (not sure why it is there)
bipmwide[, "NA" := NULL]
# save this out
write.csv(bipmwide, paste0(file ="data/bipm_wide_396days_", Sys.Date(), ".csv"), row.names = F)

#cleanup
mem_used()
rm(i, ptm, backgroundmap, pm, problemid, sidlinkpm)
# end of file
