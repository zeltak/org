
# BI Air Pol - DATA CLEANING
# started Mar 30, 2016
# Margherita De Carli

library(data.table)
library(ggplot2)
library(Hmisc)
library(ggmap)

setwd("J:/PM/Just_Lab/projects/BI_Air_Pol")

load("data/FromHeather/BIdeID_2016-03-22.RData")
bideid <- BIdeID
rm(BIdeID)
head(bideid)
dim(bideid)

# calculate birth weight Fenton score for each baby
source("code/Fenton_calculator.R")
bideid[BSEX == "M", sex := 1]
bideid[BSEX == "F", sex := 2]
bideid[, c("size", "bwzfenton", "bwpctilefenton") := bwzfenton(sex, birth_weight, round(GA_days))]

# histogram of the birth weight Fenton z score
ggplot(bideid) + 
  geom_histogram(aes(bwzfenton), binwidth = .01) +
  theme_bw()

ggplot(bideid) + 
  geom_histogram(aes(bwzfenton), binwidth = .01) +
  facet_grid(. ~ BSEX) +
  theme_bw()

# who does have a birth weight Fenton z score > 5 standard deviations
bideid[abs(bwzfenton) > 5,]
bideid[which.max(abs(bwzfenton)),]
# export our big outliers
outliers_bwz <- bideid[abs(bwzfenton) > 5, .(ID, momID, gestation, birth_weight, BSEX,
                                             gestation_week, gestation_day, GA_days, 
                                             LMP, bwzfenton, bwpctilefenton)]

problems <- list("outliers (abs(bwzfenton) > 5) for birth weight Fenton z-score" = outliers_bwz) 

# check BBirths
bideid[, describe(BBirths)]
bideid[, .N, by = c("momID", "LMP")][, table(N)]
bideid[BBirths > 1, describe(BBirths)]
bideid[BBirths == 2, .N, by = "momID"]
bideid[BBirths == 2, .N, by = "momID"][, table(N)] 
# there are 1918 moms who had twins only once
# there are 12 moms who had twins twice 
# there is one mom who has BBirths = 2 but there is only one kid in bideid 
bideid[BBirths == 2, .N, by = "momID"][N == 1] 

########## check 1 for BBirth ########## 
bideid[momID == 6743724, .(ID, momID, BBirths, LMP)] # this mom has BBirths = 2 but only one kid...

problems[[2]] <- bideid[momID == 6743724, .(ID, momID, BBirths, LMP)]
names(problems)[2] <- "ID with BBirth = 2 but no twin brother"
########################################

bideid[momID %in% bideid[BBirths == 2, .N, by = "momID"][N == 4, momID] & BBirths == 2, 
       .(ID, momID, BBirths, LMP)][, .N, by = c("momID", "LMP")] # the 12 moms who had twins twices seem ok 
bideid[BBirths == 3, .N, by = "momID"][, table(N)] # there are 99 moms who had three kids (only once)
bideid[BBirths == 4, .N, by = "momID"][, table(N)] # there is a mom who had four kids!
bideid[momID == 4095369]

bideid[, .N, by = c("momID", "LMP")][, table(N)]
bideid[, table(BBirths)] # why BBirths[1] is different from N[1]?

bideid[BBirths == 1, .N, by = c("momID", "LMP")][, table(N)]
bideid[BBirths == 1, .N, by = c("momID", "LMP")][N == 2, ]

########## check 2 for BBirth ########## 
bideid[momID == 7616340, .(ID, momID, BBirths, LMP)] # there are two twins that have BBirths == 1
problems[[3]] <- bideid[momID == 7616340, .(ID, momID, BBirths, LMP)]
names(problems)[3] <- "two twins have BBirth = 1"
########################################

bideid[BBirths == 2, .N, by = c("momID", "LMP")][, table(N)]
bideid[BBirths == 2, .N, by = c("momID", "LMP")][N == 1, ]

########## check 3 for BBirth ########## 
bideid[momID %in% bideid[BBirths == 2, .N, by = c("momID", "LMP")][N == 1, momID] &
         BBirths == 2, .(ID, momID, BBirths, LMP)][order(momID)] # here there are 66 kids that have BBirths
# equal to 2 (they should be twins!) but their LMPs are different. What are the differences?
bideid[momID %in% bideid[BBirths == 2, .N, by = c("momID", "LMP")][N == 1, momID] &
         BBirths == 2, .(ID, momID, BBirths, LMP)][order(momID, LMP)][, diff(LMP), by = "momID"][order(momID)] 

# if the difference is 1 day is fine
problematicmomID <- bideid[momID %in% bideid[BBirths == 2, .N, by = c("momID", "LMP")][N == 1, momID] &
                             BBirths == 2, .(ID, momID, BBirths, LMP)][order(momID, LMP)][, diff(LMP), by = "momID"][V1 > 1, momID] 
problematicID <- bideid[momID %in% problematicmomID & BBirths == 2, 
                        .(ID, momID, gestation, birth_weight, BSEX,
                          gestation_week, gestation_day, GA_days,
                          LMP, bwzfenton, bwpctilefenton)][order(momID, LMP)]
problematicID[, diff(LMP), by = "momID"]
rep(problematicID[, diff(LMP), by = "momID"][, V1], each = 2)
problematicID[, diffindays := rep(problematicID[, diff(LMP), by = "momID"][, V1], each = 2)]

problems[[4]] <- problematicID
names(problems)[4] <- "dataframe with kids who should be twins but their LMPs differ of > 1"
########################################

names(problems) 
# let's create a csv file for Heather with the IDs to check

problems_csv <- bideid[abs(bwzfenton) > 5, .(ID, momID, gestation, birth_weight, BSEX,
                                             gestation_week, gestation_day, GA_days, 
                                             LMP, bwzfenton, bwpctilefenton)]
problems_csv[, diffindays := NA]
problems_csv[, problem := "outliers (abs(bwzfenton) > 5) for birth weight Fenton z-score"]

problems_csv2 <- bideid[momID %in% problematicmomID & BBirths == 2,
                        .(ID, momID, gestation, birth_weight, BSEX,
                          gestation_week, gestation_day, GA_days,
                          LMP, bwzfenton, bwpctilefenton)][order(momID, LMP)]
problems_csv2[, diffindays := rep(problems_csv2[, diff(LMP), by = "momID"][, V1], each = 2)]
problems_csv2[, problem := "dataframe with kids who should be twins but their LMPs differ of > 1"]

problems_csv <- rbind(problems_csv, problems_csv2)

# let's save this dataset and send it to Heather so that she can check it!
# write.table(problems_csv, file = paste0("data/datacleaning_check_", Sys.Date(), ".csv"), sep = ",",
#             row.names = FALSE)

# check GUID vs x and y
# let's check only for the one who have exposure
names(bideid)

# import table linking subjectID with gridID
sidlinkpm <- readRDS("data/FromItai/cases_aodguid_2016-03-18.rds")
# look at exposure link table
class(sidlinkpm)
dim(sidlinkpm)
sidlinkpm[1:2,]

setdiff(sidlinkpm[, ID], intersect(bideid[, ID], sidlinkpm[, ID])) # there are 48 IDs that are in bideid 
# but they aren't in sidlinkpm 
setdiff(bideid[, ID], intersect(bideid[, ID], sidlinkpm[, ID])) # there are 64 IDs that have GUID 
# but they aren't in bideid 
bideid[ID %in% setdiff(bideid[, ID], intersect(bideid[, ID], sidlinkpm[, ID]))]

sidlinkpm[, distcentroid := sqrt((X - long_aod)^2 + (Y - lat_aod)^2)]
head(sidlinkpm)
sidlinkpm[, describe(distcentroid)]

# where are these kids?
backgroundmap <- get_map("Boston, MA", zoom = 7)
ggmap(backgroundmap, extent = "normal", darken = c(0.5, "white")) + 
  geom_point(aes(x = X, y = Y, col = distcentroid), alpha = 1, data = sidlinkpm[distcentroid > 0.01]) +
  theme_bw()
# 0.01 decimal degrees should be ~ 800 m
sidlinkpm[distcentroid > 0.01, .N] # 590 IDs

backgroundmap <- get_map("Boston, MA", zoom = 8)
ggmap(backgroundmap, extent = "normal", darken = c(0.5, "white")) + 
  geom_point(aes(x = X, y = Y, col = distcentroid), alpha = 1, data = sidlinkpm[distcentroid > 0.05]) +
  theme_bw()
sidlinkpm[distcentroid > 0.05, .N] # 159 IDs

backgroundmap <- get_map("Boston, MA", zoom = 8)
ggmap(backgroundmap, extent = "normal", darken = c(0.5, "white")) + 
  geom_segment(aes(x = long_aod, xend = X, y = lat_aod, yend = Y, col = distcentroid), alpha = 1, data = sidlinkpm[distcentroid > 0.05]) +
  theme_bw() +
  ggtitle("Distances > 0.05 decimal degrees (~ 4 km)\nbetween 159 IDs and their centroids")
# ggsave(filename = paste0("figures/159dist_IDcoord_centroid_", Sys.Date(), ".png"), width = 6, height = 6)

# end file 