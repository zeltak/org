#create matirx and convert numeric ID to character ID
#met2003
# all.metm <- makepointsmatrix(met2003, "long_met", "lat_met", "stn")
# met2003[, day := date]
# met2003[, stnc := as.character(stn)]
# met2003[, stn := NULL]
# setnames(met2003,"stnc", "stn")
# str(all.metm)
# 
# #grid
# aodidm <- makepointsmatrix(am2.lu.nd.pb, "long_aod", "lat_aod", "guid")
# m2g<-copy(am2.lu.nd.pb)
# m2g[, guidc := as.character(guid)]
# m2g[, guid := NULL]
# m2g[, day := date]
# setnames(m2g,"guidc", "guid")
# 
# #9:11-10:11 takes 1 hour
# # use the nearestbyday() function to spatial merge
# closesttemp <- nearestbyday(aodidm , all.metm, 
#                             m2g, met2003[,c("day", "stn", "tempc"), with = F], 
#                             "guid", "stn", "closesttemp", "tempc", knearest = 2)
# 


#closesttempknn-which nearest neigbour rdid it take
#closesttempnobs-number of obsv that were avilable