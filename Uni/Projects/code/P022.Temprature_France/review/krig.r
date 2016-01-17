library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)
library(gstat)
library(automap)
library(magrittr)

lct = Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)

#setwd("/home/michael/Dropbox/BGU/Itai/daily_kriging")

dat <- readRDS("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099_review/mod2005.rds")
dat = as.data.frame(dat)
# dat$date %<>% tolower %>% as.Date("%d%b%Y")
head(dat)
coordinates(dat) = ~ Longitude + Latitude
#define cord system- longlat is WGS84 unprojected
proj4string(dat) = "+proj=longlat"
# for the loop to null final
final = NULL

for(i in unique(dat$date)) {

  dat1 = dat[dat$date == i, ]
  dat1 = dat1[, c("tm", "elev_m", "pcturb", "NDVI")]
  dat1 = dat1[complete.cases(dat1@data), ]
  
  v = autofitVariogram(tm ~ elev_m + pcturb + NDVI, dat1)
  g = gstat(formula = tm ~ elev_m + pcturb + NDVI, model = v$var_model, data = dat1)
  # use CV LOOCV
  cv = gstat.cv(g)

  result = data.frame(
#     lon = coordinates(dat1)[, 1],
#     lat = coordinates(dat1)[, 2],
    date = i,
    obs = cv$observed,
    pred = cv$var1.pred,
    num_insee=dat1$num_insee
      )
  
  final = rbind(final, result)
  
}

saveRDS(final, "cv_result.rds")
