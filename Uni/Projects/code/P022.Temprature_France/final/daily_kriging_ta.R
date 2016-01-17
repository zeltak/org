library(gstat)
library(automap)
library(magrittr)

lct = Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)

setwd("/home/michael/Dropbox/BGU/Itai/daily_kriging")

dat = readRDS("mod2005.rds")
dat = as.data.frame(dat)
# dat$date %<>% tolower %>% as.Date("%d%b%Y")

coordinates(dat) = ~ Longitude + Latitude
proj4string(dat) = "+proj=longlat"

final = NULL

for(i in unique(dat$date)) {

  dat1 = dat[dat$date == i, ]
  dat1 = dat1[, c("tm", "elev_m", "pcturb", "NDVI")]
  dat1 = dat1[complete.cases(dat1@data), ]
  
  v = autofitVariogram(tm ~ elev_m + pcturb + NDVI, dat1)
  g = gstat(formula = tm ~ elev_m + pcturb + NDVI, model = v$var_model, data = dat1)
  cv = gstat.cv(g)

  result = data.frame(
    date = i,
    obs = cv$observed,
    pred = cv$var1.pred
  )
  
  final = rbind(final, result)
  
}

saveRDS(final, "cv_result.rds")

