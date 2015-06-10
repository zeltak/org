library(raster) 
library(maptools) 
library(rgdal)


roads.v <- readShapeSpatial("/media/NAS/Uni/Data/GIS/Europe/france/railway/railwgs84.shp")
plot(roads.v)
summary(roads.v)   


LU.r <- raster("/home/zeltak/ZH_tmp/tras1.tif")
LU.r
plot(LU.r)
lines(roads.v)
r <- LU.r  # this will be the template
r[] <- NA  # assigns all values as NA
summary(r) # shows you what you have: all NA's

roads.r <- rasterize(roads.v, r, field=1)
summary(roads.r)          # pixels crossed by a road have "1" 
plot(roads.r, add=TRUE)


roaddist.r <- distance(roads.r)
class(roaddist.r)
# Check:
plot(roaddist.r)
lines(roads.v)
