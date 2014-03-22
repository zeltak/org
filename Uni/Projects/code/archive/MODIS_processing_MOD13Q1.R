library(rgdal)
library(raster)
library(rgeos)
# Obtain the MODIS tool from: http://lpdaac.usgs.gov/landdaac/tools/modis/index.asp

###################################################################################################

MRT = "C:\\MRT\\bin\\"
workd = "C:\\MOD13Q1\\"
temp_workd = "C:\\TEMP\\"

setwd(temp_workd)
files_list = list.files(workd)

for(i in 162:length(files_list))# / 2))
  {
  #BLOCK1 = files_list[i*2-1]
  #BLOCK2 = files_list[i*2]
  
  # mosaic the blocks:
  #mosaicname = file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")
  #write(paste(workd, BLOCK1, sep=""), mosaicname)
  #write(paste(workd, BLOCK2, sep=""), mosaicname, append=T)
  #close(mosaicname)
  # generate temporary mosaic:
  #shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "0 1 0 0 0 0 0 0 0 0 0" -o ', workd, 'TmpMosaic.hdf', sep=""))
  #shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -o ', temp_workd, 'TmpMosaic.hdf', sep=""))
  
  # resample to UTM:
  filename = file(paste(MRT, "mrt", ".prm", sep=""), open="wt")
  write(paste('INPUT_FILENAME = ', workd, files_list[i], sep=""), filename) 
  write('  ', filename, append=TRUE) 
  #write('SPECTRAL_SUBSET = ( 1 1 1 )', filename, append=TRUE)
  #write('  ', filename, append=TRUE)
  write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('SPATIAL_SUBSET_UL_CORNER = ( 570000.0 3750000.0 )', filename, append=TRUE)
  write('SPATIAL_SUBSET_LR_CORNER = ( 820000.0 3330000.0 )', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write(paste('OUTPUT_FILENAME = ', temp_workd, 'tmp', i, '.tif', sep=""), filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('RESAMPLING_TYPE = NEAREST_NEIGHBOR', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('OUTPUT_PROJECTION_TYPE = UTM', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  #write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE)
  #write(' 0.0 0.0 50.0', filename, append=TRUE)
  #write(' 58.5 -126.0 45.0', filename, append=TRUE)
  #write(' 1000000.0 0.0 0.0', filename, append=TRUE)
  #write(' 0.0 0.0 0.0', filename, append=TRUE)
  #write(' 0.0 0.0 0.0 )', filename, append=TRUE)
  #write('  ', filename, append=TRUE)
  write('UTM_ZONE = 36', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('DATUM = WGS84', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('OUTPUT_PIXEL_SIZE = 250', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  close(filename)
  # Mosaic the images to get the whole area:
  shell(cmd=paste(MRT, 'resample -p ', MRT, "mrt", ".prm", sep=""))
  
  r = raster(paste("C:\\TEMP\\tmp", i, ".250m_16_days_NDVI.tif", sep = ""))
  r = r * 0.0001
  pr = raster(paste("C:\\TEMP\\tmp", i, ".250m_16_days_pixel_reliability.tif", sep = ""))
  pr[][pr[]!=0 & pr[]!=1] = NA
  r = mask(r, pr)
  writeRaster(r, paste0("C:\\MODIS\\MOD13Q1_NDVI_",i,".tif"), format = "GTiff", overwrite = TRUE)
  }

#####
setwd("C:\\MODIS\\")
files = list.files("C:\\MODIS\\")
r = stack(files)

# 23 images per year, date of image in filename = composite images were taken during 16 days up to that date
# start = c(2000, 3)
writeRaster(r, "F:\\MODIS\\MOD13Q1_NDVI.tif", format = "GTiff", overwrite = FALSE)

#####
forest = readOGR("F:\\Original Layers\\KKL_ForestsStands2011_Layer\\KKL_ForsetsStands2011_UTM_1105.shp",
                 layer = "KKL_ForsetsStands2011_UTM_1105")

yatir = forest[forest@data$F_H_P==43060010101,]
yatir = gCentroid(yatir)
yatir = array(extract(result, yatir))
gilboa = forest[forest@data$F_H_P==14760012203,]
gilboa = gCentroid(gilboa)
gilboa = array(extract(result, gilboa))
birya = forest[forest@data$F_H_P==12030030105,]
birya = gCentroid(birya)
birya = array(extract(result, birya))

lahav = forest[forest@data$F_H_P==41280009101,]
lahav = gCentroid(lahav)
lahav = array(extract(result, lahav))

#dat = data.frame(yatir = yatir, gilboa = gilboa, birya = birya)

ts1 = ts(lahav, frequency = 23, start = c(2000, 3))
plot(ts1)

#####
library(raster)
library(rgeos)
library(bfast)

r = brick("F:\\MODIS\\MOD13Q1_NDVI.tif")
#r = overlay(r, fun = function(x) mean(x, na.rm=TRUE), progress = "text")
#writeRaster(r, "F:\\MODIS\\MOD13Q1_NDVI_mean.tif", format = "GTiff", overwrite = FALSE)
#r1 = crop(r[[1]], extent(forest))
#pol = rasterToPolygons(r1, na.rm = FALSE)
#writeOGR(pol, "F:\\R Results\\MODIS", "pol_MODIS", driver="ESRI Shapefile")

pol = readOGR("F:\\R Results\\MODIS",
                 layer = "pol_MODIS_1105")
forest = readOGR("F:\\Original Layers\\KKL_ForestsStands2011_Layer\\KKL_ForsetsStands2011_UTM_1105.shp",
                 layer = "KKL_ForsetsStands2011_UTM_1105")
pol$FOR_NO = over(gCentroid(pol, byid = TRUE), forest)$FOR_NO

pol = pol[pol$FOR_NO %in% c(4102,4128), ]

dat = extract(r, gCentroid(pol, byid = TRUE))

ts1 = ts(dat[5,], frequency = 23, start = c(2000, 3))
#plot(ts1)
#ts1 = window(ts1, c(2000,1),c(2011,12))
ts1 = stl(ts1, s.window = "periodic", 
	s.degree = 1, 
	l.degree = 0, 
	#t.window = 5,
	na.action = na.contiguous)
plot(ts1)

