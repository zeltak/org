library(rgdal)
library(raster)
library(rgeos)
# Obtain the MODIS tool from: http://lpdaac.usgs.gov/landdaac/tools/modis/index.asp

MRT = "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Soft_lib/Modis reprojection tool_MRT/MRT_download_Linux64/MRT/bin/"
workd = "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/ztmp/"
temp_workd = "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/ztmp/"

#forest = readOGR("F:\\Original Layers\\KKL_ForestsStands2011_Layer\\KKL_ForsetsStands2011_UTM_1105.shp",
  layer = "KKL_ForsetsStands2011_UTM_1105")

setwd(temp_workd)
#To get a list of all files in that directory
files_list = list.files(workd)

for(i in 1:(length(files_list) / 4))
  {
  BLOCK1 = files_list[i*4-1]
  BLOCK2 = files_list[i*4]

  # mosaic the blocks:
  mosaicname = file(paste(MRT, "TmpMosaic.prm", sep=""), open="wt")
  write(paste(workd, BLOCK1, sep=""), mosaicname)
  write(paste(workd, BLOCK2, sep=""), mosaicname, append=T)
  close(mosaicname)
  # generate temporary mosaic:
  ###NOTE: shell command only needed on windowz
  #shell(cmd=paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -s "0 1 0 0 0 0 0 0 0 0 0" -o ', workd, 'TmpMosaic.hdf', sep=""))
  paste(MRT, 'mrtmosaic -i ', MRT, 'TmpMosaic.prm -o ', temp_workd, 'TmpMosaic.hdf', sep="")
x
  
  # resample to UTM:
  filename = file(paste(MRT, "mrt", ".prm", sep=""), open="wt")
  write(paste('INPUT_FILENAME = ', temp_workd, 'TmpMosaic.hdf', sep=""), filename) 
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
  write('OUTPUT_PROJECTION_PARAMETERS = ( ', filename, append=TRUE)
  write(' 0.0 0.0 50.0', filename, append=TRUE)
  write(' 58.5 -126.0 45.0', filename, append=TRUE)
  write(' 1000000.0 0.0 0.0', filename, append=TRUE)
  write(' 0.0 0.0 0.0', filename, append=TRUE)
  write(' 0.0 0.0 0.0 )', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('UTM_ZONE = 36', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('DATUM = WGS84', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  write('OUTPUT_PIXEL_SIZE = 1000', filename, append=TRUE)
  write('  ', filename, append=TRUE)
  close(filename)
  # Mosaic the images to get the whole area:
  paste(MRT, 'resample -p ', MRT, "mrt", ".prm", sep="")
}  


  r = raster(paste("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/ztmp", i, ".1_km_16_days_NDVI.tif", sep = ""))
  r = r * 0.0001
  pr = raster(paste("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/ztmp/", i, ".1_km_16_days_pixel_reliability.tif", sep = ""))
  pr[][pr[]!=0 & pr[]!=1]=NA
  r = mask(r, pr)
  if(i==1) {result = r} else {result = stack(result, r)}
  }

# 23 images per year, date of image in filename = composite images were taken during 16 days up to that date
# start = c(2000, 3)
writeRaster(result, "F:\\MODIS\\MOD13A2_NDVI.img", format = "HFA", overwrite = TRUE)

yatir = forest[forest@data$F_H_P==43060010101,]
yatir = gCentroid(yatir)
yatir = array(extract(result, yatir))
gilboa = forest[forest@data$F_H_P==14760012203,]
gilboa = gCentroid(gilboa)
gilboa = array(extract(result, gilboa))
birya = forest[forest@data$F_H_P==12030030105,]
birya = gCentroid(birya)
birya = array(extract(result, birya))

dat = data.frame(yatir = yatir, gilboa = gilboa, birya = birya)

ts1 = ts(dat$yatir, frequency = 23, start = c(2000, 3))

