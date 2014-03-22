 ## 1. Extract each raster from HDF files
import arcpy
# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")
from arcpy import env



for i in (0,):
    # Set the current workspace (where the hdf files are located)
    env.workspace = "f:\\wtmp\hdf\\"

    # Get a list of HDF files from the workspace using
    #an internal GIS function "listRasters"
    rasterList = arcpy.ListRasters("*", "hdf")

    # Extract rasters from HDF files in the workspace
        # Change "0" to the hdf layer needed
    for raster in rasterList:
        output = "f:\\wtmp\\hdf\\output\\layer" + str(i) + "\\" + str(raster)[:-4] + ".tif"
        layer=str(i)
        arcpy.ExtractSubDataset_management(raster, output, layer)
    print "Layer" + str(i) +": 1. Extractining rasters - Finished"



    ## 2. Mosaic each raster to one large dataset
    # glob (from iglob lib) is used to grab part of the filename (string). here we take the date part below
    from glob import iglob

    #set where the tif files are located
    env.workspace ="f:\\wtmp\\hdf\\output\\layer" + str(i)


    # Get a list of dates from file names
    DateList = list()#creates an empty list for date
    rasterList = arcpy.ListRasters("*", "tif") #list of all tif raster files
    for raster in rasterList:
        date = raster[1:8]
        DateList.append(date)

    # Remove redundant dates, make unique
    UniqueDateList = list(set(DateList))
    # Sort the list
    SortedUniqueDateList = sorted(UniqueDateList)

    # Do not use the TIFF format as a mosaic output (outname). It'll lose values.
    #   ESRI GRID is preferrable, default (no extension designation)

    #this will have a loop inside a loop! that is first the loop looks for all tiles in the same date and then the 2nd loop
    #will moasiac all these tiles. then the next date is proccesed.

    for date in SortedUniqueDateList:
        output = "f:\\wtmp\\hdf\\output\\mosaic"
        outname = "m_" + str(date) + "_" + str(i)
        inputs = ''
        for fn in iglob("f:\\wtmp\\hdf\\output\\layer" + str(i) +"\\*" +str(date) + "*.tif"):
            inputs += str(fn) + ';'
        arcpy.MosaicToNewRaster_management(inputs, output, outname, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]", "8_BIT_UNSIGNED", "", "1", "LAST", "FIRST")

        #Clipping
        inputras = "F:\\wtmp\\hdf\\output\\mosaic\\m_" + str(date) + "_" + str(i)
        clipshp = "F:\\Uni\\Projects\\P020_Temprature_ITALY\\3.Work\\2.Gather_data\\$GIS Repo\\clip.shp"
        outras = "F:\\wtmp\\hdf\\output\\clip\\m_" + str(date) + "_" + str(i)

        # Process: Extract by Mask
        arcpy.gp.ExtractByMask_sa(inputras, clipshp, outras)
    print "Layer" + str(i) +": 2. Mosaic & Clip - Finished"

 #3. Extract XYZ values to .DBF tables

env.workspace = "F:\\wtmp\\hdf\\output\\clip\\"

rasterList = arcpy.ListRasters("*", "GRID")
for raster in rasterList:
    tmptbl = "f:\\wtmp\\hdf\\output\\tbl" + str(raster)
    Outlocation = "f:\\wtmp\\hdf\\output\\xytable"

    # Process: Sample
    arcpy.gp.Sample_sa(raster, raster, tmptbl, "NEAREST")
    # Process: Table to dBASE (multiple)
    arcpy.TableToDBASE_conversion(tmptbl, Outlocation)

print "End of Layer of " +str(i)




print "REAL End!!"
