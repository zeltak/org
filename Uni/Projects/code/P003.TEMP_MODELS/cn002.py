# ---------------------------------------------------------------------------
# cn002.py
# Created on: 2011-04-25 13:31:26.00000
#   (generated by ArcGIS/ModelBuilder)
# Description:
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Local variables:
a2003_002 = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.1.Raw_data\\MODIS_TEMP\\Night only\\a2003_002.hdf"
a2003_002_tif = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\hdf_2_tiff\\a2003_002.tif"

arcpy.ExtractSubDataset_management(a2003_002, a2003_002_tif, "0")

