# ---------------------------------------------------------------------------
# ndv1.py
# Created on: 2011-05-02 16:56:50.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Load required toolboxes
arcpy.ImportToolbox("C:/Users/ekloog/AppData/Roaming/ESRI/Desktop10.0/ArcToolbox/My Toolboxes/$etai.tbx")


# Local variables:
MOD_month01_hdf = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.1.Raw_data\\MODIS_NDVI\\MOD_month01.hdf"
a2003_001_tif = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF\\a2003_001.tif"

# Process: Extract Subdataset
arcpy.gp.toolbox = "C:/Users/ekloog/AppData/Roaming/ESRI/Desktop10.0/ArcToolbox/My Toolboxes/$etai.tbx";
# Warning: the toolbox C:/Users/ekloog/AppData/Roaming/ESRI/Desktop10.0/ArcToolbox/My Toolboxes/$etai.tbx DOES NOT have an alias. 
# Please assign this toolbox an alias to avoid tool name collisions
# And replace arcpy.gp.ExtractSubDataset(...) with arcpy.ExtractSubDataset_ALIAS(...)
arcpy.gp.ExtractSubDataset(MOD_month01_hdf, a2003_001_tif, "0")

