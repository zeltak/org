# ---------------------------------------------------------------------------
# cn018.py
# Created on: 2011-05-02 10:36:36.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Check out any necessary licenses
arcpy.CheckOutExtension("spatial")

for i in range(33, 365):
# Local variables:
  a2003_001_shp__2_ = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\point_utm\\a2003_%03d.shp"% i
  a2003_001 = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\wind_krig\\a2003_%03d"% i
  a2003_001_shp = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join1\\a2003_%03d.shp"% i

# Process: Extract Values to Points
  arcpy.gp.ExtractValuesToPoints_sa(a2003_001_shp__2_, a2003_001, a2003_001_shp, "NONE", "VALUE_ONLY")

