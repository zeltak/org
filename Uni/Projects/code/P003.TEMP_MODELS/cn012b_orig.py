# Import arcpy module
import arcpy

# Local variables:
a2003_001_dbf = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\daily_stmp\\a2003_001.dbf"
a2003_001_Layer = "a2003_001_Layer"
a2003_001_shp = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\metdbf_shp\\a2003_001.shp"
a2003_001_Project = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\metshp_utm_proj\\a2003_001.shp"

# Process: Make XY Event Layer
arcpy.MakeXYEventLayer_management(a2003_001_dbf, "X", "y", a2003_001_Layer, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119522E-09;0.001;0.001;IsHighPrecision", "")

# Process: Feature To Point
arcpy.FeatureToPoint_management(a2003_001_Layer, a2003_001_shp, "CENTROID")

# Process: Project
arcpy.Project_management(a2003_001_shp, a2003_001_Project, "PROJCS['WGS_1984_UTM_Zone_19N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-69.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]", "", "PROJCS['WGS_1984_UTM_Zone_19N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-69.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")