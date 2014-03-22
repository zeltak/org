# ---------------------------------------------------------------------------
# $tmp.py
# Created on: 2011-05-11 09:08:47.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

for i in range(1, 32):
# Local variables:
  a2003_001_shp = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_%03d.shp" % i
  a2003_001_shp__2_ = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp" 
  a2003_001_SpatialJoin2 = "C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join5\\a2003_%03d.shp" % i

  try:
# Process: Spatial Join
    arcpy.SpatialJoin_analysis(a2003_001_shp, a2003_001_shp__2_, a2003_001_SpatialJoin2, "JOIN_ONE_TO_ONE", "KEEP_ALL", "Join_Count \"Join_Count\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Join_Count,-1,-1;TARGET_FID \"TARGET_FID\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TARGET_FID,-1,-1;Join_Cou_1 \"Join_Cou_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Join_Cou_1,-1,-1;TARGET_F_1 \"TARGET_F_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TARGET_F_1,-1,-1;Join_Cou_2 \"Join_Cou_2\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Join_Cou_2,-1,-1;dist_m_emi \"dist_m_emi\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,dist_m_emi,-1,-1;TARGET_F_2 \"TARGET_F_2\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TARGET_F_2,-1,-1;Join_Cou_3 \"Join_Cou_3\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Join_Cou_3,-1,-1;dist_met \"dist_met\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,dist_met,-1,-1;TARGET_F_3 \"TARGET_F_3\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TARGET_F_3,-1,-1;SID \"SID\" true true false 8 Text 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,SID,-1,-1;DATE \"DATE\" true true false 8 Date 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,DATE,-1,-1;TMEAN \"TMEAN\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TMEAN,-1,-1;WMEAN \"WMEAN\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,WMEAN,-1,-1;TMIN \"TMIN\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,TMIN,-1,-1;X \"X\" true true false 15 Double 5 14 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,X,-1,-1;Y \"Y\" true true false 15 Double 5 14 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Y,-1,-1;D \"D\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,D,-1,-1;M \"M\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,M,-1,-1;C \"C\" true true false 8 Long 0 8 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,C,-1,-1;ORIG_FID \"ORIG_FID\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,ORIG_FID,-1,-1;RASTERVALU \"RASTERVALU\" true true false 19 Double 8 18 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,RASTERVALU,-1,-1;POINTID \"POINTID\" true true false 6 Long 0 6 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,POINTID,-1,-1;GRID_CODE \"GRID_CODE\" true true false 10 Double 0 10 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,GRID_CODE,-1,-1;POINTID_1 \"POINTID_1\" true true false 6 Long 0 6 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,POINTID_1,-1,-1;GRID_CODE_ \"GRID_CODE_\" true true false 10 Double 0 10 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,GRID_CODE_,-1,-1;pointid_12 \"pointid_12\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,pointid_12,-1,-1;elevation \"elevation\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,elevation,-1,-1;OBJECTID \"OBJECTID\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,OBJECTID,-1,-1;Id \"Id\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Id,-1,-1;X_1 \"X_1\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,X_1,-1,-1;Y_1 \"Y_1\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Y_1,-1,-1;guid \"guid\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,guid,-1,-1;Shape_Leng \"Shape_Leng\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Shape_Leng,-1,-1;Shape_Area \"Shape_Area\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,Shape_Area,-1,-1;OID_ \"OID_\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,OID_,-1,-1;GRID_RASTE \"GRID_RASTE\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,GRID_RASTE,-1,-1;COUNT_SUM \"COUNT_SUM\" true true false 8 Float 2 7 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,COUNT_SUM,-1,-1;LU3_SUM \"LU3_SUM\" true true false 8 Float 2 7 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,LU3_SUM,-1,-1;PER_URBAN \"PER_URBAN\" true true false 8 Float 2 7 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\Jmod2_Join4\\a2003_001.shp,PER_URBAN,-1,-1;Join_Count_1 \"Join_Count_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,Join_Count,-1,-1;TARGET_FID_1 \"TARGET_FID_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,TARGET_FID,-1,-1;POINTID_12_13 \"POINTID_12_13\" true true false 6 Long 0 6 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,POINTID,-1,-1;GRID_CODE_1 \"GRID_CODE_1\" true true false 10 Double 0 10 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,GRID_CODE,-1,-1;OBJECTID_1 \"OBJECTID_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,OBJECTID,-1,-1;Id_1 \"Id_1\" true true false 9 Long 0 9 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,Id,-1,-1;X_12 \"X_12\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,X,-1,-1;Y_12 \"Y_12\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,Y,-1,-1;guid_1 \"guid_1\" true true false 19 Double 0 0 ,First,#,C:\\Users\\ekloog\\Documents\\$Doc\\3.PostDoc\\3.1.Projetcs\\3.1.3.TEMP_MODELS\\3.1.1.4.Work\\2.Gather_data\\NVDI_TIFF_6_guid\\a2003_001.shp,guid,-1,-1", "CLOSEST", "", "dist_ndvi")
  except arcpy.ExecuteError:
    pass

