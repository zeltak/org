
libname mods 'P:\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2011 (keep= date station tempc)
            DATAFILE= "P:\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2011
            DATAFILE= "P:\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2011; by station  dist;

data stn_closest_XXYY_2011; set stn_closest_XXYY_2011; by station  dist;
if first.station;
run;

proc sort data = m1_met_2011; by station   ;run;
proc sort data = stn_closest_XXYY_2011 ; by station ;run;

data m1_met_2011;
merge m1_met_2011(in=a) stn_closest_XXYY_2011 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2011 (drop= xx yy);
set m1_met_2011;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2011 left join mods.Mod3_2011fs_pred
     on m1_met_2011.glong = Mod3_2011fs_pred.glong and m1_met_2011.glat = Mod3_2011fs_pred.glat and  m1_met_2011.date = Mod3_2011fs_pred.date;
run;

