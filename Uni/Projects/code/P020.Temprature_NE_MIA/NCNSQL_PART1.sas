

libname mods 'Z:\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2000*/


PROC IMPORT OUT= WORK.lst2000pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2000.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2000
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2000.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2000
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2000.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2000
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2000.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2000pre (drop=xx yy);
set lst2000pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2000 (drop=xx yy);
set ndvi2000;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2000prew  as
   select *
    from lst2000pre left join grid
     on lst2000pre.glong = grid.glong and  lst2000pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2000   ;
set lst2000prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2000  as
   select *
    from  lst2000 left join ndvi2000
     on lst2000.glong = ndvi2000.glong and lst2000.glat = ndvi2000.glat  and  lst2000.month = ndvi2000.month ;
run;

/*save mod2*/
data mods.mod2_2000;
set mod2_2000;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2000_s1  as
   select *
    from LST_within1km_stn left join mod2_2000
     on LST_within1km_stn.glong = mod2_2000.glong and LST_within1km_stn.glat = mod2_2000.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2000; by date station   ;run;
proc sort data = mod1_2000_s1 ; by date station ;run;

data  mod1_2000_s2;
merge  mod1_2000_s1(in=a) met2000 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2000_s2; by station date dist;
data mod1_2000_s2s; set mod1_2000_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2000 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2000_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2000  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2000;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2000;
    ods output  SolutionR =  SolutionR2000;
	run;


data check_s1;
 set work.Solutionr2000;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2000;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2000_v1;
 merge mods.mod2_2000 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2000 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2000_v4;
 MERGE Mod2_2000_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2000_v4 OUT = Mod2_2000_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2000_pred;
 set Mod2_2000_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2000_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2000_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;


/*2001*/


PROC IMPORT OUT= WORK.lst2001pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2001.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2001
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2001.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2001
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2001.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2001
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2001.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2001pre (drop=xx yy);
set lst2001pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2001 (drop=xx yy);
set ndvi2001;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2001prew  as
   select *
    from lst2001pre left join grid
     on lst2001pre.glong = grid.glong and  lst2001pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2001   ;
set lst2001prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2001  as
   select *
    from  lst2001 left join ndvi2001
     on lst2001.glong = ndvi2001.glong and lst2001.glat = ndvi2001.glat  and  lst2001.month = ndvi2001.month ;
run;

/*save mod2*/
data mods.mod2_2001;
set mod2_2001;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2001_s1  as
   select *
    from LST_within1km_stn left join mod2_2001
     on LST_within1km_stn.glong = mod2_2001.glong and LST_within1km_stn.glat = mod2_2001.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2001; by date station   ;run;
proc sort data = mod1_2001_s1 ; by date station ;run;

data  mod1_2001_s2;
merge  mod1_2001_s1(in=a) met2001 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2001_s2; by station date dist;
data mod1_2001_s2s; set mod1_2001_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2001 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2001_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2001  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2001;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2001;
    ods output  SolutionR =  SolutionR2001;
	run;


data check_s1;
 set work.Solutionr2001;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2001;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2001_v1;
 merge mods.mod2_2001 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2001 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2001_v4;
 MERGE Mod2_2001_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2001_v4 OUT = Mod2_2001_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2001_pred;
 set Mod2_2001_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2001_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2001_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2002*/


PROC IMPORT OUT= WORK.lst2002pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2002.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2002
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2002.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2002
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2002.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2002
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2002.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2002pre (drop=xx yy);
set lst2002pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2002 (drop=xx yy);
set ndvi2002;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2002prew  as
   select *
    from lst2002pre left join grid
     on lst2002pre.glong = grid.glong and  lst2002pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2002   ;
set lst2002prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2002  as
   select *
    from  lst2002 left join ndvi2002
     on lst2002.glong = ndvi2002.glong and lst2002.glat = ndvi2002.glat  and  lst2002.month = ndvi2002.month ;
run;

/*save mod2*/
data mods.mod2_2002;
set mod2_2002;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2002_s1  as
   select *
    from LST_within1km_stn left join mod2_2002
     on LST_within1km_stn.glong = mod2_2002.glong and LST_within1km_stn.glat = mod2_2002.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2002; by date station   ;run;
proc sort data = mod1_2002_s1 ; by date station ;run;

data  mod1_2002_s2;
merge  mod1_2002_s1(in=a) met2002 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2002_s2; by station date dist;
data mod1_2002_s2s; set mod1_2002_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2002 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2002_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2002  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2002;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2002;
    ods output  SolutionR =  SolutionR2002;
	run;


data check_s1;
 set work.Solutionr2002;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2002;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2002_v1;
 merge mods.mod2_2002 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2002 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2002_v4;
 MERGE Mod2_2002_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2002_v4 OUT = Mod2_2002_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2002_pred;
 set Mod2_2002_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2002_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2002_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2003*/


PROC IMPORT OUT= WORK.lst2003pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2003.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2003.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2003.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2003.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2003pre (drop=xx yy);
set lst2003pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2003 (drop=xx yy);
set ndvi2003;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2003prew  as
   select *
    from lst2003pre left join grid
     on lst2003pre.glong = grid.glong and  lst2003pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2003   ;
set lst2003prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2003  as
   select *
    from  lst2003 left join ndvi2003
     on lst2003.glong = ndvi2003.glong and lst2003.glat = ndvi2003.glat  and  lst2003.month = ndvi2003.month ;
run;

/*save mod2*/
data mods.mod2_2003;
set mod2_2003;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2003_s1  as
   select *
    from LST_within1km_stn left join mod2_2003
     on LST_within1km_stn.glong = mod2_2003.glong and LST_within1km_stn.glat = mod2_2003.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2003; by date station   ;run;
proc sort data = mod1_2003_s1 ; by date station ;run;

data  mod1_2003_s2;
merge  mod1_2003_s1(in=a) met2003 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2003_s2; by station date dist;
data mod1_2003_s2s; set mod1_2003_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2003 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2003_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2003  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2003;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2003;
    ods output  SolutionR =  SolutionR2003;
	run;


data check_s1;
 set work.Solutionr2003;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2003;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2003_v1;
 merge mods.mod2_2003 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2003 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2003_v4;
 MERGE Mod2_2003_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2003_v4 OUT = Mod2_2003_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2003_pred;
 set Mod2_2003_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2003_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2003_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2004*/


PROC IMPORT OUT= WORK.lst2004pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2004.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2004
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2004.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2004
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2004.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2004
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2004.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2004pre (drop=xx yy);
set lst2004pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2004 (drop=xx yy);
set ndvi2004;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2004prew  as
   select *
    from lst2004pre left join grid
     on lst2004pre.glong = grid.glong and  lst2004pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2004   ;
set lst2004prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2004  as
   select *
    from  lst2004 left join ndvi2004
     on lst2004.glong = ndvi2004.glong and lst2004.glat = ndvi2004.glat  and  lst2004.month = ndvi2004.month ;
run;

/*save mod2*/
data mods.mod2_2004;
set mod2_2004;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2004_s1  as
   select *
    from LST_within1km_stn left join mod2_2004
     on LST_within1km_stn.glong = mod2_2004.glong and LST_within1km_stn.glat = mod2_2004.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2004; by date station   ;run;
proc sort data = mod1_2004_s1 ; by date station ;run;

data  mod1_2004_s2;
merge  mod1_2004_s1(in=a) met2004 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2004_s2; by station date dist;
data mod1_2004_s2s; set mod1_2004_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2004 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2004_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2004  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2004;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2004;
    ods output  SolutionR =  SolutionR2004;
	run;


data check_s1;
 set work.Solutionr2004;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2004;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2004_v1;
 merge mods.mod2_2004 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2004 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2004_v4;
 MERGE Mod2_2004_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2004_v4 OUT = Mod2_2004_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2004_pred;
 set Mod2_2004_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2004_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2004_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2005*/


PROC IMPORT OUT= WORK.lst2005pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2005.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2005.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2005.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2005.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2005pre (drop=xx yy);
set lst2005pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2005 (drop=xx yy);
set ndvi2005;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2005prew  as
   select *
    from lst2005pre left join grid
     on lst2005pre.glong = grid.glong and  lst2005pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2005   ;
set lst2005prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2005  as
   select *
    from  lst2005 left join ndvi2005
     on lst2005.glong = ndvi2005.glong and lst2005.glat = ndvi2005.glat  and  lst2005.month = ndvi2005.month ;
run;

/*save mod2*/
data mods.mod2_2005;
set mod2_2005;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2005_s1  as
   select *
    from LST_within1km_stn left join mod2_2005
     on LST_within1km_stn.glong = mod2_2005.glong and LST_within1km_stn.glat = mod2_2005.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2005; by date station   ;run;
proc sort data = mod1_2005_s1 ; by date station ;run;

data  mod1_2005_s2;
merge  mod1_2005_s1(in=a) met2005 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2005_s2; by station date dist;
data mod1_2005_s2s; set mod1_2005_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2005 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2005_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2005  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2005;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2005;
    ods output  SolutionR =  SolutionR2005;
	run;


data check_s1;
 set work.Solutionr2005;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2005;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2005_v1;
 merge mods.mod2_2005 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2005 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2005_v4;
 MERGE Mod2_2005_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2005_v4 OUT = Mod2_2005_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2005_pred;
 set Mod2_2005_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2005_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2005_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2006*/


PROC IMPORT OUT= WORK.lst2006pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2006.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2006.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2006.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2006.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2006pre (drop=xx yy);
set lst2006pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2006 (drop=xx yy);
set ndvi2006;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2006prew  as
   select *
    from lst2006pre left join grid
     on lst2006pre.glong = grid.glong and  lst2006pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2006   ;
set lst2006prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2006  as
   select *
    from  lst2006 left join ndvi2006
     on lst2006.glong = ndvi2006.glong and lst2006.glat = ndvi2006.glat  and  lst2006.month = ndvi2006.month ;
run;

/*save mod2*/
data mods.mod2_2006;
set mod2_2006;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2006_s1  as
   select *
    from LST_within1km_stn left join mod2_2006
     on LST_within1km_stn.glong = mod2_2006.glong and LST_within1km_stn.glat = mod2_2006.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2006; by date station   ;run;
proc sort data = mod1_2006_s1 ; by date station ;run;

data  mod1_2006_s2;
merge  mod1_2006_s1(in=a) met2006 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2006_s2; by station date dist;
data mod1_2006_s2s; set mod1_2006_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2006 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2006_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2006  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2006;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2006;
    ods output  SolutionR =  SolutionR2006;
	run;


data check_s1;
 set work.Solutionr2006;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2006;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2006_v1;
 merge mods.mod2_2006 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2006 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2006_v4;
 MERGE Mod2_2006_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2006_v4 OUT = Mod2_2006_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2006_pred;
 set Mod2_2006_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2006_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2006_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2007*/


PROC IMPORT OUT= WORK.lst2007pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2007.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2007.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2007.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2007.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2007pre (drop=xx yy);
set lst2007pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2007 (drop=xx yy);
set ndvi2007;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2007prew  as
   select *
    from lst2007pre left join grid
     on lst2007pre.glong = grid.glong and  lst2007pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2007   ;
set lst2007prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2007  as
   select *
    from  lst2007 left join ndvi2007
     on lst2007.glong = ndvi2007.glong and lst2007.glat = ndvi2007.glat  and  lst2007.month = ndvi2007.month ;
run;

/*save mod2*/
data mods.mod2_2007;
set mod2_2007;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2007_s1  as
   select *
    from LST_within1km_stn left join mod2_2007
     on LST_within1km_stn.glong = mod2_2007.glong and LST_within1km_stn.glat = mod2_2007.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2007; by date station   ;run;
proc sort data = mod1_2007_s1 ; by date station ;run;

data  mod1_2007_s2;
merge  mod1_2007_s1(in=a) met2007 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2007_s2; by station date dist;
data mod1_2007_s2s; set mod1_2007_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2007 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2007_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2007  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2007;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2007;
    ods output  SolutionR =  SolutionR2007;
	run;


data check_s1;
 set work.Solutionr2007;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2007;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2007_v1;
 merge mods.mod2_2007 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2007 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2007_v4;
 MERGE Mod2_2007_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2007_v4 OUT = Mod2_2007_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2007_pred;
 set Mod2_2007_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2007_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2007_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2008*/


PROC IMPORT OUT= WORK.lst2008pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2008.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2008.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2008.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2008.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2008pre (drop=xx yy);
set lst2008pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2008 (drop=xx yy);
set ndvi2008;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2008prew  as
   select *
    from lst2008pre left join grid
     on lst2008pre.glong = grid.glong and  lst2008pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2008   ;
set lst2008prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2008  as
   select *
    from  lst2008 left join ndvi2008
     on lst2008.glong = ndvi2008.glong and lst2008.glat = ndvi2008.glat  and  lst2008.month = ndvi2008.month ;
run;

/*save mod2*/
data mods.mod2_2008;
set mod2_2008;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2008_s1  as
   select *
    from LST_within1km_stn left join mod2_2008
     on LST_within1km_stn.glong = mod2_2008.glong and LST_within1km_stn.glat = mod2_2008.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2008; by date station   ;run;
proc sort data = mod1_2008_s1 ; by date station ;run;

data  mod1_2008_s2;
merge  mod1_2008_s1(in=a) met2008 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2008_s2; by station date dist;
data mod1_2008_s2s; set mod1_2008_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2008 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2008_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2008  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2008;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2008;
    ods output  SolutionR =  SolutionR2008;
	run;


data check_s1;
 set work.Solutionr2008;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2008;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2008_v1;
 merge mods.mod2_2008 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2008 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2008_v4;
 MERGE Mod2_2008_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2008_v4 OUT = Mod2_2008_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2008_pred;
 set Mod2_2008_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2008_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2008_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2009*/


PROC IMPORT OUT= WORK.lst2009pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2009.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2009.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2009.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2009.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2009pre (drop=xx yy);
set lst2009pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2009 (drop=xx yy);
set ndvi2009;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2009prew  as
   select *
    from lst2009pre left join grid
     on lst2009pre.glong = grid.glong and  lst2009pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2009   ;
set lst2009prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2009  as
   select *
    from  lst2009 left join ndvi2009
     on lst2009.glong = ndvi2009.glong and lst2009.glat = ndvi2009.glat  and  lst2009.month = ndvi2009.month ;
run;

/*save mod2*/
data mods.mod2_2009;
set mod2_2009;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2009_s1  as
   select *
    from LST_within1km_stn left join mod2_2009
     on LST_within1km_stn.glong = mod2_2009.glong and LST_within1km_stn.glat = mod2_2009.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2009; by date station   ;run;
proc sort data = mod1_2009_s1 ; by date station ;run;

data  mod1_2009_s2;
merge  mod1_2009_s1(in=a) met2009 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2009_s2; by station date dist;
data mod1_2009_s2s; set mod1_2009_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2009 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2009_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2009  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2009;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2009;
    ods output  SolutionR =  SolutionR2009;
	run;


data check_s1;
 set work.Solutionr2009;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2009;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2009_v1;
 merge mods.mod2_2009 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2009 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2009_v4;
 MERGE Mod2_2009_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2009_v4 OUT = Mod2_2009_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2009_pred;
 set Mod2_2009_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2009_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2009_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2010*/


PROC IMPORT OUT= WORK.lst2010pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2010.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2010.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2010.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2010.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2010pre (drop=xx yy);
set lst2010pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2010 (drop=xx yy);
set ndvi2010;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2010prew  as
   select *
    from lst2010pre left join grid
     on lst2010pre.glong = grid.glong and  lst2010pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2010   ;
set lst2010prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2010  as
   select *
    from  lst2010 left join ndvi2010
     on lst2010.glong = ndvi2010.glong and lst2010.glat = ndvi2010.glat  and  lst2010.month = ndvi2010.month ;
run;

/*save mod2*/
data mods.mod2_2010;
set mod2_2010;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2010_s1  as
   select *
    from LST_within1km_stn left join mod2_2010
     on LST_within1km_stn.glong = mod2_2010.glong and LST_within1km_stn.glat = mod2_2010.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2010; by date station   ;run;
proc sort data = mod1_2010_s1 ; by date station ;run;

data  mod1_2010_s2;
merge  mod1_2010_s1(in=a) met2010 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2010_s2; by station date dist;
data mod1_2010_s2s; set mod1_2010_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2010 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2010_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2010  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2010;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2010;
    ods output  SolutionR =  SolutionR2010;
	run;


data check_s1;
 set work.Solutionr2010;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2010;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2010_v1;
 merge mods.mod2_2010 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2010 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2010_v4;
 MERGE Mod2_2010_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2010_v4 OUT = Mod2_2010_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2010_pred;
 set Mod2_2010_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2010_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2010_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

/*2011*/


PROC IMPORT OUT= WORK.lst2011pre (drop=x y emis_scale ntc dtc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2011.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;

/*all NDVI points*/
PROC IMPORT OUT= WORK.ndvi2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2011.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*all Met stations points*/
PROC IMPORT OUT= WORK.met2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2011.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;

/*ALL guid points for ALL area and closest station (met) to it*/
PROC IMPORT OUT= WORK.key_full2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2011.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
/*all met points within 1km of a sattelite point */

PROC IMPORT OUT= WORK.LST_within1km_stn
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;
PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;

data LST_within1km_stn (drop=xx yy);
set LST_within1km_stn;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data lst2011pre (drop=xx yy);
set lst2011pre;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ndvi2011 (drop=xx yy);
set ndvi2011;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;



proc sql;
  create table lst2011prew  as
   select *
    from lst2011pre left join grid
     on lst2011pre.glong = grid.glong and  lst2011pre.glat = grid.glat ;
run;




/*add month to lst file*/
/* deleing missing elev deltes outside map points */

data lst2011   ;
set lst2011prew;
if near_water=1 then delete;
/*if nemia ne 1 then delete;*/
month = month(DATE);
if elev=. then delete;
run;



/*add NDVI to lst file*/
/*big dataset with all sattelite points and Ntckin for them*/
/*also save the mod2 file*/



proc sql;
  create table  mod2_2011  as
   select *
    from  lst2011 left join ndvi2011
     on lst2011.glong = ndvi2011.glong and lst2011.glat = ndvi2011.glat  and  lst2011.month = ndvi2011.month ;
run;

/*save mod2*/
data mods.mod2_2011;
set mod2_2011;
run; 


/*subset large all lst dataset to only relevant within 1km of station datset */



/*this next step will produce all satellite grid/day combos only within 1.5km of a monitor*/


proc sql;
  create table mod1_2011_s1  as
   select *
    from LST_within1km_stn left join mod2_2011
     on LST_within1km_stn.glong = mod2_2011.glong and LST_within1km_stn.glat = mod2_2011.glat ;
run;

/*merge all grid/day combos only within 1.5km of a monitor and the actuall met air temp data*/

proc sort data = met2011; by date station   ;run;
proc sort data = mod1_2011_s1 ; by date station ;run;

data  mod1_2011_s2;
merge  mod1_2011_s1(in=a) met2011 (in=b)  ;
  by date station;
    if b;
	run;



/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1_2011_s2; by station date dist;
data mod1_2011_s2s; set mod1_2011_s2; by station date dist;
if first.date;
run;

/*delete days where no day or night sat data are avilable*/
/*also save the mod1 file*/
data  mods.mod1_2011 (drop= OBJECTID Join_Count dist TARGET_FID month  pressure_m stype  area source _type_ _freq_ x y );
set mod1_2011_s2s;
if tempc > 130 then delete;
if elev < -100 then delete;
if ndvi >1 then delete;
run; 


/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



	
proc mixed data = mods.mod1_2011  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=pdataA_2011;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  SolutionF2011;
    ods output  SolutionR =  SolutionR2011;
	run;


data check_s1;
 set work.Solutionr2011;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
    if Effect = "Ntckin" then delete;
	Ovr_Int = Estimate;
run;


data check_s1_Ntckin(keep = date Ovr_Ntckin);
 set check_s1;
    if Effect = "Intercept" then delete;
	    Ovr_Ntckin = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_Ntckin;  by date;run;

data mean_s1;
 merge check_s1_Int check_s1_Ntckin ;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mods.mod2_2011;    by date;run;
proc sort data = mean_s1;        by date;run;

data Mod2_2011_v1;
 merge mods.mod2_2011 (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Fixed Effect */

proc transpose data = work.Solutionf2011 prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  Mod2_2011_v4;
 MERGE Mod2_2011_v1 transp_3_s1;
RUN;

PROC STANDARD DATA = Mod2_2011_v4 OUT = Mod2_2011_v4 REPLACE;
  VAR fix_Intercept--fix_NDVI;
RUN;



data mods.Mod2_2011_pred;
 set Mod2_2011_v4;
  pred = fix_intercept + Ntckin*fix_Ntckin  + elev*fix_elev + purban*fix_purban + NDVI*fix_NDVI + OVR_int + Ntckin*OVR_Ntckin;
run;

/*check mod 2 predictions*/

/*proc means data=mods.Mod2_2011_pred n min max mean std nmiss;*/
/*var ; */
/*run; */
/**/
/*proc summary nway data=mods.Mod2_2011_pred;*/
/*class glat glong;*/
/*var pred;*/
/*output out=OUTPUTFILE mean=pred;*/
/*run; */
/**/
/*PROC EXPORT DATA= OUTPUTFILE*/
/*            OUTFILE= "c:\Users\ekloog\Documents\tmp\gtgOUTDATA.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;
