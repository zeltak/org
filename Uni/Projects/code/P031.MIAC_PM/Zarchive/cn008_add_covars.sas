 /**********************************************************************
 *   PRODUCT:   SAS
 *   VERSION:   9.3
 *   CREATOR:   External File Interface
 *   DATE:      24OCT13
 *   DESC:      Generated SAS Datastep Code
 *   TEMPLATE SOURCE:  (None Specified.)
 ***********************************************************************/
    data WORK.REG    ;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile 'f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/region_guid.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat guid best32. ;
       informat region_1 best32. ;
       format guid best12. ;
       format region_1 best12. ;
    input
                guid
                region_1
    ;
    if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
    run;


libname aod '\\Drobo\Shared_Data\MIAC_USA\2003\' ;

/*import full gridid*/

PROC IMPORT OUT= lu
            DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN004_LU_full_dataset\full_LU.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  


/*all Met stations points for all years */
PROC IMPORT OUT= met
  DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN002_NCDC_allyears\ncdc00_12.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 



/*add met data*/


/*clean data*/
/*proc means data=met;*/
/*var ;*/
/*run;*/
/* */

data met;
set met;
tempc=  (5/9)*(TEMP-32);
if Tempc  = 9999.9 then Temp  = .;
if WDSP  = 999.9  then WDSP  = .;
if SLP   = 9999.9 then SLP   = .;
if Visib = 999.9  then Visib = .; 
if dewp  = 9999.9 then dewp  = .;
run;



/*add aod values to the mod2 dataset*/

proc sort data = lu; by  Lat_aod  Long_aod   ;run;
proc sort data = aod.aod_ne_mod2 ; by Lat_aod  Long_aod ;run;

data DATA2;
merge aod.aod_ne_mod2(in=a) lu (in=b)  ;
  by Lat_aod  Long_aod;
    if a;
	run; 

/*clip data from points outside study area and remove water points*/

/*proc means data=DATA2 n min max mean std nmiss;*/
/*var; */
/*run; */
 

data DATA2;
set DATA2;
if elev_m=. then delete;
if wflag=1 then delete;
month=month(date);
drop  state_fips  fips_1;
run; 


 

/*all NDVI points*/
					  


PROC IMPORT OUT= WORK.ndvi2003
            DATAFILE= "\\Drobo\Shared_Data\EAST_USA_MAIAC\data\FN006_NDVI_yearly\ndvi2003.dbf"
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;



data ndvi2003;
set ndvi2003;
ndviid=compress(x||y);
run; 

/*on first year to create the unique id */


/**/
/*proc summary nway data=ndvi2003;*/
/*class x y ndviid;*/
/*var ndvi;*/
/*output out=ndOUTPUTFILE mean=ndvi;*/
/*run; */
/**/
/*data ndOUTPUTFILE;*/
/*set ndOUTPUTFILE;*/
/*keep x y ndviid;*/
/*run; */
/**/
/**/
/*PROC EXPORT DATA=  ndOUTPUTFILE*/
/*            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\ndvi_ID.dbf" */
/*			            DBMS=DBF REPLACE;*/
/*						RUN;*/
						 




/*add ndvi*/

	
proc sort data = Ndvi2003; by  month ndviid   ;run;
proc sort data = data2 ; by month ndviid   ;run;

data data4;
merge DATA2(in=a) Ndvi2003 (in=b keep=month ndviid ndvi)  ;
  by month ndviid ;
    if a;
	run;


proc means data=DATA4 n min max mean std nmiss;
var; 
run; 




/*all pbl points*/
PROC IMPORT OUT= pbl
            DATAFILE= "\\Drobo\Shared_Data\HPBL\P2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data pblx;
set pbl;
date=mdy(v2,v3,v1);
format date date9.;
run; 
						 

/*add pbl to mod2*/
proc sort data = pblx; by  date pblid   ;run;
proc sort data = data4 ; by date pblid   ;run;

data DATA5;
merge data4(in=a) pblx (in=b keep=date pblid pbl)  ;
  by date pblid ;
    if a;
	run;

/*Add met data*/

data met2003;
set met;
where c=2003;
run; 

proc sort data = met2003; by  date stn   ;run;
proc sort data = data5 ; by date stn     ;run;

data DATA6;
merge data5(in=a) met2003 (in=b drop=temp)  ;
  by date stn   ;
    if a;
	run;

/*proc means data=DATA6 n min max mean std nmiss;*/
/*var; */
/*run; */


/*clean all and ready mod2*/

libname aodm2 '\\Drobo\Shared_Data\EAST_USA_MAIAC\models\' ;

 
data aodm2.mod2_2003;
set DATA6;
if tempc=. then delete;
if ndvi=. then delete;
run; 

data expo_2003;
set aodm2.mod2_2003;
keep DATE aod guid tempc WDSP NDVI dist_PE pcturb_1km Mjrrdden_1 SumOfEMISS pop_sqkm elev_m ah_gm3 visib pbl;
run ;

libname ds 'y:\EAST_USA_MAIAC\LU\midatlantic_newengland\' ;

PROC IMPORT OUT= stack (rename=(guid_=guid))
  DATAFILE= "y:/EAST_USA_MAIAC/NEI05/midatlneweng_nei05stacks.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 


PROC IMPORT OUT= reg (rename=(region_1=region))
  DATAFILE= "f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/region_guid.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

proc sort data = ds.Midatlnewenglc06; by guid   ;run;
proc sort data =  ds.Midatlnewengvar_cntynei05v2; by guid   ;run;
proc sort data = stack ; by guid ;run;
proc sort data = expo_2003 ; by guid ;run;
proc sort data = reg ; by guid ;run;


data aodm2.expo_2003X;
merge expo_2003(in=a) ds.Midatlnewenglc06 (in=b )ds.Midatlnewengvar_cntynei05v2
(in=d keep=guid NOXsum--nei05nonpntcntypm25)stack (in=e) reg (in=f) ;
  by guid;
    if a;
	run; 



PROC EXPORT DATA= aodm2.expo_2003X
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN009_ALL_mods_base\mod2_2003.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  




/*start with mod1*/

PROC IMPORT OUT= pmwithin
            DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\pm_within1km_sitecode.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data pmwithin;
set pmwithin;
keep long_aod lat_aod guid sitecode dist2mon;
run; 

proc sort data = pmwithin; by guid   ;run;
proc sort data = aodm2.mod2_2003 ; by guid ;run;
data mod2X;
merge aodm2.mod2_2003 (in=a)  pmwithin (in=b )  ;
  by guid;
    if b;
	run; 




libname pm  'f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN001_PM_allyears\' ;



/*CREATE A FULL ALL YEAR PM FILE*/

data pm2003;
set pm.all_pm;
where date>='01JAN2003'D and date<='31DEC2003'D ;
run; 


/*join wuth the pm-met dataset by date and monitor ID*/

proc sort data = pm2003; by date sitecode   ;run;
proc sort data = mod2x ; by date sitecode ;run;

data  mod1y1;
merge mod2x(in=a) pm2003 (in=b)  ;
  by  date sitecode;
    if b;
	run;


/*to leave only THE 1 closest sat data point to station in each day*/


proc sort data=mod1y1; by sitecode date dist2mon;
data mod1y2; set mod1y1; by sitecode date dist2mon;
if first.date;
run;


data mod1y3;
set mod1y2;
if aod=. then delete;
drop reg region;
/*if aod > 0.9 then delete;*/
/*if aod < 0.00000000000001 then delete;*/
run; 

							  
 
libname ds 'y:\EAST_USA_MAIAC\LU\midatlantic_newengland\' ;

PROC IMPORT OUT= stack (rename=(guid_=guid))
  DATAFILE= "y:/EAST_USA_MAIAC/NEI05/midatlneweng_nei05stacks.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;



PROC IMPORT OUT= reg (rename=(region_1=region))
  DATAFILE= "f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/region_guid.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc sort data = ds.Midatlnewenglc06; by guid   ;run;
proc sort data =  ds.Midatlnewengvar_cntynei05v2; by guid   ;run;
proc sort data = mod1y3 ; by guid ;run;
proc sort data = stack ; by guid ;run;
proc sort data = reg ; by guid ;run;


data mod1y4;
merge mod1y3(in=a) ds.Midatlnewenglc06 (in=b )ds.Midatlnewengvar_cntynei05v2
(in=d keep=guid NOXsum--nei05nonpntcntypm25)stack (in=e) reg (in=f)  ;
  by guid;
    if a;
	run; 



PROC EXPORT DATA= mod1y4 
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN009_ALL_mods_base\mod1_2003.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
