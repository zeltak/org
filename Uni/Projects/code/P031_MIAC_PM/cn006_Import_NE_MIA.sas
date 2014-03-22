/*start in 10:30*/


libname aod 'Y:\Data\USA\MAIAC_USA\2004\' ;


/*import relevant ne_MIA tiles */

options mprint;

%macro import (Tile = );

 data final_h&tile._short_NEMIA;
 set aod.final_h&tile; drop in QC year;
 if lat_aod < 36.7 then delete;
 if lat_aod > 47.459687 then delete;
 if long_aod < -83.5 then delete;
 if long_aod > -66.950005 then delete;
 run;


/*create a summary for each day of aod per x,y (since there could be a few obs. per day in same grid)*/

proc summary nway data=final_h&tile._short_NEMIA;
class lat_aod long_aod date;
var aod;
output out=aod.final_h&tile._short_NEMIA mean=aod;
run; 


 %mend;

 %Import(Tile = 01v00);
 %Import(Tile = 01v01);
 %Import(Tile = 01v02);
 %Import(Tile = 02v00);
 %Import(Tile = 02v01);
 %Import(Tile = 02v02);


/*save only valid area*/

 data aod.aod_ne_mod2;
 set aod.Final_h02v00_short_nemia aod.Final_h02v01_short_nemia aod.Final_h02v02_short_nemia aod.Final_h01v00_short_nemia aod.Final_h01v01_short_nemia aod.Final_h01v02_short_nemia;
 if aod=. then delete;
drop _type_ _freq_ ;
format Lat_aod  16.4;
format Long_aod 16.4;
 run;

/*import full cliped area*/


PROC IMPORT OUT= grid
            DATAFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\basegrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  

 data grid;
 set grid;
 keep lat_aod long_aod guid;
 format Lat_aod  16.4;
format Long_aod 16.4;
 run;


proc sort data = grid  ; by  Lat_aod  Long_aod   ;run;
proc sort data = aod.aod_ne_mod2 ; by Lat_aod  Long_aod ;run;

data aod.aod_ne_mod2;
merge aod.aod_ne_mod2(in=a) grid (in=b)  ;
  by Lat_aod  Long_aod;
    if b;
	run; 


data tstx;
set aod.aod_ne_mod2;
if aod=. then delete;
run;


/*export to csv for R*/

PROC EXPORT DATA= tstx
            OUTFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN008_model_prep\aod_ne_mod2_2004.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

 proc datasets lib=work kill; run;





libname aod 'Y:\Data\USA\MAIAC_USA\2003\' ;


/*import relevant ne_MIA tiles */

options mprint;

%macro import (Tile = );

 data final_h&tile._short_NEMIA;
 set aod.final_h&tile; drop in QC year;
 if lat_aod < 36.7 then delete;
 if lat_aod > 47.459687 then delete;
 if long_aod < -83.5 then delete;
 if long_aod > -66.950005 then delete;
 run;


/*create a summary for each day of aod per x,y (since there could be a few obs. per day in same grid)*/

proc summary nway data=final_h&tile._short_NEMIA;
class lat_aod long_aod date;
var aod;
output out=aod.final_h&tile._short_NEMIA mean=aod;
run; 


 %mend;

 %Import(Tile = 01v00);
 %Import(Tile = 01v01);
 %Import(Tile = 01v02);
 %Import(Tile = 02v00);
 %Import(Tile = 02v01);
 %Import(Tile = 02v02);


/*save only valid area*/



 data aod.aod_ne_mod2;
 set aod.Final_h02v00_short_nemia aod.Final_h02v01_short_nemia aod.Final_h02v02_short_nemia aod.Final_h01v00_short_nemia aod.Final_h01v01_short_nemia aod.Final_h01v02_short_nemia;
 if aod=. then delete;
drop _type_ _freq_ ;
format Lat_aod  16.4;
format Long_aod 16.4;
 run;

/*import full cliped area*/

PROC IMPORT OUT= grid
            DATAFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\basegrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  

 data grid;
 set grid;
 keep lat_aod long_aod guid;
 format Lat_aod  16.4;
format Long_aod 16.4;
 run;



proc sort data = grid  ; by  Lat_aod  Long_aod   ;run;
proc sort data = aod.aod_ne_mod2 ; by Lat_aod  Long_aod ;run;

data aod.aod_ne_mod2;
merge aod.aod_ne_mod2(in=a) grid (in=b)  ;
  by Lat_aod  Long_aod;
    if b;
	run; 


data tstx;
set aod.aod_ne_mod2;
if aod=. then delete;
run;




/*export to csv for R*/

PROC EXPORT DATA= tstx
            OUTFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN008_model_prep\aod_ne_mod2_2003.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*start in 10:30*/


libname aod 'Y:\Data\USA\MAIAC_USA\2010\' ;


/*import relevant ne_MIA tiles */

options mprint;

%macro import (Tile = );

 data final_h&tile._short_NEMIA;
 set aod.final_h&tile; drop in QC year;
 if lat_aod < 36.7 then delete;
 if lat_aod > 47.459687 then delete;
 if long_aod < -83.5 then delete;
 if long_aod > -66.950005 then delete;
 run;


/*create a summary for each day of aod per x,y (since there could be a few obs. per day in same grid)*/

proc summary nway data=final_h&tile._short_NEMIA;
class lat_aod long_aod date;
var aod;
output out=aod.final_h&tile._short_NEMIA mean=aod;
run; 


 %mend;

 %Import(Tile = 01v00);
 %Import(Tile = 01v01);
 %Import(Tile = 01v02);
 %Import(Tile = 02v00);
 %Import(Tile = 02v01);
 %Import(Tile = 02v02);


/*save only valid area*/

 data aod.aod_ne_mod2;
 set aod.Final_h02v00_short_nemia aod.Final_h02v01_short_nemia aod.Final_h02v02_short_nemia aod.Final_h01v00_short_nemia aod.Final_h01v01_short_nemia aod.Final_h01v02_short_nemia;
 if aod=. then delete;
drop _type_ _freq_ ;
format Lat_aod  16.4;
format Long_aod 16.4;
 run;

/*import full cliped area*/


PROC IMPORT OUT= grid
            DATAFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\basegrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  

 data grid;
 set grid;
 keep lat_aod long_aod guid;
 format Lat_aod  16.4;
format Long_aod 16.4;
 run;


proc sort data = grid  ; by  Lat_aod  Long_aod   ;run;
proc sort data = aod.aod_ne_mod2 ; by Lat_aod  Long_aod ;run;

data aod.aod_ne_mod2;
merge aod.aod_ne_mod2(in=a) grid (in=b)  ;
  by Lat_aod  Long_aod;
    if b;
	run; 


data tstx;
set aod.aod_ne_mod2;
if aod=. then delete;
run;


/*export to csv for R*/

PROC EXPORT DATA= tstx
            OUTFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN008_model_prep\aod_ne_mod2_2010.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  



							 /*start in 10:30*/


libname aod 'Y:\Data\USA\MAIAC_USA\2011\' ;


/*import relevant ne_MIA tiles */

options mprint;

%macro import (Tile = );

 data final_h&tile._short_NEMIA;
 set aod.final_h&tile; drop in QC year;
 if lat_aod < 36.7 then delete;
 if lat_aod > 47.459687 then delete;
 if long_aod < -83.5 then delete;
 if long_aod > -66.950005 then delete;
 run;


/*create a summary for each day of aod per x,y (since there could be a few obs. per day in same grid)*/

proc summary nway data=final_h&tile._short_NEMIA;
class lat_aod long_aod date;
var aod;
output out=aod.final_h&tile._short_NEMIA mean=aod;
run; 


 %mend;

 %Import(Tile = 01v00);
 %Import(Tile = 01v01);
 %Import(Tile = 01v02);
 %Import(Tile = 02v00);
 %Import(Tile = 02v01);
 %Import(Tile = 02v02);


/*save only valid area*/

 data aod.aod_ne_mod2;
 set aod.Final_h02v00_short_nemia aod.Final_h02v01_short_nemia aod.Final_h02v02_short_nemia aod.Final_h01v00_short_nemia aod.Final_h01v01_short_nemia aod.Final_h01v02_short_nemia;
 if aod=. then delete;
drop _type_ _freq_ ;
format Lat_aod  16.4;
format Long_aod 16.4;
 run;

/*import full cliped area*/


PROC IMPORT OUT= grid
            DATAFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\basegrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  

 data grid;
 set grid;
 keep lat_aod long_aod guid;
 format Lat_aod  16.4;
format Long_aod 16.4;
 run;


proc sort data = grid  ; by  Lat_aod  Long_aod   ;run;
proc sort data = aod.aod_ne_mod2 ; by Lat_aod  Long_aod ;run;

data aod.aod_ne_mod2;
merge aod.aod_ne_mod2(in=a) grid (in=b)  ;
  by Lat_aod  Long_aod;
    if b;
	run; 


data tstx;
set aod.aod_ne_mod2;
if aod=. then delete;
run;


/*export to csv for R*/

PROC EXPORT DATA= tstx
            OUTFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN008_model_prep\aod_ne_mod2_2011.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

