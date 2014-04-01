libname aod 'f:\Uni\Projects\P042_Medicare_THROM\3.1.10.1.Raw_data\medicare admission data\' ;



  PROC IMPORT OUT= WORK.zip
             DATAFILE= "f:\Uni\Projects\3.1.17.Medicare_MIA\3.1.10.1.Raw_data\import into GIS\zip_usa_2005.dbf" 
			             DBMS=DBF   REPLACE;
						      GETDELETED=NO;
							  RUN; 

data zip;
set zip;
zipcode=zip;
run; 

/*export a zip code file to create a keytable with guid*/

proc summary nway data=zip;
class zipcode;
var x y ;
output out=zipagg mean= x y ;
run; 

PROC EXPORT DATA= zip 
            OUTFILE= "f:\Uni\data\gis\USA\zipcodes_xy\zipxy.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

data Ne_mid_thromb;
set aod.Ne_mid_thromb;
zipcode=zipcode_den;
run; 


proc sort data = Ne_mid_thromb; by zipcode   ;run;
proc sort data = zip  ; by zipcode ;run;

data DATA3;
merge Ne_mid_thromb(in=a drop=zipcode_den) zip  (in=b keep=zipcode x y)  ;
  by zipcode;
  if a;
run;  

PROC EXPORT DATA= DATA3
            OUTFILE= "f:\Uni\Projects\P042_Medicare_THROM\3.1.10.4.Work\2.Gather_data\FN001_export_casesXY\casesXy.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
