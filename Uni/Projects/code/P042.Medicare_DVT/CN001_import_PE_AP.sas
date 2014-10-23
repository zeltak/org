libname aod 'z:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\' ;


 

  PROC IMPORT OUT= WORK.zip
             DATAFILE= "z:\Projects\P017.Medicare_MIA\3.1.10.1.Raw_data\import into GIS\zip_usa_2005.dbf" 
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
            OUTFILE= "z:\Data\GIS\USA\zipcodes_xy\zipxy.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;





data Ne_mid_thromb;
set aod.Ne_mid_thrombpe;
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
            OUTFILE= "Z:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN001_export_casesXY\casesXy_PE_AP.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
