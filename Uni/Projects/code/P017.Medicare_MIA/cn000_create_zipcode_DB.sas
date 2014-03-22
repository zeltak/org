

 PROC IMPORT OUT= WORK.zip
             DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.1.Raw_data\a.import into GIS\zip_usa_2005.dbf" 
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

PROC EXPORT DATA= zipagg
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\ArcGIS\zipagg.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



proc sort data = NEMC; by zipcode   ;run;
proc sort data = zip ; by zipcode ;run;

data NEMCXY;
merge NEMC(in=a) zip  (in=b keep=zipcode x y)  ;
  by zipcode;
    if a;
	run; 

data NEMCXY;
set NEMCXY;
if x=. then delete;
run; 



PROC EXPORT DATA= NEMCXY
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\ArcGIS\MIA_Hospital_zip_XY.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
