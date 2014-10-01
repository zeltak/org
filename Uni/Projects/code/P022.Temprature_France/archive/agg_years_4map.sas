libname aod 'Z:\Projects\P022_Temprature_France\3.work\cn035_stage3-final' ;



				

options mprint;
%macro import(year=);

proc summary nway data=aod.Clean_final&year;
class Longitude Latitude;
var Final_Pred;
output out=OUTPUTFILE&year mean=Final_Pred Longitude Latitude ;
run; 	

PROC EXPORT DATA= OUTPUTFILE&year 
            OUTFILE= "Z:\Projects\P022_Temprature_France\3.work\cn035_stage3-final\agg&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
%MEND ;

/*%import(year=2000);*/
/*%import(year=2001);*/
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 	
%import(year=2009); 
%import(year=2010); 
%import(year=2011);


data all;
set OUTPUTFILE OUTPUTFILE2001 OUTPUTFILE2002 OUTPUTFILE2003 OUTPUTFILE2004 OUTPUTFILE2005 OUTPUTFILE2006 OUTPUTFILE2007 OUTPUTFILE2008 OUTPUTFILE2009 OUTPUTFILE2010 OUTPUTFILE2011;
run;  

proc summary nway data=all;
class Longitude Latitude;
var Final_Pred;
output out=OUTPUTFILE_all mean=Final_Pred Longitude Latitude ;
run; 	


PROC EXPORT DATA= OUTPUTFILE&year 
            OUTFILE= "Z:\Projects\P022_Temprature_France\3.work\cn035_stage3-final\agg&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
