/*import all aod data (mod2)*/

PROC IMPORT OUT= WORK.mod2
            DATAFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN010_mod2_files\mod2.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




PROC IMPORT OUT= pmwithin
            DATAFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN007_Key_tables\AOD_within1km_stn.dbf"
			            DBMS=DBF   REPLACE;
                        GETDELETED=NO;
                        run;


PROC IMPORT OUT= pmmet
  DATAFILE= "s:\EOME\LUR_MEX\AOD\data\merged_mondata\mex_air_long_2013-05-21.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 
data pmmet;
set pmmet;
rename day=date;
run; 

/*#check how many monitors*/
proc summary nway data=pmmet;
class mon;
var rhmean;
output out=OUTPUTFILE mean= rhmean;
run; 


/*join the relevant AOD days to the aod points within 1.5km of mon stattion (we use the if 'b' option*/

proc sort data = pmwithin; by aodid   ;run;
proc sort data = mod2 ; by aodid ;run;
data mod2X;
merge mod2(in=a)pmwithin (in=b keep=aodid dist pm_x pm_y mon tden_1 elev_1 tden_OSM_1)  ;
  by aodid;
    if b;
	run; 


/*join wuth the pm-met dataset by date and monitor ID*/

proc sort data = pmmet; by date mon   ;run;
proc sort data = mod2X ; by date mon ;run;

data  mod1X1;
merge mod2X(in=a) pmmet (in=b)  ;
  by  date mon;
    if b;
	run;


/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=mod1X1; by mon date dist;
data mod1X2; set mod1X1; by mon date dist;
if first.date;
run;


data mod1X3;
set mod1X2;
if aod=. then delete;
if aod > 0.9 then delete;
if aod < 0.00000000000001 then delete;
run; 




PROC EXPORT DATA=  mod1X3 
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN009_mod1_files\mod1.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;



/*divide all file into files based on year*/
option mprint;
%macro Year(year=);

data y&year;
 set mod1X3;
  if year = &year;
run;
	
PROC EXPORT DATA=  y&year
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN009_mod1_files\y&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
%mend year;


%year(year=2003);
%year(year=2004);
%year(year=2005);
%year(year=2006);
%year(year=2007);
%year(year=2008); 
%year(year=2009);
%year(year=2010);
%year(year=2011); 





PROC EXPORT DATA=  mod1X3 
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN009_mod1_files\mod1.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
