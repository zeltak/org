/*LIBRARIES*/





/*mask and get rid of bad grid cells, create yearly fintemp map*/


Options mprint;
%macro import(year=);


libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



PROC IMPORT OUT= gbad (keep=pred_m3  glong glat)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data gbad;
set gbad;
badgrids=0;
if pred_m3 <= 1.1 then badgrids=1;
run; 


proc sql;
  create table Fintmpc_&year  as
   select *
    from mods.Fintmpc_&year left join gbad
     on Fintmpc_&year..glong = gbad.glong and Fintmpc_&year..glat = gbad.glat;
run;

/*Get rid of bad values (water etc*/

Data mods.Fintmpc_&year ;
set Fintmpc_&year;
if badgrids=1 then delete;
run; 


proc summary nway data=mods.Fintmpc_&year;
class glong glat;
var fintemp;
output out=OUT&year mean=fintemp;
run; 

PROC EXPORT DATA= OUT&year 
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_003_longterm_maps_fintemp\OUT&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
proc datasets lib=work kill nolist memtype=data;
quit;


%MEND ;

%import(year=2000);
%import(year=2001);
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
