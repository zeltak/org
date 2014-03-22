libname an1 'f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods\' ;

proc printto log="nul:"; run;

/*proc printto ; run;*/

options mprint;
%macro import(year=);

PROC IMPORT OUT= basegrid
  DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\basegrid.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

data Final_100kmet&year (drop= lat_aod long_aod);
set an1.Final_100kmet&year;
xlat_aod= round(lat_aod,0.00001);
 xlong_aod= round( long_aod,0.00001);
run; 


data Final_100kmet&year (drop= xlat_aod xlong_aod);
set Final_100kmet&year;;
lat_aod= xlat_aod;
long_aod= xlong_aod;
run; 



proc sort data = basegrid; by lat_aod long_aod  ;run;
proc sort data = Final_100kmet&year ; by lat_aod long_aod ;run;

data Final_100kmet&year.g;
merge Final_100kmet&year(in=a) basegrid (in=b)  ;
  by lat_aod long_aod;
    if a;
	run; 



PROC IMPORT OUT= mod2_T&year._v5
  DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods_CV\mod2_&year._pred.csv" 
  DBMS=CSV REPLACE;
  GETNAMES=YES;
  DATAROW=2; 
RUN;
 



proc sort data = mod2_T&year._v5  ; by guid date ;run;
proc sort data = Final_100kmet&year.g;          by guid date ;run;

data mod2_T&year._v6;
merge mod2_T&year._v5(in=a keep=date guid pred region ) Final_100kmet&year.g (in=b);
  by guid date;
    if a ;
	run; 

data mod2_T&year._v6;
set mod2_T&year._v6;
m=month(date);
 if (m=1 or m=2) then bimon=1;
 if (m=3 or m=4) then bimon=2;
 if (m=5 or m=6) then bimon=3;
 if (m=7 or m=8) then bimon=4;
 if (m=9 or m=10) then bimon=5;
 if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= mod2_T&year._v6
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods\mod2predmpm_&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  



/*proc datasets lib=work kill nolist memtype=data;*/
/*quit;*/



%MEND ;

%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);
%import(year=2009);
%import(year=2010);
%import(year=2011);

