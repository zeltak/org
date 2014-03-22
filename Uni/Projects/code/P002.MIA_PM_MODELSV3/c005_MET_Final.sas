/*** Macro to import MET dataset ***/


libname MET 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;



 

options mprint;
%macro import(place=);

proc import datafile="c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\NCDC Met data\&place..txt"
 dbms=dlm out=work.&place replace;
   delimiter=",";
     getnames=yes;
      guessingrows=500;
run;



data MET.MET&place(keep = stn temp wdsp stn slp visib dewp date2  );
 set work.&place;
  TEMP = ___TEMP;  
   WDSP  = __WDSP;
   stn = STN___; 
   date2=_YEARMODA;
   slp=__SLP;
   visib=_VISIB;
   dewp=___DEWP;
   run; 



/** Delete from workspace **/

proc datasets lib = work; delete MET&place; run; quit;


%mend;

%import(place=MD);
%import(place=dc);
%import(place=delware);
%import(place=nj);
%import(place=ny);
%import(place=penn);
%import(place=Virginia);
%import(place=WVirginia);

/*CREATE A FULL ALL place MET FILE*/

data all_MET;
set MET.metct MET.metma MET.metnh MET.metri MET.metvt MET.metme ;
run; 

/*FIX DATE AND TRANSFORM TO DATE9*/
data all_MET;
set all_MET;
c=substr(date2,1,8);
m=substr(date2,9,2);
d=substr(date2,11,2);
date=mdy(m,d,c);
format date date9.;
drop date2;
run;



/*ADD XY TO MET DATA*/
/*also delete wrong station locations*/



PROC IMPORT OUT= WORK.metxy
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\NCDC Met data\metXY.dbf" 
			            DBMS=DBF REPLACE;
						     GETDELETED=NO;
							 RUN; 

data metxy (keep= stn long_met lat_met);
set metxy (rename=(LONG_DD= long_met LAT_DD=lat_met usaf=stn)); 
run; 



proc sort data = metxy; by stn ;run;
proc sort data = all_MET ; by stn ;run;

data metallxy;
 merge all_MET (in=a)  metxy ;
   by stn;
   if temp=. then delete;
      run; 


ods trace on;
proc freq data =  metallxy;
 table lat_met / list;
  ods output OneWayFreqs = OneWayLat;
run;
ods trace off;

data OneWayLat;
 set OneWayLat; 
  keep Lat_met Frequency;
   where Frequency > 1;
run;


ods trace on;
proc freq data =  metallxy;
 table long_met / list;
  ods output OneWayFreqs = OneWayLong;
run;
ods trace off;


data OneWayLong;
 set OneWayLong; 
  keep Long_met Frequency;
   where Frequency > 1;
run;


proc sort data = metallxy;   by long_met   ;run;
proc sort data = OneWayLong; by long_met   ;run;

data metallxy;
merge metallxy (in=a) OneWayLong (in=b)  ;
 by long_met   ;
    if b;
	run; 

proc sort data = metallxy;   by lat_met   ;run;
proc sort data = OneWayLat;  by lat_met   ;run;

data metallxy;
merge metallxy (in=a) OneWayLat (in=b)  ;
 by lat_met   ;
    if b;
	run; 


proc freq data =  metallxy;
 table lat_met / list;
run;



/*DELETE IMPLAUSIBLE VALUES AND CREATE ABS.HUMIDTY*/

data metallxy;
set metallxy;
wvp_mb = 6.11*(10**(((7.5 * dewp)/(237.7 + dewp))));
ah_gm3 = (1000* (100 * wvp_mb))/((273.15 + temp)* 461.5);
if visib > 100 then delete;
if visib < 0.0000000000000001 then delete;
if wdsp >100 then delete;
if wdsp < 0.00000000000000000000001 then delete;
if ah_gm3 > 400 then delete;
run; 


data met.all_MET;
set  metallxy;
run; 





/*CREATE YEARLY FILES WITH UNIQUE MET STATIONS*/



data met2000xy;
set metallxy;
if c=2000;
run; 

proc summary nway data=met2000xy;
class stn;
var long_met lat_met;
output out=metagg2000 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2000 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2000.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




data met2001xy;
set metallxy;
if c=2001;
run; 

proc summary nway data=met2001xy;
class stn;
var long_met lat_met;
output out=metagg2001 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2001 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2001.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




data met2002xy;
set metallxy;
if c=2002;
run; 

proc summary nway data=met2002xy;
class stn;
var long_met lat_met;
output out=metagg2002 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2002 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2002.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




data met2003xy;
set metallxy;
if c=2003;
run; 

proc summary nway data=met2003xy;
class stn;
var long_met lat_met;
output out=metagg2003 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2003 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


						



data met2004xy;
set metallxy;
if c=2004;
run; 

proc summary nway data=met2004xy;
class stn;
var long_met lat_met;
output out=metagg2004 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2004 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2004.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

						



data met2005xy;
set metallxy;
if c=2005;
run; 

proc summary nway data=met2005xy;
class stn;
var long_met lat_met;
output out=metagg2005 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2005 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2005.dbf" 
			            DBMS=DBF REPLACE;
						RUN;





data met2006xy;
set metallxy;
if c=2006;
run; 

proc summary nway data=met2006xy;
class stn;
var long_met lat_met;
output out=metagg2006 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2006 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2006.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




data met2007xy;
set metallxy;
if c=2007;
run; 

proc summary nway data=met2007xy;
class stn;
var long_met lat_met;
output out=metagg2007 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2007 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2007.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




data met2008xy;
set metallxy;
if c=2008;
run; 

proc summary nway data=met2008xy;
class stn;
var long_met lat_met;
output out=metagg2008 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg2008 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




/*PART 5: EXPORT YEARLT MET FILES>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */ 

	libname met2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

						 


data met2.met2000xy;
set met2000xy;
run;

data met2.met2001xy;
set met2001xy;
run;

data met2.met2002xy;
set met2002xy;
run;

data met2.met2003xy;
set met2003xy;
run;

data met2.met2004xy;
set met2004xy;
run;

data met2.met2005xy;
set met2005xy;
run;

data met2.met2006xy;
set met2006xy;
run;

data met2.met2007xy;
set met2007xy;
run;

data met2.met2008xy;
set met2008xy;
run;









