/*** Macro to import MET dataset ***/


libname MET 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;



 

options mprint;
%macro import(place=);

proc import datafile="c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\NCDC Met data\&place..txt"
 dbms=dlm out=&place replace;
   delimiter=",";
     getnames=yes;
      guessingrows=500;
run;



data MET.MET&place(keep = stn temp wdsp stn slp visib dewp date2  );
 set &place;
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

%import(place=ct12);
%import(place=me12);
%import(place=nh12);
%import(place=vt12);
%import(place=ri12);
%import(place=ma12);

/*CREATE A FULL ALL place MET FILE*/

data all_MET12;
set MET.metct12 MET.metma12 MET.metnh12 MET.metri12 MET.metvt12 MET.metme12 ;
run; 

/*FIX DATE AND TRANSFORM TO DATE9*/
data all_MET12;
set all_MET12;
c=substr(date2,1,8);
m=substr(date2,9,2);
d=substr(date2,11,2);
date=mdy(m,d,c);
format date date9.;
drop date2;
run;


data all_MET12(where=(date>="01JAN2009"D and date<="31DEC2011"D )) ;;
set all_MET12;
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
proc sort data = all_MET12 ; by stn ;run;

data metallxy12;
 merge all_MET12 (in=a)  metxy (in=b);
   by stn;
   if temp=. then delete;
   if a;
      run; 


ods trace on;
proc freq data =  metallxy12;
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
proc freq data =  metallxy12;
 table long_met / list;
  ods output OneWayFreqs = OneWayLong;
run;
ods trace off;


data OneWayLong;
 set OneWayLong; 
  keep Long_met Frequency;
   where Frequency > 1;
run;


proc sort data = metallxy12;   by long_met   ;run;
proc sort data = OneWayLong;   by long_met   ;run;

data metallxy12;
merge metallxy12 (in=a) OneWayLong (in=b)  ;
 by long_met   ;
    if b;
	run; 

proc sort data = metallxy12;   by lat_met   ;run;
proc sort data = OneWayLat;    by lat_met   ;run;

data metallxy12;
merge metallxy12 (in=a) OneWayLat (in=b)  ;
 by lat_met   ;
    if b;
	run; 


proc freq data =  metallxy12;
 table lat_met / list;
run;



/*DELETE IMPLAUSIBLE VALUES AND CREATE ABS.HUMIDTY*/

data metallxy12;
set metallxy12;
wvp_mb = 6.11*(10**(((7.5 * dewp)/(237.7 + dewp))));
ah_gm3 = (1000* (100 * wvp_mb))/((273.15 + temp)* 461.5);
if visib > 100 then delete;
if visib < 0.0000000000000001 then delete;
if wdsp >100 then delete;
if wdsp < 0.00000000000000000000001 then delete;
if ah_gm3 > 400 then delete;
if lat_met=0 then delete;
if long_met=0 then delete;
run; 


data met.all_MET12;
set  metallxy12;
run; 





/*CREATE YEARLY FILES WITH UNIQUE MET STATIONS*/






data met2009xy12(drop = Frequency);
set metallxy12;
if c=2009;
run; 

proc summary nway data=met2009xy12;
class stn;
var long_met lat_met;
output out=metagg200912 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg200912 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2009.dbf" 
			            DBMS=DBF REPLACE;
						RUN;



data met2010xy12(drop = Frequency);
set metallxy12;
if c=2010;
run; 

proc summary nway data=met2010xy12;
class stn;
var long_met lat_met;
output out=metagg201012 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg201012 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2010.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

data met2011xy12(drop = Frequency);
set metallxy12;
if c=2011;
run; 

proc summary nway data=met2011xy12;
class stn;
var long_met lat_met;
output out=metagg201112 mean=long_met lat_met;
run;

PROC EXPORT DATA= WORK.metagg201112 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\metagg2011.dbf" 
			            DBMS=DBF REPLACE;
						RUN;



proc freq data = met2009xy12;
 table Lat_met*Long_met / list;
  ods output List = List2009(keep = Lat_met Long_met Frequency);
run;
quit;

proc sort data =  met2009xy12; by Lat_met Long_met ;run;
proc sort data =  List2009;    by Lat_met Long_met ;run;

data met2009xy12;
merge met2009xy12(in=a) List2009 (in=b);
   by Lat_met Long_met;
  if a and b;
  if frequency <= 20 then delete;
run;  


proc freq data = met2010xy12;
 table Lat_met*Long_met / list;
  ods output List = List2010(keep = Lat_met Long_met Frequency);
run;
quit;

proc sort data =  met2010xy12; by Lat_met Long_met ;run;
proc sort data =  List2010;    by Lat_met Long_met ;run;

data met2010xy12;
merge met2010xy12(in=a) List2010 (in=b)  ;
  by Lat_met Long_met;
  if a;
  if frequency <= 20 then delete;
run;  

proc freq data = met2011xy12;
 table Lat_met*Long_met / list;
  ods output List = List2011(keep = Lat_met Long_met Frequency);
run;
quit;

proc sort data =  met2011xy12; by Lat_met Long_met ;run;
proc sort data =  List2011;    by Lat_met Long_met ;run;

data met2011xy12;
merge met2011xy12(in=a) List2011(in=b)  ;
   by Lat_met Long_met;
  if a;
  if frequency <= 20 then delete;
run;  




data met.met2009xy;
set met2009xy12;
run;

data met.met2010xy;
set met2010xy12;
run;

data met.met2011xy;
set met2011xy12;
run;
