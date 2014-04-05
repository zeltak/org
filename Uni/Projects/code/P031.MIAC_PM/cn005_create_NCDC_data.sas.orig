/*IMPORT THE NCDC DATA*/


proc import datafile="f:\Uni\Projects\p031_MIAC_PM\0.raw\ncdc\USA2000-2012.txt"
 dbms=dlm out=y2012;
   delimiter=",";
     getnames=yes;
      guessingrows=500;
run;



data y2012(keep = stn temp wdsp stn slp visib dewp date2  );
 set y2012;
  TEMP = ___TEMP;  
   WDSP  = __WDSP;
   stn = STN___; 
   date2=_YEARMODA;
   slp=__SLP;
   visib=_VISIB;
   dewp=___DEWP;
   run; 


/*FIX DATE AND TRANSFORM TO DATE9*/
data y2012 (drop= c m d);
set y2012;
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
            DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\0.raw\ncdc\NCDCXY_NEMIA.dbf" 
			            DBMS=DBF REPLACE;
						     GETDELETED=NO;
							 RUN; 

data metxy (keep= stn long_met lat_met);
set metxy (rename=(LONG_DD= long_met LAT_DD=lat_met usaf=stn)); 
run; 



proc sort data = metxy; by stn ;run;
proc sort data = y2012 ; by stn ;run;

data metallxy;
 merge y2012 (in=a)  metxy  (in=b) ;
   by stn;
   if b;
      run; 

data metallxy;
set metallxy;
if temp=. then delete;
wvp_mb = 6.11*(10**(((7.5 * dewp)/(237.7 + dewp))));
ah_gm3 = (1000* (100 * wvp_mb))/((273.15 + temp)* 461.5);
if visib > 100 then visib=.;
if visib < 0.0000000000000001 then delete;
if wdsp >100 then delete;
if wdsp < 0.00000000000000000000001 then delete;
if ah_gm3 > 400 then delete;
if stn=999999 then delete;
run; 

data metallxy ;
set metallxy;
c=year(date);
if c=2013 then delete;
run; 

 proc sort data = metallxy nodupkey out=metallxyz;
 by date stn;
 run;



proc freq data=metallxyz;
table stn / list;
 ods output onewayfreqs=listy;
run;

data listyx;
set listy;
if Frequency < 4700 then delete;
run; 

proc sort data = listyx; by stn  ;run;
proc sort data = metallxyz ; by stn  ;run;

data DATA3;
merge metallxyz(in=a) listyx (in=b keep=stn)  ;
  by stn ;
    if b;
	run; 

PROC EXPORT DATA=  DATA3
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN002_NCDC_allyears\ncdc00_12.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*for grid*/

proc summary nway data=DATA3;
class lat_met long_met;
var stn;
output out=OUTPUTFILEmet mean=stn;
run; 

data OUTPUTFILEmet (keep=lat_met long_met stn);
set OUTPUTFILEmet;
run; 

PROC EXPORT DATA= OUTPUTFILEmet 
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\metID.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
