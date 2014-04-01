/*import station and XY cords for NE and MIA*/

PROC IMPORT OUT= pwsxy_NE
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\NE\stations\pws-stations-data-XY.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 
PROC IMPORT OUT= airxy_NE
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\NE\stations\airport-stations-data-XY.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

PROC IMPORT OUT= pwsxy_MIA
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\MIA\stations\pws-stations-data-XY.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 
PROC IMPORT OUT= airxy_MIA
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\MIA\stations\airport-stations-data-XY.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


options mprint;
%macro import(year=);


/*#NE*/

PROC IMPORT OUT= NE_P&year
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\NE\&year.\pws-stations-observations.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

PROC IMPORT OUT= NE_A&year
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\NE\&year.\airport-stations-observations.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;




data NE_P&year;
set NE_P&year;
stype=0;
run; 


data NE_a&year;
set NE_a&year;
stype=1;
run; 


proc sort data = NE_A&year; by  station   ;run;
proc sort data = airxy_NE ; by station  ;run;

data NE_a&year.xy;
merge NE_A&year(in=a) airxy_NE (in=b)  ;
  by station ;
    if a;
	run; 


proc sql;
 create table NE_a&year.xy2 as 
 select x,y,station,year_utc,month_utc,day_utc,hour_utc,minute_utc,temperature_m,humidity,wind_speed_m,wind_direction_m,wind_direction_i,pressure_m,location,state,stype
   from NE_a&year.xy;
quit;



proc sort data = NE_P&year; by  station   ;run;
proc sort data = pwsxy_NE ; by station  ;run;

data NE_p&year.xy;
merge NE_P&year(in=a) pwsxy_NE (in=b)  ;
  by station ;
    if a;
	run; 

proc sql;
 create table NE_p&year.xy2 as 
 select x,y,station,year_utc,month_utc,day_utc,hour_utc,minute_utc,temperature_m,humidity,wind_speed_m,wind_direction_m,wind_direction_i,pressure_m,location,state,stype
   from NE_p&year.xy;
quit;

data NE_C&year.xy;
set NE_p&year.xy2 NE_a&year.xy2;
seconds_utc=00;
dateutc=mdy(month_utc,day_utc,year_utc);
format dateutc date9.;
timeutc=hms(hour_utc,minute_utc,seconds_utc);
format timeutc time8.;
run; 


data NE_C&year.xy_EST;
set NE_C&year.xy;
*** COMBINE GMT DATE AND TIME INTO A DATETIME VARIABLE ***;
gmt_datetime=dhms(dateutc,0,0,timeutc);

*** CONVERT DATETIME VARIABLE FROM GMT TO EST ***;
est_datetime=gmt_datetime - 5*60*60;
*** FOR EST - SPLIT OUT THE DATE AND TIME ***;
time=timepart(est_datetime);
date=datepart(est_datetime);

*** DROP UNNEEDED VARIABLES ***;
drop gmt_datetime est_datetime;

format  time time5. date mmddyy10.;
run;


proc means data=NE_C&year.xy_EST n min max mean std nmiss;
var temperature_m humidity wind_speed_m wind_direction_m pressure_m stype; 
run; 

data NE_C&year.xy_EST;
set NE_C&year.xy_EST;
if temperature_m < -30 then temperature_m =.;
if humidity < 0 then humidity =.;
if wind_speed_m < 0 then wind_speed_m =.;
if pressure_m < 0 then pressure_m =.;
if pressure_m > 4000 then pressure_m =.;
run; 



proc summary nway data=NE_C&year.xy_EST;
class station date ;
var X Y temperature_m humidity wind_speed_m wind_direction_m pressure_m stype;
output out=NE_C&year._out mean= X Y temperature_m humidity wind_speed_m wind_direction_m pressure_m stype  ;
run; 

data  NE_C&year._out (where=(date>="01JAN&year"D and date<="31DEC&year"D )) ;
set  NE_C&year._out;
area="NE";
run;
proc means data= NE_C&year._out n min max mean std nmiss;
var temperature_m humidity wind_speed_m wind_direction_m pressure_m; 
run; 






/*#MIA*/

PROC IMPORT OUT= MIA_P&year
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\MIA\&year.\pws-stations-observations.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

PROC IMPORT OUT= MIA_A&year
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\WU\MIA\&year.\airport-stations-observations.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;




data MIA_P&year;
set MIA_P&year;
stype=0;
run; 


data MIA_a&year;
set MIA_a&year;
stype=1;
run; 


proc sort data = MIA_A&year; by  station   ;run;
proc sort data = airxy_MIA ; by station  ;run;

data MIA_a&year.xy;
merge MIA_A&year(in=a) airxy_MIA (in=b)  ;
  by station ;
    if a;
	run; 


proc sql;
 create table MIA_a&year.xy2 as 
 select x,y,station,year_utc,month_utc,day_utc,hour_utc,minute_utc,temperature_m,humidity,wind_speed_m,wind_direction_m,wind_direction_i,pressure_m,location,state,stype
   from MIA_a&year.xy;
quit;



proc sort data = MIA_P&year; by  station   ;run;
proc sort data = pwsxy_MIA ; by station  ;run;

data MIA_p&year.xy;
merge MIA_P&year(in=a) pwsxy_MIA (in=b)  ;
  by station ;
    if a;
	run; 

proc sql;
 create table MIA_p&year.xy2 as 
 select x,y,station,year_utc,month_utc,day_utc,hour_utc,minute_utc,temperature_m,humidity,wind_speed_m,wind_direction_m,wind_direction_i,pressure_m,location,state,stype
   from MIA_p&year.xy;
quit;

data MIA_C&year.xy;
set MIA_p&year.xy2 MIA_a&year.xy2;
seconds_utc=00;
dateutc=mdy(month_utc,day_utc,year_utc);
format dateutc date9.;
timeutc=hms(hour_utc,minute_utc,seconds_utc);
format timeutc time8.;
run; 


data MIA_C&year.xy_EST;
set MIA_C&year.xy;
*** COMBIMIA GMT DATE AND TIME INTO A DATETIME VARIABLE ***;
gmt_datetime=dhms(dateutc,0,0,timeutc);

*** CONVERT DATETIME VARIABLE FROM GMT TO EST ***;
est_datetime=gmt_datetime - 5*60*60;
*** FOR EST - SPLIT OUT THE DATE AND TIME ***;
time=timepart(est_datetime);
date=datepart(est_datetime);

*** DROP UNMIAEDED VARIABLES ***;
drop gmt_datetime est_datetime;

format  time time5. date mmddyy10.;
run;


proc means data=MIA_C&year.xy_EST n min max mean std nmiss;
var temperature_m humidity wind_speed_m wind_direction_m pressure_m stype; 
run; 

data MIA_C&year.xy_EST;
set MIA_C&year.xy_EST;
if temperature_m < -30 then temperature_m =.;
if humidity < 0 then humidity =.;
if wind_speed_m < 0 then wind_speed_m =.;
if pressure_m < 0 then pressure_m =.;
if pressure_m > 4000 then pressure_m =.;
run; 



proc summary nway data=MIA_C&year.xy_EST;
class station date ;
var X Y temperature_m humidity wind_speed_m wind_direction_m pressure_m stype;
output out=MIA_C&year._out mean= X Y temperature_m humidity wind_speed_m wind_direction_m pressure_m stype  ;
run; 

data  MIA_C&year._out (where=(date>="01JAN&year"D and date<="31DEC&year"D )) ;
set  MIA_C&year._out;
area="MIA";
run;
proc means data= MIA_C&year._out n min max mean std nmiss;
var temperature_m humidity wind_speed_m wind_direction_m pressure_m; 
run; 



/*Combine all*/

data NEMIA_&year(drop=_type_ _freq_);
set MIA_C&year._out NE_C&year._out;
run; 


PROC EXPORT DATA= NEMIA_&year
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN002_WU yearly\NEMIA_&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;


data NEMIA_&year;
set NEMIA_&year;
drop wind_direction_m;
source="WU";
run; 

%MEND ;

/*%import(year=2000);*/
/*%import(year=2001);*/
/*%import(year=2002);*/
/*%import(year=2003);*/
/*%import(year=2004);*/
/*%import(year=2005);*/
/*%import(year=2006);*/
/*%import(year=2007);*/
/*%import(year=2008); */
/*%import(year=2009); */
/*%import(year=2010); */
%import(year=2011); 
