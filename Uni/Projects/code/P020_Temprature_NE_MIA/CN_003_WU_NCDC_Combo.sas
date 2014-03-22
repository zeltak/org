/*IMPORT THE NCDC DATA*/


libname data "f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\NCDC\";

DATA STATION;
    LENGTH
        F1               $ 7
        F2               $ 6
        F3               $ 30
        F4               $ 6
        F5               $ 3
        F6               $ 6
        F7               $ 7
        F8               $ 8
        F9               $ 10
        F10              $ 9
        F11              $ 8 ;
    FORMAT
        F1               $CHAR7.
        F2               $CHAR6.
        F3               $CHAR30.
        F4               $CHAR6.
        F5               $CHAR3.
        F6               $CHAR6.
        F7               $CHAR7.
        F8               $CHAR8.
        F9               $CHAR10.
        F10              $CHAR9.
        F11              $CHAR8. ;
    INFORMAT
        F1               $CHAR7.
        F2               $CHAR6.
        F3               $CHAR30.
        F4               $CHAR6.
        F5               $CHAR3.
        F6               $CHAR6.
        F7               $CHAR7.
        F8               $CHAR8.
        F9               $CHAR10.
        F10              $CHAR9.
        F11              $CHAR8. ;
    INFILE 'f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\NCDC\Station.txt'
        LRECL=100
        ENCODING="WLATIN1"
        TERMSTR=CRLF
        TRUNCOVER ;
    INPUT
        @1     F1               $CHAR7.
        @8     F2               $CHAR6.
        @14    F3               $CHAR30.
        @44    F4               $CHAR6.
        @50    F5               $CHAR3.
        @53    F6               $CHAR6.
        @59    F7               $CHAR7.
        @66    F8               $CHAR8.
        @74    F9               $CHAR10.
        @84    F10              $CHAR9.
        @93    F11              $CHAR8. ;
RUN;

data Station(keep = USAF WBAN St Name Lat Lon start_date end_date);
 set Station;
 if _n_ > 2;
  St   = F5;
  USAF = F1*1;
  WBAN = F2*1;
  Name = F3;
  Lat = (F7*1)/1000;
  Lon = (F8*1)/1000;

  start_y = (substr(f10, 1, 4))*1;
  start_m = (substr(f10, 5, 2))*1;
  start_d = (substr(f10, 7, 2))*1;
 
  start_date = mdy(start_m,start_d,start_y); format start_date date9.;

  end_y = (substr(f11, 1, 4))*1;
  end_m = (substr(f11, 5, 2))*1;
  end_d = (substr(f11, 7, 2))*1;

  end_date = mdy(end_m,end_d,end_y); format end_date date9.;

run;



/*** Macro to import MET dataset ***/

%macro state(data = );

proc import datafile="f:\Uni\Projects\P020_Temprature_NE_MIA\1_Raw_data\NCDC\&data..txt"
 dbms=dlm out=work.&data replace;
   delimiter=",";
     getnames=yes;
      guessingrows=500;
run;

data &data;
length State $ 20;
 set &data;
 State = "&data";
 stn = STN___; 
  TEMP = ___TEMP;  					Label Temp  = "Mean temperature for the day in degrees Fahrenheit to tenths";
   WDSP  = __WDSP;				    Label WDSP  = "Mean wind speed for the day in knots to tenths";
   slp=__SLP;                       Label SLP   = "Mean sea level pressure for the day in millibars to tenths";
   visib=_VISIB;                    Label Visib = "Mean visibility for the day in miles to tenths";
   dewp=___DEWP;                    Label DEWP  = "Mean dew point for the day in degrees Fahrenheit to tenths";

/*** WBAN & USAF combination ***/


if Temp  = 9999.9 then Temp  = .;
if WDSP  = 999.9  then WDSP  = .;
if SLP   = 9999.9 then SLP   = .;
if Visib = 999.9  then Visib = .; 
if dewp  = 9999.9 then dewp  = .;

/**** Relative Humidity ****/

Tc  = 5.0/9.0*(TEMP-32.0);
Tdc = 5.0/9.0*(DEWP-32.0);
Es=6.11*10.0**(7.5*Tc/(237.7+Tc));
E=6.11*10.0**(7.5*Tdc/(237.7+Tdc));
RH =(E/Es)*100; Label RH  = "Relative Humidity (%)";
AT = -42.379 + 2.04901523*Temp + 10.14333127*RH - 0.22475541*Temp*RH - 6.83783*10**(-3)*(Temp**(2)) - 5.481717*10**(-2)*(RH**(2)) + 1.22874*10**(-3)*(Temp**(2))*(RH) + 8.5282*10**(-4)*(Temp)*(RH**(2)) - 1.99*10**(-6)*(Temp**(2))*(RH**(2));
Label AT  = "Apparent Temperature";

/*** Date ***/

c=substr(_YEARMODA,1,8);
m=substr(_YEARMODA,9,2);
d=substr(_YEARMODA,11,2);

Date = mdy(m,d,c); format Date date9.;

/*** date interval ***/

if "01jan1951"d <= Date <= "31dec2011"d;

USAF = Stn;

keep State USAF wban temp wdsp slp visib dewp RH AT Date;

run;


proc sort data = &data;   by wban USAF; run;
proc sort data = Station; by wban USAF; run;

data &data;
 merge Station(in=a) &data(in=b);
  by WBAN USAF;
  if b;
run;

proc means data = &data min max;
 var date;
  class USAF wban;
   ods output summary = sum_&data;
run;

data sum_&data;
 set sum_&data;
  format date_Min date_Max date9.;
run;

proc sort data = sum_&data;  by wban USAF; run;
proc sort data = Station;    by wban USAF; run;

data sum_&data;
 merge Station(in=a) sum_&data(in=b);
  by WBAN USAF;
  if b;
run;

proc sort data = &data; by wban USAF date; run;

%mend;

%state(data = Connecticut);
%state(data = Delaware);
%state(data = District_of_Columbia);
%state(data = Maine);
%state(data = Maryland);
%state(data = Massachusetts);

%state(data = New_Hampshire);
%state(data = New_Jersey);
%state(data = New_York);
%state(data = Pennsylvania);
%state(data = Rhode_Island);
%state(data = Vermont);
%state(data = Virginia);
%state(data = West_Virginia);


data all ;
set Connecticut Delaware District_of_Columbia Maine Maryland Massachusetts New_Hampshire New_Jersey New_York Pennsylvania Rhode_Island Vermont Virginia West_Virginia;
where date>="01JAN2000"D and date<="31DEC2011"D ;
run;


data allx;
set all;
rename lon=x lat=y usaf=stn  ;
temperature_m=  (5/9)*(TEMP-32);
wind_speed_m=wdsp*1.61;
humidity=RH;
stype=.;
area="";
pressure_m=slp;
source="NCDC";
run; 



data allx(drop=stn);
set allx;
if temperature_m < -30 then temperature_m =.;
if humidity < 0 then humidity =.;
if wind_speed_m < 0 then wind_speed_m =.;
if pressure_m < 0 then pressure_m =.;
if pressure_m > 4000 then pressure_m =.;
station = put(stn, 7.) ;
run; 


proc sql;
 create table allnew as 
 select station,date,x,y,temperature_m,humidity,wind_speed_m,pressure_m,stype,area,source
   from allx;
quit;



/*divide all file into files based on year*/
option mprint;
%macro Year(year=);
data y&year (drop=c);
 set allnew;
  c = Year(date);
   if c = &year;
run;

%mend year;

%year(year=2000);
%year(year=2001);
%year(year=2002);
%year(year=2003);
%year(year=2004);
%year(year=2005);
%year(year=2006);
%year(year=2007);
%year(year=2008);
%year(year=2009); 
%year(year=2010); 
%year(year=2011); 




/*COMBINE WU AND NCDC DATA*/




/*import WU data*/


options mprint;
%macro import(year=);

PROC IMPORT OUT= Nemia_&year
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN002_WU yearly\NEMIA_&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


/*rename variable*/

Data Nemia_&year;
set Nemia_&year;
rename temperatur=tempc wind_speed=ws;
drop wind_direc;
run; 

Data y&year;
set y&year;
rename temperature_m=tempc wind_speed_m=ws;
drop wind_direc;
run; 

data met&year;
set y&year Nemia_&year;
run; 


/*to get rid of dupilcate stations that appear both in the MIA and NE 'WU' datasets*/

proc sort data = met&year; by station date;  run; 
data met&year; set met&year; by station date;
if first.date;
run;

PROC EXPORT DATA=  met&year
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



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


/*EXPORT A KEYTABLE OF ALL STATION LOCATION ACROSS ALL YEARS*/

data metKT;
set Met2000 Met2001 Met2002 Met2003 Met2004 Met2005 Met2006 Met2007 Met2008 Met2009 Met2010 Met2011;
run; 
 


proc summary nway data=metKT;
class station;
var x y;
output out=metgrid mean=x y;
run; 

PROC EXPORT DATA= metgrid
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\met_full_grid.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

