libname Monitors "Y:\France_LST\RAW\Monitors";

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
    INFILE 'C:\Users\FNORDIO\Desktop\NNDC Climate Data Online\Station.txt'
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

proc import datafile="Y:\France_LST\RAW\Monitors\&data..txt"
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

/*** Date ***/

c=substr(_YEARMODA,1,8);
m=substr(_YEARMODA,9,2);
d=substr(_YEARMODA,11,2);

Date = mdy(m,d,c); format Date date9.;

/*** date interval ***/

if "01jan2000"d <= Date <= "31dec2011"d;

USAF = Stn;

keep State USAF wban temp wdsp slp visib dewp Date;

run;

%mend;

%state(data = NOAA_France);


%macro monitor;
  
%do year = 2000 %to 2011;
   
data Monitors.Monitors&year;
 set Noaa_france;
   if "01JAN&year"d <= date <= "31DEC&year"d ;
run;

proc sort data = Station;                by USAF WBAN; run;
proc sort data = Monitors.Monitors&year; by USAF WBAN; run;

data Monitors.Monitors&year(drop = St start_date end_date);
 merge Monitors.Monitors&year (in = a) Station (in = b);
  by USAF WBAN;
  if a;
run;

proc sort data = Monitors.Monitors&year nodopukey out = Monitors.Station&Year(keep = USAF WBAN Name Lat Lon); by Lat Lon; run;

%end;

%mend;

%monitor;


/**** Creo la Serie completa *****/

data seriesj;
 input Date ddmmyy10. Value;
  format Date ddmmyy10.;
cards;
01/01/2000 1
31/12/2011 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;


option mprint;
%macro year(year = );

data daily_&year;
 set daily;
  if "01JAN&year"d <= date <= "31DEC&year"d;
run;


data OneWayFreqs_&year;
 set Monitors.Station&year; 
run;


data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set daily_&year;
     if _n_ = 1 then do;
        elenco = trim(left(Date));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(Date));
      elenco_new = elenco;
       call symputx("Lista",elenco_new);
      output;
     end;
run;

DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; 
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set daily_&year;
  where date = &date;
run;

data Daily&date;
  if _N_ = 1 then set Daily&date;
 set OneWayFreqs_&year;
run;

proc append base = Final_&year data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

%mend full;

%full(List = &Lista);

%mend;


%year(year = 2000);
%year(year = 2001);
%year(year = 2002);
%year(year = 2003);
%year(year = 2004);
%year(year = 2005);
%year(year = 2006);
%year(year = 2007);
%year(year = 2008);
%year(year = 2009);
%year(year = 2010);
%year(year = 2011);


option mprint;

%macro year(year = );

Data France_&year(keep = USAF WBAN TEMP--DATE Temp_C);
 set Noaa_france;
  if "01jan&year"d <= date <= "31dec&year"d;
   Temp_C = (TEMP - 32)*(5/9);
run;
quit;

proc sort data = France_&year;  by date USAF WBAN; run;
proc sort data = Final_&year;   by date USAF WBAN; run;

data Monitors.France_&year(keep = USAF WBAN Date Temp_C Name Lat Lon);
 merge France_&year(in=a) Final_&year(in=b);   
 by date USAF WBAN; 
 if b;
run;

proc datasets lib = work;     delete Final_&year OneWayFreqs_&year France_&year Daily_&year; run;quit;
proc datasets lib = Monitors; delete Monitors&year Station&year; run;quit;


%mend;

%year(year = 2000);
%year(year = 2001);
%year(year = 2002);
%year(year = 2003);
%year(year = 2004);
%year(year = 2005);
%year(year = 2006);
%year(year = 2007);
%year(year = 2008);
%year(year = 2009);
%year(year = 2010);
%year(year = 2011);

proc datasets lib = work; delete Daily Italy List Seriesj Station; run;quit;
