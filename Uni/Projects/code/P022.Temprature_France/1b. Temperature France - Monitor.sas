libname Monitors "Y:\France_LST\RAW\Monitors";

/*** Data ***/

filename DIRLIST pipe 'dir "Y:\France_LST\LU\GIS\met data\" /b';

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

options noquotelenmax;
options mprint;
%macro readin (finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  
  ** Get the current filename;
  data _null_; set dirlist; if _n_ = &filen then call symput('filename', buffer); run;
  %put File Number &filen. is &filename.; ** Goes to log so we can check progress; 

***  Here is where you put in the proc import procedure for instance - the "meat";

data WORK.tmp&filen.;              
 INFILE "Y:\France_LST\LU\GIS\met data\&filename"
    LRECL=78
    ENCODING="WLATIN1"
    TERMSTR=CRLF
    DLM=';'
    MISSOVER
    DSD ;
    INPUT
        num_insee        
        latitude         
        longitude        
        date :DDMMYY10.            
        tm               
        tn               
        tx               
        tntxm            
        ffm              
        rr               
        um               
        un               
        ux               
        visib_max        
        visib_min        
		;

label num_insee = "id of the station";
label latitude  = "in degrees minutes seconds (2 numbers for the degrees, 2 numbers for the minutes and 2 numbers for the seconds)";
label longitude = "in degrees minutes seconds (2 numbers for the degrees, 2 numbers for the minutes and 2 numbers for the seconds)";
label date      = "Date";
label tm        = "daily average temperature (for automatic stations) (C°)";
label tn        = "daily minimum temperature (C°)";
label tx        = "daily maximum temperature (C°)";
label tntxm     = "daily average temperature for manual stations, ie (tn+tx)/2, which is not really an average temperature (C°)";
label ffm       = "daily average wind speed measured at 10 meters high (meter/second)";
label rr        = "daily cumulative rainfall (mm)";
label um        = "daily average relative humidity (%)";
label un        = "daily minimum relative humidity (%)";
label ux        = "daily maximum relative humidity (%)";
label visib_max = "daily duration with relative humidity<40% (minutes)";
label visib_min = "daily duration with relative humidity>80% (minutes)";

RUN;


data tmp&filen.;
 set tmp&filen.;
  keep num_insee--tx;
   format date date9.;
   if _N_  = 1 then delete;
   if date = . then delete; 
run;

  data tmp&filen.; set tmp&filen.; site = &filen.;  ** Adding an identifier variable;
  run;

****  End of programming with the individual files – the "meat";

** Concatenating the tmp datasets into the final SAS data file;
  %if &filen. = 1 %then %do; 
              data &finalfile; set tmp&filen.;            run; %end;
  %else %do;  data &finalfile; set &finalfile tmp&filen.; run; %end;

     proc datasets lib=work; delete tmp&filen.; run;

  %end; 
%mend;

****  Running the macro and defining the final data set name as here.testxls;

%readin(Misure);


data Monitors.Temperature;
 set Misure;
  year = year(date);
run;
quit;


proc summary data = Monitors.Temperature nway;
 class num_insee latitude longitude year;
  var tm;
    output out = Monitors.Summary N(tm) = N;
run;
quit;


%macro year;

%do year = 2000 %to 2011;

data Monitors.List&year(keep = num_insee Latitude Longitude year N);
 set Monitors.Summary;
  if year = &year;
  if  250 <= N <= 366;
run;

proc export data=Monitors.List&year
   outfile="Y:\France_LST\RAW\Monitors\Monitor_&year..csv"
   dbms=csv
   replace;
run;

%end;
%mend; 

%year;


%macro year;

%do year = 2000 %to 2011;

data Monitors.List&year(keep = num_insee id_&year);
 set Monitors.List&year;
  id_&year = num_insee;
run;

proc sort data = Monitors.List&year; by num_insee; run;

%end;
%mend; 

%year;

data Monitors.Monitor;
 merge Monitors.List2000 Monitors.List2001 Monitors.List2002 Monitors.List2003 Monitors.List2004 
       Monitors.List2005 Monitors.List2006 Monitors.List2007 Monitors.List2008 Monitors.List2009 
       Monitors.List2010 Monitors.List2011;
  by num_insee;
run;
quit;

proc datasets lib = monitors; 
 delete List2000 List2001 List2002 List2003 List2004 List2005 List2006 List2007 List2008 List2009 List2010 List2011;
run;
quit;

data Monitors.Monitor;
 set Monitors.Monitor;
  if id_2000 = . then delete;
  if id_2001 = . then delete;
  if id_2002 = . then delete;
  if id_2003 = . then delete;
  if id_2004 = . then delete;
  if id_2005 = . then delete;
  if id_2006 = . then delete;
  if id_2007 = . then delete;
  if id_2008 = . then delete;
  if id_2009 = . then delete;
  if id_2010 = . then delete;
  if id_2011 = . then delete;
run;

%macro year;

%do year = 2000 %to 2011;

proc import out = Monitors.DD&year
   datafile= "Y:\France_LST\RAW\Monitors\monitor_&year._dd.csv"
   dbms=csv
   replace;
run;

proc append base = Monitors.LatLon data = Monitors.DD&year; run;

proc datasets lib = Monitors;  delete DD&year; run;

%end;
%mend; 

%year;

proc sort data = monitors.latlon nodupkey out = monitors.latlon(keep = num_insee latdd longdd); by latdd longdd; run;

proc sort data = monitors.latlon;  by num_insee; run;
proc sort data = monitors.monitor; by num_insee; run;

data monitors.Monitor_Final(drop = id_2000 -- id_2011);
 merge monitors.monitor (in = a) monitors.latlon (in=b);
  by num_insee; 
  if a;
run;

data monitors.Monitor_Final;
 set monitors.Monitor_Final;
  if latdd = . then delete;
run;

proc datasets lib = monitors; 
 delete Latlon Summary Monitor;
run;
quit;

proc sort data = Monitors.Temperature;     by num_insee; run;
proc sort data = monitors.Monitor_Final;   by num_insee; run;

data Monitors.Temp_Final;
 merge Monitors.Temperature(in=a)  monitors.Monitor_Final (in=b); 
  by num_insee; 
  if b;
run;

%macro year;

%do year = 2000 %to 2011;

data Monitors.Temp_&year(drop = tn--site);
  set Monitors.Temp_Final;
   if year = &year;
run;

%end;
%mend; 

%year;

%macro year;

%do year = 2000 %to 2011;

proc sort data = Monitors.Temp_&year out = Monitors.Station&year(keep = num_insee latdd longdd) nodupkey; by num_insee; run;

%end;
%mend; 

%year;


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

proc sort data = Monitors.Temp_&year;  by date num_insee; run;
proc sort data = Final_&year;          by date num_insee; run;

data Monitors.France_&year(keep = num_insee Date tm Name latdd longdd);
 merge Monitors.Temp_&year(in=a) Final_&year(in=b);   
 by date num_insee; 
 if b;
run;

proc datasets lib = work;     delete Final_&year OneWayFreqs_&year France_&year Daily_&year; run;quit;
proc datasets lib = Monitors; delete Temp_&year Station&year; run;quit;


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
