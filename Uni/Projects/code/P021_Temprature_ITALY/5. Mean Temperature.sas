libname Monitor "C:\Models\3.Data\Meteo NOAA + WU Italy";
libname MODIS   "C:\Models\3.Data";
libname Stage2  "C:\Models\3.Data\Stage 2 Datasets";

proc printto log="nul:"; run;

PROC IMPORT OUT= Buffer60km
  DATAFILE= "C:\Models\60km Buffer.dbf" 
    DBMS=DBF   REPLACE;
     GETDELETED=NO;
RUN; 

data Buffer60km;
 set Buffer60km;
  keep id_station Latitude Longitude;
run;


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2000 1
31/12/2011 1
run;

/*creates the completed time series for above range*/
/*the output file is 'daily'*/

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

/*create a list of dates for cycle-first type macro*/
%macro year;

%do year = 2000 %to 2011;

data Daily&year;
 set Daily;
  if "01JAN&year"d <= date <= "31DEC&year"d;
run;

data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set Daily&year;
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

%let j=1;

%do %while (%scan(&Lista,&j) ne);
 %let date = %scan(&Lista,&j);

data Daily&date;
 set Monitor.Unico&year;
  where date = &date;
run;

proc sort data      = Daily&date;                  by id_station; run;
proc transpose data = Daily&date  out = Transpose; by id_station; run;

data Transpose(drop = _NAME_ );
 set Transpose;
  if _NAME_ = "Temperature";
run;

data DATA3;
 merge Buffer60km(in=a) Transpose(in=b);
  by id_station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;


proc summary nway data=DATA3;
 class Longitude Latitude;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date Longitude Latitude Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = Monitor.Final60kmet&year data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;

%end;

%mend;

%year;

proc sort data = Buffer60km; by id_station; run;




/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Monitor.Unico2000;
  where date = &date;
run;

proc sort data      = Daily&date;                  by id_station; run;
proc transpose data = Daily&date  out = Transpose; by id_station; run;

data Transpose(drop = _NAME_ );
 set Transpose;
  if _NAME_ = "Temperature";
run;

data DATA3;
 merge Buffer60km(in=a) Transpose(in=b);
  by id_station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;


proc summary nway data=DATA3;
 class Longitude Latitude;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date Longitude Latitude Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = Monitor.Final60kmet2000 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);


option mprint;
%macro model;

%do year = 2000 %to 2011;

data Monitor.Final60kmet&year;
 set Monitor.Final60kmet&year;
  format Longitude Best12.; 
  format Latitude  Best12.; 
run;
quit;

%end;

%mend;

%model;



option mprint;
%macro model;

%do year = 2000 %to 2011;

proc sort data = Monitor.Final60kmet&year;   by Longitude Latitude date; run;
proc sort data = Stage2.Pred_final_s2_&year; by Longitude Latitude date; run;

data Stage2.EPNT_final_s2_&year; 
 merge Stage2.Pred_final_s2_&year (in=a) Monitor.Final60kmet&year (in=b);
  by Longitude Latitude date; 
  if a;
run;

%end;

%mend;

%model;

option mprint;
%macro model;

%do year = 2000 %to 2011;

data Stage2.EPNT_final_s2_&year(drop = xnym ynym xnymx ynymx); 
 set Stage2.EPNT_final_s2_&year; 
  if TEST_AVE = . then delete;
  if Pred     = . then delete;
   xnym=put(Longitude,Best12.);
   ynym=put(Latitude, Best12.);
   xnymx = xnym*10000;
   ynymx = ynym*10000;
   guid = compress(xnymx||ynymx); 
run;

%end;

%mend;

%model;



/****** Stage 3 ***********/

/*YEAR 2008*/

proc printto log="C:\Models\2.Code\2.Gather_data\filename.log"; run;

libname Stage3 "C:\Models\3.Data\Stage 3 Datasets";
ods listing close;*to suppress the output printing;

%macro year;

%do year = 2000 %to 2011;

proc sort data = Stage2.Epnt_final_s2_&year; by Longitude Latitude; run;

proc mixed data= Stage2.Epnt_final_s2_&year  method = reml;
 model Pred =  Test_Ave /s ;
  ods output Solutionf = Stage3.solutionf&year;
   by Longitude Latitude;
run;
quit;


data Intercept(keep = Longitude Latitude Intercept); set Stage3.solutionf&year; if effect = "Intercept";
 Intercept = Estimate;
run;

data Slope(keep = Longitude Latitude Slope);         set Stage3.solutionf&year; if effect = "TEST_AVE";
 Slope = Estimate;
run;

proc sort data = Intercept; by Longitude Latitude;run;
proc sort data = Slope;     by Longitude Latitude;run;

data Unique; 
 merge Intercept Slope;
  by Longitude Latitude;
run;
quit;

proc sort data = Unique;                by Longitude Latitude;run;
proc sort data = Stage2.Grid_day&year;  by Longitude Latitude;run;

data Stage3.Final&year;
merge Unique(in=a) Stage2.Grid_day&year(in=b);
  by Longitude Latitude;
   if a;
run; 

%end;

%mend;

%year;


%macro year;

%do year = 2000 %to 2011;


proc sort data = Stage3.Final&year;         by Longitude Latitude;run;
proc sort data = Monitor.Final60kmet&year;  by Longitude Latitude;run;

data Stage3.Final&year;
 merge Stage3.Final&year (in=a) Monitor.Final60kmet&year (in=b);
  by Longitude Latitude;
  if a;
run;
quit;


proc sort data = Stage3.Final&year;   by Longitude Latitude Date; run;
proc sort data = Stage2.Pred_final_s2_&year; by Longitude Latitude Date; run;
 
data Stage3.Final&year;  
 merge Stage2.Pred_final_s2_&year (in=a) Stage3.Final&year (in=b);
  by Longitude Latitude Date;
  if b;
run;
quit;


data Stage3.Final&year(keep = Longitude Latitude Date Pred_New);  
 set Stage3.Final&year;  
  if   Pred = . then Pred_new = (Intercept + Slope*(Test_Ave));
  else Pred_new = Pred;
run;
quit;

%end;

%mend;

%year;


%macro year;

%do year = 2000 %to 2011;

proc summary data = stage3.Final&year nway;
 class Longitude Latitude;
 var Pred_new;
   output out = Summary&year(drop=_Type_ _Freq_) mean(Pred_New) = Pred_New N(Pred_New)= N;
run;
quit;

%end;

%mend;

%year;


%macro year;

%do year = 2000 %to 2011;

proc univariate data = Summary&year;
title "Histogram &year";
 var Pred_New;
  histogram Pred_New;
run;
quit;
%end;

%mend;

%year;

/**** Clean Water ****/

PROC IMPORT OUT = Koppen(keep = Longitude Latitude regions_co waterflag)
            DATAFILE= "C:\Models\3.Data\Land Use Variables\waterflagcopen.dbf" 
			DBMS=dbf   REPLACE;
RUN; 

data Koppen; set Koppen;  format Latitude  best12.;  format Longitude best12.; run;

proc sort data = Koppen;   by Longitude Latitude; run;


%macro year;

%do year = 2000 %to 2011;

proc sort data = Summary&year;   by Longitude Latitude; run;

data Summary&year;
 merge Summary&year (in = a) Koppen (in=b);
  by Longitude Latitude;
  if a;
run;

%end;

%mend;

%year;

%macro year;

%do year = 2000 %to 2011;

data Summary&year;
 set Summary&year;
  if waterflag = 1 then delete;
run;

%end;

%mend;

%year;

%macro year;

%do year = 2000 %to 2011;

PROC EXPORT 
	    DATA=Summary&year 
 	    OUTFILE="C:\Users\FNORDIO\Desktop\maps\Final Maps\Final_Map&year"
 	    DBMS=dbf
	    REPLACE;
run;
Quit;

%end;

%mend;

%year;
