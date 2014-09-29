libname Monitor "Y:\France_LST\RAW\Monitors";
libname MODIS   "Y:\France_LST\Stage 1";
libname Stage2  "Y:\France_LST\Stage 2";

proc printto log="nul:"; run;

PROC IMPORT OUT= Buffer60km
  DATAFILE= "Y:\France_LST\bubles.dbf" 
    DBMS=DBF   REPLACE;
     GETDELETED=NO;
RUN; 

data Buffer60km;
 set Buffer60km;
  keep num_insee Latitude Longitude;
   format num_insee best12.;
   format Latitude  best12.;
   format Longitude best12.;
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

%do year = 2003 %to 2011;

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
 set Monitor.France_&year._a;
  where date = &date;
run;

proc sort data      = Daily&date;                  by num_insee; run;
proc transpose data = Daily&date  out = Transpose; by num_insee; run;

data Transpose(drop = _NAME_ );
 set Transpose;
  if _NAME_ = "tm";
run;

data DATA3;
 merge Buffer60km(in=a) Transpose(in=b);
  by num_insee;
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

proc sort data      = Daily&date;                  by num_insee; run;
proc transpose data = Daily&date  out = Transpose; by num_insee; run;

data Transpose(drop = _NAME_ );
 set Transpose;
  if _NAME_ = "Temperature";
run;

data DATA3;
 merge Buffer60km(in=a) Transpose(in=b);
  by num_insee;
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

%do year = 2006 %to 2011;

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


proc printto log="nul:"; run;

libname Stage3 "Y:\France_LST\Stage 3";

ods listing close;*to suppress the output printing;


option mprint;
%macro model(year =);

proc sort data = stage2.Epnt_final_s2_&year; by Longitude Latitude; run;

proc mixed data= stage2.Epnt_final_s2_&year  method = reml;
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

data stage3.Unique&year; 
 merge Intercept Slope;
  by Longitude Latitude;
run;
quit;


%mend;

%model(year= 2011);


option mprint;
%macro model;

%do year = 2000 %to 2011;

proc sort data = Stage3.Unique&year;        by Longitude Latitude;run;
proc sort data = Monitor.Final60kmet&year;  by Longitude Latitude;run;

data Stage3.Final&year;
merge Stage3.Unique&year(in=a) Monitor.Final60kmet&year(in=b);
  by Longitude Latitude;
   if b;
run; 

data Stage3.Final&year(keep = Longitude Latitude Date Pred_New);  
 set Stage3.Final&year;  
  Pred_new = (Intercept + Slope*(Test_Ave));
run;
quit;

%end;

%mend;

%model;



/**** Add Pred 2 when Available ****/


option mprint;
%macro model;

%do year = 2000 %to 2011;

proc sort data = Stage3.Final&year;           by Longitude Latitude date;run;
proc sort data = Stage2.Pred_final_s2_&year;  by Longitude Latitude date;run;

data Stage3.S_Final&year;
merge Stage3.Final&year(in=a) Stage2.Pred_final_s2_&year(in=b);
  by Longitude Latitude date;
   if a;
run; 

data Stage3.S_Final&year;
 set Stage3.S_Final&year;
  Final_Pred = Pred;
  if Pred = . Then Final_Pred = Pred_new;
run;

%end;

%mend;

%model;

proc summary data = Stage3.S_final2003 nway;
 class Longitude Latitude;
  var Final_Pred;
    output out = summary2003(drop=_type_ _freq_) mean(Final_Pred) = Final_Pred 
                                             n(Final_Pred) = N; 
run;
quit;

proc summary data = Stage3.S_final2007 nway;
 class Longitude Latitude;
  var Final_Pred;
    output out = summary2007(drop=_type_ _freq_) mean(Final_Pred) = Final_Pred 
                                             n(Final_Pred) = N; 
run;
quit;

PROC IMPORT OUT= Water
  DATAFILE= "Y:\France_LST\lu_water.dbf" 
    DBMS=DBF   REPLACE;
     GETDELETED=NO;
RUN; 

data Water(keep = Longitude Latitude Waterflag);
 set Water;
  format Longitude Best12.; 
  format Latitude  Best12.; 
run;
quit;

proc sort data = Water; by longitude latitude; run;



option mprint;
%macro model;

%do year = 2000 %to 2011;

proc sort data = Water;                by Longitude Latitude;run;
proc sort data = Stage3.S_final&year;  by Longitude Latitude;run;

data Stage3.Clean_final&year(keep = Date Longitude Latitude Final_Pred waterflag);
 merge Stage3.S_final&year (in=a) water (in=b);
  by Longitude Latitude;
   if b;
run; 

data Stage3.Clean_final&year;
 merge Stage3.Clean_final&year;
  if waterflag = 0;
run; 


%end;

%mend;

%model;
