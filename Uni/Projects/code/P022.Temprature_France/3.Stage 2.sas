/*** NB: 349433 Grid Cells ***/

libname Monitor "Y:\France_LST\RAW\Monitors";
libname MODIS   "Y:\France_LST\Stage 1";
libname Stage2  "Y:\France_LST\Stage 2";

/***** Add Elevation and Perct Urban ****/


PROC IMPORT OUT = Elevation(drop = DTckin gridid)
            DATAFILE= "Y:\France_LST\LU\lu_variables\france_1st_stfc_elev.csv" 
			DBMS=CSV   REPLACE;
RUN; 


PROC IMPORT OUT = Pct_urban(drop = DTckin gridid)
            DATAFILE= "Y:\France_LST\LU\lu_variables\france_1st_stfc_pcturb.csv" 
			DBMS=CSV   REPLACE;
RUN; 

option mprint;
%macro csv;

%do year = 2000 %to 2011;

data Stage2.S2_temp_&year(keep = Longitude Latitude NTckin Date m);
 set Modis.Final_temp_&year;
  if T_night = . then delete;
  m = month(date);
run;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Elevation;             by Longitude Latitude; run;
proc sort data = Stage2.S2_temp_&year;  by Longitude Latitude; run;

data Stage2.E_Final_s2_&year;
 merge Elevation (in=a) Stage2.S2_temp_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete S2_temp_&year; run; quit;

%end;

%mend;

%csv;

option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Pct_urban;                by Longitude Latitude; run;
proc sort data = Stage2.E_Final_s2_&year;  by Longitude Latitude; run;

data Stage2.EP_Final_s2_&year;
 merge Pct_urban (in=a) Stage2.E_Final_s2_&year(in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete E_Final_s2_&year; run; quit;

%end;

%mend;

%csv;

/***** Add NDVI ****/

option mprint;
%macro csv;

%do year = 2001 %to 2011;

proc sort data = Modis.Final_NDVI_&year;    by Longitude Latitude m; run;
proc sort data = Stage2.Ep_Final_s2_&year;  by Longitude Latitude m; run;

data Stage2.EPN_Final_s2_&year;
 merge Modis.Final_NDVI_&year(in=a) Stage2.Ep_Final_s2_&year(in=b);
   by Longitude Latitude m; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete Ep_Final_s2_&year; run; quit;

%end;

%mend;

%csv;



option mprint;
%macro model(year=);

/* %do year = 2000 %to 2001; */

proc mixed data = Modis.EP_Final_s1_&year method=reml;
 class date;
  model tm = NTckin elev_m pcturb NDVI/  s ;
   random intercept NTckin / subject = date type = UN s;
   ods output solutionf = Fix_&year solutionr = Random_&year;
run;
quit;

data Fix_&year(keep = Effect Estimate); set Fix_&year; run;

proc transpose data = Fix_&year  out = Fix_&year(drop = _name_);  id Effect; run;

data Random_int(keep = date ran_int);
 set Random_&year;
  if Effect = "Intercept";
  ran_int = Estimate;
run; 

data Random_Slope(keep = date ran_slope);
 set Random_&year;
  if Effect = "NTckin";
  ran_slope = Estimate;
run; 

proc sort data = Random_int;   by date; run; quit;
proc sort data = Random_Slope; by date; run; quit;

data Random; 
 merge Random_Slope Random_int;
  by date;
run;
quit;

data Complete; merge Random Fix_&year; run;

data Complete(drop = NTckin--NDVI); 
 set Complete;
  fix_NTckin = NTckin;
  fix_NDVI   = NDVI;
  fix_Elev   = Elev_m;
  fix_Pcturb = Pcturb;
run;

PROC STANDARD DATA = Complete OUT = Param_&year REPLACE;
  VAR Intercept--fix_Pcturb;
RUN;

proc sort data = Param_&year; by date; run;

proc datasets lib = work; delete Random Complete Random_int Random_Slope Fix_&year Random_&year; run; 

proc sort data = Stage2.Epn_final_s2_&year; by date; run;

data Stage2.Pred_final_s2_&year; 
 merge Stage2.Epn_final_s2_&year (in=a) Param_&year (in = b);
  by date;
  if a;
run;
quit;

data Stage2.Pred_final_s2_&year(keep = Longitude Latitude m Date Pred);
 set Stage2.Pred_final_s2_&year;
  Pred = Intercept + (fix_NTckin*NTckin) + (fix_NDVI*NDVI) + (fix_Elev*elev_m) + (fix_Pcturb*Pcturb) + Ran_int + (Ran_slope*NTckin);
run;

/* %end; */

%mend;

%model(year = 2004);
%model(year = 2009);

