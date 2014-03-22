/*** NB: 349433 Grid Cells ***/

libname Monitor "C:\Models\3.Data\Meteo NOAA + WU Italy";
libname MODIS   "C:\Models\3.Data";
libname Stage2  "C:\Models\3.Data\Stage 2 Datasets";

/****************************************/ 
/*creates the complete time series range*/
/****************************************/

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


%macro year;

%do year = 2000 %to 2011;

/*create a list of dates for cycle-first type macro*/

data Daily&year;
 set Daily;
   if "01jan&year"d <= date <= "31dec&year"d;
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

/***** Macro AOD *********/

/* X is Longitude, Y is Latitude */

options mprint;

%macro DATA(DataList = );

%let j=1;

%do %while (%scan(&DataList,&j) ne);
 %let data = %scan(&DataList,&j);

data guid&data;
 set Modis.Italy_grid;
 date = &data;
   format date date9.;
run;

proc append base = Stage2.Grid_Day&year 
            data = guid&data;
run;

proc datasets lib = work; delete guid&data; run;

%let j=%eval(&j+1);
%end;

%mend DATA;

%DATA(DataList = &Lista);

%end;

%mend;

%year;

/*** 1029 Grid Cells close to the water! ***/

%macro year;

%do year = 2000 %to 2011;

data Stage2.St2_&year(keep = Longitude Latitude Date NTckin);
 set Modis.Final_temp_&year;
 if T_Night = . then delete;
run;

%end;

%mend;

%year;


/*** 1029 Grid Cells close to the water! ***/

%macro year;

%do year = 2000 %to 2011;

data Stage2.St2_&year(keep = Longitude Latitude Date T_Night Emis);
 set Modis.Final_temp_&year;
 if T_Night = . then delete;
run;

%end;

%mend;

%year;


/***** Add Elevation and Perct Urban ****/


PROC IMPORT OUT = Elevation
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_elevv2.csv" 
			DBMS=CSV   REPLACE;
RUN; 


PROC IMPORT OUT = Pct_urban
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_pcturbv3.csv" 
			DBMS=CSV   REPLACE;
RUN; 


option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Elevation;         by Longitude Latitude; run;
proc sort data = Stage2.St2_&year;  by Longitude Latitude; run;

data Stage2.E_Final_s2_&year;
 merge Elevation (in=a) Stage2.St2_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete St2_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Pct_urban;                by Longitude Latitude; run;
proc sort data = Stage2.E_Final_s2_&year;  by Longitude Latitude; run;

data Stage2.EP_Final_s2_&year;
 merge Pct_urban (in=a) Stage2.E_Final_s2_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete E_Final_s2_&year; run; quit;

%end;

%mend;

%csv;

/****************************************/
/************* NDVI *********************/
/****************************************/


option mprint;
%macro csv;

%do year = 2000 %to 2011;

data Stage2.Ep_final_s2_&year;
 set Stage2.Ep_final_s2_&year;
  m = month(date);
run;

proc sort data = Modis.Final_ndvi_&year;    by Longitude Latitude m; run;
proc sort data = Stage2.EP_Final_s2_&year;  by Longitude Latitude m; run;

data Stage2.EPN_Final_s2_&year;
 merge Modis.Final_ndvi_&year (in=a) Stage2.EP_Final_s2_&year (in=b);
   by Longitude Latitude m; 
   if b;
run;
quit;

proc datasets lib = Stage2; delete EP_Final_s2_&year; run; quit;

%end;

%mend;

%csv;

/********************************************/
/********** PopDens, Water ******************/
/********************************************/

PROC IMPORT OUT = Pop_Dens(drop = modis_id)
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_pop06v3.csv" 
			DBMS=CSV   REPLACE;
RUN; 

PROC IMPORT OUT = Koppen(keep = Longitude Latitude regions_co waterflag)
            DATAFILE= "C:\Models\3.Data\Land Use Variables\waterflagcopen.dbf" 
			DBMS=dbf   REPLACE;
RUN; 

data Koppen; set Koppen;  format Latitude  best12.;  format Longitude best12.; run;

proc sort data = Koppen;   by Longitude Latitude; run;
proc sort data = Pop_Dens; by Longitude Latitude; run;

data Koppen_Dens;
 merge Koppen Pop_Dens;
  by Longitude Latitude; 
run;

/************************************************/
/******* Predict Using Mod1 Prediction **********/
/************************************************/


option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Stage2.Epn_final_s2_&year; by Longitude Latitude; run;
proc sort data = Koppen_Dens;               by Longitude Latitude; run;

data Stage2.EPNK_final_s2_&year;
 merge Stage2.EPN_final_s2_&year(in=a) Koppen_Dens(in=b); 
  by Longitude Latitude;
  if a;
run;

proc datasets lib = Stage2; delete EPN_Final_s2_&year; run; quit;

%end;

%mend;

%csv;





option mprint;
%macro model;

%do year = 2008 %to 2011;

proc mixed data = Modis.EPDK_Final_s1_&year method=reml;
 class date;
  model Temperature = NTckin elev_m pcturb NDVI/  s ;
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

data Stage2.Pred_final_s2_&year(drop = modis_id); 
 merge Stage2.Epn_final_s2_&year (in=a) Param_&year (in = b);
  by date;
  if a;
run;
quit;

data Stage2.Pred_final_s2_&year(keep = Longitude Latitude m Date Pred);
 set Stage2.Pred_final_s2_&year;
 NTckin =  T_Night/ (Emis**0.25);
  Pred = Intercept + (fix_NTckin*NTckin) + (fix_NDVI*NDVI) + (fix_Elev*elev_m) + (fix_Pcturb*Pcturb) + Ran_int + (Ran_slope*NTckin);
run;

%end;

%mend;

%model;



option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Modis.Epdk_final_s1_&year nodupkey out=Station_&year(keep = Station Lat_Station Long_Station); by Lat_Station Long_station; run;

proc summary data = Stage2.Pred_final_s2_&year nway;
  class Longitude Latitude;
   var Pred;
    output out = Map&year mean(Pred) = Pred;
run;


proc export data = Map&year 
 dbms = DBF outfile = "C:\Users\FNORDIO\Desktop\Map Stage2\Map&year..dbf"
 replace;
run;
quit;

proc export data = Station_&year 
 dbms = DBF outfile = "C:\Users\FNORDIO\Desktop\Map Stage2\Station_&year..dbf"
 replace;
run;
quit;

%end;

%mend;

%csv;


