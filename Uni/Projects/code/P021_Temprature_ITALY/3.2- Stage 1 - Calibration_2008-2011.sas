/*** NB: 349433 Grid Cells ***/

libname Monitor "C:\Models\3.Data\New Monitors";
libname MODIS   "C:\Models\3.Data";
libname Italy   "C:\Models\3.Data";


PROC IMPORT OUT= Grid
            DATAFILE= "C:\Models\3.Data\Meteo NOAA + WU Italy\Full Grid Italy.dbf" 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 


/*** NB. 84 Stations (3 Stations without grid points) ***/


option mprint;
%macro csv;

%do year = 2008 %to 2011;

PROC IMPORT OUT = Grid_Station&year
            DATAFILE= "C:\Models\3.Data\Meteo NOAA + WU Italy\New Monitor - Grid\met&year..dbf" 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 


data Grid_Station&year(keep = Station Latitude Longitude Lat_Station Long_Station Distance);
 retain Station Lat_Station Long_Station Latitude Longitude Distance;
 set Grid_Station&year;
  format Latitude  16.4;
  format Longitude 16.4;
   Lat_Station  = Y;
   Long_Station = X;
    Distance = geodist(Latitude, Longitude, Lat_Station, Long_Station);
run;

proc freq data = Grid_Station&year;
 table Station;
 ods output onewayfreqs = Station&year(keep = Station);
run;

proc sort data = Station&year;        by Station; run;
proc sort data = Monitor.Italy_&year; by Station; run;

data Monitor.Italy_&year;
 merge Monitor.Italy_&year (in=a) Station&year (in=b);
  by Station;
  if b;
run;

%end;

%mend;

%csv;



/**** Join Station ****/

option mprint;
%macro csv;

%do year = 2008 %to 2011;

data Final_temp_&year;
 set Modis.Final_temp_&year;
  if T_Night = . and T_Day = . then delete;
run;
quit;

/*** Join Monitors ***/

proc sql;
  create table Modis.Temp_Station_&year as
   select *
    from Final_Temp_&year right join Grid_Station&year(rename=(Longitude=tLongitude) rename=(Latitude=tLatitude)) 
     on  Final_Temp_&year..Longitude = Grid_Station&year..tLongitude and 
         Final_Temp_&year..Latitude  = Grid_Station&year..tLatitude;
run;

proc datasets lib = work; delete Final_temp_&year; run;

data Temp_Station_&year;
 set Modis.Temp_Station_&year;
  drop tmpLatitude tmpLongitude tLatitude tLongitude;
   if initaly = . then delete;
run;

/*** Join Temperature from the Monitors ***/

proc sort data = Monitor.Italy_&year; by station date; run;
proc sort data = Temp_station_&year;  by station date; run;

data Modis.Final_St1_&year; 
 merge Monitor.Italy_&year(in=a) Temp_station_&year(in=b);
  by station date;
  if b;
run;

proc sort data = Modis.Final_St1_&year; by station date distance; run;

data Modis.Final_St1_&year;
  set Modis.Final_St1_&year;
  count + 1;
   by station date distance;
   if first.date then count = 1;
run;

data Modis.Final_St1_&year(drop = count);
 set Modis.Final_St1_&year;
  if count = 1;
run;

proc datasets lib = work; delete Temp_station_&year; run; quit;

%end;

%mend;

%csv;


/***** Add NDVI ****/

option mprint;
%macro csv;

%do year = 2008 %to 2011;

data Italy.Final_st1_&year;
 set Italy.Final_st1_&year;
  m = month(date);
run;
quit;

proc sort data = Italy.Final_ndvi_&year; by Longitude Latitude m; run; quit;
proc sort data = Italy.Final_st1_&year;  by Longitude Latitude m; run; quit;

%end;

%mend;

%csv;

option mprint;
%macro csv;

%do year = 2008 %to 2011;

data Italy.Final_s1_&year;
 merge Italy.Final_st1_&year (in=a) Italy.Final_ndvi_&year (in=b);
  by Longitude Latitude m; 
  if a;
run;
quit;

proc datasets lib = Italy; delete Final_st1_&year; run; quit;

%end;

%mend;

%csv;

/***** Add Elevation and Perct Urban, Pop Density, Koppen ****/


PROC IMPORT OUT = Elevation
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_elevv2.csv" 
			DBMS=CSV   REPLACE;
RUN; 


PROC IMPORT OUT = Pct_urban
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_pcturbv3.csv" 
			DBMS=CSV   REPLACE;
RUN; 

PROC IMPORT OUT = Pop_Dens
            DATAFILE= "C:\Models\3.Data\Land Use Variables\italy_modisfc_pop06v3.csv" 
			DBMS=CSV   REPLACE;
RUN; 

/*** Koppen ***/

PROC IMPORT OUT = Koppen(keep = Longitude Latitude regions_co waterflag)
            DATAFILE= "C:\Models\3.Data\Land Use Variables\waterflagcopen.dbf" 
			DBMS=dbf   REPLACE;
RUN; 

data Koppen; set Koppen;  format Latitude  best12.;  format Longitude best12.; run;

option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Elevation;             by Longitude Latitude; run;
proc sort data = Italy.Final_s1_&year;  by Longitude Latitude; run;

data Italy.E_Final_s1_&year;
 merge Elevation (in=a) Italy.Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Italy; delete Final_s1_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Pct_urban;               by Longitude Latitude; run;
proc sort data = Italy.E_Final_s1_&year;  by Longitude Latitude; run;

data Italy.EP_Final_s1_&year;
 merge Pct_urban (in=a) Italy.E_Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Italy; delete E_Final_s1_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Pct_urban;               by Longitude Latitude; run;
proc sort data = Italy.E_Final_s1_&year;  by Longitude Latitude; run;

data Italy.EP_Final_s1_&year;
 merge Pct_urban (in=a) Italy.E_Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Italy; delete E_Final_s1_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Pop_dens;                 by Longitude Latitude; run;
proc sort data = Italy.EP_Final_s1_&year;  by Longitude Latitude; run;

data Italy.EPD_Final_s1_&year(drop = modis_id);
 merge Pop_dens (in=a) Italy.EP_Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Italy; delete EP_Final_s1_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2008 %to 2011;

proc sort data = Koppen;                    by Longitude Latitude; run;
proc sort data = Italy.EPD_Final_s1_&year;  by Longitude Latitude; run;

data Italy.EPDK_Final_s1_&year(drop = modis_id);
 merge Koppen (in=a) Italy.EPD_Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Italy; delete EPD_Final_s1_&year; run; quit;

%end;

%mend;

%csv;


/**** Simple Check Stage 1 ****/

option mprint;
%macro csv(Type = );

%do year = 2000 %to 2007;

proc mixed data = Italy.Epdk_final_s1_&year method=reml;
 class date;
  model Temperature = &Type elev_m pcturb NDVI/  s outpred = Pred_&year;
   random intercept &Type / subject = date type = UN;
run;
quit;

ods trace on;
proc reg data = Pred_&year; 
 model Temperature = Pred;
  ods output ParameterEstimates = Parameter&year;
  ods output FitStatistics      = FitStatistics&year;
run;
quit;
ods trace off;

data Parameter&year(keep = Year Estimate StdErr);
retain Year Estimate StdErr;
 set Parameter&year;
  Year = &year;
  if Variable = "Pred";
run;
quit;

data FitStatistics&year(keep = Label1 nValue1 Label2 nValue2);
 set FitStatistics&year;
 if _n_ = 1;
run;

data A_Results&year;
 merge Parameter&year FitStatistics&year;
run;

proc append data = Results&year base = Results_&Type force; run; quit;

proc datasets lib = work; delete Parameter&year FitStatistics&year Results&year; run;

%end;

%mend;


%csv(Type = NTckin);


/**** SPATIAL R2 ****/

option mprint;
%macro csv;

%do year = 2000 %to 2007;

proc summary data = Pred_&year nway;
 class id_Station;
  Var Temperature Pred;
   output out = Summary&year mean(Temperature) = Temperature mean(Pred) = Pred;
run;


ods trace on;
proc reg data = Summary&year;
 model Temperature = Pred;
  ods output ParameterEstimates = Parameter&year;
  ods output FitStatistics      = FitStatistics&year;
run;
quit;
ods trace off;

data Parameter&year(keep = Year Estimate StdErr);
retain Year Estimate StdErr;
 set Parameter&year;
  Year = &year;
  if Variable = "Pred";
run;
quit;

data FitStatistics&year(keep = Label1 nValue1 Label2 nValue2);
 set FitStatistics&year;
 if _n_ = 1;
run;

data Spatial&year;
 merge Parameter&year FitStatistics&year;
run;

proc append data = Spatial&year base = A_Spatial_Final; run; quit;

proc datasets lib = work; delete Parameter&year FitStatistics&year Summary&year Spatial&year; run;


%end;

%mend;

%csv;
