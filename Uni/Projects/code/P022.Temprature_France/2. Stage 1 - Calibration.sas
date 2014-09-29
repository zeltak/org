/*** NB: 349433 Grid Cells ***/

libname Monitor "Y:\France_LST\RAW\Monitors";
libname MODIS   "Y:\France_LST\Stage 1";


/*** NB. 84 Stations (3 Stations without grid points ***/

PROC IMPORT OUT = Grid_Station(keep = Latitude Longitude Lat Lon USAF WBAN Name Distance)
            DATAFILE= "Y:\France_LST\Monitor_Grid_Stage1.dbf" 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data Grid_Station(drop = Lat Lon);
 set Grid_Station;
  format Latitude  16.4;
  format Longitude 16.4;
  format WBAN Best12.;
  format USAF Best12.;
  
   Lat_Station  = Lat;
   Long_Station = Lon;

run;


/**** Join Station ****/

option mprint;
%macro csv;

%do year = 2008 %to 2009;

data Final_temp_&year;
 set Modis.Final_temp_&year;
  if T_Night = . and T_Day = . then delete;
run;
quit;

/*** Join Monitors ***/

proc sql;
  create table Modis.Temp_Station_&year as
   select *
    from Final_Temp_&year right join Grid_Station(rename=(Longitude=tLongitude) rename=(Latitude=tLatitude)) 
     on  Final_Temp_&year..Longitude = Grid_Station.tLongitude and 
         Final_Temp_&year..Latitude  = Grid_Station.tLatitude;
run;

proc datasets lib = work; delete Final_temp_&year; run;

data Temp_Station_&year;
 set Modis.Temp_Station_&year;
  drop tmpLatitude tmpLongitude tLatitude tLongitude;
   if inFrance = . then delete;
run;

/*** Join Temperature from the Monitors ***/

proc sort data = Monitor.France_&year; by WBAN USAF date; run;
proc sort data = Temp_station_&year;   by WBAN USAF date; run;

data Modis.Final_St1_&year; 
 merge Monitor.France_&year(in=a) Temp_station_&year(in=b);
  by WBAN USAF date;
  if b;
run;

proc sort data = Modis.Final_St1_&year; by WBAN USAF date Distance; run;

data Modis.Final_St1_&year;
  set Modis.Final_St1_&year;
  count + 1;
   by WBAN USAF date Distance;
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

%do year = 2000 %to 2011;

data Modis.Final_st1_&year;
 set Modis.Final_st1_&year;
  m = month(date);
run;
quit;

proc sort data = Modis.Final_ndvi_&year; by Longitude Latitude m; run; quit;
proc sort data = Modis.Final_st1_&year;  by Longitude Latitude m; run; quit;

%end;

%mend;

%csv;

option mprint;
%macro csv;

%do year = 2000 %to 2011;

data Modis.Final_s1_&year;
 merge Modis.Final_st1_&year (in=a) Modis.Final_ndvi_&year (in=b);
  by Longitude Latitude m; 
  if a;
run;
quit;

proc datasets lib = Modis; delete Final_st1_&year; run; quit;

%end;

%mend;

%csv;

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

proc sort data = Elevation;             by Longitude Latitude; run;
proc sort data = Modis.Final_s1_&year;  by Longitude Latitude; run;

data Modis.E_Final_s1_&year;
 merge Elevation (in=a) Modis.Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Modis; delete Final_s1_&year; run; quit;

%end;

%mend;

%csv;


option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Pct_urban;               by Longitude Latitude; run;
proc sort data = Modis.E_Final_s1_&year;  by Longitude Latitude; run;

data Modis.EP_Final_s1_&year;
 merge Pct_urban (in=a) Modis.E_Final_s1_&year (in=b);
   by Longitude Latitude; 
   if b;
run;
quit;

proc datasets lib = Modis; delete E_Final_s1_&year; run; quit;

%end;

%mend;

%csv;


/**** Simple Check Stage 1 ****/

option mprint;
%macro csv(Type = );

%do year = 2000 %to 2011;

proc mixed data = Modis.Ep_Final_s1_&year method=reml;
 class date;
  model Temp_C = &Type elev_m pcturb NDVI/  s outpred = Pred_&year;
   random intercept &Type / subject = date type = UN;
run;
quit;

ods trace on;
proc reg data = Pred_&year; 
 model Temp_C = Pred;
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

data Results&year;
 merge Parameter&year FitStatistics&year;
run;

proc append data = Results&year base = Results_&Type force; run; quit;

proc datasets lib = work; delete Parameter&year FitStatistics&year Results&year; run;

%end;

%mend;

%csv(Type = DTckin);
%csv(Type = NTckin);


/**** SPATIAL R2 ****/

option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc summary data = Pred_&year nway;
 class WBAN USAF;
  Var Temp_C Pred;
   output out = Summary&year mean(Temp_C) = Temp_C mean(Pred) = Pred;
run;


ods trace on;
proc reg data = Summary&year;
 model Temp_C = Pred;
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

proc append data = Spatial&year base = Spatial_Final; run; quit;

proc datasets lib = work; delete Parameter&year FitStatistics&year Summary&year Spatial&year; run;


%end;

%mend;

%csv;

/**** Import All available data from MODIS *****

/*** Create Season ***/

data Final_temp_2001;
 set Modis.Final_temp_2001;
  Month = month(date);
	if Month in (3,4,5)   then Season = "Spring";
	if Month in (6,7,8)   then Season = "Summer";
	if Month in (9,10,11) then Season = "Autumn";
	if Month in (12,1,2)  then Season = "Winter";
run;
quit;

/**** Create the Dataset with the missing ***/

data Modis.Final_temp_2001_no_miss;
 set Final_temp_2001;
  if T_Night = . then delete;
run;

option mprint;
%macro month;

%do i = 1 %to 12;
 
data month_&i._score;
 set Modis.Final_temp_2001_no_miss;
  if Month = &i;
run;   

data month_&i;
 set month_&i._score;
  if T_Day = . then delete;
run; 
 
%end;

%mend;

%month;

/**** Export CSV ****/

option mprint;
%macro csv;

%do i = 1 %to 12;
 
proc export
  data = month_&i
  outfile="C:\Users\FNORDIO\Desktop\Input Method Temperature\month_&i..csv"
  dbms=csv
  replace;
run;
 
proc export
  data = month_&i._score
  outfile="C:\Users\FNORDIO\Desktop\Input Method Temperature\month_&i._score.csv"
  dbms=csv
  replace;
run;

%end;

%mend;

%csv;

