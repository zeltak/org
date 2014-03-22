
libname Monitor "Y:\France_LST\RAW\Monitors";
libname MODIS   "Y:\France_LST\Stage 1";


PROC IMPORT OUT = Grid_Station/*(keep = Longitude Latitude Distance num_insee latdd longdd)*/
            DATAFILE= "Y:\France_LST\LST_within1km_stn.dbf" 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data Grid_Station(keep = num_insee Longitude Latitude Lat_Station Long_Station Distance);
 set Grid_Station;
  format Latitude  16.4;
  format Longitude 16.4;
  format num_insee Best12.;
  
   Lat_Station  = Latdd;
   Long_Station = Longdd;
   Distance = geodist(Latitude, Longitude, Lat_Station, Long_Station);

run;

proc freq data = Grid_Station;
 table num_insee;
 ods output onewayfreqs = onewayfreqs(keep = num_insee);
run;
quit;

/*** NB. 792 Stations in Total ***/

option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc sort data = Monitor.France_&year; by num_insee; run;
proc sort data = onewayfreqs;          by num_insee; run;

data Monitor.France_&year._a;
 merge Monitor.France_&year (in = a) onewayfreqs (in = b);
  by num_insee;
  if b;
run;
quit;

proc summary data = Monitor.France_&year._a nway;
 class num_insee;
  var tm;
  output out = monitor.summary&year mean(tm) = tm N(tm) = N;
run;
quit;


proc datasets lib = Monitor; delete France_&year; run; quit;

%end;

%mend;

%csv;



/**** Join Station ****/

option mprint;
%macro csv;

%do year = 2000 %to 2011;

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

proc sort data = Monitor.France_&year._a; by num_insee date; run;
proc sort data = Temp_station_&year;      by num_insee date; run;

data Modis.Final_St1_&year; 
 merge Monitor.France_&year._a(in=a) Temp_station_&year(in=b);
  by num_insee date;
  if b;
run;

proc sort data = Modis.Final_St1_&year; by num_insee date Distance; run;

data Modis.Final_St1_&year;
  set Modis.Final_St1_&year;
  count + 1;
   by num_insee date Distance;
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
  model tm = &Type elev_m pcturb NDVI/  s outpred = Pred_&year;
   random intercept &Type / subject = date type = UN;
run;
quit;

ods trace on;
proc reg data = Pred_&year; 
 model tm  = Pred;
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

%csv(Type = NTckin);


/**** SPATIAL R2 ****/

option mprint;
%macro csv;

%do year = 2000 %to 2011;

proc summary data = Pred_&year nway;
 class num_insee;
  Var tm  Pred;
   output out = Summary&year mean(tm) = tm mean(Pred) = Pred;
run;


ods trace on;
proc reg data = Summary&year;
 model tm = Pred;
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



