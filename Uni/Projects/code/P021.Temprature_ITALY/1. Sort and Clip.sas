libname Italy "C:\Models\3.Data";

/**** Import the Clipped Grid ****/

proc import
   out = Italy.Italy_Grid 
    datafile="C:\Models\3.data\modis\Clipped_Grid.dbf"
	replace;
run;


/*imports the Temperature matlab output*/

option mprint;
%macro import(year = );

data T1_&year;
 infile "C:\Models\3.data\modis\T&year\Output\OutputMergedh18v04.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T2_&year;
 infile "C:\Models\3.data\modis\T&year\Output\OutputMergedh18v05.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T3_&year;
 infile "C:\Models\3.data\modis\T&year\Output\OutputMergedh19v04.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T4_&year;
 infile "C:\Models\3.data\modis\T&year\Output\OutputMergedh19v05.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;


data Italy.Italy_Temp&year(drop = Day Night Jdate Reference);

 set T1_&year T2_&year T3_&year T4_&year;

  T_Day   = Day   - 273.15; 
  T_Night = Night - 273.15;

   DTckin =  T_Day  / (Emis**0.25);
   NTckin =  T_Night/ (Emis**0.25);

    jdate = reference*1;
    DATE  = datejul(jdate);

  format DATE  date9.;
run;

/***  Like merge with (in = b) ***/

proc sql;
  create table Italy.Final_Temp_&year as
   select *
    from Italy.Italy_Temp&year right join Italy.Italy_Grid(rename=(Longitude=tmpLongitude) rename=(Latitude=tmpLatitude)) 
     on  Italy_Temp&year..Longitude = Italy_Grid.tmpLongitude and 
         Italy_Temp&year..Latitude  = Italy_Grid.tmpLatitude;
run;

proc datasets lib=Italy; delete Italy_Temp&year; run; quit;
proc datasets lib=work;  delete T1_&year T2_&year T3_&year T4_&year; run; quit;

%mend;

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

/**************/
/**** NDVI ****/
/**************/

libname NDVI "Y:\France_LST\Stage 1";

option mprint;
%macro import(year = );

data Italy.NDVI_&year(drop = Lat Long Reference jdate lat_ndvi long_ndvi);
 set NDVI.NDVI;
 if year = &year;
  Latitude  = Lat;
  Longitude = Long;
run;

/***  Like merge with (in = b) ***/

proc sql;
  create table Italy.Final_NDVI_&year as
   select *
    from Italy.NDVI_&year right join Italy.Italy_Grid(rename=(Longitude=tmpLongitude) rename=(Latitude=tmpLatitude)) 
     on  NDVI_&year..Longitude = Italy_Grid.tmpLongitude and 
         NDVI_&year..Latitude  = Italy_Grid.tmpLatitude;
run;

data Italy.Final_NDVI_&year; 
 set Italy.Final_NDVI_&year;
  drop tmpLatitude tmpLongitude initaly Date Year;
run;

proc datasets lib = Italy; delete NDVI_&year; run; quit;

%mend;

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

