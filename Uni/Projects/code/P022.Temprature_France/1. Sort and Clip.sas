libname France "Y:\France_LST\Stage 1";

option mprint;
%macro import(year = );

data T1_&year;
 infile "Y:\France_LST\RAW\output&year\OutputMergedh17v03.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T2_&year;
 infile "Y:\France_LST\RAW\output&year\OutputMergedh17v04.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T3_&year;
 infile "Y:\France_LST\RAW\output&year\OutputMergedh18v03.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;

data T4_&year;
 infile "Y:\France_LST\RAW\output&year\OutputMergedh18v04.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Latitude Longitude Day Night Emis Reference;
run;


data France.France_Temp&year(drop = Day Night Jdate Reference);

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
  create table France.Final_Temp_&year as
   select *
    from France.France_Temp&year right join France.Clipped(rename=(Longitude=tmpLongitude) rename=(Latitude=tmpLatitude)) 
     on  France_Temp&year..Longitude = Clipped.tmpLongitude and 
         France_Temp&year..Latitude  = Clipped.tmpLatitude;
run;

proc datasets lib=France; delete France_Temp&year; run; quit;
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

data France.NDVI_&year(drop = Lat Long Reference jdate lat_ndvi long_ndvi);
 set NDVI.NDVI;
 if year = &year;
  Latitude  = Lat;
  Longitude = Long;
run;

/***  Like merge with (in = b) ***/

proc sql;
  create table France.Final_NDVI_&year as
   select *
    from France.NDVI_&year right join France.Clipped(rename=(Longitude=tmpLongitude) rename=(Latitude=tmpLatitude)) 
     on  NDVI_&year..Longitude = Clipped.tmpLongitude and 
         NDVI_&year..Latitude  = Clipped.tmpLatitude;
run;

data France.Final_NDVI_&year; 
 set France.Final_NDVI_&year;
  drop tmpLatitude tmpLongitude inFrance Date Year;
run;

proc datasets lib = France; delete NDVI_&year; run; quit;

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

