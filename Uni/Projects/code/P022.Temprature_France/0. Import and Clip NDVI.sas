libname NDVI 'Y:\France_LST\Stage 1' ;


data NDVI.T1;
 infile 'Y:\NDVI\raw italy_france\TieName-17v03.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long NDVI Reference;
run;

data NDVI.T2;
 infile 'Y:\NDVI\raw italy_france\TieName-17v04.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	NDVI	Reference;
run;

data NDVI.T3;
 infile 'Y:\NDVI\raw italy_france\TieName-18v03.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	NDVI	Reference;
run;

data NDVI.T4;
 infile 'Y:\NDVI\raw italy_france\TieName-18v04.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	NDVI	Reference;
run;

data NDVI.T5;
 infile 'Y:\NDVI\raw italy_france\TieName-18v05.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	NDVI	Reference;
run;

data NDVI.T6;
 infile 'Y:\NDVI\raw italy_france\TieName-19v04.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	NDVI	Reference;
run;

data NDVI.T7;
 infile 'Y:\NDVI\raw italy_france\TieName-19v05.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long  NDVI	Reference;
run;



data NDVI.NDVI;
set NDVI.T1 NDVI.T2 NDVI.T3 NDVI.T4 NDVI.T5 NDVI.T6 NDVI.T7;
 jdate = reference*1;
 DATE  = datejul(jdate); 
 format DATE  date9.;
   year = year(date);
   m = month(date);
    lat_ndvi  = lat;
    long_ndvi = long;
run; 


proc datasets lib = NDVI; delete T1 T2 T3 T4 T5 T6 T7; run; quit;
