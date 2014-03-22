

libname a2003 'y:\MIAC_USA\2003\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2003\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2003\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2003\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2003.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2003.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2003.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2003.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2003\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2003.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2004 'y:\MIAC_USA\2004\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2004\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2004\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2004\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2004.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2004.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2004.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2004.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2004\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2004.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2005 'y:\MIAC_USA\2005\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2005\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2005\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2005\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2005.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2005.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2005.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2005.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2005\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2005.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2006 'y:\MIAC_USA\2006\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2006\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2006\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2006\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2006.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2006.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2006.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2006.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2006\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2006.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2007 'y:\MIAC_USA\2007\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2007\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2007\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2007\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2007.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2007.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2007.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2007.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2007\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2007.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2008 'y:\MIAC_USA\2008\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2008\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2008\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2008\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2008.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2008.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2008.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2008.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2008\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2008.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2009 'y:\MIAC_USA\2009\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2009\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2009\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2009\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2009.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2009.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2009.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2009.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2009\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2009.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2010 'y:\MIAC_USA\2010\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2010\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2010\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2010\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2010.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2010.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2010.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2010.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2010\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2010.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2011 'y:\MIAC_USA\2011\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2011\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2011\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2011\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2011.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2011.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2011.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2011.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2011\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2011.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */



libname a2012 'y:\MIAC_USA\2012\' ;



options mprint;
%macro import(Tile = );

data T1;
 infile "y:\MIAC_USA\2012\TileName-&Tile-part1.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile "y:\MIAC_USA\2012\TileName-&Tile-part2.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile "y:\MIAC_USA\2012\TileName-&Tile-part3.txt" dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data Final_h&Tile;
set T1 T2 T3 ;
run; 

						 
data a2012.Final_h&Tile(drop = reference char_id jdata lat long);
 set Final_h&Tile;
  char_id = put(Reference, 12.) ; 
  Jdata = (SUBSTR (char_id, 1,8))*1;  
  DATE = DATEJUL(Jdata); 
  Format DATE date9.;
  m = month(DATE);
   year = year(date);
    lat_aod  = lat;
    long_aod = long;
run;



proc summary nway data = a2012.Final_h&Tile;
 class lat_aod long_aod;
  var AOD;
   output out = a2012.Grid_h&Tile(drop = _Type_ _Freq_) mean(AOD)= AOD N(AOD) = N;
run; 

PROC EXPORT DATA= a2012.Grid_h&Tile
            OUTFILE= "y:\MIAC_USA\2012\Grid_h&Tile..dbf" 
			            DBMS=DBF REPLACE;
						RUN;

proc datasets lib = work; delete T1 T2 T3  Final_h&Tile; run;

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);
%import(Tile = 02v04);



/*data a2012.GRIDGRID;*/
/*set*/
/*Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h00v03  Grid_h00v04 */
/*Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h01v03  Grid_h01v04 */
/*Grid_h02v00 Grid_h02v01 Grid_h02v02 Grid_h02v03  Grid_h02v04 ;*/
/*run; */

