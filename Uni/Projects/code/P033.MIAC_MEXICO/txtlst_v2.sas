


data T1;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2003\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2003\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T3;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2004\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T4;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2004\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T5;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2005\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T6;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2005\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T7;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2006\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T8;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2006\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T9;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2007\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T10;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2007\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T11;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2008\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T12;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2008\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data T13;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2009\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T14;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2009\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T15;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2010\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data T16;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2010\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T17;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2011\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T18;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2011\TileName-00v00-part2.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;


data alleyars ;
set T1 T2 T3 T4 T5 T6 T7 T8 T9 T10  T11 T12 T13 T14 T15 T16 T17 T18;
run; 


						 
data T2003 (drop=reference char_id jdata  lat long);
set alleyars ;
char_id = put(Reference, 12.) ; 
Jdata = (SUBSTR (char_id, 1,8))*1;  
DATE = DATEJUL(Jdata); 
Format DATE date9.;
year=year(date);
lat_aod=lat;
long_aod=long;
run;


/*export full RAW grid */

proc summary nway data=T2003;
class lat_aod long_aod;
var aod;
output out=OUTaod mean=aod;
run; 


DATA OUTaod;
  SET OUTaod;
      aodid = _N_;  
  RUN;


PROC EXPORT DATA= OUTaod
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN007_Key_tables\fullRAWgrid.dbf" 
            DBMS=DBF REPLACE;
RUN;




/*divide all file into files based on year*/
options mprint;
%macro import(year=);

data y&year;
 set T2003;
  if year = &year;
run;
	

proc summary nway data=y&year;
class lat_aod long_aod;
var aod;
output out=OUTaody&year mean=aod;
run; 



PROC EXPORT DATA= OUTaody&year
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN007_Key_tables\OUTaody&year.dbf" 
            DBMS=DBF REPLACE;
RUN;
%MEND ;

%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 
%import(year=2009);
%import(year=2010);
%import(year=2011);


/*GO TO ARCGIS AND CREATE THE CLIP THERE AND THEN REIMPORT*/




/*clip the full RAW grid to only study area*/

PROC IMPORT OUT= cgrid
            DATAFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN007_Key_tables\fullRAWgrid_with_clipID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




proc sort data = Outaod; by lat_aod long_aod  ;run;
proc sort data = T2003 ; by lat_aod long_aod     ;run;

data DATA5;
merge T2003 (in=a) Outaod (in=b keep=lat_aod long_aod aodid)  ;
  by lat_aod long_aod     ;
    if a;
	run; 


proc sort data = cgrid; by aodid   ;run;
proc sort data = DATA5 ; by aodid      ;run;

data DATA6;
merge DATA5 (in=a) cgrid (in=b keep=aodid clipmexcit)  ;
  by aodid     ;
    if a;
	run; 


/*proc freq data=DATA6;*/
/*table INGRID /list;*/
/*run; */


data DATA7 (drop= q_c);
set DATA6;
if clipmexcit=0 then delete;
run; 

/*export clipped grid*/


proc summary nway data=DATA7;
class lat_aod long_aod;
var aodid;
output out=OUTaod2 mean=aodid;
run; 



PROC EXPORT DATA= OUTaod2
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN007_Key_tables\fullCLIPEDgrid.dbf" 
            DBMS=DBF REPLACE;
RUN;


data DATA7_mod2;
set DATA7;
if aod=. then delete;
if aod > 0.9 then delete;
run; 

/*export mod2*/


PROC EXPORT DATA= DATA7_mod2
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN010_mod2_files\mod2.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 

/**** Create Data ****/ 
/*creates the complete time series range*/

data OUTaod2;
set OUTaod2;
keep long_aod lat_aod;
run; 


data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2003 1
31/12/2011 1
run;

/*creates the completed time series for above range*/
/*the output file is 'daily'*/

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

/*create a list of dates for cycle-first type macro*/

data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set Daily;
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

%put &lista;



proc printto log="nul:"; run;
ods listing close;*to suppress the output printing;


/*launch the macro*/

%put &Lista;

/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = date long_aod lat_aod);
  if _N_ = 1 then set Daily&date;
 set OUTaod2;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%mend full;

%full(List = &Lista);

ods listing ;
proc printto; run;





/*export mod 3 files*/
/**/


proc sort data = cgrid ; by lat_aod long_aod  ;run;
proc sort data = final  ; by lat_aod long_aod     ;run;

data finalv2;
merge final  (in=a) cgrid (in=b keep=lat_aod long_aod  aodid)  ;
  by lat_aod long_aod     ;
    if a;
	run; 





PROC EXPORT DATA=finalv2 
            OUTFILE= "f:\Uni\Projects\P031_MIAC_MEXICO\3.Work\2.Gather_data\FN011_mod3_files\allmod3.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
