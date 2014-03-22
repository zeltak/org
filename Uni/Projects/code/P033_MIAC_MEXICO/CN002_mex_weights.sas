
PROC IMPORT OUT= aodagg
            DATAFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\t2003aodmean.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= aodxy 
            DATAFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\meanaodxy.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 





proc sort data = final; by long_aod lat_aod date  ;run;
proc sort data = Aodagg ; by long_aod lat_aod date ;run;

data DATA3;
merge final(in=a) Aodagg (in=b)  ;
  by long_aod lat_aod date;
	run; 


 proc freq data=DATA3;
 table lat_aod*long_aod / list;
 ods output list=list;
 run; 

proc sort data = list; by Frequency ;run; 


/*land use*/

/*data*/
PROC IMPORT OUT= lus
  DATAFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\midatlneweng1kmluvariables.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

data lu(keep= gridid mjrrdden_1km ndvi_id pblidx);
set lus;
pblidx=pblid*1;
run; 
	
data lu (drop=pblidx);
set lu;
pblid=pblidx;
run; 


/*key*/

PROC IMPORT OUT= aod_lu_key
            DATAFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\meanaodxy_gridid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data  aod_lu_key (drop=aod long_aod lat_aod);
set  aod_lu_key;
glong= round(long_aod,0.00001);
glat= round(lat_aod,0.00001);
run;


data  data3 (drop= long_aod lat_aod);
set  data3;
glong= round(long_aod,0.00001);
glat= round(lat_aod,0.00001);
run;


proc sort data = data3; by glong glat   ;run;
proc sort data = aod_lu_key ; by glong glat   ;run;

data DATA4;
merge data3(in=a) aod_lu_key (in=b)  ;
  by glong glat  ;
    if a;
	run; 



proc sort data = lu; by gridid  ;run;
proc sort data = DATA4 ; by gridid ;run;

data aodagg3;
merge DATA4(in=a) lu (in=b)  ;
  by gridid ;
    if a;
	run; 

/*to clear (clip) non NE points*/

data aodagg4 ;
set aodagg3;
if gridid=. then delete;
drop OBJECTID--TARGET_FID;
run; 

proc summary nway data=aodagg4;
class glong glat;
var aod;
output out=OUTPUTFILE mean=aod;
run; 

PROC EXPORT DATA= OUTPUTFILE  
            OUTFILE= "c:\Users\ekloog\Documents\tmp\OUTPUTFILEOUTPUTFILEOUTPUTFILEOUTDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
 

 proc freq data=aodagg4;
 table glat*glong / list;
 ods output list=list;
 run; 

proc sort data = list; by Frequency ;run; 




/*import met*/


data aodagg5;
set aodagg4;
month=month(date);
run; 

libname aod 'f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\' ;


/*NDVI*/

proc sort data=aod.Ndvi03trid; by ndvi_id; run;

proc transpose data=aod.Ndvi03trid out=Nendvi03_tr(rename=(_NAME_=mos col1=ndvi));
	var jan feb mar apr may jun jul aug sep oct nov dec;
   by ndvi_id;
run;

data Nendvi03_tr; 
	set Nendvi03_tr;
	select (mos);
	when ('jan') month=1;
	when ('feb') month=2;
	when ('mar') month=3;
	when ('apr') month=4;
	when ('may') month=5;
	when ('jun') month=6;
	when ('jul') month=7;
	when ('aug') month=8;
	when ('sep') month=9;
	when ('oct') month=10;
	when ('nov') month=11;
	when ('dec') month=12;
	otherwise;
	end;
	drop mos;
run;

proc sql;
  create table aodagg6 as
   select *
    from aodagg5 left join Nendvi03_tr 
     on aodagg5.month = Nendvi03_tr.month and aodagg5.ndvi_id  = Nendvi03_tr.ndvi_id  ;
run;



/*PBL*/

data pbl2003;
set aod.pbl2003;
 date=mdy(V2,V3,V1);
 format date date7.;
 pblidx=pblid*1;
 drop pblid;
run; 

data pbl2003(drop=pblidx);
set pbl2003;
pblid=pblidx;
run; 


proc sql;
  create table aodagg7 as
   select *
    from aodagg6 left join pbl2003
     on aodagg6.pblid = pbl2003.pblid and aodagg6.date  = pbl2003.date  ;
run;


data aodagg7;
set aodagg7;
drop V1--Lat_pbl x1 y1;
run; 


/*MET*/

PROC IMPORT OUT= met2003
            DATAFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\met2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc freq data=met2003;
table station ;
ods output onewayfreqs=onewayfreqs;
run; 

data metcut;
set onewayfreqs;
keep station Frequency ;
if Frequency < 300 then delete;
run; 


proc sort data = metcut; by station   ;run;
proc sort data = met2003 ; by station ;run;

data met2003full;
merge met2003(in=a) metcut (in=b)  ;
  by station;
    if b;
	run; 

proc sort data = met2003full nodupkey Out = grid(keep = x y station); by x y station; run; 
data grid;
set grid;
if x=. then delete;
run; 

PROC EXPORT DATA= grid
            OUTFILE= "f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\metfullxy.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 




proc sql;
  create table aodagg8 as
   select *
    from aodagg7 left join met2003
     on aodagg7.station = met2003.station and aodagg7.date = met2003.date  ;
run;

data aodagg8 ;
set aodagg8 ;
drop stype--source x y;
run; 

proc means data=aodagg8 n min max mean std nmiss;
var ; 
run; 


libname aod 'f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\' ;


data aodagg8;
set aod.aodweights;
run; 





libname aod 'f:\Uni\Projects\3.1.0.TMP_PROJECTS\stacey_alexef\data\' ;

data aod.aodweights;
set aodagg9;
obs=0;
if aod=. then obs=1;
run; 


proc logistic data=aod.aodweights descending;
 class month;
  class month / param=ref ;
  model obs = ndvi pbl humidity pressure_m month;
  OUTPUT OUT=PROB PREDICTED=PHAT;
run;

data prob2 (keep=Date gridid glong glat PHAT);
set prob;
run; 

data prob3;
set prob2;
wt=1/PHAT;
tv=1;
run; 

proc means data=prob3 mean;
var wt; 
ods output summary=summary;
run; 

data summary;
set summary;
tv=1;
run; 

proc sort data = summary ; by tv   ;run;
proc sort data = prob3 ; by tv  ;run;

data prob4;
merge prob3(in=a) summary (in=b)  ;
  by tv ;
    if a;
	run; 

data aod.prob5 (keep=Date gridid glong glat normwt);
set prob4;
normwt=wt/wt_Mean;
run; 




proc sort data = list; by Frequency ;run; 


data aod.prob5 (drop=gridid);
set prob5;
run; 

proc summary nway data=aod.prob5 ;
class glong glat;
var normwt;
output out=OUTPUTFILE mean=normwt;
run; 

data aod.prob5agg ;
set OUTPUTFILE;
run; 
