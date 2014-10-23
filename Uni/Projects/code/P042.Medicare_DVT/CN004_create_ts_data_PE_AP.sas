libname full 'Z:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\PM\' ;



libname cc 'Z:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN008_cases\' ;

data cases ;
set cc.cases;
drop inc_bin_25--fcol_bin_m;
run; 


proc summary nway data=cases;
class guid;
var Avg_p_A65 Avg_per_mi Avg_pctcol  Avg_P05300;
output out=ses mean=Avg_p_A65 Avg_per_mi Avg_pctcol  Avg_P05300;
run;  

/*create PE data*/

data pecases;
set cases;
where PulmEmb2=1;
run;

data cc.pecases;
set cases;
where PulmEmb2=1;
run;


data pecases;
set pecases;
count=1;
run;


/*This step creates a dataeset with counts per day (date) for cases*/

proc summary nway data=pecases;
class date guid;
var count ;
output out=cases_count sum=;
run;

proc sort data =  full.fullpm; by guid date  ;run;
proc sort data = cases_count ; by guid date;run;

data casespm;
merge cases_count(in=a)  full.fullpm (in=b  )  ;
  by guid date;
    if a;
	run; 

/*#ses*/
proc sort data = ses ; by guid  ;run;
proc sort data = casespm ; by guid ;run;

data casespms;
merge casespm(in=a) ses (in=b drop=_type_ _freq_)  ;
  by guid;
    if a;
	run; 


data times4;
set casespms;
if pmnew=. then delete;
format date JULIAN.;
run;

/*get meanpm for each guid for 7 year period*/


proc summary data=times4;
class guid;
var pmnewma1 ;
output out=new mean=mpmguid;
run;

proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;

data times5;
merge times4   new ;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm=pmnewma1-mpmguid;
run;

proc means data=times6 n min max mean std nmiss p25 p50 p75;
var Avg_p_A65--Avg_P05300; 
run; 

data times7;
set times6;

if Avg_P05300 <  39414.97   then inc_bin_25 = 0;
else inc_bin_25=1;
if Avg_P05300 <    50878.60  then inc_bin_m = 0;
else inc_bin_m=1;
if Avg_P05300 <  66699.0 then inc_bin_75 = 0;
else inc_bin_75=1;
          



if Avg_per_mi < 4.1 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 1.66 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 26.87 then min_bin_75 = 0;
else min_bin_75=1;


if  Avg_p_A65 < 11.2 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 <  13.82 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 16.1 then age_bin_75 = 0;
else age_bin_75=1;



if Avg_pctcol < 17 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 24.9 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 36 then col_bin_75 = 0;
else col_bin_75=1;

run;



/*export to r*/


PROC EXPORT DATA= WORK.times7
            OUTFILE= "Z:\Projects\P042_Medicare_DVT\3.1.10.4.Work\3.Analysis\AN002_timeseries\all_0008_PE.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;













/*create AP data*/

data pecases;
set cases;
where Partery2=1;
run;



data pecases;
set pecases;
count=1;
run;


/*This step creates a dataeset with counts per day (date) for cases*/

proc summary nway data=pecases;
class date guid;
var count ;
output out=cases_count sum=;
run;

proc sort data =  full.fullpm; by guid date  ;run;
proc sort data = cases_count ; by guid date;run;

data casespm;
merge cases_count(in=a)  full.fullpm (in=b  )  ;
  by guid date;
    if a;
	run; 

/*#ses*/
proc sort data = ses ; by guid  ;run;
proc sort data = casespm ; by guid ;run;

data casespms;
merge casespm(in=a) ses (in=b drop=_type_ _freq_)  ;
  by guid;
    if a;
	run; 


data times4;
set casespms;
if pmnew=. then delete;
format date JULIAN.;
run;

/*get meanpm for each guid for 7 year period*/


proc summary data=times4;
class guid;
var pmnewma1 ;
output out=new mean=mpmguid;
run;

proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;

data times5;
merge times4   new ;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm=pmnewma1-mpmguid;
run;

proc means data=times6 n min max mean std nmiss p25 p50 p75;
var Avg_p_A65--Avg_P05300; 
run; 

data times7;
set times6;

if Avg_P05300 <  39414.97   then inc_bin_25 = 0;
else inc_bin_25=1;
if Avg_P05300 <    50878.60  then inc_bin_m = 0;
else inc_bin_m=1;
if Avg_P05300 <  66699.0 then inc_bin_75 = 0;
else inc_bin_75=1;
          



if Avg_per_mi < 4.1 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 1.66 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 26.87 then min_bin_75 = 0;
else min_bin_75=1;


if  Avg_p_A65 < 11.2 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 <  13.82 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 16.1 then age_bin_75 = 0;
else age_bin_75=1;



if Avg_pctcol < 17 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 24.9 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 36 then col_bin_75 = 0;
else col_bin_75=1;

run;



/*export to r*/


PROC EXPORT DATA= WORK.times7
            OUTFILE= "Z:\Projects\P042_Medicare_DVT\3.1.10.4.Work\3.Analysis\AN002_timeseries\all_0008_AP.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

