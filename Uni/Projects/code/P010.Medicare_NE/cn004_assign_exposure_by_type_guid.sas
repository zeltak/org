
libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;
PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN002_calculate_SES\SES_NE_LU.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


data poll;
set poll.poll_lag_V5;
where date>='01Jan2000'D and date<='31Dec2006'D ; 
run;

data inc;
set inc(rename=(Avg_P05300=med_inc));;
run; 

proc sort data=poll;
by guid;
run;

proc sort data=inc;
by guid;
run;

data poll_v3;
merge poll inc ;
by guid;
run;

PROC IMPORT OUT= WORK.regguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = regguid; by guid   ;run;
proc sort data = poll_v3  ; by guid ;run;

data poll_v3;
merge poll_v3(in=a) regguid (in=b keep=guid reg_id)  ;
  by guid;
    if a;
	run; 

data poll_v3;
set poll_v3;
if guid=. then delete;
run; 


PROC IMPORT OUT= WORK.zipguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\zipcode_guid.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 


options mprint;
%macro import(type=);


/*import mortality cases*/


PROC IMPORT OUT= WORK.CASES
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\&type..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;






/*create cound data*/

/*This step addes a new variable where each case gets a "1" */

data cases;
set cases(rename=(dateadmi=date));
count=1;
run;


/*This step creates a dataeset with counts per day (date) for cases*/

proc summary nway data=cases;
class date guid;
var count ;
output out=cases_count sum=;
run;

proc sort data = Zipguid; by guid   ;run;
proc sort data = cases_count ; by guid ;run;

data cases_count;
merge cases_count(in=a) Zipguid (in=b keep=guid zipcode )  ;
  by guid;
    if a;
	run; 




proc sort data=poll_v3;
by guid date;
run;

proc sort data=cases_count;
by guid date;
run;


data times;
merge cases_count (in=a)   poll_v3 (in=b);
by guid date;
  if a;
run;

data times4;
set times;
if pmnew_l1 = . then delete;
format date JULIAN.;
run;

/*get meanpm for each guid for 7 year period*/


proc summary data=times4;
class guid;
var pmnew_l0 ;
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
deltapm=pmnew_l0-mpmguid;
delta2pm=pmnew_l0-pmnewmayear;
run;


data times7;
set times6;
if med_inc <  29250 then inc_bin_25 = 0;
else inc_bin_25=1;
if med_inc <  37703 then inc_bin_m = 0;
else inc_bin_m=1;
if med_inc < 4772 then inc_bin_75 = 0;
else inc_bin_75=1;
if Avg_per_mi < 1.74 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 2.33 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 3.30 then min_bin_75 = 0;
else min_bin_75=1;
if  Avg_p_A65 < 12.26 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 < 14.62 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 16.37 then age_bin_75 = 0;
else age_bin_75=1;
if Avg_pctcol < 14.58 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 19.67 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 28.97 then col_bin_75 = 0;
else col_bin_75=1;
if Avg_pctnoh < 10.85 then hs_bin_25 = 0;
else hs_bin_25=1;
if Avg_pctnoh < 14.49 then hs_bin_m = 0;
else hs_bin_m=1;
if Avg_pctnoh < 18.64 then hs_bin_75 = 0;
else hs_bin_75=1;
run;


/*export to r*/


PROC EXPORT DATA= WORK.times7
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\short\m25\&type.matt0106.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


%MEND ;
/**/
%import(type=copd   );
%import(type=ari    );
%import(type=pneum  );
%import(type=mi     );
%import(type=chf    );
%import(type=diab   );
%import(type=resp );
%import(type=cvd    );
%import(type=ihd    );
%import(type=stroke    );
%import(type=gi    );
%import(type=strisc    );
%import(type=strhem    );
