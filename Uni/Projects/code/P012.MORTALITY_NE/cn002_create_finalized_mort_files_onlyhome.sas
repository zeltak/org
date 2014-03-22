PROC IMPORT OUT= WORK.mortguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\cases_guid_reg_final.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


/*add lung cancer dummy variable*/

data cases;
set mortguid;
icd=put(ENICON_1,1.1);
if icd = "C" then lung=1;
else lung=0;
run; 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;



data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create home dataset*/

data cases_home;
set cases;
if dplace=1 then delete ;
run; 



/*create count data-home*/
/*This step creates a dataeset with counts per day (date) for cases*/



proc summary nway data=cases_home;
 class date   guid;
   var count lung lpm ;
 output out=cases_count_home sum(count)=count sum(lung)=lung mean(lpm)=lpm;
run;



/*pollution part*/


libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


data poll;
set poll.poll_lag_v5;
/*where date>='01Jan2001'D and date<='31Dec2006'D ; */
run;




PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN002_SES\S3_edu_ses_inc_cleaned.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



/*create income dataset*/


proc sort data=poll;
by guid;
run;

proc sort data=inc;
by guid;
run;


data poll_v2;
merge poll inc ;
by guid;
run;

data poll_v3;
set poll_v2(rename=(Avg_P05300=med_inc));;
run;


proc summary nway data=poll_v3;
class  guid;
var avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc;
output out=ses mean=;
run;




/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home   poll_v3 ;
by guid date;
run;

data times2;
set times;
if count = . then delete;
run;

data times3;
set times2;
dp=0;
if pmnew=. then delete;
run;





data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set  merged2;
run;



data times4;
set mergedfin;
format date JULIAN.;
run;







/*get meanpm for each guid for 7 year period*/

proc summary data=times4;
class guid;
var pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear ;
output out=new mean= mpm0 mpm1 mpm2 pmnewmayear;
run;





proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;


data times5;
merge times4   new;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm0=pmnew_l0-mpm0;
deltapm1=pmnew_l1-mpm1;
deltapm2=pmnew_l2-mpm2;
if Avg_pctcol=. then delete;
run;


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = lu; by guid   ;run;
proc sort data = times6 ; by guid ;run;

data times6;
merge times6(in=a) lu (in=b)  ;
  by guid;
    if a;
	run; 





data times7;
set times6;
lungrate=lung*9/population;
run;



proc means data=times7 p25 p50 p75 ;
var med_inc avg_per_mi avg_p_a65 avg_pctcol Avg_pctnoh;
run;


data times8;
set times7;
if med_inc < 44006 then inc_bin_25 = 0;
else inc_bin_25=1;
if med_inc < 52402 then inc_bin_m = 0;
else inc_bin_m=1;
if med_inc < 62019 then inc_bin_75 = 0;
else inc_bin_75=1;
if Avg_per_mi < 4.78 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 8.83 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 18.68 then min_bin_75 = 0;
else min_bin_75=1;
if  Avg_p_A65 < 11.84 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 < 14.04 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 15.62 then age_bin_75 = 0;
else age_bin_75=1;
if Avg_pctcol < 21.84 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 28.52 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 40.06 then col_bin_75 = 0;
else col_bin_75=1;
if Avg_pctnoh < 9.67 then hs_bin_25 = 0;
else hs_bin_25=1;
if Avg_pctnoh < 13.80 then hs_bin_m = 0;
else hs_bin_m=1;
if Avg_pctnoh < 21.00 then hs_bin_75 = 0;
else hs_bin_75=1;
if pop_sqkm < 120 then urb = 0;
else urb=1;
run;


PROC EXPORT DATA= times8
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_home.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
















/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/



/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/



/*CREATE ONLY RESP FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "J" then delete;
run; 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;



data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create home dataset*/

data cases_home;
set cases;
if dplace=1 then delete ;
run; 



/*create count data-home*/
/*This step creates a dataeset with counts per day (date) for cases*/



proc summary nway data=cases_home;
 class date   guid;
   var count lung lpm ;
 output out=cases_count_home sum(count)=count sum(lung)=lung mean(lpm)=lpm;
run;





/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home   poll_v3 ;
by guid date;
run;

data times2;
set times;
if count = . then delete;
run;

data times3;
set times2;
dp=0;
if pmnew=. then delete;
run;





data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set  merged2;
run;



data times4;
set mergedfin;
format date JULIAN.;
run;







/*get meanpm for each guid for 7 year period*/

proc summary data=times4;
class guid;
var pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear ;
output out=new mean= mpm0 mpm1 mpm2 pmnewmayear;
run;





proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;


data times5;
merge times4   new;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm0=pmnew_l0-mpm0;
deltapm1=pmnew_l1-mpm1;
deltapm2=pmnew_l2-mpm2;
if Avg_pctcol=. then delete;
run;


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = lu; by guid   ;run;
proc sort data = times6 ; by guid ;run;

data times6;
merge times6(in=a) lu (in=b)  ;
  by guid;
    if a;
	run; 





data times7;
set times6;
lungrate=lung*9/population;
run;



proc means data=times7 p25 p50 p75 ;
var med_inc avg_per_mi avg_p_a65 avg_pctcol Avg_pctnoh;
run;


data times8;
set times7;
if med_inc < 44006 then inc_bin_25 = 0;
else inc_bin_25=1;
if med_inc < 52402 then inc_bin_m = 0;
else inc_bin_m=1;
if med_inc < 62019 then inc_bin_75 = 0;
else inc_bin_75=1;
if Avg_per_mi < 4.78 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 8.83 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 18.68 then min_bin_75 = 0;
else min_bin_75=1;
if  Avg_p_A65 < 11.84 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 < 14.04 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 15.62 then age_bin_75 = 0;
else age_bin_75=1;
if Avg_pctcol < 21.84 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 28.52 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 40.06 then col_bin_75 = 0;
else col_bin_75=1;
if Avg_pctnoh < 9.67 then hs_bin_25 = 0;
else hs_bin_25=1;
if Avg_pctnoh < 13.80 then hs_bin_m = 0;
else hs_bin_m=1;
if Avg_pctnoh < 21.00 then hs_bin_75 = 0;
else hs_bin_75=1;
if pop_sqkm < 120 then urb = 0;
else urb=1;
run;


PROC EXPORT DATA= times8
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_home_resp.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;







/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/



/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*CREATE ONLY CVD FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
ucd2=put(ucod,2.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "I" then delete;
run; 

/*#delete stroke and non related*/

data cases;
set cases;
if ucd2 = "I6" then delete;
if ucd2 = "I0" then delete;
if ucd2 = "I8" then delete;
if ucd2 = "I9" then delete;
run; 

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;



data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create home dataset*/

data cases_home;
set cases;
if dplace=1 then delete ;
run; 



/*create count data-home*/
/*This step creates a dataeset with counts per day (date) for cases*/



proc summary nway data=cases_home;
 class date   guid;
   var count lung lpm ;
 output out=cases_count_home sum(count)=count sum(lung)=lung mean(lpm)=lpm;
run;





/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home   poll_v3 ;
by guid date;
run;

data times2;
set times;
if count = . then delete;
run;

data times3;
set times2;
dp=0;
if pmnew=. then delete;
run;





data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set  merged2;
run;



data times4;
set mergedfin;
format date JULIAN.;
run;







/*get meanpm for each guid for 7 year period*/

proc summary data=times4;
class guid;
var pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear ;
output out=new mean= mpm0 mpm1 mpm2 pmnewmayear;
run;





proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;


data times5;
merge times4   new;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm0=pmnew_l0-mpm0;
deltapm1=pmnew_l1-mpm1;
deltapm2=pmnew_l2-mpm2;
if Avg_pctcol=. then delete;
run;


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = lu; by guid   ;run;
proc sort data = times6 ; by guid ;run;

data times6;
merge times6(in=a) lu (in=b)  ;
  by guid;
    if a;
	run; 





data times7;
set times6;
lungrate=lung*9/population;
run;



proc means data=times7 p25 p50 p75 ;
var med_inc avg_per_mi avg_p_a65 avg_pctcol Avg_pctnoh;
run;


data times8;
set times7;
if med_inc < 44006 then inc_bin_25 = 0;
else inc_bin_25=1;
if med_inc < 52402 then inc_bin_m = 0;
else inc_bin_m=1;
if med_inc < 62019 then inc_bin_75 = 0;
else inc_bin_75=1;
if Avg_per_mi < 4.78 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 8.83 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 18.68 then min_bin_75 = 0;
else min_bin_75=1;
if  Avg_p_A65 < 11.84 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 < 14.04 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 15.62 then age_bin_75 = 0;
else age_bin_75=1;
if Avg_pctcol < 21.84 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 28.52 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 40.06 then col_bin_75 = 0;
else col_bin_75=1;
if Avg_pctnoh < 9.67 then hs_bin_25 = 0;
else hs_bin_25=1;
if Avg_pctnoh < 13.80 then hs_bin_m = 0;
else hs_bin_m=1;
if Avg_pctnoh < 21.00 then hs_bin_75 = 0;
else hs_bin_75=1;
if pop_sqkm < 120 then urb = 0;
else urb=1;
run;


PROC EXPORT DATA= times8
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_home_cvd.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;




/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/



/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/





/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/



/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/
/*MANUALLY DELETE ALL FILES BUT POLLUTION AND RUN*/


/*CREATE ONLY CVD FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
ucd2=put(ucod,2.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "I" then delete;
run; 

/*#delete stroke and non related*/

data cases;
set cases;
if ucd2 ne "I6" then delete;
run; 

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;



data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create home dataset*/

data cases_home;
set cases;
if dplace=1 then delete ;
run; 



/*create count data-home*/
/*This step creates a dataeset with counts per day (date) for cases*/



proc summary nway data=cases_home;
 class date   guid;
   var count lung lpm ;
 output out=cases_count_home sum(count)=count sum(lung)=lung mean(lpm)=lpm;
run;





/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home   poll_v3 ;
by guid date;
run;

data times2;
set times;
if count = . then delete;
run;

data times3;
set times2;
dp=0;
if pmnew=. then delete;
run;





data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set  merged2;
run;



data times4;
set mergedfin;
format date JULIAN.;
run;







/*get meanpm for each guid for 7 year period*/

proc summary data=times4;
class guid;
var pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear ;
output out=new mean= mpm0 mpm1 mpm2 pmnewmayear;
run;





proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;


data times5;
merge times4   new;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm0=pmnew_l0-mpm0;
deltapm1=pmnew_l1-mpm1;
deltapm2=pmnew_l2-mpm2;
if Avg_pctcol=. then delete;
run;


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = lu; by guid   ;run;
proc sort data = times6 ; by guid ;run;

data times6;
merge times6(in=a) lu (in=b)  ;
  by guid;
    if a;
	run; 





data times7;
set times6;
lungrate=lung*9/population;
run;



proc means data=times7 p25 p50 p75 ;
var med_inc avg_per_mi avg_p_a65 avg_pctcol Avg_pctnoh;
run;


data times8;
set times7;
if med_inc < 44006 then inc_bin_25 = 0;
else inc_bin_25=1;
if med_inc < 52402 then inc_bin_m = 0;
else inc_bin_m=1;
if med_inc < 62019 then inc_bin_75 = 0;
else inc_bin_75=1;
if Avg_per_mi < 4.78 then min_bin_25 = 0;
else min_bin_25=1;
if Avg_per_mi < 8.83 then min_bin_m = 0;
else min_bin_m=1;
if Avg_per_mi < 18.68 then min_bin_75 = 0;
else min_bin_75=1;
if  Avg_p_A65 < 11.84 then age_bin_25 = 0;
else age_bin_25=1;
if  Avg_p_A65 < 14.04 then age_bin_m = 0;
else age_bin_m=1;
if  Avg_p_A65 < 15.62 then age_bin_75 = 0;
else age_bin_75=1;
if Avg_pctcol < 21.84 then col_bin_25 = 0;
else col_bin_25=1;
if Avg_pctcol < 28.52 then col_bin_m = 0;
else col_bin_m=1;
if Avg_pctcol < 40.06 then col_bin_75 = 0;
else col_bin_75=1;
if Avg_pctnoh < 9.67 then hs_bin_25 = 0;
else hs_bin_25=1;
if Avg_pctnoh < 13.80 then hs_bin_m = 0;
else hs_bin_m=1;
if Avg_pctnoh < 21.00 then hs_bin_75 = 0;
else hs_bin_75=1;
if pop_sqkm < 120 then urb = 0;
else urb=1;
run;


PROC EXPORT DATA= times8
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_home_stroke.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

