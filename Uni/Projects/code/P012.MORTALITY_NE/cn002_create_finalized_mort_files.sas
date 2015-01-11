
/*pollution part*/


libname poll "Z:\Projects\P001_NE_PM_MODELS\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


data poll;
set poll.poll_lag_v5;
where date>='01Jan2001'D and date<='31Dec2008'D ; 
run;




PROC IMPORT OUT= WORK.INC 
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN002_SES\S3_edu_ses_inc_cleaned.dbf" 
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


/*#create full datasest*/



PROC IMPORT OUT= WORK.maguid
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\MA_only_grids.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data maguid;
set maguid;
keep guid long_aod lat_aod;
run; 

proc sort data = maguid nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 


/**** Create Data ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/03/2000 1
31/12/2008 1
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

/*clear editor*/
/*DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; */
/*clear log*/
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


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

data Daily&date(keep = date guid Long_aod Lat_aod);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%mend full;

%full(List = &Lista);

options notes source source2 ; *re-instate LOG WINDOW printing;





















/*import cases*/
/*import cases*/
/*import cases*/




PROC IMPORT OUT= WORK.mortguid
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\cases_guid_reg_final.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc summary nway data=mortguid;
class guid;
var reg_id;
output out=reg_file mean=reg_id;
run; 

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


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




proc sort data = Cases_count_oustidehome; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data Cases_count_oustidehome;
merge Cases_count_oustidehome(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

data Cases_count_oustidehome;
set Cases_count_oustidehome;
if count=. then count=0;
dp=1;
run; 


proc sort data = cases_count_home; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data cases_count_home;
merge cases_count_home(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

	

data cases_count_home;
set cases_count_home;
if count=. then count=0;
dp=0;
run; 











/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;



data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;













/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome (in=a) poll_reg2 (in=b) ;
by date reg_id;
if a;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3 (in=a)  ses (in=b) ;
if a;
by guid;
if guid=. then delete;
dp=1;
run;

/*assign exposure to 2 different scenarios*/

data outhome_v4y;
set outhome_v4;
where count ne 0;
run; 


data outhome_v4x;
set outhome_v4;
where count = 0;
drop pmnew_l0--pmnewmayear;
run; 



proc sort data = poll_v3; by date guid   ;run;
proc sort data = outhome_v4x ; by date guid ;run;

data DATA3;
merge outhome_v4x(in=a) poll_v3 (in=b)  ;
  by date guid;
    if a;
	run; 

proc sql;
 create table dataX as 
 select date,guid, _TYPE_,_FREQ_,count,lung,reg_id,lpm,Long_AOD,lat_AOD,pmnew_l0,pmnew_l1,pmnew_l2,temp_f_l0,temp_f_l1,temp_f_l2,pmnewmayear,Avg_pctcol,Avg_pctnoh,Avg_per_mi,Avg_p_A65,med_inc,dp
   from DATA3;
quit;


data dataXY;
set outhome_v4y dataX;
run; 


/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home (in=a)   poll_v3 (in=b);
if a;
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



/*merge the 2 sets back togheter*/

data merged1;
set dataXY;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
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
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;















/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/
/*RESP*/






/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "J" then delete;
run; 




data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




proc sort data = Cases_count_oustidehome; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data Cases_count_oustidehome;
merge Cases_count_oustidehome(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

data Cases_count_oustidehome;
set Cases_count_oustidehome;
if count=. then count=0;
dp=1;
run; 


proc sort data = cases_count_home; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data cases_count_home;
merge cases_count_home(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

	

data cases_count_home;
set cases_count_home;
if count=. then count=0;
dp=0;
run; 











/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;



data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;













/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome (in=a) poll_reg2 (in=b) ;
by date reg_id;
if a;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3 (in=a)  ses (in=b) ;
if a;
by guid;
if guid=. then delete;
dp=1;
run;

/*assign exposure to 2 different scenarios*/

data outhome_v4y;
set outhome_v4;
where count ne 0;
run; 


data outhome_v4x;
set outhome_v4;
where count = 0;
drop pmnew_l0--pmnewmayear;
run; 



proc sort data = poll_v3; by date guid   ;run;
proc sort data = outhome_v4x ; by date guid ;run;

data DATA3;
merge outhome_v4x(in=a) poll_v3 (in=b)  ;
  by date guid;
    if a;
	run; 

proc sql;
 create table dataX as 
 select date,guid, _TYPE_,_FREQ_,count,lung,reg_id,lpm,Long_AOD,lat_AOD,pmnew_l0,pmnew_l1,pmnew_l2,temp_f_l0,temp_f_l1,temp_f_l2,pmnewmayear,Avg_pctcol,Avg_pctnoh,Avg_per_mi,Avg_p_A65,med_inc,dp
   from DATA3;
quit;


data dataXY;
set outhome_v4y dataX;
run; 


/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home (in=a)   poll_v3 (in=b);
if a;
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



/*merge the 2 sets back togheter*/

data merged1;
set dataXY;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
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
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_resp.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;














/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/
/**/
/**/
/*CVD*/






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

/*add lung cancer dummy variable*/



data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




proc sort data = Cases_count_oustidehome; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data Cases_count_oustidehome;
merge Cases_count_oustidehome(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

data Cases_count_oustidehome;
set Cases_count_oustidehome;
if count=. then count=0;
dp=1;
run; 


proc sort data = cases_count_home; by guid date   ;run;
proc sort data = final ; by guid date  ;run;

data cases_count_home;
merge cases_count_home(in=a) final (in=b)  ;
  by guid date;
    if  b;
	run; 

	

data cases_count_home;
set cases_count_home;
if count=. then count=0;
dp=0;
run; 











/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;



data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;













/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome (in=a) poll_reg2 (in=b) ;
by date reg_id;
if a;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3 (in=a)  ses (in=b) ;
if a;
by guid;
if guid=. then delete;
dp=1;
run;

/*assign exposure to 2 different scenarios*/

data outhome_v4y;
set outhome_v4;
where count ne 0;
run; 


data outhome_v4x;
set outhome_v4;
where count = 0;
drop pmnew_l0--pmnewmayear;
run; 



proc sort data = poll_v3; by date guid   ;run;
proc sort data = outhome_v4x ; by date guid ;run;

data DATA3;
merge outhome_v4x(in=a) poll_v3 (in=b)  ;
  by date guid;
    if a;
	run; 

proc sql;
 create table dataX as 
 select date,guid, _TYPE_,_FREQ_,count,lung,reg_id,lpm,Long_AOD,lat_AOD,pmnew_l0,pmnew_l1,pmnew_l2,temp_f_l0,temp_f_l1,temp_f_l2,pmnewmayear,Avg_pctcol,Avg_pctnoh,Avg_per_mi,Avg_p_A65,med_inc,dp
   from DATA3;
quit;


data dataXY;
set outhome_v4y dataX;
run; 


/*normal PM*/



proc sort data=poll_v3;
by guid date;
run;

proc sort data=Cases_count_home;
by guid date;
run;


data times;
merge Cases_count_home (in=a)   poll_v3 (in=b);
if a;
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



/*merge the 2 sets back togheter*/

data merged1;
set dataXY;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
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
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\allmort_cvd.csv" 
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


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;


data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;



/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome poll_reg2 ;
by date reg_id;
if pmnew_l0=. then delete;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3   ses ;
by guid;
if guid=. then delete;
if avg_pctcol=. then delete;
if pmnew_l0=. then delete;
dp=1;
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





/*merge the 2 sets back togheter*/

data merged1;
set Outhome_v4;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
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
run;


PROC EXPORT DATA= times8
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\cvd_mort.csv" 
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




/*CREATE ONLY STROKE FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
ucd2=put(ucod,2.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "I" then delete;
run; 

/*#delete stroke*/

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


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;


data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;



/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome poll_reg2 ;
by date reg_id;
if pmnew_l0=. then delete;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3   ses ;
by guid;
if guid=. then delete;
if avg_pctcol=. then delete;
if pmnew_l0=. then delete;
dp=1;
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





/*merge the 2 sets back togheter*/

data merged1;
set Outhome_v4;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
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
run;


PROC EXPORT DATA= times8
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\stroke_mort.csv" 
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




/*CREATE ONLY diab FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
ucd2=put(ucod,2.1);
if ucd = "C" then lung=1;
else lung=0;
if ucd ne "E" then delete;
run; 

/*#delete stroke*/

data cases;
set cases;
if ucd2 ne "E1" then delete;
run; 
/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;


data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;



/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome poll_reg2 ;
by date reg_id;
if pmnew_l0=. then delete;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3   ses ;
by guid;
if guid=. then delete;
if avg_pctcol=. then delete;
if pmnew_l0=. then delete;
dp=1;
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





/*merge the 2 sets back togheter*/

data merged1;
set Outhome_v4;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
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
run;


PROC EXPORT DATA= times8
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\diab_mort.csv" 
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




/*CREATE ONLY lungcancer FILE */


/*add lung cancer dummy variable*/

data cases;
set mortguid;
ucd=put(ucod,1.1);
ucd2=put(ucod,3.1);
if ucd ne "C" then delete;
run; 


/*#delete stroke*/

data cases;
set cases;
if ucd2 ne "C34" then delete;
run; 
/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


/*FULL DATA SET CALCULATIONS*/

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */


data cases;
set cases(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

data cases_outhome;
set cases;
if dplace=0 then delete ;
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


/*create count data-outside home*/

proc summary nway data=cases_home;
class date guid;
var count lung reg_id lpm;
output out=Cases_count_oustidehome sum(count)= count sum(lung)= lung mean(reg_id)=reg_id mean(lpm)=lpm;
run;




/*#create regions set PM*/

proc sort data=poll_v3;
by guid;
run;

proc sort data=cases;
by guid;
run;


data poll_reg;
merge poll_v3 cases (keep=guid reg_id) ;
by guid;
if reg_id=. then delete;
run;


proc summary nway data=poll_reg;
class date   reg_id;
var pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 temp_f_l1 temp_f_l2 pmnewmayear;
output out=poll_reg2 mean=;
run;



/*add reg pm by day to out home deaths*/


proc sort data=poll_reg2;
by date reg_id;
run;

proc sort data=Cases_count_oustidehome;
by date reg_id;
run;


data outhome_v3;
merge Cases_count_oustidehome poll_reg2 ;
by date reg_id;
if pmnew_l0=. then delete;
run;




proc sort data=ses;
by guid ;
run;

proc sort data=outhome_v3;
by guid ;
run;


data outhome_v4;
merge outhome_v3   ses ;
by guid;
if guid=. then delete;
if avg_pctcol=. then delete;
if pmnew_l0=. then delete;
dp=1;
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





/*merge the 2 sets back togheter*/

data merged1;
set Outhome_v4;
keep date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 pmnewmayear temp_f_l0 temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;

data merged2;
set times3;
keep  date guid count lung lpm pmnew_l0 pmnew_l1 pmnew_l2 temp_f_l0 pmnewmayear temp_f_l1 temp_f_l2 avg_pctcol avg_pctnoh avg_per_mi avg_p_a65 med_inc dp;
run;


data mergedfin;
set merged1 merged2;
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
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
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
run;


PROC EXPORT DATA= times8
            OUTFILE= "Z:\Projects\P012.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\lung_mort.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
		  

