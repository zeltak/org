libname full 'Y:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\PM\' ;



PROC IMPORT OUT= cases
            DATAFILE= "Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN002_Cases_guid_MIA\cases_PE_AP_clipped_XY.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data cases (rename=(guid_=guid ));
set cases;
date=input(adate,date9.);
format date mmddyy10.;
drop adate;
run; 

data cases (where=(date>="01MAR2000"D ));
set cases;
run; 


proc sort data = cases; by guid date    ;run;
proc sort data = full.fullpm ; by guid date ;run;

data casespm;
merge cases(in=a) full.fullpm (in=b)  ;
  by guid date;
    if a;
	run; 

data casespm;
set casespm;
if pmnew=. then delete;
dow=weekday(date);
run; 


/*#ses*/

PROC IMPORT OUT= ne_ses
            DATAFILE= "Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN002_calculate_SES\SES_NE_LU.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= mia_ses
  DATAFILE= "Y:\Projects\P017.Medicare_MIA\3.1.10.1.Raw_data\SES\midatl_guid_cbg00wtdv2.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

data mia_ses_clean;
set mia_ses;
Avg_p_A65=pct65upest;
Avg_per_mi=pctnonwht_wtd;
Avg_pctcol=pctbachorhigher_wtd;
Avg_P05300=medhhin_wtd;
guid=guid_;
drop pct65upest pctnonwht_wtd  pop_65upest pctbachorhigher_wtd medhhin_wtd guid_ pop00est pctlowinc_wtd;
run;

data ne_ses_clean;
set ne_ses;
drop join_Count--y avg_pctnoh pop_den--P_ospace;
run; 



proc sql;
  create table ne_ses_clean2 as
  select Avg_p_A65,Avg_per_mi,Avg_pctcol,Avg_P05300,guid
  from ne_ses_clean
quit;

proc sql;
  create table mia_ses_clean2 as
  select Avg_p_A65,Avg_per_mi,Avg_pctcol,Avg_P05300,guid
  from mia_ses_clean
quit;

data ses;
set ne_ses_clean2 mia_ses_clean2;
run; 



proc sort data = Casespm; by guid   ;run;
proc sort data = ses ; by guid ;run;

data Casesppses;
merge Casespm(in=a) ses (in=b)  ;
  by guid;
    if a;
	run; 

data Casesppsesbin;
set Casesppses;
if Avg_P05300 <  29250 then inc_bin_25 = 0;
else inc_bin_25=1;
if Avg_P05300 <  37703 then inc_bin_m = 0;
else inc_bin_m=1;
if Avg_P05300 < 4772 then inc_bin_75 = 0;
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
if inc_bin_m=0 then finc_bin_m=1;
if inc_bin_m=1 then finc_bin_m=0;
if col_bin_m=0 then fcol_bin_m=1;
if col_bin_m=1 then fcol_bin_m=0;
run;



/*add urb/rural and LU*/


PROC IMPORT OUT= x1
            DATAFILE= "Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN002_Cases_guid_MIA\cases_DVT_clipped_XY.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

 data x1(rename=(guid_=guid ));
set x1;
date=input(adate,date9.);
format date mmddyy10.;
drop adate;
run; 
proc summary nway data=x1;
class guid;
var mon20;
output out=OUTPUTFILE1 mean=mon20;
run; 


PROC IMPORT OUT= WORK.pmguidma
            DATAFILE= "Y:\Projects\P002.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_emission.dbf" 
			            DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN; 

PROC IMPORT OUT= WORK.pmguidne
            DATAFILE= "Y:\Projects\P001_NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
			            DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN; 




proc sql;
  create table pmguidma11 as
  select guid,p_open
  from pmguidma
quit;

proc sql;
  create table pmguidne11 as
  select guid,p_open
  from pmguidne
quit;

data po;
set pmguidma11 pmguidne11;
run; 


proc sort data = cases; by guid   ;run;
proc sort data = OUTPUTFILE1 ; by  guid  ;run;
proc sort data = po ; by  guid  ;run;

data DATA3;
merge cases(in=a) OUTPUTFILE1 (in=b) po (in=c)  ;
  by  guid ;
    if a;
	run; 



data data4;
set data3;
m=month(date);
season=1;
if m=1 or m=2 or m=3 or m=10 or m=11 or m=12 then season=0;
run; 


libname xo 'Y:\Projects\P051.PAD_NEMIA\2.work\' ;

data xo.apd_counts;
set DATA4;
where Partery2=1 or Partery=1;
run; 


libname full 'Y:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\PM\' ;

data cases ;
set xo.apd_counts;
drop pmnew--temp_fmayear;
run; 

/*rename*/
data cases ;
set cases (rename=(date=INDATE));
run; 


/*Create data with: ID (QID), event date (indate)
/***Create control days***/
/***same day of the week within same month***/
data CVA;set cases;
DOW=WEEKDAY(INDATE);
CONTROL1=INDATE-7;
CONTROL2=INDATE-14;
CONTROL3=INDATE-21;
CONTROL4=INDATE-28;
CONTROL5=INDATE+7;
CONTROL6=INDATE+14;
CONTROL7=INDATE+21;
CONTROL8=INDATE+28;
FORMAT CONTROL1 CONTROL2 CONTROL3 CONTROL4 CONTROL5 CONTROL6 CONTROL7 CONTROL8
DDMMYY9.;
MONTH1=MONTH(CONTROL1);
MONTH2=MONTH(CONTROL2);
MONTH3=MONTH(CONTROL3);
MONTH4=MONTH(CONTROL4);
MONTH5=MONTH(CONTROL5);
MONTH6=MONTH(CONTROL6);
MONTH7=MONTH(CONTROL7);
MONTH8=MONTH(CONTROL8);
month=month(indate);
RUN;
/*Keep only days within the same month*/
DATA CVA;SET CVA;
if month1=month then control1=control1;else control1=.;
if month2=month then control2=control2;else control2=.;
if month3=month then control3=control3;else control3=.;
if month4=month then control4=control4;else control4=.;
if month5=month then control5=control5;else control5=.;
if month6=month then control6=control6;else control6=.;
if month7=month then control7=control7;else control7=.;
if month8=month then control8=control8;else control8=.;
run;

/*Rename case date as "control0"*/
data cva;set cva (rename=(indate=control0));run;

/*Sort by ID*/

 proc sort data= cva;
 by QID;
 run;

/*Restracture*/
PROC TRANSPOSE DATA=cva OUT=cva1
 PREFIX=control;
 VAR control0-control8 ;
 BY QID;
 run;

 /*Delete dates that were not within the same month*/
 data cva1;set cva1 (keep=QID _NAME_ control1);
 where control1>.;run;


 /***Rename as case day and control days***/
 data cva1;set cva1;
 if _NAME_="control0" then case=1;else case=0;
 if case=1 then Time=1;else Time=2;
 run;
 proc freq data=cva1;table case;run;


data cva1;set cva1 (rename=(control1=day));
FORMAT CONTROL1 DDMMYY9.;
run;


/*add back guid*/


data cguid ;
set xo.apd_counts;
keep qid guid age sex race;
run;

 proc sort data= cguid;
 by qid;
 run;
 proc sort data= cva1;
 by qid;
 run;
 data cva2;
 merge cva1  cguid  ;
 by qid;
 run;



 /*Merge PM by aodid and date*/

data  poll_v3 ;
set full.fullpm;
keep date--pmnew temp_f;
run; 



data  poll_v3 (rename=(date=day));
set poll_v3;
run; 


 proc sort data=  poll_v3;
 by day guid;
 run;
 proc sort data= cva2;
 by day guid;
 run;
 data cva3;
 merge cva2   poll_v3  ;
 by day guid;
 run;

 data cva4;
 merge cva3;
 if case=. then delete;
 run;

 data cva5;
 set cva4;
 dow=weekday(day);
 if dow=1 then wd1=1; else wd1=0;
if dow=2 then wd2=1; else wd2=0;
if dow=3 then wd3=1; else wd3=0;
if dow=4 then wd4=1; else wd4=0;
if dow=5 then wd5=1; else wd5=0;
if dow=6 then wd6=1; else wd6=0;
 run;



 /*export to r*/


PROC EXPORT DATA= cva5
            OUTFILE= "Y:\Projects\P051.PAD_NEMIA\2.work\CXO_APD_counts.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;





