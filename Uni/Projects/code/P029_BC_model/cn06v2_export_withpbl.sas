libname aod 's:/ENVEPI/Airs/BC Model/nasforschwartzbcmodel/' ;


/*we use sampledate as key var to match pbl and other temporal data*/



/*import nas dataset with all day/case combo*/
data bc9508;
set aod.nasexpdays_ap_met_95_10;
run; 

data bc9508;
set bc9508;
date=sampledate+1;
format DATE  mmddyy8.;
run; 

/*import 3 hour reading of pbl for MA*/
libname pbl 's:/ENVEPI/spatialdata_climate/PBL/' ;

data pbl;
set pbl.pblmasshpbl_ap08years_2;
run; 


/*subset for only countway data countway*/

data pblcw ;
set pbl;
where NLAT=42.209000 and ELON_posne= -71.061000;
run; 

/*in order to create a 24 hour mean from 9am to 9am if the hour is less then 9am then bring the day 1 day back*/

data pblcwh ;
set pblcw;
if EST_NHour < 9 then sampledate = ESTDate - 1;
else sampledate = ESTDate;
format sampledate mmddyy10.;
run;

/*create a 24 hour mean from 9am to 9amm*/

proc summary nway data=pblcwh;
class sampledate;
var hpbl;
output out=hpbl_mean mean=hpbl;
run; 

proc sort data = bc9508; by sampledate   ;run;
proc sort data = hpbl_mean ; by sampledate ;run;

data bc9508pbl;
merge  bc9508 (in=a) hpbl_mean (in=b)  ;
  by sampledate;
    if a;
	run; 

/*test*/

/*data hpbl_meanx (drop=hpbl sampledate);*/
/*set hpbl_mean;*/
/*date=sampledate;*/
/*format date mmddyy10.;*/
/*pblcw=hpbl;*/
/*run; */
/**/
/**/
/**/
/*PROC IMPORT OUT= sumreg*/
/*  DATAFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/sumreg3.csv" */
/*    DBMS=CSV REPLACE;*/
/*	  GETNAMES=YES;*/
/*	    DATAROW=2; */
/*		RUN;*/
/*		 */
/*proc sort data = sumreg; by date   ;run;*/
/*proc sort data = hpbl_meanx ; by date ;run;*/
/**/
/*data DATA3;*/
/*merge sumreg(in=a) hpbl_meanx (in=b)  ;*/
/*  by date;*/
/*    if a;*/
/*	run; */






data nas3;
set  bc9508pbl;
w_dir_1=0;
if winddir_ind=1 then w_dir_1 =1;
w_dir_2=0;
if winddir_ind=2 then w_dir_2 =1;
w_dir_3=0;
if winddir_ind=3 then w_dir_3 =1;
day = JULDATE( date );
dayz=compress(day);
yearday=substr(dayz,3,3); 
weekday=weekday(date);
nlcd_ind=0;
if nlcd_urb01 > 765 then nlcd_ind=1;
Year=year(date);
m=month(date);
run; 


data nas3;
set nas3;
drop day dayz AddStartDate AddEndDate _type_ _freq_ ;
if m=1 then delete;
if m=2 then delete;
if m=3 then delete;
if m=4 then delete;
if m=1 then delete;
if m=11 then delete;
if m=12 then delete;
run; 

data A1 A2 A3 A4 A5 A6 A7 A8 A9 A10;
   set nas3;
  if 1        <= _n_ <= 5000000  then output A1;
  if 5000001 <= _n_ <= 20000000  then output A2;
 run;



PROC EXPORT DATA= a1
            OUTFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A1.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

 
							 PROC EXPORT DATA= a2
            OUTFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A2.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


data nas3_95;
set nas3;
where year=1995;
run; 



PROC EXPORT DATA= nas3_95
            OUTFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1/A95.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
