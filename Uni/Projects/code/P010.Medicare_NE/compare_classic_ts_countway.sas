libname cway 'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.1.Raw_data\d.countway data\' ;


data poll_lag;
set cway.poll_lag;
run; 




PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN002_calculate_SES\SES_NE_LU.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;






PROC IMPORT OUT= WORK.CASES 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\resp.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


/*export to subset to boston*/

data CASES_bos;
set CASES ;
where y <= 42.70;
run; 

data CASES_bos;
set CASES_bos ;
where y > 42.00;
run; 


data CASES_bos;
set CASES_bos ;
where x > -71.64;
run; 

PROC EXPORT DATA= CASES_bos 
            OUTFILE= "C:\Users\ekloog\Documents\tmp\casa_resp.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

 
data cases2;
set CASES_bos(rename=(dateadmi=date));
count=1;
run;

/*This step creates a dataeset with counts per day (date) for cases*/

proc summary nway data=cases2;
class date ;
var count ;
output out=cases_count sum=;
run;


/*add SES*/
/*proc sort data = inc; by guid   ;run;*/
/*proc sort data = cases_count ; by guid ;run;*/
/**/
/*data cases_count;*/
/*merge cases_count(in=a) inc (in=b)  ;*/
/*  by guid;*/
/*    if a;*/
/*	run; */



proc sort data=poll_lag;
by  date;
run;

proc sort data=cases_count;
by  date;
run;


data times;
merge cases_count (in=a)   poll_lag (in=b keep=date pm25_pred tempc);
by  date;
  if a;
run;

data times4;
set times;
if pm25_pred = . then delete;
format date JULIAN.;
run;

PROC EXPORT DATA= WORK.times4 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\3.Analysis\AN002_compare_2_classic_timeseries\cw.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
