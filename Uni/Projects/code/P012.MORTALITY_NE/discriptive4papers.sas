/*pollution part*/


libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


data poll;
set poll.poll_lag_v5;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


/*varience stuff*/

data poll2;
set  poll;
pmo=pmnewmayear-pmnewma1;
run; 


proc univariate data=poll2;
var pmo;
histogram  pmo;
run;
 


proc varcomp data=poll2;
class date guid;
model pmo=date guid;
run;





proc means data=poll mean median std  range  QRANGE q1 q3 n ;
var pmnewma1 pmnewmayear temp_fma1; 
run; 




/*cases discripitives*/


PROC IMPORT OUT= WORK.mortguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\cases_guid_reg_final.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data cases;
set mortguid(rename=(ddate=date));
count=1;
where date>='01MAR2000'D and date<='31Dec2008'D ; 
run;


proc freq data=cases;
table sex ;
run; 


data cases;
set cases;
 if race = "01" then brace=1;
 if race = "02" then brace=2;
 if race in ("00","03","04","05","06","07","08","09","10","11","12","13","14","15","99") then brace = 3;
run; 




proc freq data=cases;
table brace ;
run; 




proc means data=cases mean ;
var age; 
run; 



data cases;
set cases;
 if educ >= 12 then nedu=1;
if educ < 12 then nedu=0;
if educ = 99 then nedu=0;
run; 

proc freq data=cases;
table nedu ;
run; 

