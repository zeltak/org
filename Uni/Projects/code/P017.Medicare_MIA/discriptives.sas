
libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


data poll;
set poll.poll_lag_V5;
where date>='03Mar2000'D and date<='31Dec2006'D ; 
run;

data pollY (keep=date pmnew con guid);
set poll_v3;
con=1;
run; 


proc mixed data = pollY method=reml;
class guid  date ;
   model pmnew = / s outpred=pdataA;
  random int   / sub = guid s ;
    random int   / sub = date s ;
  ods output  SolutionF =  SolutionF;
    ods output  SolutionR =  SolutionR;
run;



/**/
/*discriptveis*/
/**/



options mprint;
%macro import(year=);

data &year;
set all2;
where &year =1;
run; 

%MEND ;

%import(year=cvd);
%import(year=resp);
%import(year=stroke);
%import(year=copd);

data cvd;
set cvd;
if race=1 then brace=1; *white;
if race = 3 then brace=2; *black;
if race ne 1 or race ne 2 then brace=3; *black

run; 


proc freq data=cvd;
table brace / list;
run; 




data map(where=(date="01JUN2000"D )) ;;
set all2;
run;




PROC EXPORT DATA= map
            OUTFILE= "c:\Users\ekloog\Documents\tmp\miamap.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

 
/*table discriptives*/

data all3;
set all2;
 if race = "1" then brace=1;
 if race = "2" then brace=2;
 if race in ("0","3","4","5","6") then brace = 3;
run; 


/*get percent sex*/

/*all*/

proc freq data=all3;
table sex ;
run; 

/*by type*/

proc freq data=all3;
table sex ;
where cvd=1;
run; 

proc freq data=all3;
table sex ;
where resp=1;
run; 

proc freq data=all3;
table sex ;
where copd=1;
run; 

proc freq data=all3;
table sex ;
where stroke=1;
run; 


/*all race*/

proc freq data=all3;
table brace ;
run; 

/*by type*/

proc freq data=all3;
table brace ;
where cvd=1;
run; 

proc freq data=all3;
table brace ;
where resp=1;
run; 

proc freq data=all3;
table brace ;
where copd=1;
run; 

proc freq data=all3;
table brace ;
where stroke=1;
run; 

proc means data=all3  mean ;
var age; 
where cvd=1;
run; 

proc means data=all3  mean ;
var age; 
where resp=1;
run; 

proc means data=all3  mean ;
var age; 
where copd=1;
run; 

proc means data=all3  mean ;
var age; 
where stroke=1;
run; 


/*urban rural*/

PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\MIA_ur_guid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data pmguid;
set pmguid;
guid2=input(guid, 8.); 
drop guid;
run;



data pmguid;
set pmguid;
guid=guid2; 
drop guid2;
run; 

proc sort data = pmguid; by guid   ;run;
proc sort data = all3 ; by guid ;run;

data all4;
merge all3(in=a) pmguid (in=b)  ;
  by guid;
    if a;
	run; 


proc freq data=all4;
table mon20 ;
run; 


/*pollution*/


proc means data=poll_v3 mean min max  median std range  QRANGE q1 q3 n ;
var pmnewma1 temp_fma1; 
run; 
