
libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;

PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN0021_calculate_SES\SES_NE.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


data poll;
set poll.poll_lag;
where date>='01Jan2000'D and date<='31Dec2006'D ; 
run;


proc means data=poll  mean min max median std range Qrange q1 q3 ;
var pmnew pmnewmayear temp_f; 
run; 


options mprint;
%macro import(type=);


/*import mortality cases*/


PROC IMPORT OUT= WORK.&type
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid\&type..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data &type;
set &type;
date=dateadmi;
run; 

proc sort data = &type; by guid date  ;run;
proc sort data = poll ; by guid date  ;run;

data  poll_&type;
merge &type (in=a) poll (in=b keep=guid date pmnewmayear)  ;
  by guid date;
    if a;
	run; 

data  poll_&type._high;
set  poll_&type;
where pmnewmayear > 9.8;
run; 

data  poll_&type._low;
set  poll_&type;
where pmnewmayear <= 9.8;
run; 


proc means data= poll_&type._high n mean;
var age; 
TITLE "poll_&type._high";
run; 

proc freq data= poll_&type._high ;
table sex race2 / list;
TITLE "poll_&type._high";
run; 

proc means data= poll_&type._low n mean;
var age; 
TITLE "poll_&type._low";
run; 

proc freq data= poll_&type._low ;
table sex race2 / list;
TITLE "poll_&type._low";
run; 


%MEND ;

%import(type=resp );
%import(type=cvd    );
%import(type=stroke );
%import(type=diab2 );


