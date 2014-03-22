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




/*home death=0  /////// outside home=1*/

data cases;
set cases;
if placdth=5 or placdth=6  then dplace=0;
else dplace=1;
run; 


/*create outside home dataset*/

/*data cases_outhome;*/
/*set cases;*/
/*if dplace=0 then delete ;*/
/*run; */


/*create home dataset*/

data cases_home;
set cases;
if dplace=1 then delete ;
run; 

/*clean data*/

data cases_home;
set cases_home;
if AGE <20 then delete;
if EDUC >40 then delete;
run; 

data cases_home;
set cases_home;
if race=01 then nrace=1;
else nrace=0;
if EDUC>12 then nedu=1;
else nedu=0;
run; 


proc sort data = cases_home; by guid date   ;run;
proc sort data = Poll_v3 ; by guid date   ;run;

data DATA3;
merge cases_home(in=a) Poll_v3 (in=b)  ;
  by guid date  ;
    if a;
	run; 


data data3;
set data3;
if x=. then delete;
if lpm <-100 then delete:
run; 




PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\close_2_monitor.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = lu; by guid   ;run;
proc sort data = data3 ; by guid ;run;

data data4;
merge data3(in=a) lu (in=b keep=guid mon20)  ;
  by guid;
    if a;
	run; 



	PROC EXPORT DATA= DATA4 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN003_logistic\cases.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
