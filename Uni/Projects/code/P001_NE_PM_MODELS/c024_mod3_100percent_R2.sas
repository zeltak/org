
options mprint;
%macro import(year=);



PROC IMPORT OUT= WORK.pmmon_&year
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN008_mod3_corr\pmguidt&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



PROC IMPORT OUT= mod3_&year
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



data mod3_&year._v2;
set mod3_&year ;
newdate = input(date,DATE9.);
format newdate mmddyy10.;
drop date;
run;


data mod3_&year._v3;
set mod3_&year._v2(rename=(newdate=date ));;
run;





proc sort data = pmmon_&year; by guid date   ;run;
proc sort data = mod3_&year._v3 ; by guid date ;run;

data m3_merg_&year;
merge pmmon_&year(in=a) mod3_&year._v3 (in=b)  ;
  by guid date;
    if a;
	run; 

proc reg data=m3_merg_&year;
model pm25= pm_mod3;
run;
quit; 

PROC EXPORT DATA= m3_merg_&year 
            OUTFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN008_mod3_r2_predictions\T&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);
%import(year=2009);
%import(year=2010);
%import(year=2011);




symbol1 v=circle h=0.5 w=0.5 c=blue;
symbol2 v=circle h=0.5 w=0.5 c=red;
proc gplot data=M3_merg_2011;
Title "TITLE";
  plot pm_mod3*date pm25*date/ grid overlay;
     label VAR1 = "LABLE1";
	       label VAR2 = "LABLE2";
		   where pm25<100;
		   run; 
		   quit; 
