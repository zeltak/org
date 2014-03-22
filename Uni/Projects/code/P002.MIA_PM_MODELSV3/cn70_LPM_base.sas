
options mprint;
%macro import(year=);



PROC IMPORT OUT= WORK.pmmon_&year
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN008_mod3_corr\pmguidt&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



PROC IMPORT OUT= mod3_&year
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T&year..dbf" 
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




data Final_;
set M3_merg_2000 M3_merg_2001 M3_merg_2002 M3_merg_2003 M3_merg_2004 M3_merg_2005 M3_merg_2006 M3_merg_2007 M3_merg_2008;
run; 

PROC EXPORT DATA= WORK.Final_ 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN070_LPM_stage_base\LPM_all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
