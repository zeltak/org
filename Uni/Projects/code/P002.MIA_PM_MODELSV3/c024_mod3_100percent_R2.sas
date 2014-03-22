
options mprint;
%macro import(year=);



PROC IMPORT OUT= WORK.pmmon_&year
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN008_mod3_corr\pmguidt&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



PROC IMPORT OUT= mod3_&year._v3
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



proc sort data = pmmon_&year; by guid date   ;run;
proc sort data = mod3_&year._v3 ; by guid date ;run;

data m3_merg_&year;
merge pmmon_&year(in=a) mod3_&year._v3 (in=b)  ;
  by guid date;
    if a;
	run; 


PROC EXPORT DATA= m3_merg_&year 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN008_mod3_r2_predictions\T&year..dbf" 
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




