
PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_emission.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


options mprint;
%macro import(year=);


PROC IMPORT OUT= mod3_&year
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_&year..csv" 
			            DBMS=CSV REPLACE;
						     GETNAMES=YES;
							      DATAROW=2; 
								  RUN;
	


proc sort data = mod3_&year; by guid   ;run;
proc sort data = lu ; by guid ;run;

data mod3_&year;
merge mod3_&year(in=a) lu(in=b keep=guid population)  ;
  by guid;
    if a;
	run; 


	data mod3_&year;
	set mod3_&year;
	where population >10;
	run; 

PROC EXPORT DATA= WORK.mod3_&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN005_mod3\ufullgrid_mpm_&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
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


