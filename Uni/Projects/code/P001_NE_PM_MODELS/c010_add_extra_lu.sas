PROC IMPORT OUT= WORK.forest
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\forest.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.tden
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\tden.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


options mprint;
%macro import(year=);

PROC IMPORT OUT= Ymod1&year
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 
proc sort data = forest; by guid   ;run;
proc sort data = tden; by guid   ;run;
proc sort data = Ymod1&year ; by guid ;run;

data Ymod1&year;
merge Ymod1&year(in=a) tden (in=b keep=guid tden) forest (in=c keep=guid per_fore_1)  ;
  by guid;
  if a;
run;  

PROC EXPORT DATA= Ymod1&year 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\withlu\t&year..csv" 
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
%import(year=2009); 
%import(year=2010); 
%import(year=2011); 




options mprint;
%macro import(year=);

PROC IMPORT OUT= Ymod2&year
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 
proc sort data = forest; by guid   ;run;
proc sort data = tden; by guid   ;run;
proc sort data = Ymod2&year ; by guid ;run;

data Ymod2&year;
merge Ymod2&year(in=a) tden (in=b keep=guid tden) forest (in=c keep=guid per_fore_1)  ;
  by guid;
  if a;
run;  

PROC EXPORT DATA= Ymod2&year 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\withlu\y&year..csv" 
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
%import(year=2008); 
%import(year=2009); 
%import(year=2010); 
%import(year=2011); 
