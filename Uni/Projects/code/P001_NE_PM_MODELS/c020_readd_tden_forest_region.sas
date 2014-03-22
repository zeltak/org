PROC IMPORT OUT= WORK.reg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


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



%macro mod3expo(year=);

PROC IMPORT OUT= T&year
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DAAROW=2; 
		RUN;
		 

data T&year;
set T&year;
drop reg_id reg_name;
run; 

proc sort data = T&year; by guid   ;run;
proc sort data = reg; by guid ;run;
proc sort data = forest; by guid ;run;
proc sort data = tden; by guid ;run;

data T&year;
merge T&year(in=a) reg (in=b keep=guid reg_id reg_name) forest (in=c keep=guid per_fore_1)  tden (in=d keep=guid tden);
  by guid;
    if a;
	run; 
PROC EXPORT DATA= WORK.T&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


%mend mod3expo;

%mod3expo(year=2000);
%mod3expo(year=2001);
%mod3expo(year=2002);
%mod3expo(year=2003);
%mod3expo(year=2004);
%mod3expo(year=2005);
%mod3expo(year=2006);
%mod3expo(year=2007);
%mod3expo(year=2008);


/*FOR MOD2 FILES*/


%macro mod3expo(year=);

PROC IMPORT OUT= TT&year
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DAAROW=2; 
		RUN;
		 

data TT&year;
set TT&year;
drop reg_id reg_name tden per_fore_1 ;
run; 

proc sort data = TT&year; by guid   ;run;
proc sort data = reg; by guid ;run;
proc sort data = forest; by guid ;run;
proc sort data = tden; by guid ;run;

data TT&year;
merge TT&year(in=a) reg (in=b keep=guid reg_id reg_name) forest (in=c keep=guid per_fore_1)  tden (in=d keep=guid tden);
  by guid;
    if a;
	run; 


PROC EXPORT DATA= WORK.TT&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


%mend mod3expo;

%mod3expo(year=2000);
%mod3expo(year=2001);
%mod3expo(year=2002);
%mod3expo(year=2003);
%mod3expo(year=2004);
%mod3expo(year=2005);
%mod3expo(year=2006);
%mod3expo(year=2007);
%mod3expo(year=2008);



/*FOR MOD3 FILES*/

%macro mod3expo(year=);


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid&year( drop=reg_id);
set fullg.y&year ;
run; 

proc sort data = fgrid&year; by guid   ;run;
proc sort data = reg; by guid ;run;


data fgrid&year;
merge fgrid&year(in=a) reg (in=b keep=guid reg_id reg_name);
  by guid;
    if a;
	run; 


data fullg.y&year ;
set  fgrid&year;
run; 


 %mend mod3expo;

%mod3expo(year=2000);
%mod3expo(year=2001);
%mod3expo(year=2002);
%mod3expo(year=2003);
%mod3expo(year=2004);
%mod3expo(year=2005);
%mod3expo(year=2006);
%mod3expo(year=2007);
%mod3expo(year=2008);


proc univariate data=Fgrid2000;
var reg_id;
run;
 
proc means data=Fgrid2000 n min max mean std nmiss;
var reg_id; 
run; 
