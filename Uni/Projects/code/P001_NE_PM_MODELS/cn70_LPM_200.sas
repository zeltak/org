PROC IMPORT OUT= WORK.pemis
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\GIS\200m\pemis.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



data pemis;
set pemis;
dist_pemis=NEAR_DIST\1000;
run; 





PROC IMPORT OUT= WORK.a1
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\GIS\200m\dist_a1.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



data a1;
set a1;
dist_A1=NEAR_DIST\1000;
run; 

libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\GIS\200m\' ;


data grid200;
set aod.grid200;
run; 


 
proc sort data = pemis; by pointid   ;run;
proc sort data = A1; by pointid   ;run;
proc sort data = grid200 ; by pointid ;run;

data DATA3;
merge grid200(in=a) a1 (in=b keep=pointid dist_A1) pemis(in=c keep=pointid dist_pemis)  ;
  by pointid;
    if a;
	run; 


PROC EXPORT DATA= DATA3 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN070_LPM_stage_200x200_base\lu200.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


PROC IMPORT OUT= WORK.poinguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\GIS\200m\g200_points_guid.dbf" 
            DBMS=DBF   REPLACE;
     GETDELETED=NO;
RUN;  


PROC IMPORT OUT= Ygrid
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN070_LPM_stage_200x200_base\lu200.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc sort data = poinguid; by pointid   ;run;
proc sort data = Ygrid ; by pointid  ;run;

data Ygrid3;
merge Ygrid(in=a) poinguid (in=b keep=pointid guid)  ;
  by pointid ;
    if a;
	run; 
		 

data Ygrid3;
set Ygrid3;
if guid=. then delete;
run; 


PROC EXPORT DATA= Ygrid3 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN070_LPM_stage_200x200_base\lpmmap.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



proc summary nway data=Ygrid3;
class guid;
var lpm;
output out=OUTPUTFILE mean=lpm;
run; 

PROC IMPORT OUT= Y23
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\short\m23\old_by_guid\resp0106.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		

proc sort data = Y23; by guid   ;run;
proc sort data = OUTPUTFILE ; by guid ;run;

data Y23_2;
merge Y23(in=a) OUTPUTFILE (in=b keep=guid lpm)  ;
  by guid;
  if a;
run;  

data Y23_2;
set Y23_2;
pmnewmayearlpm=pmnewmayear+lpm;
run; 

PROC EXPORT DATA= WORK.Y23_2
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.10.Medicare_NE\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\short\m23\old_by_guid\resp0106lpm.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
