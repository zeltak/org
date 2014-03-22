/*** Macro to import PM dataset ***/


libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;


 

options mprint;
%macro import(year=);

proc import datafile="c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.1.Raw_data\views PM\&year..txt"
 dbms=dlm out=work.PM&year replace;
   delimiter=",";
     getnames=yes;
      guessingrows=500;
run;


data pm.PM&year(drop = MF_Value Latitude Longitude);
 set work.PM&year;
  PM25 = MF_Value;  
   Lat_PM  = Latitude;
   Long_PM = Longitude; 
   if MF_Value < 0 then delete;
    format date date9.; 
run; 

/** Delete from workspace **/

proc datasets lib = work; delete PM&year; run; quit;


%mend;

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

/*CREATE A FULL ALL YEAR PM FILE*/

data pm.all_pm;
set pm.pm2000 pm.pm2001 pm.pm2002 pm.pm2003 pm.pm2004 pm.pm2005 pm.pm2006 pm.pm2007 pm.pm2008;
run; 


/*CREATE A FULL UNIQUE PM STATION ID WITH XY*/

proc summary nway data=pm.all_pm;
class sitecode;
var long_pm lat_pm ;
output out=pm.pmsite mean=long_pm lat_pm;
run; 


PROC EXPORT DATA= pm.pmsite
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pm_sites.dbf" 
		   DBMS=DBF REPLACE;
		   RUN;
						 


/*Prepare data for mod3 (GAMM) correlations*/


PROC IMPORT OUT= WORK.id_reg 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_sitecode.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



options mprint;
%macro import(year=);

data pm&year;
set pm.pm&year;
run; 




proc sort data = PM&year ; by sitecode;run;
proc sort data = id_reg ; by sitecode ;run;

data PM&year._v2;
 merge PM&year (in=a) id_reg (in=b keep=sitecode guid);
   by sitecode;
   if guid=. then delete;
   if a;
   run; 


%MEND ;

%import(year=2000);


PROC EXPORT DATA= WORK.PM&year._v2
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN008_mod3_corr\pmguidt&year..csv" 
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




/*discriptives for paper*/

proc means data=pm.all_pm n min max mean std nmiss qrange;
var pm25; 
run; 
