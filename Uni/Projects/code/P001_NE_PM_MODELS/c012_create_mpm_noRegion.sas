
libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

options mprint;
%macro import(year=);


data pm&year;
set pm.pm&year;
 if pm25 < 0 then delete;
 if pm25 > 32 then delete;
run; 

proc means data = pm&year mean var maxdec = 3;
 class date;
  var pm25;
   ods output summary = summary&year;
run;
quit;



data summary&year(drop = PM25_Mean PM25_var);
 set summary&year;
  mpm_F = PM25_Mean;
  var_F = PM25_var;
run;
 
proc means data = summary&year nmiss;
 var mpm_F ;
run;


PROC IMPORT OUT= WORK.mod2pred&year
      DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T&year._m2_pred.dbf" 
	           DBMS=DBF REPLACE;
					        GETDELETED=NO;
							RUN; 

proc sort data = summary&year;   by date ;run;
proc sort data = mod2pred&year ; by date ;run;

data mod2pred&year._V2;
merge mod2pred&year(in=a) summary&year(in=b keep=date mpm_F var_F);
  by date;
    if a;
run; 




data mod2pred&year._V3(keep=guid Lat_AOD Long_AOD mpm_F bimon pred date);
 set mod2pred&year._V2;
	m = month(date); 
if (m=1 or m=2)  then bimon=1; 
if (m=3 or m=4)  then bimon=2;
if (m=5 or m=6)  then bimon=3;
if (m=7 or m=8)  then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;
run; 


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/


PROC EXPORT DATA= WORK.mod2pred&year._V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T&year._m2_pred_mpm.csv" 
			DBMS=CSV REPLACE;
			 PUTNAMES=YES;
RUN;
							  

/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid&year;
 set fullg.y&year ;
run; 


proc sort data = summary&year; by date ;run;
proc sort data = fgrid&year ;  by date ;run;

data fgrid&year._V2;
merge fgrid&year(in=a)  summary&year (in=b keep=date  mpm_F)  ;
  by date ;
    if a;
	run; 


data fgrid&year._V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid&year._V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 




PROC EXPORT DATA= WORK.fgrid&year._V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_&year..csv" 
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



proc freq data = mod2pred2011_V3;
table bimon / list;
run; 


proc gplot data = mod2pred2011_V3;
 plot pred*date;
run;
quit;

proc means data = mod2pred2011_V3 var maxdec = 3; 
 class date;
  var pred;
  ods output Summary = Summary_Var;
run;
quit;

symbol1 v=dot h=0.5 w=0.5 c=blue;
proc gplot data=Pm2011;
Title "TITLE";
  plot pm25*date/ grid;
     label VAR1 = "LABLE1";
	       label VAR2 = "LABLE2";
		   run; 
		   quit; 
