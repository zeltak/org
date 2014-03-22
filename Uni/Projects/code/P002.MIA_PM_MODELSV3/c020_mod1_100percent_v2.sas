libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;

options mprint;
%macro import(year=);

PROC IMPORT OUT= WORK.out&year 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\out&year..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data out&year._v2;
  set out&year;
   newdate = input(date,date9.); 
  format newdate date9.;
  drop date;
run;


data out&year._v3;
set out&year._v2(rename=(newdate=date ));;
s_avgsq=s_avgs*s_avgs;
drop  reg_name;
run;



proc mixed data = out&year._v3 method=reml;
class reg_id zid date sitecode;
 weight normwt;
   model pm25 = aod Temp_F_x t_avgs s_avgs  st_avgs  / s outpred=pdataA_&year;
    random int aod Temp_F_x / sub = date s ;
	random int aod  / sub = date(reg_id) s;
    random x1--x18 / sub = zid type=toep(1) s;
	ods output  SolutionF =  SolutionF&year.A;
    ods output  SolutionR =  SolutionR&year.A;
run;



data aod.SolutionF&year.A;     set SolutionF&year.A; run;
data aod.SolutionR&year.A;     set SolutionR&year.A; run;
data aod.pdataA_&year;        set pdataA_&year;         run;










PROC EXPORT DATA= pdataA_&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\pdataA_&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;



/*OVERALL*/


proc reg data = pdataA_&year;
model pm25= pred / clb;
title "OA";
ods output FitStatistics = overall&year;
run;
quit;


data overall&year(keep = Type nValue2);
set overall&year;
where label1="Root MSE";
  Type = "overall&year";
run;



/*calculating s/t R2*/

proc summary nway data=pdataA_&year;
 class sitecode;
  var pm25  pred;
   output out=T&year._agg mean=annmeanpm25  annpred;
run;



proc sort data=pdataA_&year;
 by sitecode;
run;

proc sort data=T&year._agg;
 by sitecode;
run;


data N2pdataA_&year;
 merge pdataA_&year T&year._agg(keep=sitecode annmeanpm25  annpred);
  by sitecode;
run;


data N3pdataA_&year;
 set N2pdataA_&year;
  delpm = pm25 -annmeanpm25;
   delpred    = pred    - annpred;
run;



/*#spatial*/
proc reg data = N3pdataA_&year;
weight normwt;
model annmeanpm25 = annpred / clb;
  title "Spatial &year";
  ods output FitStatistics = Spatial&year;
run;
quit;

 

data Spatial&year(keep = Type nValue2);
set Spatial&year;
where label1="Root MSE";
  Type = "Spatial &year";
run;


/*title "Temporal"*/
proc reg data = N3pdataA_&year;
weight normwt;
model  delpm = delpred / clb;
ods output FitStatistics = Temporal&year;
run;
quit;

 

data Temporal&year(keep = Type nValue2);
set Temporal&year;
where label1="Root MSE";
Type = "Temporal &year";
run;


data Table_&year;
set Overall&year Temporal&year Spatial&year;
R2=nValue2;
run; 


PROC EXPORT DATA= WORK.Table_&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.5.Results\mod1\Full_&year.xls" 
            DBMS=EXCEL5 REPLACE;
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


