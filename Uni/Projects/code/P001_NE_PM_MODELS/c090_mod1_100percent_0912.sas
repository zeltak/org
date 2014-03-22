libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;

options mprint;
%macro import(year=);

PROC IMPORT OUT= WORK.out&year 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\out&year..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




data out&year._v3;
set out&year;
s_avgsq=s_avgs*s_avgs;
if sitecode="330130003" then delete;
if sitecode="230172011" then delete;
if guid=71454315 then delete;
run;



proc mixed data = out&year._v3 method=reml;
class zid date ;
 weight normwt;
   model pm25 = aod Temp_F_x t_avgs s_avgs  st_avgs  / s outpred=pdataA_&year;
    random int aod Temp_F_x / sub = date s ;
      random x1--x21 / sub = zid type=toep(1) s;
  ods output  SolutionF =  SolutionF&year.A;
    ods output  SolutionR =  SolutionR&year.A;
run;



data aod.SolutionF&year.A;     set SolutionF&year.A; run;
data aod.SolutionR&year.A;     set SolutionR&year.A; run;
data aod.pdataA_&year;        set pdataA_&year;         run;





PROC EXPORT DATA= pdataA_&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\pdataA_&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;



/*OVERALL*/


proc reg data = pdataA_&year;
model pm25= pred / clb;
title "OA&year";
ods output FitStatistics = overall&year;
run;
quit;


%MEND ;
%import(year=2009);
%import(year=2010);
%import(year=2011);

/**/
/*symbol1 v=dot h=0.5 w=0.5 c=blue;*/
/*proc gplot data=pdataA_xxx;*/
/*Title "TITLE";*/
/*  plot pm25*pred/ grid;*/
/*     label VAR1 = "LABLE1";*/
/*	       label VAR2 = "LABLE2";*/
/*		   run; */
/*		   quit; */
/**/
/**/
/**/
/*proc mixed data = out2011_v3 method=reml;*/
/*class date ;*/
/*   model pm25 = aod  / s outpred=pdataA_xxx;*/
/*    random int aod / sub = date s ;*/
/*        run;*/
/*proc reg data = pdataA_xxx;*/
/*model pm25= pred / clb;*/
/*title "OA&year";*/
/*run;*/
/*quit;*/
