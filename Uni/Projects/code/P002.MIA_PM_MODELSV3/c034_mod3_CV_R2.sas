libname out10 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;




options mprint;
%macro import(year=, stp=);


data mod1_t&year._10p_&stp;
set out10.pred_&stp._&year;
run; 


PROC IMPORT OUT= mod3_&year._&stp
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN010_mod3_CV_pred_mpm\poll_T&year._&stp..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data xmod3_&year._&stp;
set mod3_&year._&stp ;
newdate = input(date,DATE9.);
format newdate mmddyy10.;
drop date;
run;


data mod3_&year._&stp;
set xmod3_&year._&stp(rename=(newdate=date ));;
run;



proc sort data = mod1_t&year._10p_&stp; by guid date   ;run;
proc sort data = mod3_&year._&stp ; by guid date ;run;

data m3_merg_&year._&stp;
merge mod1_t&year._10p_&stp(in=a) mod3_&year._&stp (in=b)  ;
  by guid date;
    if a;
	run; 

data m3_merg_&year._&stp;
set m3_merg_&year._&stp;
if pm_mod3=. then delete;
run; 

			 



%MEND ;

%import(year=2000, stp = s1);
%import(year=2000, stp = s2);
%import(year=2000, stp = s3);
%import(year=2000, stp = s4);
%import(year=2000, stp = s5);
%import(year=2000, stp = s6);
%import(year=2000, stp = s7);
%import(year=2000, stp = s8);
%import(year=2000, stp = s9);
%import(year=2000, stp = s10);



%import(year=2001, stp = s1);
%import(year=2001, stp = s2);
%import(year=2001, stp = s3);
%import(year=2001, stp = s4);
%import(year=2001, stp = s5);
%import(year=2001, stp = s6);
%import(year=2001, stp = s7);
%import(year=2001, stp = s8);
%import(year=2001, stp = s9);
%import(year=2001, stp = s10);





%import(year=2002, stp = s1);
%import(year=2002, stp = s2);
%import(year=2002, stp = s3);
%import(year=2002, stp = s4);
%import(year=2002, stp = s5);
%import(year=2002, stp = s6);
%import(year=2002, stp = s7);
%import(year=2002, stp = s8);
%import(year=2002, stp = s9);
%import(year=2002, stp = s10);



%import(year=2003, stp = s1);
%import(year=2003, stp = s2);
%import(year=2003, stp = s3);
%import(year=2003, stp = s4);
%import(year=2003, stp = s5);
%import(year=2003, stp = s6);
%import(year=2003, stp = s7);
%import(year=2003, stp = s8);
%import(year=2003, stp = s9);
%import(year=2003, stp = s10);



%import(year=2004, stp = s1);
%import(year=2004, stp = s2);
%import(year=2004, stp = s3);
%import(year=2004, stp = s4);
%import(year=2004, stp = s5);
%import(year=2004, stp = s6);
%import(year=2004, stp = s7);
%import(year=2004, stp = s8);
%import(year=2004, stp = s9);
%import(year=2004, stp = s10);



%import(year=2005, stp = s1);
%import(year=2005, stp = s2);
%import(year=2005, stp = s3);
%import(year=2005, stp = s4);
%import(year=2005, stp = s5);
%import(year=2005, stp = s6);
%import(year=2005, stp = s7);
%import(year=2005, stp = s8);
%import(year=2005, stp = s9);
%import(year=2005, stp = s10);



%import(year=2006, stp = s1);
%import(year=2006, stp = s2);
%import(year=2006, stp = s3);
%import(year=2006, stp = s4);
%import(year=2006, stp = s5);
%import(year=2006, stp = s6);
%import(year=2006, stp = s7);
%import(year=2006, stp = s8);
%import(year=2006, stp = s9);
%import(year=2006, stp = s10);



%import(year=2007, stp = s1);
%import(year=2007, stp = s2);
%import(year=2007, stp = s3);
%import(year=2007, stp = s4);
%import(year=2007, stp = s5);
%import(year=2007, stp = s6);
%import(year=2007, stp = s7);
%import(year=2007, stp = s8);
%import(year=2007, stp = s9);
%import(year=2007, stp = s10);




%import(year=2008, stp = s1);
%import(year=2008, stp = s2);
%import(year=2008, stp = s3);
%import(year=2008, stp = s4);
%import(year=2008, stp = s5);
%import(year=2008, stp = s6);
%import(year=2008, stp = s7);
%import(year=2008, stp = s8);
%import(year=2008, stp = s9);
%import(year=2008, stp = s10);


data all_10p_2000;
 set m3_merg_2000_s1 m3_merg_2000_s2 m3_merg_2000_s3 m3_merg_2000_s4 m3_merg_2000_s5 m3_merg_2000_s6 m3_merg_2000_s7 m3_merg_2000_s8 m3_merg_2000_s9 m3_merg_2000_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2000
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2000.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2001;
 set m3_merg_2001_s1 m3_merg_2001_s2 m3_merg_2001_s3 m3_merg_2001_s4 m3_merg_2001_s5 m3_merg_2001_s6 m3_merg_2001_s7 m3_merg_2001_s8 m3_merg_2001_s9 m3_merg_2001_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2001
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2001.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2002;
 set m3_merg_2002_s1 m3_merg_2002_s2 m3_merg_2002_s3 m3_merg_2002_s4 m3_merg_2002_s5 m3_merg_2002_s6 m3_merg_2002_s7 m3_merg_2002_s8 m3_merg_2002_s9 m3_merg_2002_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2002
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2002.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   

data all_10p_2003;
 set m3_merg_2003_s1 m3_merg_2003_s2 m3_merg_2003_s3 m3_merg_2003_s4 m3_merg_2003_s5 m3_merg_2003_s6 m3_merg_2003_s7 m3_merg_2003_s8 m3_merg_2003_s9 m3_merg_2003_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2003
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2003.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2004;
 set m3_merg_2004_s1 m3_merg_2004_s2 m3_merg_2004_s3 m3_merg_2004_s4 m3_merg_2004_s5 m3_merg_2004_s6 m3_merg_2004_s7 m3_merg_2004_s8 m3_merg_2004_s9 m3_merg_2004_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2004
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2004.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2005;
 set m3_merg_2005_s1 m3_merg_2005_s2 m3_merg_2005_s3 m3_merg_2005_s4 m3_merg_2005_s5 m3_merg_2005_s6 m3_merg_2005_s7 m3_merg_2005_s8 m3_merg_2005_s9 m3_merg_2005_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2005
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2005.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2006;
 set m3_merg_2006_s1 m3_merg_2006_s2 m3_merg_2006_s3 m3_merg_2006_s4 m3_merg_2006_s5 m3_merg_2006_s6 m3_merg_2006_s7 m3_merg_2006_s8 m3_merg_2006_s9 m3_merg_2006_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2006
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2006.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2007;
 set m3_merg_2007_s1 m3_merg_2007_s2 m3_merg_2007_s3 m3_merg_2007_s4 m3_merg_2007_s5 m3_merg_2007_s6 m3_merg_2007_s7 m3_merg_2007_s8 m3_merg_2007_s9 m3_merg_2007_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2007
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2007.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


data all_10p_2008;
 set m3_merg_2008_s1 m3_merg_2008_s2 m3_merg_2008_s3 m3_merg_2008_s4 m3_merg_2008_s5 m3_merg_2008_s6 m3_merg_2008_s7 m3_merg_2008_s8 m3_merg_2008_s9 m3_merg_2008_s10;
run;



 PROC EXPORT DATA= WORK.all_10p_2008
             OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN006_mod3pred__localPM\m3si_2008.csv" 
			             DBMS=CSV REPLACE;
						      PUTNAMES=YES;
							  RUN;
							   


















							   



						   
/*CV AND SPATIAL TEMPORAL CV RESULTS*/

/*Full*/
/**/
/*proc reg data = all_10p_2000;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2000;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2000(keep = Type nValue2);*/
/*set overall_2000;*/
/*where label1="Root MSE";*/
/*  Type = "overall2000";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2000;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2000_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2000;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2000_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2000;*/
/* merge all_10p_2000 T2000_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2000;*/
/* set ST_2000;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2000;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2000";*/
/*  ods output FitStatistics = Spatial2000;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2000(keep = Type nValue2);*/
/*set Spatial2000;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2000";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2000;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2000";*/
/*ods output FitStatistics = Temporal2000;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2000(keep = Type nValue2);*/
/*set Temporal2000;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2000";*/
/*run;*/
/**/
/**/
/*data Table_2000;*/
/*set Overall2000 Temporal2000 Spatial2000;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2001;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2001;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2001(keep = Type nValue2);*/
/*set overall_2001;*/
/*where label1="Root MSE";*/
/*  Type = "overall2001";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2001;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2001_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2001;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2001_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2001;*/
/* merge all_10p_2001 T2001_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2001;*/
/* set ST_2001;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2001;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2001";*/
/*  ods output FitStatistics = Spatial2001;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2001(keep = Type nValue2);*/
/*set Spatial2001;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2001";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2001;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2001";*/
/*ods output FitStatistics = Temporal2001;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2001(keep = Type nValue2);*/
/*set Temporal2001;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2001";*/
/*run;*/
/**/
/**/
/*data Table_2001;*/
/*set Overall2001 Temporal2001 Spatial2001;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2002;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2002;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2002(keep = Type nValue2);*/
/*set overall_2002;*/
/*where label1="Root MSE";*/
/*  Type = "overall2002";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2002;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2002_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2002;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2002_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2002;*/
/* merge all_10p_2002 T2002_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2002;*/
/* set ST_2002;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2002;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2002";*/
/*  ods output FitStatistics = Spatial2002;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2002(keep = Type nValue2);*/
/*set Spatial2002;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2002";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2002;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2002";*/
/*ods output FitStatistics = Temporal2002;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2002(keep = Type nValue2);*/
/*set Temporal2002;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2002";*/
/*run;*/
/**/
/**/
/*data Table_2002;*/
/*set Overall2002 Temporal2002 Spatial2002;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2003;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2003;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2003(keep = Type nValue2);*/
/*set overall_2003;*/
/*where label1="Root MSE";*/
/*  Type = "overall2003";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2003;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2003_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2003;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2003_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2003;*/
/* merge all_10p_2003 T2003_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2003;*/
/* set ST_2003;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2003;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2003";*/
/*  ods output FitStatistics = Spatial2003;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2003(keep = Type nValue2);*/
/*set Spatial2003;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2003";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2003;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2003";*/
/*ods output FitStatistics = Temporal2003;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2003(keep = Type nValue2);*/
/*set Temporal2003;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2003";*/
/*run;*/
/**/
/**/
/*data Table_2003;*/
/*set Overall2003 Temporal2003 Spatial2003;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2004;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2004;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2004(keep = Type nValue2);*/
/*set overall_2004;*/
/*where label1="Root MSE";*/
/*  Type = "overall2004";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2004;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2004_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2004;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2004_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2004;*/
/* merge all_10p_2004 T2004_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2004;*/
/* set ST_2004;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2004;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2004";*/
/*  ods output FitStatistics = Spatial2004;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2004(keep = Type nValue2);*/
/*set Spatial2004;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2004";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2004;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2004";*/
/*ods output FitStatistics = Temporal2004;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2004(keep = Type nValue2);*/
/*set Temporal2004;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2004";*/
/*run;*/
/**/
/**/
/*data Table_2004;*/
/*set Overall2004 Temporal2004 Spatial2004;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2005;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2005;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2005(keep = Type nValue2);*/
/*set overall_2005;*/
/*where label1="Root MSE";*/
/*  Type = "overall2005";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2005;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2005_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2005;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2005_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2005;*/
/* merge all_10p_2005 T2005_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2005;*/
/* set ST_2005;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2005;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2005";*/
/*  ods output FitStatistics = Spatial2005;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2005(keep = Type nValue2);*/
/*set Spatial2005;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2005";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2005;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2005";*/
/*ods output FitStatistics = Temporal2005;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2005(keep = Type nValue2);*/
/*set Temporal2005;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2005";*/
/*run;*/
/**/
/**/
/*data Table_2005;*/
/*set Overall2005 Temporal2005 Spatial2005;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2006;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2006;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2006(keep = Type nValue2);*/
/*set overall_2006;*/
/*where label1="Root MSE";*/
/*  Type = "overall2006";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2006;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2006_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2006;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2006_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2006;*/
/* merge all_10p_2006 T2006_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2006;*/
/* set ST_2006;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2006;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2006";*/
/*  ods output FitStatistics = Spatial2006;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2006(keep = Type nValue2);*/
/*set Spatial2006;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2006";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2006;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2006";*/
/*ods output FitStatistics = Temporal2006;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2006(keep = Type nValue2);*/
/*set Temporal2006;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2006";*/
/*run;*/
/**/
/**/
/*data Table_2006;*/
/*set Overall2006 Temporal2006 Spatial2006;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2007;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2007;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2007(keep = Type nValue2);*/
/*set overall_2007;*/
/*where label1="Root MSE";*/
/*  Type = "overall2007";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2007;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2007_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2007;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2007_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2007;*/
/* merge all_10p_2007 T2007_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2007;*/
/* set ST_2007;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2007;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2007";*/
/*  ods output FitStatistics = Spatial2007;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2007(keep = Type nValue2);*/
/*set Spatial2007;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2007";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2007;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2007";*/
/*ods output FitStatistics = Temporal2007;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2007(keep = Type nValue2);*/
/*set Temporal2007;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2007";*/
/*run;*/
/**/
/**/
/*data Table_2007;*/
/*set Overall2007 Temporal2007 Spatial2007;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/**/
/**/
/*						   */
/*/*CV AND SPATIAL TEMPORAL CV RESULTS*/*/
/**/
/*/*Full*/*/
/**/
/*proc reg data = all_10p_2008;*/
/*model pm25 = pm_mod3 / clb;*/
/*title "OA with sitcode-si";*/
/*ods output FitStatistics = overall_2008;*/
/*run;*/
/*quit;*/
/**/
/**/
/*data overall2008(keep = Type nValue2);*/
/*set overall_2008;*/
/*where label1="Root MSE";*/
/*  Type = "overall2008";*/
/*run;*/
/**/
/**/
/*/*SPATIAL Temporal R2*/*/
/**/
/*proc summary nway data=all_10p_2008;*/
/* class sitecode;*/
/*  var pm25 pm_mod3;*/
/*   output out=T2008_agg mean=annmeanpm25  annpred;*/
/*run;*/
/**/
/**/
/*proc sort data=all_10p_2008;*/
/* by sitecode;*/
/*run;*/
/**/
/*proc sort data=T2008_agg;*/
/* by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2008;*/
/* merge all_10p_2008 T2008_agg(keep=sitecode annmeanpm25 annpred);*/
/*  by sitecode;*/
/*run;*/
/**/
/**/
/*data ST_2008;*/
/* set ST_2008;*/
/*  delpm = pm25 -annmeanpm25;*/
/*  delpred    = pm_mod3   - annpred;*/
/*run;*/
/**/
/**/
/*/*#spatial*/*/
/**/
/*ods trace on;*/
/*proc reg data = ST_2008;*/
/*model annmeanpm25 = annpred / clb;*/
/*  title "Spatial with sitcode 2008";*/
/*  ods output FitStatistics = Spatial2008;*/
/*run;*/
/*quit;*/
/**/
/*data Spatial2008(keep = Type nValue2);*/
/*set Spatial2008;*/
/*where label1="Root MSE";*/
/*  Type = "Spatial 2008";*/
/*run;*/
/**/
/**/
/*/*title "Temporal"*/*/
/*proc reg data = ST_2008;*/
/*model  delpm = delpred / clb;*/
/*title "Temporal with sitcode 2008";*/
/*ods output FitStatistics = Temporal2008;*/
/*run;*/
/*quit;*/
/**/
/* */
/**/
/*data Temporal2008(keep = Type nValue2);*/
/*set Temporal2008;*/
/*where label1="Root MSE";*/
/*Type = "Temporal 2008";*/
/*run;*/
/**/
/**/
/*data Table_2008;*/
/*set Overall2008 Temporal2008 Spatial2008;*/
/*CVR2=nValue2;*/
/*run; */
/**/
/**/
/**/
/**/
/*/*ALL FILES*/*/
/**/
/*/*CV Tables*/*/
/**/
/**/
/*data overallCV;*/
/*set overall2000 overall2001 overall2002 overall2003 overall2004 overall2005 overall2006 overall2007 overall2008;*/
/*R2=nValue2;*/
/*run; */
/**/
/**/
/*data table_CV;*/
/*set table_2000 table_2001 table_2002 table_2003 table_2004 table_2005 table_2006 table_2007 table_2008;*/
/*R2=nValue2;*/
/*run; */
/**/
/**/
/**/
/*PROC EXPORT DATA= WORK.overallCV*/
/*            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.5.Results\mod3cv\mod3CV.xls" */
/*			            DBMS=EXCEL5 REPLACE;*/
/*						RUN; */
/**/
/**/
/**/
/*PROC EXPORT DATA= WORK.table_CV*/
/*            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.5.Results\mod3cv\mod3_ST_CV.xls" */
/*			            DBMS=EXCEL5 REPLACE;*/
/*						RUN; */
