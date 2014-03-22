libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;


PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_sitecode_within9km.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 





options mprint;
%macro import(year=);


/**** Import 200% dataset *****/


PROC IMPORT OUT= WORK.mod2_T&year
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\sas\out&year..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;





/*Step s1*/

/*1)GET THE Overall AOD TEMP SLOPES + INTERCEPT */


data check_s1;
 set aod.Solutionr&year.a;
run;

data check_s1_int(keep = date Ovr_Int);
 set check_s1;
  if zid = 1 then delete;
    if Effect = "AOD" then delete;
	if Effect = "Temp_F_x" then delete;
	Ovr_Int = Estimate;
run;

data check_s1_AOD(keep = date Ovr_AOD);
 set check_s1;
  if zid = 1 then delete;
    if Effect = "Intercept" then delete;
	if Effect = "Temp_F_x" then delete;
    Ovr_AOD = Estimate;
run;

data check_s1_Temp(keep = date Ovr_Temp);
 set check_s1;
  if zid = 1 then delete;
    if Effect = "Intercept" then delete;
	if Effect = "AOD" then delete;
    Ovr_Temp = Estimate;
run;


proc sort data = check_s1_Int;  by date;run;
proc sort data = check_s1_AOD;  by date;run;
proc sort data = check_s1_Temp; by date;run;

data mean_s1;
 merge check_s1_Int check_s1_AOD check_s1_Temp;
  by date;
run;

/*** Join the Overall slope and intercept with 200% dataset ***/

proc sort data = mod2_T&year;    by date;run;
proc sort data = mean_s1;        by date;run;

data mod2_T&year._v1;
 merge mod2_T&year (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 


/* Assign Random Effect X1--X18 and Fixed Effects */ 

data transp_2_s1;
 set aod.Solutionr&year.a;
  keep Effect Estimate;
   if zid = 1.000000000000000;
run;

proc transpose data = transp_2_s1 out=transp_2_s1 prefix=Ran_;
  id Effect;
run;

/* Assign Fixed Effect */

proc transpose data = aod.Solutionf&year.a prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  mod2_T&year._v4;
 MERGE mod2_T&year._v1 transp_2_s1 transp_3_s1;
RUN;

PROC STANDARD DATA = mod2_T&year._v4 OUT = mod2_T&year._v4 REPLACE;
  VAR ran_x1--fix_st_avgs;
RUN;

data mod2_T&year._v5;
 set mod2_T&year._v4;
  if ovr_int  = . then ovr_int  = 0;
  if ovr_aod  = . then ovr_aod  = 0;
  if ovr_temp = . then ovr_temp = 0;
 run;

/*GET READY FOR CORRELATIONS*/

data mod2_T&year._v6;
 set mod2_T&year._v5;
  pred = fix_intercept + AOD*fix_AOD + Temp_F_x*fix_Temp_F_x + t_avgs*fix_t_avgs + 
         s_avgs*fix_s_avgs + st_avgs*fix_st_avgs 
             
         + X1*Ran_X1
		 + X2*Ran_X2
		 + X3*Ran_X3
		 + X4*Ran_X4
		 + X5*Ran_X5
		 + X6*Ran_X6
		 + X7*Ran_X7
		 + X8*Ran_X8
		 + X9*Ran_X9
		 + X10*Ran_X10
		 + X11*Ran_X11
		 + X12*Ran_X12
		 + X13*Ran_X13
		 + X14*Ran_X14
		 + X15*Ran_X15
		 + X16*Ran_X16
		 + X17*Ran_X17
		 + X18*Ran_X18 
		 + X19*Ran_X19 
		 + X20*Ran_X20 
		 + X21*Ran_X21 
         + OVR_int + AOD*OVR_aod + temp_F_X*OVR_temp;
run;

data mod2_T&year._v6;
 set mod2_T&year._v6;
  if pred < 0.00000000001 then delete;
    if pred > 32 then delete;
run; 


PROC EXPORT DATA= mod2_T&year._v6
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T&year._m2_pred.dbf" 
            DBMS=DBF REPLACE;
RUN;
quit;


%MEND ;

%import(year=2009);
%import(year=2010);
%import(year=2011);




/*TO CHECK R2 CORR*/



 options mprint;
 %macro import(year=);

 
PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\pdataA_&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
data pmguid (drop=pred);
set pmguid;
predmod1=pred;
run; 


proc sort data = Mod2_t&year._v6; by guid date ;run;
proc sort data = pmguid;          by guid date ;run;

data DATA3;
merge pmguid(in=a) Mod2_t&year._v6(in=b keep=guid date pred);
  by guid date;
    if a and b;
	run; 


proc corr data = DATA3;
 var pm25 pred;
run; 
/**/
/*symbol1 v=circle h=0.5 w=0.5 c=blue;*/
/*symbol2 v=circle h=0.5 w=0.5 c=red;*/
/**/
/*proc gplot data=DATA3;*/
/*Title "Mod2 Prediction &year vs Actual PM2.5";*/
/*  plot pm25*date pred*date/ grid OVERLAY;*/
/* run; */
/*quit; */
/**/
/*proc gplot data=DATA3;*/
/*Title "Mod2 Prediction &year vs Actual PM2.52";*/
/*  plot pm25*pred/ grid OVERLAY;*/
/* run; */
/*quit; */


 %MEND ;


  %import(year=2009);
   %import(year=2010);
    %import(year=2011);



symbol1 v=dot h=0.5 w=0.5 c=blue;
proc gplot data=data3;
Title "TITLE";
  plot pm25*date;
     label VAR1 = "LABLE1";
	       label VAR2 = "LABLE2";
		   run; 
		   quit; 



PROC IMPORT OUT= WORK.pd2011
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\pdataA_2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
data pd2011 (drop=pred t_avgs s_avgs st_avgs Temp_F_x ah_gm3_F wdsp_f visib_f );
set pd2011;
predmod1=pred;
t_avgsm1=t_avgs;
s_avgsm1=s_avgs;
st_avgsm1=st_avgs;
Temp_F_x1 =Temp_F_x;
ah_gm3_f1=ah_gm3_F;
wdsp_f1=wdsp_f;
visib_f1=visib_f;
run; 


proc sort data = mod2_T2011_v6; by guid date  ;run;
proc sort data = pd2011 ; by guid date ;run;

data DATA33_2011;
merge pd2011(in=a) mod2_T2011_v6 (in=b)  ;
  by guid date;
    if a;
	run; 

proc corr data = DATA33_2011;
 var t_avgsm1 t_avgs;
 run; 

proc corr data = DATA33_2011;
 var ah_gm3_f1 ah_gm3_F;
 run;

proc corr data = DATA33_2011;
 var wdsp_f1 wdsp_f;
 run;

 proc corr data = DATA33_2011;
 var visib_f1 visib_f;;
 run;

 proc corr data = DATA33_2011;
 var  st_avgsm1 st_avgs;
 run;

 proc corr data = DATA33_2011;
 var  s_avgsm1 s_avgs;
 run;



/**/
/*symbol1 v=dot h=0.5 w=0.5 c=blue;*/
/*proc gplot data=DATA33;*/
/*Title "TITLE";*/
/*  plot st_avgsm1*st_avgs/ grid;*/
/*     label VAR1 = "LABLE1";*/
/*	       label VAR2 = "LABLE2";*/
/*		   run; */
/*		   quit; */



PROC IMPORT OUT= WORK.pd2010
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\pdataA_2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
data pd2010 (drop=pred t_avgs s_avgs st_avgs visib_f wdsp_f ah_gm3_F);
set pd2010;
predmod1=pred;
t_avgsm1=t_avgs;
s_avgsm1=s_avgs;
st_avgsm1=st_avgs;
visib_f1=visib_f;
wdsp_f1=wdsp_f;
ah_gm3_F1=ah_gm3_F;
run; 


proc sort data = mod2_T2010_v6; by guid date  ;run;
proc sort data = pd2010 ; by guid date ;run;

data DATA33;
merge pd2010(in=a) mod2_T2010_v6 (in=b)  ;
  by guid date;
    if a;
	run; 

proc corr data = DATA33;
 var ah_gm3_F1 ah_gm3_F;
 run; 


 proc gplot data=DATA33;
Title "TITLE";
  plot t_avgsm1*t_avgs/ grid;
     label VAR1 = "LABLE1";
	       label VAR2 = "LABLE2";
		   run; 
		   quit; 



