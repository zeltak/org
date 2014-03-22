libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;
libname out10 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\10pout\' ;


options mprint;
%macro Final(Stp=);

/**** Import 90% dataset *****/

PROC IMPORT OUT= WORK.mod1_T2003_90_&Stp
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\mod1_T2003_90_&stp..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data mod1_T2003_90_&Stp;
set mod1_T2003_90_&Stp;
 newdate = input(date,date9.);
  format newdate date9.;
drop date;
run;

data mod1_T2003_90_&Stp;
 set mod1_T2003_90_&Stp(rename=(newdate=date));
run;

/**** Import 10% dataset *****/


PROC IMPORT OUT= WORK.mod1_T2003_10_&Stp
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\mod1_T2003_10_&stp._short.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data mod1_T2003_10_&Stp;
set mod1_T2003_10_&Stp;
 newdate = input(date,date9.);
  format newdate date9.;
drop date;
run;

data mod1_T2003_10_&Stp;
 set mod1_T2003_10_&Stp(rename=(newdate=date));
run;

/**** PROC MIXED FOR 90% ****/


proc mixed data = WORK.mod1_T2003_90_&Stp method=reml;
class reg_id zid date ;
 weight normwt;
   model pm25 = aod Temp_F_x t_avgs s_avgs st_avgs / s outpred = pred_&Stp._2003;
     random int aod Temp_F_x / sub = date s;
           random int aod  / sub = date(reg_id) s;
          random x1--x18 / sub = zid type=toep(1) s;
	
parms 


(   2.3374)
(  107.23)
(  0.005230)
(  8.2898)
(  64.0451)
(  0.02286)
(  6.4012)


/ noiter;


	ods output  SolutionF =  Sol_Fix_&Stp._2003;
    ods output  SolutionR =  Sol_Ran_&stp._2003;
run;


data aod.pred_&stp._2003   ; set pred_&stp._2003    ;run;
data aod.Sol_Fix_&stp._2003; set Sol_Fix_&stp._2003 ;run;
data aod.Sol_Ran_&stp._2003; set Sol_Ran_&stp._2003 ;run;


/*Step &stp*/

/*1)GET THE Overall AOD TEMP SLOPES + INTERCEPT */


data check_&stp;
 set aod.Sol_Ran_&stp._2003;
run;

data check_&stp._int(keep = date Ovr_Int);
 set check_&stp;
  if reg_id = 1 then delete; 
  if reg_id = 2 then delete; 
  if reg_id = 3 then delete; 
  if reg_id = 4 then delete; 
  if reg_id = 5 then delete; 
  if reg_id = 6 then delete; 
  if reg_id = 7 then delete; 
  if zid = 1 then delete;
    if Effect = "AOD" then delete;
	if Effect = "Temp_F_x" then delete;
	if SiteCode = " ";
	Ovr_Int = Estimate;
run;

data check_&stp._AOD(keep = date Ovr_AOD);
 set check_&stp;
  if reg_id = 1 then delete; 
  if reg_id = 2 then delete; 
  if reg_id = 3 then delete; 
  if reg_id = 4 then delete; 
  if reg_id = 5 then delete; 
  if reg_id = 6 then delete; 
  if reg_id = 7 then delete; 
  if zid = 1 then delete;
    if Effect = "Intercept" then delete;
	if Effect = "Temp_F_x" then delete;
	if SiteCode = " ";
    Ovr_AOD = Estimate;
run;

data check_&stp._Temp(keep = date Ovr_Temp);
 set check_&stp;
  if reg_id = 1 then delete; 
  if reg_id = 2 then delete; 
  if reg_id = 3 then delete; 
  if reg_id = 4 then delete; 
  if reg_id = 5 then delete; 
  if reg_id = 6 then delete; 
  if reg_id = 7 then delete; 
  if zid = 1 then delete;
    if Effect = "Intercept" then delete;
	if Effect = "AOD" then delete;
	if SiteCode = " ";
    Ovr_Temp = Estimate;
run;


proc sort data = check_&stp._Int;  by date;run;
proc sort data = check_&stp._AOD;  by date;run;
proc sort data = check_&stp._Temp; by date;run;

data mean_&stp;
 merge check_&stp._Int check_&stp._AOD check_&stp._Temp;
  by date;
run;

/*** Join the Overall slope and intercept with 10% dataset ***/

proc sort data = Mod1_t2003_10_&stp;    by date;run;
proc sort data = mean_&stp;             by date;run;

data Mod1_t2003_10_&stp._v1;
 merge Mod1_t2003_10_&Stp(in=a) mean_&stp(in=b) ;
   by date;
   if a;
   run; 

/* 2)GET THE REGION specific AOD TEMP SLOPES + INTERCEPT */

data transp_1_&stp;
 set check_&stp;
  keep Effect reg_id date Estimate;
   if reg_id < 0.9999999999 then delete;
run;

proc sort data = transp_1_&stp; by date reg_id; run;

proc transpose data = transp_1_&Stp out=transp_1_&stp;
 by date reg_id;
  id Effect;
run;

data transp_1_&stp(drop = AOD Intercept);
 set transp_1_&stp;
  reg_AOD  = AOD;
  reg_Int  = Intercept;
run;


/*** Join the REGION specific slope and intercept with 10% dataset ***/

proc sort data = Mod1_t2003_10_&stp._v1;     by date reg_id;run;
proc sort data = transp_1_&stp;             by date reg_id;run;

data Mod1_t2003_10_&stp._v2(drop = _NAME_);
 merge Mod1_t2003_10_&stp._v1 (in=a) transp_1_&stp(in=b) ;
   by date reg_id;
   if a;
run; 




/* Assign Random Effect X1--X18 and Fixed Effects */ 

data transp_2_&stp;
 set aod.Sol_ran_&stp._2003;
  keep Effect Estimate;
   if zid = 1.000000000000000;
run;

proc transpose data = transp_2_&Stp out=transp_2_&Stp prefix=Ran_;
  id Effect;
run;

/* Assign Fixed Effect */

proc transpose data = aod.Sol_fix_&stp._2003 prefix=fix_ out=transp_3_&stp;
  id Effect;
run;

data transp_3_&stp(drop=_label_);
 set transp_3_&stp;
   if _NAME_ = "Estimate";
run;

DATA  Mod1_t2003_10_&stp._v4;
 MERGE Mod1_t2003_10_&stp._v2 transp_2_&Stp transp_3_&stp;
RUN;

PROC STANDARD DATA=Mod1_t2003_10_&stp._v4 OUT=Mod1_t2003_10_&stp._v4 REPLACE;
  VAR Ran_x1--fix_st_avgs;
RUN;

proc contents data=Mod1_t2003_10_&stp._v4;
run;

data Mod1_t2003_10_&stp._v5;
 set Mod1_t2003_10_&stp._v4;
 if reg_int = . then reg_int = 0;
 if reg_aod = . then reg_aod = 0;
 if ovr_int = . then ovr_int = 0;
 if ovr_aod = . then ovr_aod = 0;
 if ovr_temp = . then ovr_temp = 0;
 
 run;


/*GET READY FOR CORRELATIONS*/

data Mod1_t2003_10_&stp._v6;
 set Mod1_t2003_10_&stp._v5;
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
         + OVR_int + AOD*OVR_aod + temp_F_X*OVR_temp + reg_int + reg_aod*AOD ;
run;



ods trace on;
proc corr data = Mod1_t2003_10_&stp._v6;
 var pm25 pred;
 ods output PearsonCorr = PearsonCorr_&stp;
run;
ods trace off;






data PearsonCorr_&stp(keep = Iteration pred);
 set PearsonCorr_&stp;
  if _n_ = 1;
    Iteration = "&stp";
    pred=pred_si;
run;

data out10.Mod1_t2003_10p_&stp;
set Mod1_t2003_10_&stp._v6;
run; 

%mend;

%Final(Stp=s1);
%Final(Stp=s2);
%Final(Stp=s3);
%Final(Stp=s4);
%Final(Stp=s5);
%Final(Stp=s6);
%Final(Stp=s7);
%Final(Stp=s8);
%Final(Stp=s9);
%Final(Stp=s10);



data cVTable2003;
 set  pearsoncorr_s1  pearsonCorr_s2  pearsonCorr_s3  pearsonCorr_s4
      pearsonCorr_s5  pearsonCorr_s6  pearsonCorr_s7  pearsonCorr_s8
	  pearsonCorr_s9  pearsonCorr_s10;
run;




PROC EXPORT DATA= WORK.cVTable2003 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.5.Results\mod1cv\sas_cv_2003.xls" 
            DBMS=EXCEL5 REPLACE;
RUN;




/*create a large prediction file of all 10% combined to export for the local PM part*/

data ran_all_2003;
 set Mod1_t2003_10_s1_v6 Mod1_t2003_10_s2_v6 Mod1_t2003_10_s3_v6 Mod1_t2003_10_s4_v6 Mod1_t2003_10_s5_v6 Mod1_t2003_10_s6_v6 Mod1_t2003_10_s7_v6 Mod1_t2003_10_s8_v6 Mod1_t2003_10_s9_v6 Mod1_t2003_10_s10_v6;
run;


PROC EXPORT DATA= ran_all_2003 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\sas export\t2003_all_10p.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
