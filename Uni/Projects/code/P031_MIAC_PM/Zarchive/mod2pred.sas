proc printto log="nul:"; run;



options mprint;
%macro import(year=);


PROC IMPORT OUT= mod1_&year
  DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN009_ALL_mods_base\mod1_&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

libname aod 'f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods_CV\' ;
libname aodm2 '\\Drobo\Shared_Data\EAST_USA_MAIAC\models\' ;

/*run stage 1 model*/
proc mixed data = mod1_&year  method=reml;
class region date ;
   model pm25 = aod tempc WDSP NDVI dist_PE pcturb_1km Mjrrdden_1 nei05nonpntcntypm25 pop_sqkm 
elev_m ah_gm3 visib aod*pbl pbl NOXsum PM10sum SO2sum pctmd_1km   pctld_1km pctop_1km  
pctdf_1km pctmf_1km pctev_1km   pctcr_1km pctpa_1km pctsh_1km   pctgr_1km   pm25stge30_15k
pm25stlt30_3k pm10stge30_15k     pm10stlt30_3k    noxstge30_15k noxstlt30_3k
so2stge30_15k so2stlt30_3k  / s outpred = pred_m1_&year;
     random int aod Tempc / sub = date s;
           random int aod  / sub = date(region) s;
  	ods output  SolutionF =  Sol_Fix_m1_&year;
    ods output  SolutionR =  Sol_Ran_m1_&year;
run;




/**** Import 200% dataset *****/



/*Step s1*/

/*1)GET THE Overall AOD TEMP SLOPES + INTERCEPT */


data check_s1_int(keep = date Ovr_Int);
 set Sol_Ran_m1_&year;
  if estimate=0 then delete;
  if region = 1 then delete; 
  if region = 2 then delete; 
  if region = 3 then delete; 
  if region = 4 then delete; 
  if region = 5 then delete; 
  if region = 6 then delete; 
  if region = 7 then delete; 
  if region = 8 then delete; 
  if region = 9 then delete; 
  if region = 0 then delete; 
  if Effect = "AOD" then delete;
  if Effect = "Tempc" then delete;
  Ovr_Int = Estimate;
run;

data  check_s1_AOD (keep = date Ovr_AOD);
 set Sol_Ran_m1_&year;
 if estimate=0 then delete;
   if region = 1 then delete; 
  if region = 2 then delete; 
  if region = 3 then delete; 
  if region = 4 then delete; 
  if region = 5 then delete; 
  if region = 6 then delete; 
  if region = 7 then delete; 
  if region = 8 then delete; 
  if region = 9 then delete; 
  if region = 0 then delete; 
   if Effect = "Intercept" then delete;
	if Effect = "tempc" then delete;
    Ovr_AOD = Estimate;
run;

data check_s1_Temp(keep = date Ovr_Temp);
 set Sol_Ran_m1_&year;
 if estimate=0 then delete;
   if region = 1 then delete; 
  if region = 2 then delete; 
  if region = 3 then delete; 
  if region = 4 then delete; 
  if region = 5 then delete; 
  if region = 6 then delete; 
  if region = 7 then delete; 
  if region = 8 then delete; 
  if region = 9 then delete; 
  if region = 0 then delete; 
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

proc sort data = aodm2.expo_&year.X;    by date;run;
proc sort data = mean_s1;        by date;run;

data mod2_T&year._v1;
 merge aodm2.expo_&year.X (in=a) mean_s1(in=b) ;
   by date;
   if a;
   run; 

/* 2)GET THE REGION specific AOD TEMP SLOPES + INTERCEPT */

data transp_1_s1;
 set  Sol_Ran_m1_&year;
  keep Effect region date Estimate;
   if region < 0.9999999999 then delete;
run;

proc sort data = transp_1_s1; by date region; run;

proc transpose data = transp_1_s1 out=transp_1_s1;
 by date region;
  id Effect;
run;

data transp_1_s1(drop = AOD Intercept);
 set transp_1_s1;
  reg_AOD  = AOD;
    reg_Int  = Intercept;
run;


/*** Join the REGION specific slope and intercept with 200% dataset ***/

proc sort data = mod2_T&year._v1;     by date region;run;
proc sort data = transp_1_s1;         by date region;run;

data mod2_T&year._v2(drop = _NAME_);
 merge mod2_T&year._v1 (in=a) transp_1_s1(in=b) ;
   by date region;
   if a;
run; 



/* Assign Fixed Effect */

proc transpose data = Sol_Fix_m1_&year prefix=fix_ out=transp_3_s1;
  id Effect;
run;

data transp_3_s1(drop=_label_);
 set transp_3_s1;
   if _NAME_ = "Estimate";
run;

DATA  mod2_T&year._v4;
 MERGE mod2_T&year._v2 transp_3_s1;
RUN;

PROC STANDARD DATA=mod2_T&year._v4 OUT=mod2_T&year._v4 REPLACE;
  VAR fix_Intercept--fix_so2stlt30_3k;
RUN;

data mod2_T&year._v5;
 set mod2_T&year._v4;
 if reg_int = . then reg_int = 0;
 if reg_aod = . then reg_aod = 0;
 if ovr_int = . then ovr_int = 0;
 if ovr_aod = . then ovr_aod = 0;
 if ovr_temp = . then ovr_temp = 0;
 
/*GET READY FOR CORRELATIONS*/

data mod2_T&year._v5;
 set mod2_T&year._v5;
  pred = fix_intercept
+ AOD*fix_AOD 
+ tempc*fix_tempc 
+WDSP                    *fix_WDSP                
+NDVI                    *fix_NDVI                 
+dist_PE                 *fix_dist_PE                 
+pcturb_1km              *fix_pcturb_1km                 
+Mjrrdden_1              *fix_Mjrrdden_1                 
+nei05nonpntcntypm25     *fix_nei05nonpntcntypm25                
+pop_sqkm                *fix_pop_sqkm                 
+elev_m                  *fix_elev_m                 
+ah_gm3                  *fix_ah_gm3                 
+visib                   *fix_visib                 
+pbl                     *fix_pbl                 
+NOXsum                  *fix_NOXsum                 
+PM10sum                 *fix_PM10sum                 
+SO2sum                  *fix_SO2sum                 
+pctmd_1km               *fix_pctmd_1km                   
+pctld_1km               *fix_pctld_1km                 
+pctop_1km               *fix_pctop_1km                  
+pctdf_1km               *fix_pctdf_1km                 
+pctmf_1km               *fix_pctmf_1km                 
+pctev_1km               *fix_pctev_1km                   
+pctcr_1km               *fix_pctcr_1km                 
+pctpa_1km               *fix_pctpa_1km                 
+pctsh_1km               *fix_pctsh_1km                  
+pctgr_1km               *fix_pctgr_1km                   
+pm25stge30_15k          *fix_pm25stge30_15k                
+pm25stlt30_3k           *fix_pm25stlt30_3k                 
+pm10stge30_15k          *fix_pm10stge30_15k                 
+pm10stlt30_3k           *fix_pm10stlt30_3k                
+noxstge30_15k           *fix_noxstge30_15k                
+noxstlt30_3k            *fix_noxstlt30_3k                
+so2stge30_15k           *fix_so2stge30_15k                
+so2stlt30_3k            *fix_so2stlt30_3k         
/*+(aod*pbl)*fix_aod_pbl  */
+ OVR_int 
+AOD*OVR_aod 
+ tempc*OVR_temp 
+ reg_int 
+ reg_aod*AOD ;
run;

data mod2_T&year._v5;
 set mod2_T&year._v5;
  if pred < 0 then delete;
run; 

PROC EXPORT DATA=  mod2_T&year._v5
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods_CV\mod2_&year._pred.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

/*proc sort data = mod2_T&year._v5  ; by guid date ;run;*/
/*proc sort data = mod1_&year;          by guid date ;run;*/
/**/
/*data DATA3;*/
/*merge mod1_&year(in=a) mod2_T&year._v5(in=b keep=guid date pred);*/
/*  by guid date;*/
/*    if a and b;*/
/*	run; */
/**/
/**/
/*proc corr data = ;*/
/* var pm25 pred;*/
/*run; */

proc datasets lib=work kill nolist memtype=data;
quit;



%MEND ;

%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);
%import(year=2009);
%import(year=2010);
%import(year=2011);


