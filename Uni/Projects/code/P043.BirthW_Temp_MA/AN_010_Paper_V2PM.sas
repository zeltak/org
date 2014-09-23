/*birth weight analysis*/

libname bwr 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.5.Results\RN_001_results_paper\' ;
libname bww 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN010_bwdatasets\' ;
libname x1 'z:\Projects\P011.BirthW_NE\3.1.11.4.Work\3.Analysis\4.sas analysis\' ;
libname x2 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\3.Analysis\3.SAS_analysis\results_w_PM' ;

PROC EXPORT DATA= bww.Lr
            OUTFILE= "OUTDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 






/*import PM 10x10*/

data bw9;
set x1.bw9;
keep uniqueid_y pmnew--pm12_24 popden--localpm date;
run;

proc sort data = bw9; by uniqueid_y   ;run;
proc sort data = bww.bw_all ; by uniqueid_y ;run;

data DATA3;
merge bww.bw_all(in=a) bw9 (in=b)  ;
  by uniqueid_y;
    if a;
	run; 

data DATA4;
set DATA3;
if pmnew=. then delete;
run; 

data DATA4;
set DATA4;
if mother_race="1" then mrnn=1;
if mother_race="2" then mrnn=3;
if mother_race="-" then mrnn=2;
if mother_race="3" then mrnn=4;
if mother_race="4" then mrnn=5;
if mother_race="5" then mrnn=2;
if mother_race="8" then mrnn=5;
if mother_race="9" then mrnn=5;
if ges_calc  < 20 then delete;
if ges_calc  > 42 then delete;
if ges_calc < 37 then bwcat=1;
if ges_calc >=37 then bwcat=0;
run;


data x1.Bw_nocesPM;
set DATA4;
run;

data x1.Bw_nocesPMFT;
set x1.Bw_nocesPM;
where ges_calc >= 37;
run;

data x1.Bw_nocesPMPT;
set x1.Bw_nocesPM;
where ges_calc < 37;
run;


/*##################################################################*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*##################################################################*/

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintemp_l0 pmnew_l0  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRfintemp_l0;
run;


proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdc_l0 pmnew_l0 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdc_l0;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintemp_l1  pmnew_l1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRfintemp_l1;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdc_l1  pmnew_l1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdc_l1;
run;





proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma1 pmnewma1  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRfintempma1;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma1 pmnewma1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcma1;
run;



proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3  pmnewma3 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRfintempma3;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma3  pmnewma3 vsinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcma3;
run;



proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmaweek  pmnewmaweek sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   x2.IQRfintempmaweek;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmaweek pmnewmaweek sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcmaweek;
run;


proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma2week  pmnewma2week sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   x2.IQRfintempma2week;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma2week pmnewma2week sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcma2week;
run;



proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmamonth pmnewmamonth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   x2.IQRfintempmamonth;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmamonth pmnewmamonth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcmamonth;
run;



proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3month pmnewma3month sinetime  costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRfintempma3month;
run;

proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma3month pmnewma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRtncdcma3month ;
run;




 
proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmabirth  pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.IQRtncdcmabirth;
run;

proc mixed data = x1.Bw_nocesPMFT method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmabirth pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess adtmean 
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.IQRfintempmabirth;
run;







proc mixed data = x1.Bw_nocesPM method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3month pmnewma3month sinetime adtmean  costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  x2.test;
run;







/*Strateifed 22-37*/


proc mixed data = x1.Bw_nocesPMPT method=reml covtest;
class kess  mrnn EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3month pmnewma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess adtmean 
  mrnn edu_group ges_calc parity elev   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = x2.PT_IQRfintempmabirth;
run;





/*AFT MODEL*/



data lr;
set x1.Bw_nocesPM;
lmp=bdob-(gacalc*7);
format lmp date9.;
where ges_calc >28;
where ges_calc < 46;
totpreg=bdob-lmp;
run;

proc univariate data= lr;
var ges_calc;
run;

data lr;
set lr;
if ges_calc <22 then delete;
if ges_calc > 42 then delete;
gesgroup=0;
if ges_calc > 22 and ges_calc <=26 then gesgroup=1;
if ges_calc > 26 and ges_calc <=30 then gesgroup=2;
if ges_calc > 30 and ges_calc <=33 then gesgroup=3;
if ges_calc > 33 and ges_calc <=36 then gesgroup=4;
if ges_calc > 36 and ges_calc <=40 then gesgroup=5;
if ges_calc > 40 then gesgroup=6;
run; 
 



/*3month*/

proc lifereg data = lr;
class kess  mrnn EDU_GROUP fips byob ;
  model (lmp,bdob) = fintempma3month pmnewma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;
ods OUTPUT ParameterEstimates= x2.plr_3month;
run;

proc lifereg data = lr;
class kess  mrnn EDU_GROUP fips byob ;
  model (lmp,bdob) = tncdcma3month pmnewma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;
ods OUTPUT ParameterEstimates= x2.ncdc_plr_3month;
run;



proc lifereg data = lr;
class kess  mrnn EDU_GROUP fips byob ;
  model (lmp,bdob) = fintempmabirth pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;
ods OUTPUT ParameterEstimates= x2.plr_fullbirth;
run;



proc lifereg data = lr;
class kess  mrnn EDU_GROUP fips byob ;
  model (lmp,bdob) = tncdcmabirth pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;
ods OUTPUT ParameterEstimates= x2.plr_ncdcfullbirth;
run;

/**/
/**/
/**/
/*proc lifereg data = lr;*/
/*class kess  mrnn EDU_GROUP fips byob gesgroup  ;*/
/*  model (lmp,bdob) = fintempma3month*gesgroup  pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400*/
/*diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;*/
/*ods OUTPUT ParameterEstimates= x2.plr_3month_gesgroup;*/
/*run;*/
/**/
/**/
/*proc lifereg data = lr;*/
/*class kess  mrnn EDU_GROUP fips byob gesgroup  ;*/
/*  model (lmp,bdob) = fintempmabirth*gesgroup pmnewmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400*/
/*diab hyper lungd diab_other prevpret kess adtmean mrnn edu_group ges_calc parity elev  / d=gamma;*/
/*ods OUTPUT ParameterEstimates= x2.plr_fullbirth_gesgroup;*/
/*run;*/
/**/
/**/





 proc glimmix data=x1.Bw_nocesPM  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lbw (event= "1")= IQRtncdcmabirth  pmnewmabirth age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   parity elev   byob      /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  x2.lbw270 ;
run;






 proc glimmix data=x1.Bw_nocesPM  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  bwcat (event= "1")= IQRtncdcmabirth  pmnewmabirth age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   parity elev   byob      /s dist=binary link=logit or cl  ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  x2.pt270 ;
run;





/*discriptives*/

data t1;
set bww.lr;
where ges_calc < 37;
run; 

data t1;
set bww.lr;
where BIRTHW < 2500;
run; 
 proc glimmix data=x1.Bw_nocesPM  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  bwcat (event= "1")= IQRfintempmabirth pmnewmabirth age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   parity elev   byob      /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  pt270 ;
run;
