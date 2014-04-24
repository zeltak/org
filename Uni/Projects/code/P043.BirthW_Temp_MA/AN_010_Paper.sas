/*birth weight analysis*/

libname bwr 'z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.5.Results\RN_001_results_paper\' ;
libname bww 'z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN010_bwdatasets\' ;

 ods listing close;*to suppress the output printing;

/*##################################################################*/
/*LAGS*/
/*LAGS*/
/*LAGS*/
/*LAGS*/
/*LAGS*/
/*##################################################################*/

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintemp_l0  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintemp_l0;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdc_l0 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdc_l0;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintemp_l1  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintemp_l1;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdc_l1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdc_l1;
run;





proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma1  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma1;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcma1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcma1;
run;



proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma3  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcma3 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcma3;
run;



/*perdios*/


proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempmaweek  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.fintempmaweek;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcmaweek sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcmaweek;
run;


proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma2week  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.fintempma2week;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcma2week sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcma2week;
run;




proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempmamonth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.fintempmamonth;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcmamonth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcmamonth;
run;




proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3month;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.tncdcma3month ;
run;




 
proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = tncdcmabirth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.tncdcmabirth;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.fintempmabirth;
run;




/*##################################################################*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*IQRLAGS*/
/*##################################################################*/

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintemp_l0  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQRfintemp_l0;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdc_l0 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdc_l0;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintemp_l1  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQRfintemp_l1;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdc_l1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdc_l1;
run;





proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma1  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQRfintempma1;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma1 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcma1;
run;



proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQRfintempma3;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma3 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcma3;
run;


/*perdios*/


proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmaweek  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.IQRfintempmaweek;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmaweek sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcmaweek;
run;


proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma2week  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.IQRfintempma2week;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma2week sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcma2week;
run;



proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmamonth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =   bwr.IQRfintempmamonth;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmamonth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcmamonth;
run;



proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3month sinetime  costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
  where bwcat=1;
   ods output  SolutionF =  bwr.IQRfintempma3month;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRtncdcma3month ;
run;




 
proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRtncdcmabirth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQRtncdcmabirth;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess adtmean 
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = bwr.IQRfintempmabirth;
run;




/*OLD*/


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lges (event= "1")= IQRfintempma3month adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.LGES_IQRfintempma3month ;
run;

 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lges (event= "1")= IQRfintempmamonth adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.LGES_IQRfintempmamonth ;
run;


proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lges (event= "1")= IQRtncdcmabirth adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.LGES_IQRtncdcmabirth ;
run;






proc logistic data=bww.bw_all  desc;
  class kess  MRN EDU_GROUP fips byob fips ;
       model lges  =  IQRtncdcmabirth  adtmean sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   byob fips;
ods output  ParameterEstimates =  bwr.logi_lges_IQRtncdcmabirth;
run;






/*diab*/
/*diab*/
/*diab*/
/*diab*/
/*diab*/
/*diab*/

 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRfintempma3  sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempma3 ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRtncdcma3   sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcma3 ;
run;

 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRfintempmaweek  sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempmaweek ;
run;


 proc glimmix data=bww.bw_all ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRtncdcmaweek   sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcmaweek  ;
run;

 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRfintempma2week   sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempma2week ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRtncdcma2week  sinetime costime  adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcma2week   ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRfintempmamonth  sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempmamonth ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRtncdcmamonth  sinetime costime  adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcmamonth   ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRfintempma3month  sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempma3month;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRtncdcma3month  sinetime costime sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcma3month   ;
run;


 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model   diab(event= "1")= IQRfintempmabirth  sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
 diab lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRfintempmabirth   ;
run;



 proc glimmix data=bww.bw_all  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= IQRtncdcmabirth   sinetime costime adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  bwr.DIAB_IQRtncdcmabirth;
run;




proc logistic data=bww.bw_all  desc;
  class kess  MRN EDU_GROUP fips byob fips ;
       model diab =  IQRtncdcmabirth  age_centered age_centered_sq cig_preg cig_pre med_income p_ospace 
  hyper_other lungd  kess  MRN edu_group  byob sinetime costime FIPS;
ods output  ParameterEstimates =  bwr.logi_DIAB_IQRtncdcmabirth;
run;


proc logistic data=Bw_diab37  desc;
  class kess  MRN EDU_GROUP fips byob ;
    model diab = totpm12_24  age_centered age_centered_sq cig_preg cig_pre med_income p_ospace 
  hyper_other lungd  kess  MRN edu_group  byob sinetime costime FIPS ;
  run;






proc means data=bww.Bw_noces n min max mean median std nmiss;
var med_income; 
run; 

/*interaction*/

  
data bww.Bw_noces;
set bww.Bw_noces;

edu01=0;
if myredu >12  then edu01=1;

mrn01=1;
if mrn=0 then mrn01=0;

minc01=1;
if med_income <= 50143.00 then minc01=0;




if edu01=0 then fedu01=1;
if edu01=1 then fedu01=0;

if mrn01=0 then fmrn01=1;
if mrn01=1 then fmrn01=0;

if minc01=0 then fminc01=1;
if minc01=1 then fminc01=0;
run; 


/*interact edu*/
proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN edu01 fips byob ;
 model BIRTHW = fintempma3month fintempma3month*edu01 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu01   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXedu01;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN fedu01 fips byob ;
 model BIRTHW = fintempma3month fintempma3month*fedu01 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN fedu01   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXfedu01;
run;

/*interact race*/

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  mrn01 EDU_GROUP fips byob ;
 model BIRTHW = fintempma3month fintempma3month*mrn01 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  mrn01  EDU_GROUP byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXmrn01 ;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  mrn01 EDU_GROUP fips byob ;
 model BIRTHW = fintempma3month fintempma3month*fmrn01 sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
   fmrn01  EDU_GROUP byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXfmrn01 ;
run;


/*interact income*/

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma3month fintempma3month*minc01 minc01 sinetime costime age_centered age_centered_sq cig_preg cig_pre  p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
   MRN EDU_GROUP byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXminc01 ;
run;

proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = fintempma3month fintempma3month*fminc01 fminc01 sinetime costime age_centered age_centered_sq cig_preg cig_pre  p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
    MRN  EDU_GROUP byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fintempma3monthXfminc01 ;
run;



/*Extra*/


/*lung desiease seems to work*/

 proc glimmix data=bw_noces  ;
  class kess  MRN EDU_GROUP fips byob lungd ;
    model lungd (event= "1")= totpm tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab   prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  or270day ;
run;







data Iqrfintemp_l0 ;set bwr.Iqrfintemp_l0 ;if _n_=2;run; 
data Iqrfintemp_l1 ;set bwr.Iqrfintemp_l1 ;if _n_=2;run; 
data IQRfintempma1 ;set bwr.IQRfintempma1 ;if _n_=2;run; 
data IQRfintempma3 ;set bwr.IQRfintempma3 ;if _n_=2;run; 
data IQRfintempmaweek ;set bwr.IQRfintempmaweek ;if _n_=2;run; 
data IQRfintempma2week ;set bwr.IQRfintempma2week ;if _n_=2;run; 
data IQRfintempmamonth ;set bwr.IQRfintempmamonth ;if _n_=2;run; 
data IQRfintempma3month ;set bwr.IQRfintempma3month ;if _n_=2;run; 
data Iqrfintempmabirth ;set bwr.Iqrfintempmabirth ;if _n_=2;run; 


data fintemp_paper;
set 
Iqrfintemp_l0 
Iqrfintemp_l1 
IQRfintempma1 
IQRfintempma3 
IQRfintempmaweek 
IQRfintempma2week 
IQRfintempmamonth 
IQRfintempma3month
Iqrfintempmabirth;
run;

PROC EXPORT DATA= fintemp_paper
            OUTFILE= "z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.5.Results\RN_002_results_paper_final\fintemp_paper.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


data tncdc_paper;
set 
Iqrtncdc_l0 
Iqrtncdc_l1 
IQRtncdcma1 
IQRtncdcma3 
IQRtncdcmaweek 
IQRtncdcma2week 
IQRtncdcmamonth 
IQRtncdcma3month
Iqrtncdcmabirth;
run;

PROC EXPORT DATA= tncdc_paper
            OUTFILE= "z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.5.Results\RN_002_results_paper_final\tncdc_paper.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  






data lr;
set bww.bw_all;
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
 

PROC EXPORT DATA= lr (keep= fintempma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean MRN edu_group kess  MRN EDU_GROUP fips byob lmp bdob)
            OUTFILE= "c:\Users\ekloog\Documents\tmp\bd.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


proc lifereg data = lr;
class kess  MRN EDU_GROUP fips byob ;
  model (lmp,bdob) = fintempma3month sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean MRN edu_group  / d=gamma;
ods OUTPUT ParameterEstimates= bwr.plr_3month;
run;


proc lifereg data = lr;
class kess  MRN EDU_GROUP fips byob ;
  model (lmp,bdob) = fintempmabirth sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean MRN edu_group  / d=gamma;
ods OUTPUT ParameterEstimates= bwr.plr_fullbirth;
run;



proc lifereg data = lr;
class kess  MRN EDU_GROUP fips byob gesgroup  ;
  model (lmp,bdob) = fintempma3month*gesgroup sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean MRN edu_group  / d=gamma;
ods OUTPUT ParameterEstimates= bwr.plr_3month_gesgroup;
run;


proc lifereg data = lr;
class kess  MRN EDU_GROUP fips byob gesgroup  ;
  model (lmp,bdob) = fintempmabirth*gesgroup sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab hyper lungd diab_other prevpret kess adtmean MRN edu_group  / d=gamma;
ods OUTPUT ParameterEstimates= bwr.plr_fullbirth_gesgroup;
run;




PROC IMPORT OUT= ur
            DATAFILE= "z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN007_keytables\urb_rural30km.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc freq data=ur;
table urb_rur / list;
run; 


data ur (drop=xx yy);
set ur;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

data ur (drop = xnym ynym xnymx ynymx);
set  ur;
xnym=put(glong,Best12.);
   ynym=put(glat, Best12.);
   xnymx = xnym*10000;
   ynymx = ynym*10000;
   guid = compress(xnymx||ynymx);
run;


proc sort data = ur; by guid   ;run;
proc sort data = bww.Bw_noces ; by guid  ;run;

data DATA3;
merge bww.Bw_noces(in=a) ur (in=b)  ;
  by guid ;
    if a;
	run; 

data DATA3;
set DATA3;
if urb_rur=0 then furb_rur=1;
if urb_rur=1 then furb_rur=0;
run; 

proc freq data=DATA3;
table  urb_rur  furb_rur / list;
run; 

	proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = IQRfintempma3month urb_rur*IQRfintempma3month  urb_rur sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.RURURB_IQRfintempma3month;
run;

proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob furb_rur;
 model BIRTHW = IQRfintempma3month furb_rur*IQRfintempma3month  furb_rur sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fRURURB_IQRfintempma3month;
run;


	proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = IQRfintempma3month  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  tstu;
   where urb_rur=1;
run;

proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = IQRfintempma3month  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  tstr;
   where urb_rur=0;
run;

/*full birth*/

proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = Iqrfintempmabirth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.xtstu;
   where urb_rur=1;
run;


proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = Iqrfintempmabirth  sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.xtstr;
   where urb_rur=0;
run;


	proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob urb_rur;
 model BIRTHW = Iqrfintempmabirth urb_rur*Iqrfintempmabirth  urb_rur sinetime costime
age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.RURURB_Iqrfintempmabirth;
run;

proc mixed data = DATA3 method=reml covtest;
class kess  MRN EDU_GROUP fips byob furb_rur;
 model BIRTHW = Iqrfintempmabirth furb_rur*Iqrfintempmabirth  furb_rur sinetime costime
age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.fRURURB_Iqrfintempmabirth;
run;


proc means data=DATA3 n min max mean std nmiss;
var BIRTHW Iqrfintempmabirth; 
class urb_rur;
run; 

PROC EXPORT DATA= DATA3
            OUTFILE= "c:\Users\ekloog\Documents\tmp\bdata.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
							  
	
PROC IMPORT OUT= data3
  DATAFILE= "c:\Users\ekloog\Documents\tmp\bdata.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 


proc reg data=bww.Bw_noces;
model BIRTHW= ges_calc   ;
 ods output  parameterestimates = xx;
run;
quit; 

proc glm data=bww.Bw_noces;
model BIRTHW= ges_calc /solution ;
 ods output  parameterestimates = xx;
run;
quit; 

PROC EXPORT DATA= DATA3
            OUTFILE= "c:\Users\ekloog\Documents\tmp\bdata.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

 data work.test;
 set bww.Bw_noces;
 keep birthw Iqrfintempmabirth ges_calc  sinetime costime
age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess fips  
  MRN edu_group   byob IQRfintempma3month;
 run;


PROC EXPORT DATA= test
            OUTFILE= "c:\Users\ekloog\Documents\tmp\bdata_test.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

proc mixed data = bww.Bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3month sinetime ges_calc costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQR_SENS_fintempma3month;;
run;


							  

proc mixed data = bww.Bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = Iqrfintempmabirth sinetime ges_calc costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  bwr.IQR_SENS_Iqrfintempmabirth;
run;





/*So fit a logistic model for BW < 2500 g and for GA < 37 weeks. */



  
data bww.Bw_noces;
set bww.Bw_noces;
lowbw=0;
if BIRTHW < 2500  then lowbw=1;
NSGA=0;
if gacalc < 37  then NSGA=1;
run; 

data bw_all;
set db.bw9;
keep uniqueid_y pmnew--pm12_24 localpm date;




PROC EXPORT DATA= bww.Bw_noces 
            OUTFILE= "Z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\3.Analysis\2.R_analysis\bw_nocesv2.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


 proc glimmix data=bww.Bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  NSGA (event= "1")= IQRfintempma3month adtmean age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  yy ;
run;



 proc glimmix data=bww.Bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lowbw (event= "1")= IQRfintempma3month parity  age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
  hyper lungd diab diab_other prevpret   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  xx ;
run;


 proc glimmix data=bww.Bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model  lowbw (event= "1")= IQRfintempma3month parity    /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  xx ;
run;


proc mixed data = bww.Bw_noces method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = IQRfintempma3  parity sinetime costime age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF = z1;
run;
