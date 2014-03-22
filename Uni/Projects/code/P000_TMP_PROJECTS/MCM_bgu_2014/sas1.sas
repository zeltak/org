libname aod 'P:\P000_TMP_PROJECTS\MCM_bgu_2014\exposure_weights' ;





proc mixed data = aod.Weight method=reml covtest;
class  male lopc code;
 model InfantWeigth = SO2_ppb_mean_1 pregnantage male lopc birthnum / s outpred=aod.OUTFILE; ;
  random int  / sub = code s ;
run;




data test;
set aod.weight;
drop NewAddress--P1V7
run; 
