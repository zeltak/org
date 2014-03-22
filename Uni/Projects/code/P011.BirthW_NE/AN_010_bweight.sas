
proc mixed data = bw_all method=reml covtest;
class kotck  MRN EDU_GROUP fips byob ;
 model BIRTHW = pmnewmamonth localpm  age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab  lungd diab_other kotck 
  MRN edu_group adtmean med_income p_ospace byob  mstat f_age FRN/ s outpred=OUTFILE;
  random int  / sub = fips s ;
run;






proc mixed data = bw_all method=reml covtest;
class kotck  MRN EDU_GROUP fips byob ;
 model BIRTHW = pmnewma3month  age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab  lungd diab_other kotck 
  MRN edu_group adtmean med_income p_ospace byob  mstat f_age FRN/ s outpred=OUTFILE;
  random int  / sub = fips s ;
run;







proc mixed data = bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = totpm3m tden age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab  lungd diab_other kess 
  MRN edu_group  byob  methnic prevpret parity / s outpred=OUTFILE;
  random int  / sub = fips s ;
run;



