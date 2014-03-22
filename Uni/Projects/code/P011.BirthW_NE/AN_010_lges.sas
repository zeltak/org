
data bw_noces;
set bw_noces;
if ges_calc < 35 then lgesx=1;
 if ges_calc >= 35 then lgesx=0;
run; 

data bw6;
set bw6;
if ges_calc < 36 then lgesx=1;
 if ges_calc >= 36 then lgesx=0;
run; 



proc logistic data= bw_noces descending;
class kotck MRN FRN prevpre edu_group;
  model lges = pmnewmabirth  age_centered age_centered_sq cig_preg cig_pre
gender prev_400  diab  lungd diab_other kotck 
  MRN edu_group adtmean med_income p_ospace byob ; 
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
/*where byob ne 2003;*/
  run;


proc logistic data= bw_noces descending;
class kotck MRN FRN prevpre edu_group;
  model lges = pmnewmamonth  cig_preg cig_pre gender prev_400  diab  lungd diab_other kotck MRN prevpre edu_group adtmean med_income p_ospace npar m_care mstat f_age FRN ; 
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
  run;

  

proc logistic data= bw_noces descending;
class kotck MRN FRN prevpre edu_group;
  model lgesx = pmnewma3month  cig_preg cig_pre gender prev_400  diab  lungd diab_other kotck MRN prevpre edu_group adtmean med_income p_ospace ; 
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
  run;
