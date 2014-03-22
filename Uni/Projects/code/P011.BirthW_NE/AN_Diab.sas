

/*data for preterm analysis*/
data bw_diab37;
set bw8;
where ges_calc >37;
if yrod ne 0 then delete;
if pmnewma3month < 0 then delete;
if pmnewmamonth < 0 then delete;
if ges_calc >=37 then bwcat=1;
else if ges_calc = 36 then  bwcat=2;
else if ges_calc =35 then bwcat=2;
else if ges_calc < 35 then bwcat=3;
if pctwhttr00=. then delete;
if diab_other =1 then delete;
totpm=pmnewmabirth+localpm;
totpm3m=pmnewma3month+localpm;
totpmmon=pmnewmamonth+localpm;
dow=weekday(date);
   doy=put (date,julian5.);
   doy2=substr(doy,3,3);
   sinetime=sin(2*constant('pi')*doy2/365.25);
   costime=cos(2*constant('pi')*doy2/365.25);
   newvar=put(date, date9.);
udate = substr(newvar,1,5);
run; 

data bw_diab_first;
set bw_diab;
where parity=1;
run;

 proc glimmix data=Bw_diab37  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")= pm12_24  age_centered age_centered_sq cig_preg cig_pre med_income p_ospace  
  hyper_other lungd  kess  MRN edu_group  byob sinetime costime temp_fmabirth   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  diab_pmnewmabirth ;
run;


proc logistic data=Bw_diab37  desc;
  class kess  MRN EDU_GROUP fips byob FIPS;
    model diab = totpm12_24  age_centered age_centered_sq cig_preg cig_pre med_income p_ospace 
  hyper_other lungd  kess  MRN edu_group  byob sinetime costime FIPS ;
  run;



 proc glimmix data=Bw_diab_first  ;
  class kess  MRN EDU_GROUP fips byob ;
    model diab (event= "1")=   age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender 
  hyper_other lungd  kess  MRN edu_group  byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
run;




proc means data=bw_diab n min max mean std nmiss;
var diab; 
run;	

proc freq data=bw_diab;
table diab / list;
run; 

