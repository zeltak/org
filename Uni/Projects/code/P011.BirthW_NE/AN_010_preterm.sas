 proc glimmix data=bw_noces  ;
  class kess  MRN  byob FIPS;
    model lges (event= "1")= pmnewmabirth localpm tden  dist_A1 age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab 
   lungd diab_other kess 
  MRN  byob  methnic prevpret parity /s dist=binary link=logit or ;
   random intercept / subject=FIPS ;
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
ods output oddsratios=oddratio ;
run;


localpm tden popden pcturban dist_A1 dist_pemis



 proc glimmix data=bw_noces  ;
  class kess  MRN  byob FIPS;
    model lges (event= "1")= pmnewmamonth localpm  age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab 
   lungd diab_other kess 
  MRN  byob  methnic prevpret parity /s dist=binary link=logit or ;
   random intercept / subject=FIPS ;
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
ods output oddsratios=oddratio ;
run;


 proc glimmix data=bw_noces  ;
  class kotck  MRN  byob FIPS;
    model lges (event= "1")= pmnewmabirth localpm  tden age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab 
   lungd diab_other kotck
  MRN  byob  methnic prevpret parity /s dist=binary link=logit or ;
   random intercept / subject=FIPS ;
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
ods output oddsratios=oddratio ;
run;


/*best*/
  
proc logistic data= bw_noces descending;
class kess MRN  byob ;
  model lges = pmnewmabirth localpm tden  age_centered age_centered_sq cig_preg cig_pre
  gender prev_400  diab  lungd diab_other kess
  MRN   byob methnic prevpret parity ; 
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
/*where byob ne 2000;*/
  run;




 proc glimmix data=bw_noces  ;
  class kess  MRN  byob FIPS;
    model lges (event= "1")= totpm tden  p_ospace age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab 
   lungd diab_other kess
  MRN  byob  methnic prevpret parity /s dist=binary link=logit or ;
   random intercept / subject=FIPS ;
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
ods output oddsratios=oddratio ;
run;





 proc glimmix data=bw_noces  ;
  class kess  MRN  byob FIPS;
    model lges (event= "1")= totpm3m tden  p_ospace age_centered age_centered_sq cig_preg cig_pre gender prev_400  diab 
   lungd diab_other kess
  MRN  byob  methnic prevpret parity /s dist=binary link=logit or ;
   random intercept / subject=FIPS ;
/*  where date>='31DEC2002'D and date<='31DEC2008'D ;*/
ods output oddsratios=oddratio ;
run;




