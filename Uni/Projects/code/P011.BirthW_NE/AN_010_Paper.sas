/*birth weight analysis*/



proc mixed data = bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = totpmmon tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
   ods output  SolutionF =  f30day;
run;

PROC EXPORT DATA= WORK.f30day 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.5.Results\RN_001_results_paper\F30.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 


proc mixed data = bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = totpm3m tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
     ods output  SolutionF =  f90day;
run;



PROC EXPORT DATA= WORK.f90day 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.5.Results\RN_001_results_paper\F90.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 


proc mixed data = bw_all method=reml covtest;
class kess  MRN EDU_GROUP fips byob ;
 model BIRTHW = totpm tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
     ods output  SolutionF =  f270day;
run;


PROC EXPORT DATA= WORK.f270day 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.5.Results\RN_001_results_paper\F270.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 

/*preterm births*/



 proc glimmix data=bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model lges (event= "1")= totpmmon tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  ParameterEstimates;
run;

 proc glimmix data=bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model lges (event= "1")= totpm3m  tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  ParameterEstimates;
run;




 proc glimmix data=bw_noces  ;
  class kess  MRN EDU_GROUP fips byob ;
    model lges (event= "1")= totpm tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob   /s dist=binary link=logit or cl ;
   random intercept / subject=FIPS ;
ods output  ParameterEstimates =  or270day ;
run;




PROC EXPORT DATA= WORK.or270day 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.5.Results\RN_001_results_paper\or270.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 

/*interaction*/


proc mixed data = bw_all method=reml covtest;
class kess  MRN xEDU_GROUP fips byob ;
 model BIRTHW = totpm totpm*xEDU_GROUP tden age_centered age_centered_sq cig_preg cig_pre med_income p_ospace gender prev_400
diab  hyper lungd diab_other prevpret kess  
  MRN edu_group   byob    / s cl outpred=OUTFILE;
  random int  / sub = fips s ;
     ods output  SolutionF =  f270day;
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

