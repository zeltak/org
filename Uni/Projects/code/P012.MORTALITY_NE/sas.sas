 
PROC IMPORT OUT= mort
  DATAFILE= "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase/allmort_spline.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
	

data mort ;
set mort;
if pmnew_l1="NA" then pmnew_l1=.;
if temp_f_l1="NA" then temp_f_l1=.;
run;  

data mort;
set mort;
pmacu=pmnew_l1*1;
tacu=temp_f_l1*1;
run; 

proc glimmix data=mort  ;
      model count = pmacu pmnewmayear dp temp_f_l1 med_inc Avg_per_mi Avg_pctcol Avg_p_A65 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
/*ods output oddsratios=oddratio ;*/
run;


