/*CVD*/



proc glimmix data=mort  ;
      model count = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid ;
/*   parms ( 9.3438)/  hold=1; */
run;



/*resp*/





proc glimmix data=mort  ;
      model resp = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (  8.1070)/  hold=1; 
run;

/*stroke*/


PROC IMPORT OUT= mort
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\stroke0006full.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc glimmix data=mort  ;
      model count = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (  5.7725)/  hold=1; 
run;

/*copd*/


PROC IMPORT OUT= mort
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\copd0006full.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc glimmix data=mort  ;
      model count = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (  5.370)/  hold=1; 
run;

/*ari*/


PROC IMPORT OUT= mort
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\ari0006full.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc glimmix data=mort  ;
      model count = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (   2.5679)/  hold=1; 
run;



/*pneum*/


PROC IMPORT OUT= mort
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\pneum0006full.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;


proc glimmix data=mort  ;
      model count = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd /s dist=poisson link=log  ;
   random intercept / subject=guid s;
   parms (6.1074) / hold=1 ; 
run;
