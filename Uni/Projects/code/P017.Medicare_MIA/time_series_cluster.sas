
PROC IMPORT OUT= data3
  DATAFILE= "/n/home12/ekloog/work/all_cluster.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 


/*analysis*/


/*cvd*/

/*proc glimmix data=DATA3  ;*/
/*      model cvd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (9.4684)/  hold=1; */
/*run;*/
/**/


/*cvd*/

proc glimmix data=DATA3  ;
      model cvd = deltapm mpmguid temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
/*   parms (9.4684)/  hold=1; */
run;







/**/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model copd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (5.513)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model ari = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms ( 2.624)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model pneum = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (6.2448)/  hold=1; */
/*run;*/
/**/
/**/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model resp = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (8.2172)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model mi = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms ( 5.3460)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model chf = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (6.5738)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model diab = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (3.803)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model ihd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (7.2090)/  hold=1; */
/*run;*/
/**/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model stroke = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (5.9051)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model strisc = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (4.4072)/  hold=1; */
/*run;*/
/**/
/*proc glimmix data=DATA3  ;*/
/*      model strhem = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;*/
/*   random intercept / subject=guid ;*/
/*   parms (2.253)/  hold=1; */
/*run;*/
