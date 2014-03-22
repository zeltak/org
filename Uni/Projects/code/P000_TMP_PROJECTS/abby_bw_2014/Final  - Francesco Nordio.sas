libname babby 'f:\Uni\Projects\3.1.0.TMP_PROJECTS\abby_BW\' ;



data bwcon3;
set babby.bw_pm_preLMP;
lmp=bdob-(gacalc*7);
format lmp date9.;
totpreg=bdob-lmp;
end0_12 = totpreg;
beg0_12 = end0_12 - 90;
run; 



proc printto log="nul:"; run;
DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE;




option mprint;
%macro doit; 

%let id=%sysfunc(open(bwcon3));
 
%let NObs=%sysfunc(attrn(&id,NOBS)); 

 %syscall set(id); 

  %do i=1 %to &NObs; 

   %let rc=%sysfunc(fetchobs(&id,&i)); 

/*** Code ***/

%let j=1;

%do %while (%scan(&beg0_12,&j) ne);
   %let Cycle1 = %scan(&beg0_12,&j);
   %let j=%eval(&j+1);
%end;

%let k=1;

%do %while (%scan(&end0_12,&k) ne);
   %let Cycle2 = %scan(&end0_12,&k);
   %let k=%eval(&k+1);
%end;


/*************/

data Try_mean&i;
 set bwcon3;
   bcma0_12 = mean(of pmnew_l&Cycle1 - pmnew_l&Cycle2);
   if _N_ = &i;
run;
quit;
 
/*************/

proc append base = babby.lag12_24 data = Try_mean&i; run;
proc datasets lib = work nolist; delete Try_mean&i; run; quit;

%end; 
%let id=sysfunc(close(&id)); 

%mend; 

%doit;















