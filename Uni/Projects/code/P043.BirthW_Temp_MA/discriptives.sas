libname exp 'z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN002_BW_exposure\' ;
libname bww 'z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN010_bwdatasets\' ;

PROC IMPORT OUT= bd
  DATAFILE= "z:\uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\3.Analysis\2.R_analysis\bw_nocesv2.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;



proc means data=bww.lr n mean median std range qrange q1 q3 n ;
var bw; 
run; 









 


proc means data=bww.Bw_noces n mean median std range qrange q1 q3 n ;
var fintemp tncdc_l0 adtmean; 
run; 




proc univariate data=bww.Bw_noces;
var EDU_GROUP ;
run;
 

proc freq data=bww.Bw_noces;
table  gender MRN EDU_GROUP aged / list;
run; 

proc freq data=bww.Bw_noces;
table  diab hyper lungd diab_other prevpret parity prev_400 / list;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where gender=1;
run; 
proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where gender=2;
run; 


proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where MRN=1;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where MRN=0;
run; 



proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where MRN=2;
run; 


proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where EDU_GROUP =1;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where EDU_GROUP =2;
run; 


proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where EDU_GROUP =3;
run; 


proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where EDU_GROUP =4;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where aged =1;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where aged =2;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where aged =3;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where aged =4;
run; 

proc means data=bww.Bw_noces n min max mean std nmiss;
var bw; 
where aged =5;
run; 

