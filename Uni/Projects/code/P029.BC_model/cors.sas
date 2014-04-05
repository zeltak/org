libname aod 's:\ENVEPI\Airs\nas\RAWDATA\BCPredictions\bcpredictions_9_6_12\' ;
libname aod2 's:\ENVEPI\Airs\nas\SASDATA\' ;



 

data alexbcorig;
set aod2.bcraw2012;
run; 

data alexbcorig2 (keep=date addid xinkm yinkm bc);
set alexbcorig;
bc=exp(log_BC);
run; 


data alexbcorig2;
set alexbcorig2;
 AddIDNew= AddID;
run; 

PROC IMPORT OUT= newbc
  DATAFILE= "f:\Uni\Projects\P030_BC_model\3.Work\3.Analysis\mod1_predicted\mod1pred.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;

data newbc;
set newbc ;
newdate = input(date,mmddyy10.);
format newdate mmddyy10.;
drop date;
predlag = lag(pred);
run;


data newbc;
set newbc(rename=(newdate=date ));;
run;



proc sort data = alexbcorig2; by AddIDNew date   ;run;
proc sort data = newbc ; by  AddIDNew date ;run;

data DATA3;
merge newbc (in=a) alexbcorig2 (in=b)  ;
  by AddIDNew date ;
  if a;
run;  

data DATA3;
set DATA3;
year=year(date);
run; 

proc means data=DATA3 n min max mean std nmiss;
var ; 
run; 

proc reg data=data3 ;
    model pred =  bc ;
 		by year;
	run;


/*other set*/


data alexbc;
set aod.nasbcpred9_6_12;
run; 

data alexbc2 (keep=AddIDNew date xinkm yinkm bc);
set alexbc;
date = dateval9am - 21916;
format DATE  mmddyy8.;
run; 



proc sort data = alexbc2; by AddIDNew date   ;run;
proc sort data = newbc ; by  AddIDNew date ;run;

data DATA3x;
merge newbc (in=a) alexbc2 (in=b)  ;
  by AddIDNew date ;
  if a;
run;  

data DATA3x;
set DATA3x;
year=year(date);
run; 


proc reg data=data3x ;
    model pred =  bc ;
	run;


proc sort data = data3x ; by year   ;run; 

proc reg data=data3x ;
    model pred =  bc ;
 		by year;
	run;

data DATA3x;
set DATA3x;
ratio=pred/bc;
run; 

proc means data=DATA3x n min max mean std nmiss;
var ratio; 
run; 

proc summary nway data=DATA3x;
class xx yy;
var ratio;
output out=OUTPUTFILE mean=ratio;
run; 

PROC EXPORT DATA= OUTPUTFILE 
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratioOUTDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
