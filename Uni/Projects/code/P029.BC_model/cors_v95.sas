libname aod 's:\ENVEPI\Airs\nas\RAWDATA\BCPredictions\bcpredictions_9_6_12\' ;
libname aod2 's:\ENVEPI\Airs\nas\SASDATA\' ;



 
libname cv 'f:\Uni\Projects\P030_BC_model\3.Work\3.Analysis\mod1_predicted\' ;

data newbc;
set cv.mod1pred_75p;
run; 


PROC IMPORT OUT= a1
  DATAFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A1.csv" 
    DBMS=CSV REPLAE;
	  GETNAMES=YES;
	    DATAROW=2; 
 GUESSINGROWS=100000;
		RUN;
		 
 
PROC IMPORT OUT= a2
  DATAFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A2.csv" 
    DBMS=CSV REPLAE;
	  GETNAMES=YES;
	    DATAROW=2; 
 GUESSINGROWS=100000;
		RUN;
		 
 data newbc ;
 set a1 a2;
 run; 

data newbc;
set newbc ;
newdate = input(date,mmddyy10.);
format newdate mmddyy10.;
drop date;
run;

data newbcx;
set newbc;
date = input(sampledate,mmddyy10.);
format date mmddyy10.;
run; 



data newbc;
set newbcx;
year=year(date);
if year ne 2006 then delete;
run; 



/**/
/*PROC IMPORT OUT= a95*/
/*  DATAFILE= "f:/Uni/Projects/P030_BC_model/3.Work/3.Analysis/mod1_predicted/mod1pred_A95ts.csv" */
/*    DBMS=CSV REPLACE;*/
/*	  GETNAMES=YES;*/
/*	    DATAROW=2; */
/* GUESSINGROWS=100000;*/
/*		RUN;*/
/**/
/**/
/*data a95;*/
/*set a95 ;*/
/*newdate = input(date,mmddyy10.);*/
/*format newdate mmddyy10.;*/
/*drop date;*/
/*run;*/
/**/
/*data a95(drop=pred);*/
/*set a95;*/
/*date = input(sampledate,mmddyy10.);*/
/*format date mmddyy10.;*/
/*tspred=pred*1;*/
/*run; */
/**/
/**/
/*proc sort data = a95; by AddIDNew date   ;run;*/
/*proc sort data = newbc ; by AddIDNew date  ;run;*/
/**/
/*data DATA3;*/
/*merge newbc(in=a) a95 (in=b)  ;*/
/*  by AddIDNew date ;*/
/*    if a;*/
/*	run; */


/*alex data set*/


data alexbc;
set aod.nasbcpred9_6_12;
run; 

data alexbc2 (keep=AddIDNew year date xinkm yinkm bc);
set alexbc;
date = dateval9am - 21916;
format DATE  mmddyy8.;
year=year(date);
run; 




data alexbc4 (drop=AddIDNew);
set alexbc2;
  guid2=put(AddIDNew, 6.); 
  if year ne 2006 then delete;
run; 

data alexbc5(drop=  guid2);
set alexbc4;
AddIDNew=guid2;
run; 


/*libname aod 's:\ENVEPI\Airs\BC Model\nas_oldalexandrosmodel\' ;*/
/**/
/*data bcold;*/
/*set aod.allpred7stacey;*/
/*run; */
/**/
/*proc means data= bcold n min max mean std nmiss;*/
/*var logbc; */
/*run;	*/
/**/
/*proc univariate data=bcold;*/
/*var logbc; */
/*run;*/
/* */
/**/
/**/
/*data bcold2 (keep= date AddIDNew year bcold bctmp);*/
/*length AddIDNew $ 6;*/
/*set bcold;*/
/*year=year(date);*/
/*bctmp=exp(1+logbc);*/
/*bcold=bctmp/2.71828;*/
/*if year ne 1995 then delete;*/
/*run; */
/**/
/**/
/**/
/**/
/**/
/*proc univariate data=bcold2;*/
/*var bctmp;*/
/*run;*/
/* */
/* */
/**/
/*proc sort data = alexbc5; by AddIDNew date   ;run;*/
/*proc sort data =  bcold2 ; by AddIDNew date ;run;*/
/**/
/*data DATA5x;*/
/*merge alexbc5(in=a) bcold2 (in=b)  ;*/
/*  by AddIDNew date;*/
/*    if a;*/
/*	run; */
/**/
/**/
/**/
/*data DATA5xmiss;*/
/*set DATA5x;*/
/*if bcold=. then delete;*/
/*run; */




/*merge alex and schwartz models*/

proc sort data = alexbc5; by AddIDNew date   ;run;
proc sort data = newbc ; by  AddIDNew date ;run;

data DATA3x;
merge newbc (in=a) alexbc5 (in=b)  ;
  by AddIDNew date ;
  if a;
run;  


proc reg data=data3x ;
    model pred =  bc ;
	run;

/**/
/*symbol1 v=dot h=0.5 w=0.5 c=blue;*/
/*proc gplot data=DATA3x;*/
/*Title "TITLE";*/
/*  plot bc*pred/ grid;*/
/*	by year ;*/
/*		   run; */
/*		   quit; */



/*TS*/

/*proc reg data=data3x ;*/
/*    model tspred =  bc ;*/
/*	run;*/
/**/
/**/
/*symbol1 v=dot h=0.5 w=0.5 c=blue;*/
/*proc gplot data=DATA3x;*/
/*Title "TITLE";*/
/*  plot bc*tspred/ grid;*/
/*	by year ;*/
/*		   run; */
/*		   quit; */








options mprint;
%macro import(year=);


data ratio&year;
set DATA3x;
ratio=pred/bc;
ratiots=tspred/bc;
xx=xinkm*1000;
yy=yinkm*1000;
where year=&year;
run; 

proc summary nway data=ratio&year;
class xx yy;
var ratio ratiots pred bc tspred;
output out=OUTratio&year mean=ratio ratiots pred bc ;
run; 

PROC EXPORT DATA= OUTratio&year
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratio&year.GISNEWR.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


%MEND ;
/*%import(year=1995);*/
/*%import(year=1996);*/
/*%import(year=1997);*/
/*%import(year=1998);*/
/*%import(year=1999);*/
/*%import(year=2000);*/
/*%import(year=2001);*/
/*%import(year=2002);*/
/*%import(year=2003);*/
/*%import(year=2004);*/
/*%import(year=2005);*/
%import(year=2006);
/*%import(year=2007);*/
/*%import(year=2008);	*/


proc means data=DATA3x n min max mean std nmiss;
var ; 
run; 


data diff;
set DATA3x;
diff=bc-pred;
if diff > 4 ;
xx=xinkm*1000;
yy=yinkm*1000;
run; 

PROC EXPORT DATA= diff
            OUTFILE= "c:\Users\ekloog\Documents\tmp\diff2006.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
PROC PLM SOURCE=item-store-specification <options> ; 
    EFFECTPLOT <plot-type <(plot-definition-options)>> </ options> ; 

proc plm source = diff;
  show parameters;
run;
