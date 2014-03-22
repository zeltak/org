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
set newbc;

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





/*alex data set*/


data alexbc;
set aod.nasbcpred9_6_12;
run; 

data alexbc2 (keep=AddIDNew date xinkm yinkm bc);
set alexbc;
date = dateval9am - 21916;
format DATE  mmddyy8.;
run; 


data alexbc4 (drop=AddIDNew);
set alexbc2;
  guid2=put(AddIDNew, 6.); 
run; 

data alexbc5(drop=  guid2);
set alexbc4;
AddIDNew=guid2;
run; 




/*merge alex and schwartz models*/

proc sort data = alexbc5; by AddIDNew date   ;run;
proc sort data = newbcx ; by  AddIDNew date ;run;

data DATA3x;
merge newbcx (in=a) alexbc5 (in=b)  ;
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


proc sort data = data3x; by year   ;run; 


proc reg data=data3x ;
    model pred =  bc ;
	by year ;
	run;


symbol1 v=dot h=0.5 w=0.5 c=blue;
proc gplot data=DATA3x;
Title "TITLE";
  plot bc*pred/ grid;
	by year ;
		   run; 
		   quit; 




/*sample date +1*/
/**/
/*data newbcx3 (drop=date);*/
/*set newbcx2;*/
/*datep1=sampledate+1;*/
/*format datep1  mmddyy8.;*/
/*run; */
/**/
/**/
/*data newbcx3;*/
/*set newbcx3;*/
/*date=datep1;*/
/*format date  mmddyy8.;*/
/*run; */
/**/
/*proc sort data = alexbc5; by AddIDNew date   ;run;*/
/*proc sort data = newbcx3 ; by  AddIDNew date ;run;*/
/**/
/*data DATA4x;*/
/*merge newbcx3 (in=a) alexbc5 (in=b)  ;*/
/*  by AddIDNew date ;*/
/*  if a;*/
/*run;  */
/**/
/*data DATA4x;*/
/*set DATA4x;*/
/*year=year(date);*/
/*run; */
/**/
/*ods listing;*/
/*proc reg data=data4x ;*/
/*    model pred =  bc ;*/
/*	run;*/


options mprint;
%macro import(year=);


data ratio&year;
set DATA3x;
ratio=pred/bc;
xx=xinkm*1000;
yy=yinkm*1000;
where year=&year;
run; 

proc summary nway data=ratio&year;
class xx yy;
var ratio;
output out=OUTratio&year mean=ratio;
run; 

PROC EXPORT DATA= OUTratio&year
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratio&year.GIS.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


%MEND ;
%import(year=1995);
%import(year=1996);
%import(year=1997);
%import(year=1998);
%import(year=1999);
%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);	



proc means data=OUTratio1995 n min max mean std nmiss;
var bc pred; 
run; 



options mprint;
%macro import(year=);


data ratio&year;
set DATA3x;
ratio=pred/bc;
xx=xinkm*1000;
yy=yinkm*1000;
where year=&year;
run; 

proc summary nway data=ratio&year;
class xx yy;
var ratio pred bc;
output out=OUTratio&year mean=ratio pred bc ;
run; 

PROC EXPORT DATA= OUTratio&year
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratio&year.GISx.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


%MEND ;
%import(year=1995);
%import(year=1996);
%import(year=1997);
%import(year=1998);
%import(year=1999);
%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);	



















proc summary nway data=DATA3x;
class xx yy;
var ratio;
output out=OUTPUTFILE mean=ratio;
run; 

PROC EXPORT DATA= OUTPUTFILE 
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratioOUTDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
	
data DATA3xd;
set DATA3x;
if pred < 1.5 then delete;
if pred > 3.5 then delete;
if pred in (1.5 , 2.5) and bc < 3 then delete;
if pred in (2.5, 3.5) and bc < 4.5 then delete; 
m=month(date);
xx=xinkm*1000;
yy=yinkm*1000;
run; 
proc summary nway data=DATA3xd;
class xx yy;
var pred;
output out=OUTPUTFILE mean=pred;
run; 


PROC EXPORT DATA= OUTPUTFILE 
            OUTFILE= "c:\Users\ekloog\Documents\tmp\rrtOUTDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
			




ods listing close;*to suppress the output printing;
proc freq data=DATA3xd;
table AddIDNew /  list;
ods output onewayfreqs=onewayfreqs;
where year=1995;
run; 

proc sort data = onewayfreqs; by Frequency   ;run; 

ods listing ;
proc univariate data=onewayfreqs;
var Frequency ;
histogram  Frequency ;
run;
 
proc sort data = Data3xd; by AddIDNew   ;run;
proc sort data = onewayfreqs ; by AddIDNew ;run;


proc sort data = Data3xd; by xx yy   ;run; 
proc sort data = Data3xd nodupkey out=xytable;
by  xx yy ; 
run;


data xytable;
set xytable ;
keep xx yy AddIDNew;
run; 


ods listing close;*to suppress the output printing;
proc freq data=DATA3xd;
table AddIDNew /  list;
ods output onewayfreqs=onewayfreqs;
where year=2002;
run;  


proc sort data = xytable; by AddIDNew   ;run;
proc sort data = onewayfreqs ; by AddIDNew ;run;

data n02;
merge onewayfreqs(in=a) xytable(in=b)  ;
  by AddIDNew;
    if a;
	run; 



PROC EXPORT DATA= n02
            OUTFILE= "c:\Users\ekloog\Documents\tmp\n02.dbf" 
            DBMS=DBF REPLACE;
RUN;



