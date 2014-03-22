


/*merge alex and schwartz models*/

proc sort data = DATA5xmiss; by AddIDNew date   ;run;
proc sort data = data3 ; by  AddIDNew date ;run;

data DATA6x;
merge data3 (in=a) DATA5xmiss(in=b)  ;
  by AddIDNew date ;
  if a;
run;  

data data7x;
set data6x;
if bcold > 0.0001;;
run; 



 

proc reg data=data7x ;
    model pred =  bcold ;
	run;

proc univariate data=data6x;
var bcold;
run;


symbol1 v=dot h=0.5 w=0.5 c=blue;
proc gplot data=data6x;
Title "TITLE";
  plot bcold*pred/ grid;
	by year ;
		   run; 
		   quit; 



options mprint;
%macro import(year=);


data ratio&year;
set DATA6x;
ratio=bcold/pred;
ratiots=tspred/bc;
xx=xinkm*1000;
yy=yinkm*1000;
where year=&year;
run; 

proc summary nway data=ratio&year;
class xx yy;
var ratio ratiots pred bc bcold tspred;
output out=OUTratio&year mean=ratio ratiots pred bc bcold ;
run; 

PROC EXPORT DATA= OUTratio&year
            OUTFILE= "c:\Users\ekloog\Documents\tmp\ratio&year.GISBXCOLD33.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


%MEND ;
%import(year=1995);



data bcold2tst;
length AddIDNew $ 8;
set bcold2;
if bcold < 0.0001;
run; 


proc sort data = bcold2tst; by AddIDNew  ;run;
proc sort data = DATA6x ; by AddIDNew ;run;

data zDATA;
merge bcold2tst(in=a) DATA6x (in=b keep=AddIDNew xinkm yinkm)  ;
  by AddIDNew;
    if a;
	run;

data zDATA;
set zDATA;
xx=xinkm*1000;
yy=yinkm*1000;
run;  

PROC EXPORT DATA= zDATA
            OUTFILE= "c:\Users\ekloog\Documents\tmp\zDATA.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
