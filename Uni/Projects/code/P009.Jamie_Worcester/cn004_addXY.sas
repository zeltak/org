libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9.Jamie_Worcester\3.1.9.4.Work\3.Analysis\addXY\' ;

data cf;
set aod.casesfinal;
run; 

PROC IMPORT OUT= WORK.gxy
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9.Jamie_Worcester\3.1.9.4.Work\3.Analysis\addXY\gridxy.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
						 RUN; 

proc sort data = gxy; by guid   ;run;
proc sort data = cf ; by guid ;run;

data  cfXY;
merge  cf(in=a) gxy (in=b)  ;
  by guid;
    if a;
	run; 


data cfXY;
set cfXY;
X=long_aod;
Y=lat_aod;
run; 


data aod.casesXYNEW;
set cfXY;
run; 
