libname j1 'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9.Jamie_Worcester\3.1.9.4.Work\2.Gather_data\new_step\' ;

data spat;
set j1.spatial_small;
Long_AOD=x;
Lat_AOD=y;
run; 


 
PROC IMPORT OUT= WORK.gxy
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9.Jamie_Worcester\3.1.9.4.Work\3.Analysis\addXY\gridxy.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
						 RUN; 

proc sort data = gxy; by Long_AOD  Lat_AOD ;run;
proc sort data = spat; by Long_AOD  Lat_AOD ;run;

data  cfXY;
merge spat(in=a) gxy (in=b)  ;
  by Long_AOD  Lat_AOD ;
    if a;
	run; 



data j1.cases_guid;
set cfXY;
run; 
