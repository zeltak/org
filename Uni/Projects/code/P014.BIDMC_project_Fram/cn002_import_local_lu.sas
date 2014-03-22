PROC IMPORT OUT= WORK.g3_lu
            DATAFILE= "h:\$Final\g3_lu.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.off_lu
            DATAFILE= "h:\$Final\off_lu.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.g3_tden
            DATAFILE= "h:\$Final\g3_tden.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.off_tden
            DATAFILE= "h:\$Final\off_tden.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;






proc sort data = off_lu; by ranid  ;run;
proc sort data = off_tden ; by ranid ;run;

data off_lu3;
merge off_lu(in=a) off_tden (in=b keep=Sum_Shape_ ranid)  ;
  by ranid;
    if a;
	run; 

data off_lu4 (drop=sum_Shape_);
set off_lu3;
tden= sum_Shape_;
if tden=. then tden=0;
run; 



proc sort data = g3_lu; by ranid  ;run;
proc sort data = g3_tden ; by ranid ;run;

data g3_lu3;
merge g3_lu(in=a ) g3_tden (in=b keep= ranid Sum_Shape_ )  ;
  by ranid;
    if a;
	run; 

data g3_lu4 (drop=sum_Shape_);
set g3_lu3;
tden= sum_Shape_;
if tden=. then tden=0;
if pcturban <0 then pcturban=0;
run; 






PROC EXPORT DATA= WORK.g3_lu4
            OUTFILE= "H:\$Final\R\lu_g3.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;





PROC EXPORT DATA= WORK.off_lu4
            OUTFILE= "H:\$Final\R\lu_off.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN; 
