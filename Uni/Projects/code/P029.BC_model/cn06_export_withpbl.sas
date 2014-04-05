/*import pbl*/

options mprint;
%macro import(year=);


PROC IMPORT OUT= P&year(drop=x1 y1 long_pbl lat_pbl)
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.4.Resources\DATA\hpbl\P&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 
%import(year=2009); 
%import(year=2010); 

data pblall;
set p2000 p2001 p2002 p2003 p2004 p2005 p2006 p2007 p2008 p2009 p2010;
run; 

data pblall2;
set pblall;
date=mdy(V2,V3,V1);
format date date7.;
run;




PROC IMPORT OUT= sumreg_pbl
            DATAFILE= "f:\Uni\Projects\P030_BC_model\3.Work\2.Gather_data\keytables\sumreg_pbl.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= nas_pbl
            DATAFILE= "f:\Uni\Projects\P030_BC_model\3.Work\2.Gather_data\keytables\nas_pbl.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname aod 's:\ENVEPI\Airs\BC Model\nasforschwartzbcmodel\' ;

data nas;
set aod.nasexpdays_ap_met;
xx=xinkm*1000;
yy=yinkm*1000;
run; 







/*sumreg*/

 PROC IMPORT OUT= sumreg
    DATAFILE= "s:\ENVEPI\Airs\BC Model\summerreg.csv" 
	  DBMS=CSV REPLACE;
	    GETNAMES=YES;
		  DATAROW=2; 
		  RUN;

data sumreg;
set sumreg;
xx=xinkm*1000;
yy=yinkm*1000;
run; 


data sumreg;
set sumreg ;
newdate = input(date,mmddyy10.);
format newdate mmddyy10.;
drop date;
run;

data sumreg;
set sumreg(rename=(newdate=date ));;
run;






/*assign pbl to sumreg*/


proc sort data = sumreg; by xx yy   ;run;
proc sort data = Sumreg_pbl ; by xx yy ;run;

data sumreg3;
merge sumreg(in=a) Sumreg_pbl (in=b keep=xx yy pblid)  ;
  by xx yy;
    if a;
	run; 

proc sort data = sumreg3; by pblid date   ;run;
proc sort data = pblall2 ; by pblid date ;run;

data sumreg3;
merge sumreg3(in=a) pblall2 (in=b keep=pblid date pbl)  ;
  by pblid date;
    if a;
	run;

PROC EXPORT DATA= sumreg3 
            OUTFILE= "f:\Uni\Projects\P030_BC_model\3.Work\3.Analysis\mod1\sumreg3.csv" 
			            DBMS=CSV REPLACE;
						     PTNAMES=YES;
							 RUN;
							  


/*assign pbl to nad*/


proc sort data = nas; by AddIDNew   ;run;
proc sort data = nas_pbl ; by AddIDNew ;run;

data nas3;
merge nas(in=a) nas_pbl (in=b keep=AddIDNew pblid)  ;
  by AddIDNew;
    if a;
	run; 

data nas3;
set nas3(rename=(sampledate=date ));
run; 

proc sort data = nas3; by pblid date   ;run;
proc sort data = pblall2 ; by pblid date ;run;

data nas3;
merge nas3(in=a) pblall2 (in=b keep=pblid date pbl)  ;
  by pblid date;
    if a;
	run;  


data nas3;
set nas3x;
w_dir_1=0;
if winddir_ind=1 then w_dir_1 =1;
w_dir_2=0;
if winddir_ind=2 then w_dir_2 =1;
w_dir_3=0;
if winddir_ind=3 then w_dir_3 =1;
day = JULDATE( date );
dayz=compress(day);
yearday=substr(dayz,2,3); 
weekday=weekday(date);
nlcd_ind=0;
if nlcd_urb01 > 765 then nlcd_ind=1;
Year=year(date);
run; 






PROC EXPORT DATA= nas3
            OUTFILE= "f:\Uni\Projects\P030_BC_model\3.Work\3.Analysis\mod1\nas3.csv" 
			            DBMS=CSV REPLACE;
						     PTNAMES=YES;
							 RUN;
							  
