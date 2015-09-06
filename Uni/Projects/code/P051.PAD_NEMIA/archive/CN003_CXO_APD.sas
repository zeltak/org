


libname full 'Y:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\PM\' ;

data cases ;
set xo.apd_counts;
drop pmnew--temp_fmayear;
run; 

/*rename*/
data cases ;
set cases (rename=(date=INDATE));
run; 


/*Create data with: ID (QID), event date (indate)
/***Create control days***/
/***same day of the week within same month***/
data CVA;set cases;
DOW=WEEKDAY(INDATE);
CONTROL1=INDATE-7;
CONTROL2=INDATE-14;
CONTROL3=INDATE-21;
CONTROL4=INDATE-28;
CONTROL5=INDATE+7;
CONTROL6=INDATE+14;
CONTROL7=INDATE+21;
CONTROL8=INDATE+28;
FORMAT CONTROL1 CONTROL2 CONTROL3 CONTROL4 CONTROL5 CONTROL6 CONTROL7 CONTROL8
DDMMYY9.;
MONTH1=MONTH(CONTROL1);
MONTH2=MONTH(CONTROL2);
MONTH3=MONTH(CONTROL3);
MONTH4=MONTH(CONTROL4);
MONTH5=MONTH(CONTROL5);
MONTH6=MONTH(CONTROL6);
MONTH7=MONTH(CONTROL7);
MONTH8=MONTH(CONTROL8);
month=month(indate);
RUN;
/*Keep only days within the same month*/
DATA CVA;SET CVA;
if month1=month then control1=control1;else control1=.;
if month2=month then control2=control2;else control2=.;
if month3=month then control3=control3;else control3=.;
if month4=month then control4=control4;else control4=.;
if month5=month then control5=control5;else control5=.;
if month6=month then control6=control6;else control6=.;
if month7=month then control7=control7;else control7=.;
if month8=month then control8=control8;else control8=.;
run;

/*Rename case date as "control0"*/
data cva;set cva (rename=(indate=control0));run;

/*Sort by ID*/

 proc sort data= cva;
 by QID;
 run;

/*Restracture*/
PROC TRANSPOSE DATA=cva OUT=cva1
 PREFIX=control;
 VAR control0-control8 ;
 BY QID;
 run;

 /*Delete dates that were not within the same month*/
 data cva1;set cva1 (keep=QID _NAME_ control1);
 where control1>.;run;


 /***Rename as case day and control days***/
 data cva1;set cva1;
 if _NAME_="control0" then case=1;else case=0;
 if case=1 then Time=1;else Time=2;
 run;

data cva1;set cva1 (rename=(control1=day));
FORMAT day DDMMYY9.;
run;


 

