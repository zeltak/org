/*import the combined weather dataset*/

PROC IMPORT OUT= WORK.Airtemp 
            DATAFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\met_2_sav\met.sav" 
            DBMS=SPSS REPLACE;

RUN;


/*calcualte a mean temp file for mean temp in each day*/

proc summary nway data=Airtemp;
class date;
var tmin;
output out=meantemp mean=mtmp;
run;



/*import the mod2 prediction file*/

PROC IMPORT OUT= WORK.T2003_mod2pred 
            DATAFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod2_pred\T2003_mod2pred.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

/*fix character date issue*/


data T2003_mod2pred_v2;
set T2003_mod2pred ;
newdate = input(date,mmddyy10.);
format newdate mmddyy10.;
drop date;
run;


data T2003_mod2pred_v3;
set T2003_mod2pred_v2(rename=(newdate=date ));;
run;



/*merge with mean temp*/

proc sort data= T2003_mod2pred_v3;
by date;
run;


proc sort data= meantemp;
by date;
run;


data T2003_mod2pred_v4;
merge T2003_mod2pred_v3 meantemp (keep=date mtmp) ;
by date;
if predicted=. then delete;
run;


/*create month and bimon variables*/

data T2003_mod2pred_v5; 
set T2003_mod2pred_v4; 
m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
run; 



PROC EXPORT DATA= WORK.T2003_mod2pred_v5 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3\T2003_mod2predall.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
