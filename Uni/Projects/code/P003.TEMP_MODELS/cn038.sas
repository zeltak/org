
/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\grid_2003_bimon6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred\t2003_bimon6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;












proc sort data=grid_2003_bimon1;
by guid;
run;


proc sort data=t2003_bimon1;
by guid;
run;

data grid_2003_bimon1_v2 ;
merge grid_2003_bimon1 t2003_bimon1 (keep=guid gpred);
by guid;
run;




proc sort data=grid_2003_bimon2;
by guid;
run;



proc sort data=t2003_bimon2;
by guid;
run;



data grid_2003_bimon2_v2 ;
merge grid_2003_bimon2 t2003_bimon2 (keep=guid gpred);
by guid;
run;





proc sort data=grid_2003_bimon3;
by guid;
run;



proc sort data=t2003_bimon3;
by guid;
run;


data grid_2003_bimon3_v2 ;
merge grid_2003_bimon3 t2003_bimon3 (keep=guid gpred);
by guid;
run;





proc sort data=grid_2003_bimon4;
by guid;
run;



proc sort data=t2003_bimon4;
by guid;
run;




data grid_2003_bimon4_v2 ;
merge grid_2003_bimon4 t2003_bimon4 (keep=guid gpred);
by guid;
run;







proc sort data=grid_2003_bimon5;
by guid;
run;


proc sort data=t2003_bimon5;
by guid;
run;



data grid_2003_bimon5_v2 ;
merge grid_2003_bimon5 t2003_bimon5 (keep=guid gpred);
by guid;
run;





proc sort data=grid_2003_bimon6;
by guid;
run;


proc sort data=t2003_bimon6;
by guid;
run;



data grid_2003_bimon6_v2 ;
merge grid_2003_bimon6 t2003_bimon6 (keep=guid gpred);
by guid;
run;








/*convert text to date and rename back to date2*/


data grid_2003_bimon1_v3 ;
set grid_2003_bimon1_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon1_v4 ;
set grid_2003_bimon1_v3(rename=(date2=date));
run;



data grid_2003_bimon2_v3 ;
set grid_2003_bimon2_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon2_v4 ;
set grid_2003_bimon2_v3(rename=(date2=date));
run;


 data grid_2003_bimon3_v3 ;
set grid_2003_bimon3_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon3_v4 ;
set grid_2003_bimon3_v3(rename=(date2=date));
run;




data grid_2003_bimon4_v3 ;
set grid_2003_bimon4_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon4_v4 ;
set grid_2003_bimon4_v3(rename=(date2=date));
run;




data grid_2003_bimon5_v3 ;
set grid_2003_bimon5_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon5_v4 ;
set grid_2003_bimon5_v3(rename=(date2=date));
run;


data grid_2003_bimon6_v3 ;
set grid_2003_bimon6_v2 ;
date2 =input(date,mmddyy10.);
format date2 mmddyy10.;
drop date;
run;


data grid_2003_bimon6_v4 ;
set grid_2003_bimon6_v3(rename=(date2=date));
run;






/*merge back bimons*/

data T2003allbimon;
set grid_2003_bimon1_v4 grid_2003_bimon2_v4 grid_2003_bimon3_v4 grid_2003_bimon4_v4 grid_2003_bimon5_v4 grid_2003_bimon6_v4;
run;


/*delete areas not in NE proper (clipped)*/
data T2003allbimon_v2;
set T2003allbimon;
if mixpred = . then delete;
run;



data  Mod2_2003allv4 ;
set T2003allbimon_v2;
date=newjul;
format DATE  mmddyy8.;
run;





libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset\' ;

data poll.predtemp_t2003;
set Mod2_2003allv4;
run;
