
/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon1_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon2_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon3_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon4_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon5_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\grid_2003_bimon6_s1.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon1_s1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon2_s1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon3_s1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon4_s1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon5_s1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV_CV\t2003_bimon6_s1.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s1;
set T2003allbimon_v3;
run;








/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s2.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s2.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s2;
set T2003allbimon_v3;
run;











/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s3.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s3.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s3;
set T2003allbimon_v3;
run;












/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s4.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s4.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s4;
set T2003allbimon_v3;
run;










/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s5.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s5.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s5.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s5;
set T2003allbimon_v3;
run;










/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s6.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s6.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s6;
set T2003allbimon_v3;
run;










/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s7.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s7.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s7.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s7.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s7.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s7.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s7.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s7;
set T2003allbimon_v3;
run;











/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s8.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s8.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s8.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s8.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s8.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s8.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s8.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s8;
set T2003allbimon_v3;
run;










/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s9.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s9.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s9.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s9.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s9.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s9.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s9.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s9;
set T2003allbimon_v3;
run;











/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon1_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon2 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon2_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon3_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon4_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon5_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;







/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.grid_2003_bimon6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\grid_2003_bimon6_s10.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;










/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON1 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon1_s10.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON2
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon2_s10.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON3 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon3_s10.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON4 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon4_s10.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON5 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon5_s10.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;






/*import bimon csv*/

PROC IMPORT OUT= WORK.T2003_BIMON6 
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_CV\t2003_bimon6_s10.csv" 
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


data T2003allbimon_v3;
set T2003allbimon_v2;
tmp_mod3 = mixpred+gpred;
run;

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset_CV\' ;

data poll.predtemp_t2003_s10;
set T2003allbimon_v3;
run;
