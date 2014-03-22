
libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset\' ;


/*#full year*/

data temp_t2003;
set poll.predtemp_t2003;
run;


proc summary nway data=temp_t2003;
class guid;
var tmp_mod3 x y;
output out=tmp_year mean=mtmp_mod3 x y;
run;



PROC EXPORT DATA= WORK.TMP_YEAR 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\tmp_year_guid\tmp2003_guid.dbf" 
            DBMS=DBF REPLACE;
RUN;


/*summer */


data temp_t2003_sum;
set temp_t2003;
if m=1 then delete;
if m=2 then delete;
if m=3 then delete;
if m=10 then delete;
if m=11 then delete;
if m=12 then delete;
run;


proc summary nway data=temp_t2003_sum;
class guid;
var tmp_mod3 x y;
output out=tmp_sum mean=mtmp_mod3 x y;
run;



PROC EXPORT DATA= WORK.TMP_sum 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\tmp_year_guid\tmp2003_guid_sum.dbf" 
            DBMS=DBF REPLACE;
RUN;


/*Winter */

data temp_t2003_win;
set temp_t2003;
if m=4 then delete;
if m=5 then delete;
if m=6 then delete;
if m=7 then delete;
if m=8 then delete;
if m=9 then delete;
run;


proc summary nway data=temp_t2003_win;
class guid;
var tmp_mod3 x y;
output out=tmp_win mean=mtmp_mod3 x y;
run;



PROC EXPORT DATA= WORK.TMP_win
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\tmp_year_guid\tmp2003_guid_win.dbf" 
            DBMS=DBF REPLACE;
RUN;
