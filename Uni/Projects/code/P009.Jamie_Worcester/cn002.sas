/*import  cases*/



PROC IMPORT OUT= WORK.CASES 
            DATAFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9_Jamie_Worcester\3.1.9.4.Work\2.Gather_data\lu_guis_resid_final\lu_resid.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;





/*pollution part*/


libname poll "c:\Users\ekloog\Documents\Postdoc\~work\F.Mortality study analysis\1.time sereis analysis\m.all cases\" ;

PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Documents and Settings\EKLOOG\My Documents\Postdoc\~work\E.Prepare for Mortality study\4.add pop ses\e.full MA\S3_edu_ses_inc_cleaned.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data poll;
set poll.poll_lag_means(rename=(pmnew=pm0 tempc=temp0 pm_lag1=pm1 pm_lag2=pm2 pm_lag3=pm3 pm_lag4=pm4 pm_lag5=pm5 tempc_lag1=temp1 tempc_lag2=temp2 tempc_lag3=temp3 tempc_lag4=temp4 tempc_lag5=temp5 ));;
run;

data poll2000;
set poll;
if c >2000 then delete;
run;





proc summary data=poll2000;
class guid;
var pm0;
output out=new mean=pm2000;
run;





proc sort data=cases;
by guid;
run;

proc sort data=new;
by guid ;
run;


data casespm;
merge cases  new;
by guid ;
run;


data casespm_v2;
set casespm;
if predicted=. then delete;
if guid=. then delete;
run;


libname cases "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9_Jamie_Worcester\3.1.9.4.Work\3.Analysis\final cases\" ;


data cases.casesfinal;
   set casespm_v2(rename=(predicted=localPM ));
   run;
