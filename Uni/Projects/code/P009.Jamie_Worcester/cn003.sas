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



/*create yearly datasets*/

data poll2000;
set poll;
if c ne 2000 then delete;
run;

data poll2001;
set poll;
if c ne 2001 then delete;
run;

data poll2002;
set poll;
if c ne 2002 then delete;
run;

data poll2003;
set poll;
if c ne 2003 then delete;
run;

data poll2004;
set poll;
if c ne 2004 then delete;
run;

data poll2005;
set poll;
if c ne 2005 then delete;
run;


/*calculate means per year*/

proc summary data=poll2000;
class guid;
var pm0;
output out=pm2000 mean=pm2000;
run;

proc summary data=poll2001;
class guid;
var pm0;
output out=pm2001 mean=pm2001;
run;

proc summary data=poll2002;
class guid;
var pm0;
output out=pm2002 mean=pm2002;
run;

proc summary data=poll2003;
class guid;
var pm0;
output out=pm2003 mean=pm2003;
run;

proc summary data=poll2004;
class guid;
var pm0;
output out=pm2004 mean=pm2004;
run;

proc summary data=poll2005;
class guid;
var pm0;
output out=pm2005 mean=pm2005;
run;


/*merge all yearly pm files*/

proc sort data=pm2000 ;
by guid;
run;

proc sort data=pm2001;
by guid;
run;

proc sort data=pm2002;
by guid;
run;

proc sort data=pm2003;
by guid;
run;

proc sort data=pm2004;
by guid;
run;

proc sort data=pm2005;
by guid;
run;


data pm_yearly;
merge pm2000 pm2001 pm2002 pm2003 pm2004 pm2005;
by guid ;
if guid =. then delete;
run;





proc sort data=cases;
by guid;
run;

proc sort data=pm_yearly;
by guid ;
run;


data casespm;
merge cases  pm_yearly;
by guid ;
run;


data casespm_v2;
set casespm;
if predicted=. then delete;
if guid=. then delete;
run;



libname cases "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.9_Jamie_Worcester\3.1.9.4.Work\3.Analysis\final cases\" ;



data cases.casesfinal_yearlypm;
   set casespm_v2(rename=(predicted=localPM ));
   run;



