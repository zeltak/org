/*import mortality cases*/


PROC IMPORT OUT= WORK.CASES 
            DATAFILE= "C:\Documents and Settings\EKLOOG\My Documents\Postdoc\~work\H.Hospital Admitance\1.Data\b.Assign guid\Admiss_guid_F.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

/*create cound data*/

/*This step addes a new variable where each case gets a "1" */

data cases;
set cases(rename=(dateadmi=date));
count=1;
run;

/*This step creates a dataeset with counts per day (date) for cases*/

proc summary nway data=cases;
class date guid;
var count ;
output out=cases_count sum=;
run;





libname poll "c:\Users\ekloog\Documents\Postdoc\~work\H.Hospital Admitance\1.Data\f.assign exposure\" ;

PROC IMPORT OUT= WORK.INC 
            DATAFILE= "c:\Documents and Settings\EKLOOG\My Documents\Postdoc\~work\H.Hospital Admitance\1.Data\e2.calculate SES data\SES_NE.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


data poll;
set poll.poll_lag;
run;


proc sort data=poll;
by guid;
run;

proc sort data=inc;
by guid;
run;


data poll_v2;
merge poll inc ;
by guid;
run;

data poll_v3;
set poll_v2(rename=(Avg_P05300=med_inc));;
if PMNEW=. then delete;
run;








proc sort data=poll_v3;
by guid date;
run;

proc sort data=cases_count;
by guid date;
run;


data times;
merge cases_count   poll_v3 ;
by guid date;
run;

data times2;
set times;
if count = . then delete;
run;

data times3;
set times2;
if pmnew2d = . then delete;
run;



data times4;
set times3;
format date JULIAN.;
run;



/*get meanpm for each guid for 7 year period*/


proc summary data=times4;
class guid;
var pmnew2d ;
output out=new mean=mpmguid;
run;



proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;


data times5;
merge times4   new ;
by guid ;
run;


data times6;
set times5;
if c=. then delete;
deltapm=pmnew2d-mpmguid;
run;



/*export to r*/
/**/

PROC EXPORT DATA= WORK.JUL2 
            OUTFILE= "c:\Documents and Settings\EKLOOG\My Documents\Postdoc\~work\H.Hospital Admitance\1.Data\h.export to r for ts\ts_00_06.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
