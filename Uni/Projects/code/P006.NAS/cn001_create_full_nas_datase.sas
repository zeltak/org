libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\' ;



 data poll;
set poll.poll_lag_v5a  ;
 run; 



PROC IMPORT OUT= WORK.nasguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN004_NAS_lu\nas_lu.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




data nasguid2;
set nasguid;
drop OBJECTID Join_Count target_fid AddStartDa AddEndDate seasonstar seasonendm visitfromt imtvisit addid_1 ;
run; 


data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
03mar2000 1
31dec2011 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;



data aa;
        set nasguid2;
do count = 1 to 4323;
        output;
end;
run; 


data d3;
        set daily;
count + 1;
by daily_value;
run; 

proc sort data = d3; by count  ;run;
proc sort data = aa ; by count ;run;

data DATA3;
merge aa(in=a) d3 (in=b)  ;
  by count;
    if a;
	run; 


proc sort data = poll; by guid date   ;run;
proc sort data = DATA3 ; by guid date ;run;

data DATA4;
merge DATA3(in=a) poll (in=b)  ;
  by guid date;
    if a;
	run; 


data data5;
set data4;
if pmnew=. then delete;
run; 


data data6;
set data5;
drop  count daily_value x y popden10--tden  ;
run; 

/*for local pm stage*/


proc summary nway data=data6;
class AddID;
var addlat addlong;
output out=localpm mean=addlat addlong;
run; 

libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\AN003_FULL_NAS_EXPOSURE\' ;
 

data aod.NAS_EXPO_amar;
set data6;
run;


data aod.address_localpm;
set localpm;
run;
