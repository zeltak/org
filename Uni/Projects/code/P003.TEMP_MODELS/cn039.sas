/*import the combined weather dataset*/

PROC IMPORT OUT= WORK.Airtemp 
            DATAFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\met_2_sav\met.sav" 
            DBMS=SPSS REPLACE;

RUN;




/*import ALL grid with predictions dbf*/

PROC IMPORT OUT= WORK.sid_guid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\key_tables\Sid_guid.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



data sid_guid_v2;
set sid_guid;
guid2=input(guid, 16.); 
drop guid;
run;

data sid_guid_v3;
set sid_guid_v2;
guid=guid2;
drop guid2;
run;



/*Add guid to met data*/

proc sort data= sid_guid;
by sid;
run;


proc sort data= airtemp;
by sid;
run;



data airtemp_v2 ;
merge airtemp sid_guid(keep=guid sid);
by sid;
run;


/*#get temp pred*/

libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod3_pred_dataset\' ;

data predtemp_t2003;
set poll.predtemp_t2003;
run;


/*import ALL grid with predictions dbf*/




/*merge temp pred with met data*/



proc sort data= predtemp_t2003;
by guid date;
run;


proc sort data= airtemp_v2;
by guid date;
run;



data cor_test_2003 ;
merge predtemp_t2003 airtemp_v2 ;
by guid date;
if tmin=. then delete;
run;

data cor_test_2003_v2 ;
set cor_test_2003 ;
if tmp_mod3=. then delete;
run;

proc corr data=cor_test_2003_v2;
var tmin tmp_mod3;
run;









