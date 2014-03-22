/*import local pm variables*/


PROC IMPORT OUT= g3_lpm
  DATAFILE= "h:\$Final\localpm\g3.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

/*import cases and ranid and poll*/

libname tmp "Z:\";


data geocodes_offspring;
set	tmp.geocodes_offspring;
run;

data geocodes_gen3;
set	tmp.geocodes_gen3;
run;

libname poll "H:\";


data poll;
set	poll.poll_0008;
run;

/*import guid-ranid file*/


PROC IMPORT OUT= WORK.g3_guid
            DATAFILE= "h:\$Final\g3_ranid_guid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = geocodes_gen3; by ranid   ;run;
proc sort data = g3_guid ; by ranid  ;run;

data g3_guid_v2;
merge geocodes_gen3(in=a) g3_guid (in=b keep=ranid guid) ;
  by ranid ;
    if a;
	run; 

data g3_guid_v3;
set g3_guid_v2;
if guid=. then delete;
run; 


proc sort data = g3_guid_v3; by ranid    ;run;
proc sort data = g3_lpm ; by ranid  ;run;

data g3_guid_v4;
merge g3_guid_v3(in=a) g3_lpm (in=b keep=ranid localpm)  ;
  by ranid ;
    if a;
	run; 
