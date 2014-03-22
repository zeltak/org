
PROC IMPORT OUT= pmguid
            DATAFILE= "g:\ZH_tmp\P046_Israel_MAIAC\3.Work\2.Gather_data\FN007_Key_tables\aodstn_dist.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;	


PROC IMPORT OUT= aod
            DATAFILE= "Z:\Uni\Projects\P046_Israel_MAIAC\3.Work\2.Gather_data\FN003_AOd_allyears\allaod2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;	

PROC IMPORT OUT= pm2005
            DATAFILE= "Z:\Uni\Projects\P046_Israel_MAIAC\3.Work\2.Gather_data\FN003_AOd_allyears\pm2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;	



proc sql;
  create table mod1_2000_s1  as
   select *
    from pmguid left join aod
     on pmguid.aodid=aod.aodid  ;
run;


proc sort data = pm2005; by day stn   ;run;
proc sort data = mod1_2000_s1 ; by day stn  ;run;

data  mod1_2000_s2;
merge  mod1_2000_s1(in=a) pm2005 (in=b)  ;
  by day stn ;
    if b;
	run;


proc sort data=mod1_2000_s2; by stn day dist;
data mod1_2000_s2s; set mod1_2000_s2; by stn day dist;
if first.day;
run;

data  mod1_2000 ;
set mod1_2000_s2s;
if aod = . then delete;
run; 



PROC EXPORT DATA= mod1_2000 
            OUTFILE= "Z:\Uni\Projects\P046_Israel_MAIAC\3.Work\2.Gather_data\FN003_AOd_allyears\mod12005.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
