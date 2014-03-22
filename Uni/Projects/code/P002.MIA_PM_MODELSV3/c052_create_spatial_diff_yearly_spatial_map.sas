PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_sitecode.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDLETED=NO;
							 RUN; 


libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;


 data pm;
 set pm.all_pm;
 run; 



 proc sort data = pm; by sitecode   ;run;
 proc sort data = pmguid; by sitecode ;run;

 data DATA3;
 merge pm (in=a) pmguid (in=b keep=sitecode reg_id)  ;
   by sitecode;
     if a;
	 run; 

proc summary nway data=data3;
class reg_id;
var pm25;
output out=OUTPUTFILE mean=pm25;
run; 









libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;


data poll;
set aod.poll_0008;
run; 

proc summary nway data=poll;
 class guid;
  var pm_mod3 long_aod_x lat_aod_x pmnew ;
   output out=poll_agg mean=pm_mod3 long_aod_x lat_aod_x pmnew;
run;


PROC IMPORT OUT= WORK.regguid
            DATFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 





 proc sort data = poll_agg; by guid   ;run;
 proc sort data = regguid; by guid ;run;

 data poll_agg_id;
 merge poll_agg (in=a) regguid (in=b keep=guid reg_id)  ;
   by guid;
     if a;
	 run; 


/*add regional pm*/

proc sort data = OUTPUTFILE ; by reg_id   ;run;
proc sort data = poll_agg_id ;by reg_id ;run;

data DATA5;
merge poll_agg_id(in=a) OUTPUTFILE  (in=b keep=reg_id pm25)  ;
  by reg_id;
    if a;
	run; 


data data6;
set data5;
spat_diff=pmnew-pm25;
run; 




PROC EXPORT DATA= data6
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN050_concentration_map_pmnew\conmap0008_diff_from_mean.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
