libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;


data poll;
set aod.poll_0008;
run; 

proc summary nway data=poll;
 class guid;
  var pm_mod3 long_aod_x lat_aod_x pmnew ;
   output out=poll_agg mean=pm_mod3 long_aod_x lat_aod_x pmnew;
run;


PROC EXPORT DATA= poll_agg
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN050_concentration_map_pmnew\conmap0008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
