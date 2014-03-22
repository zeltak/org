/*create yearly concentration maps*/


libname fpoll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;


options mprint;
%macro import(year=);

 data poll_&year;
 set fpoll.poll&year ;
 run; 


proc summary nway data=poll_&year;
 class guid;
  var long_aod_x lat_aod_x pmnew ;
   output out=poll_agg_&year mean=long_aod_x lat_aod_x pmnew ;
run;


PROC EXPORT DATA= poll_agg_&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN050_concentration_map_pmnew\conmap&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);


proc summary nway data=fpoll.poll_0008;
class guid;
  var long_aod_x lat_aod_x pmnew ;
   output out=poll_agg_0008 mean=long_aod_x lat_aod_x pmnew ;
run;

PROC EXPORT DATA= poll_agg_0008
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN050_concentration_map_pmnew\conmap0008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
