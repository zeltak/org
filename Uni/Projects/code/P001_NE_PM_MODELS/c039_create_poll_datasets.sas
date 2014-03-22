libname fpoll 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;


PROC IMPORT OUT= WORK.pm2000
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2000.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

							 PROC IMPORT OUT= WORK.pm2001
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2001.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

							 PROC IMPORT OUT= WORK.pm2002
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2002.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

							 PROC IMPORT OUT= WORK.pm2003
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
							 PROC IMPORT OUT= WORK.pm2004
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
							 PROC IMPORT OUT= WORK.pm2005
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
							 PROC IMPORT OUT= WORK.pm2006
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
							 PROC IMPORT OUT= WORK.pm2007
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
							 PROC IMPORT OUT= WORK.pm2008
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

								 PROC IMPORT OUT= WORK.pm2009
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

								 PROC IMPORT OUT= WORK.pm2010
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

								 PROC IMPORT OUT= WORK.pm2011
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN007_mod3_Final_poll\poll_T2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


							 PROC IMPORT OUT= mod2pm2000
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2000_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
															    
							 PROC IMPORT OUT= mod2pm2001
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2001_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2002
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2002_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2003
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2003_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2004
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2004_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2005
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2005_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2006
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2006_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2007
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2007_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
							 PROC IMPORT OUT= mod2pm2008
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2008_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;

								 PROC IMPORT OUT= mod2pm2009
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2009_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
				
								PROC IMPORT OUT= mod2pm2010
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2010_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;
				
					PROC IMPORT OUT= mod2pm2011
							             DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2011_m2_pred_mpm.csv" 
										             DBMS=CSV REPLACE;
													      GETNAMES=YES;
														       DATAROW=2; 
															   RUN;


/*join gam pred to aod pred*/

options mprint;
%macro import(year=);


data pm&year;
set pm&year ;
newdate = input(date,date9.);
format newdate date9.;
drop date;
run;


data pm&year;
set pm&year(rename=(newdate=date ));;
run;


proc sort data= pm&year;
by date guid;
run;

proc sort data= Mod2pm&year;
by date guid;
run;


data Poll_&year._s1;
merge pm&year   Mod2pm&year (keep=guid date pred);
by  date guid ;
run;


data Poll_&year._s2;
set Poll_&year._s1;
pmnew = pred;
if pm_mod3=. then delete;
run;



data Poll_&year._s4;
set Poll_&year._s2;
if pmnew =  . then pmnew=pm_mod3;
run;


data Poll_&year._s5;
set Poll_&year._s4(keep=guid date pmnew pm_mod3 Long_AOD_x Lat_AOD_x );
if pmnew=. then delete;
run;


data fpoll.poll&year;
set Poll_&year._s5;
run; 


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
%import(year=2009);
%import(year=2010);
%import(year=2011);




 data fpoll.poll_0008;
 set fpoll.poll2000 fpoll.poll2001 fpoll.poll2002 fpoll.poll2003 fpoll.poll2004 fpoll.poll2005 fpoll.poll2006 fpoll.poll2007 fpoll.poll2008 fpoll.poll2009 fpoll.poll2010 fpoll.poll2011;
 run; 



proc summary nway data=fpoll.poll_0008;
 class guid;
  var long_aod_x lat_aod_x pmnew ;
   output out=poll_agg_0008 mean=long_aod_x lat_aod_x pmnew ;
run;

PROC EXPORT DATA= poll_agg_0008 
            OUTFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN050_concentration_map_pmnew\conmap0011.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
