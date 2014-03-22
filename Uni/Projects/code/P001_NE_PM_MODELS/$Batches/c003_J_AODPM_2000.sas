
/*** import AOD and PM datafile ***/

libname AOD "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset";
libname PM  "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset";




data allpm;
set pm.all_pm;
run; 

PROC IMPORT OUT= WORK.aod
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\Archive\test_spatialjoin\aodPM.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = allpm; by date sitecode   ;run;
proc sort data = aod ; by date sitecode ;run;

data DATA3(keep = guid Sitecode Date Lat_Pm Long_PM Lat_aod Long_aod Dist AOD pm25);
 merge allpm(in=a) aod (in=b);  
  by date sitecode;
run; 

data DATA4;
 set DATA3;
  if AOD  = . then delete;
  if PM25 = . then delete;
run; 

proc sort data = DATA4; 
