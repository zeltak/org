
libname bwr 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.5.Results\RN_001_results_paper\' ;
libname bww 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN010_bwdatasets\' ;
libname x1 'z:\Projects\P011.BirthW_NE\3.1.11.4.Work\3.Analysis\4.sas analysis\' ;
libname x2 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\3.Analysis\3.SAS_analysis\results_w_PM' ;


data tst;
set bww.bw_all;
keep IQRfintemp--fintempmabirth IQRfintemp bdob byob;
run; 


PROC EXPORT DATA= WORK.RAN5_T2000 
            OUTFILE= "XX.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  





libname mods 'z:\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



PROC IMPORT OUT= WORK.maguid
            DATAFILE= "f:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN007_keytables\MAgrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN ;


data maguid (keep=   glong glat);
set maguid;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 



options mprint;
%macro import(year=);

data t&year;
set mods.Fintmpc_&year;
if glong < -73.52 then delete;
if glong > -69.90 then delete;
if glat < 41.21 then delete;
if glat > 42.91 then delete;
run; 
