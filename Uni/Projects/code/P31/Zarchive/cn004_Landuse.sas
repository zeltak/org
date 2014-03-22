libname lur 'f:\Uni\Projects\p031_MIAC_PM\0.raw\gis\' ;

/*for dist to poin source*/
libname lur2 'f:\Uni\Projects\p031_MIAC_PM\0.raw\gis\NEI\' ;


 PROC IMPORT OUT= pden
   DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\0.raw\gis\midatl_ne_aod_00cbgpopdens.csv" 
     DBMS=CSV REPLACE;
	   GETNAMES=YES;
	     DATAROW=2; 
		 RUN;

PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\ALL_IDS_MIA_NE_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
		  
proc sort data = lur.Midatlnewengvar_cntynei; by guid   ;run;
proc sort data = pmguid ; by guid   ;run;
proc sort data = pden ; by guid   ;run;

data lur3;
merge  pmguid (in=a) lur.Midatlnewengvar_cntynei(in=b) pden (in=c)  ;
  by guid  ;
    if a;
	run; 

/*the few missing are in the buffer areas around study aer and will be deleted*/

proc means data=lur3 n min max mean std nmiss;
var; 
run; 

data lur3;
set lur3;
if elev_m=. then delete;
run; 

PROC EXPORT DATA= lur3 
            OUTFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN004_LU_full_dataset\full_LU.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
