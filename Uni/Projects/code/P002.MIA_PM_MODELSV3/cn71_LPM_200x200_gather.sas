PROC IMPORT OUT= WORK.tden
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\arcgis\tdenx\tden.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


 
data tden2(drop=near_fid RASTERVALU);
set tden;
tden=RASTERVALU;
if dist_A1=-1 then dist_A1=40000;
run;


data tden3 (drop=dist_A1 near_dist);
set  tden2;
dist_A1_v2=dist_A1/1000;
dist_pemis=near_dist/1000; 
run; 

data tden4 (drop=dist_A1_v2);
set  tden3;
dist_A1=dist_A1_v2;
run; 


libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.1.Raw_data\GIS\200x200\' ;

data mdatgrid200;
set aod.mdatgrid200;
drop elev;
run; 

/*proc means data=mdatgrid200 n min max mean std nmiss;*/
/*run; */
/**/
/*proc univariate data=mdatgrid200;*/
/*var urban;*/
/*run;*/
 


proc sort data = mdatgrid200 ; by OBJECTID   ;run;
proc sort data = tden4 ; by OBJECTID ;run;

data DATA3;
merge mdatgrid200(in=a) tden4 (in=b)  ;
  by OBJECTID;
    if a;
	run; 




/*import cliped data for the MIA*/

PROC IMPORT OUT= clip
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.1.Raw_data\GIS\200x200\midatl_xy_elev_6_13.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;



PROC IMPORT OUT= midatl_xy
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.1.Raw_data\GIS\200x200\midatl_xy.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
 
data midatl_xy(drop=OID_);
set midatl_xy;
OBJECTID=gridid;
run; 


proc sort data = DATA3; by OBJECTID ;run;

proc sort data = clip ; by OBJECTID ;run;

proc sort data = midatl_xy ; by OBJECTID ;run;

data DATA6;
merge DATA3(in=a)  clip (in=b) midatl_xy (in=c keep=OBJECTID midat) ;
  by OBJECTID;
    if a;
	run; 


data DATA7;
set DATA6;
if midat=0 then delete;
if midat=. then delete;
if popdens=. then popdens=0;
if elev=. then elev=0;
run; 



PROC EXPORT DATA= DATA7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN070_LPM_stage_200x200_base\lu200.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
proc univariate data=DATA7;
var popdens urban elev tden dist_A1 dist_pemis ;
histogram  popdens urban elev tden dist_A1 dist_pemis;
run;
 
