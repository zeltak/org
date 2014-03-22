libname aod 's:\ENVEPI\Airs\BC Model\nasforschwartzbcmodel\' ;

data nasexpdays_ap_met;
set aod.nasexpdays_ap_met;
run; 

/*create unique grid*/
proc sort data = nasexpdays_ap_met nodupkey out=DATAOUTPUTNAME;
by  xinkm yinkm; 
run;

/*the data x and y was in km so its needed to multiply by 1000*/
data DATAOUTPUTNAME;
set DATAOUTPUTNAME;
xx=xinkm*1000;
yy=yinkm*1000;
run; 

data  DATAOUTPUTNAME;
set  DATAOUTPUTNAME;
keep xx yy AddIDNew;
run; 

PROC EXPORT DATA= DATAOUTPUTNAME
            OUTFILE= "f:\Uni\Projects\P030_BC_model\3.Work\2.Gather_data\$GIS_repo\files\nas.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

 PROC IMPORT OUT= sumreg
    DATAFILE= "s:\ENVEPI\Airs\BC Model\summerreg.csv" 
	  DBMS=CSV REPLACE;
	    GETNAMES=YES;
		  DATAROW=2; 
		  RUN;

proc sort data = sumreg nodupkey out=sumregNAME;
by  xinkm yinkm; 
run;


data sumregNAME;
set sumregNAME;
xx=xinkm*1000;
yy=yinkm*1000;
run; 

data  sumregNAME;
set  sumregNAME;
keep xx yy sm_id;
run; 


PROC EXPORT DATA= sumregNAME
            OUTFILE= "f:\Uni\Projects\P030_BC_model\3.Work\2.Gather_data\$GIS_repo\files\sumreg.dbf" 
			            DBMS=DBF REPLACE;
						RUN;		   

