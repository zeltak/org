libname temp 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.1.Raw_data\Temp\original Svanklot data\$FINAL\' ;



/*EPA DATA*/

proc sort data= temp.Epasites;
by siteid;
run;


proc sort data= temp.Epa_met;
by siteid;
run;


data epadata;
merge temp.Epa_met (keep=siteid Date temp_Mean temp_Min wsp_Mean epa site)  temp.Epasites (keep=siteid LATITUDE LONGITUDE) ;
by siteid;
run;


data  Fepadata (where=(date>='01JAN2003'D and date<='31DEC2003'D ));
set  epadata;
run;


/*WBug DATA*/

data wbdata;
set temp.Weatherbug2003;
keep site Date temp_mean temp_min temp_Max wb LATITUDE LONGITUDE ;
run;

data  Fwbdata (where=(date>='01JAN2003'D and date<='31DEC2003'D ));
set   wbdata;
run;


/*Wunder DATA*/

proc sort data= temp.Wunderground;
by site;
run;


proc sort data= temp.Wundergroundst;
by site;
run;


data wunderdata;
merge temp.Wunderground (keep=site Date temp_Mean temp_Min temp_Max wsp_mean wu)  temp.Wundergroundst (keep=site LATITUDE LONGITUDE) ;
by site;
run;

data wunderdata_v2;
set  wunderdata;
if temp_min=. then delete;
run;


data  Fwunderdata (where=(date>='01JAN2003'D and date<='31DEC2003'D ));
set   wunderdata_v2;
run;





/*NCDC DATA*/

PROC IMPORT OUT= WORK.NCDC_xy 
            DATAFILE= "C:\Users\ekloog\Documents\Postdoc\~work\C.Data_ga
thering\1.Data sources\e.Climate data\weatherstation_NE_location.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


proc sort data= temp.ncdc;
by stn;
run;




data Ncdc_xy;
set Ncdc_xy(rename=(usaf=stn));
run;

proc sort data= Ncdc_xy;
by stn;
run;


data ncdc;
merge temp.ncdc  Ncdc_xy (keep=stn LAT_DD LONG_DD) ;
by stn;
run;

data ncdc_v2;
set  ncdc(rename=( LAT_DD=LATITUDE  LONG_DD=LONGITUDE));;
if temp=. then delete;
run;


data  Fncdc (where=(date>='01JAN2003'D and date<='31DEC2003'D ))
;set  ncdc_v2;
run;




/*exports*/


PROC EXPORT DATA= WORK.FEPADATA 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\Met\single files after sas\fepa.sav" 
            DBMS=SPSS REPLACE;
RUN;


PROC EXPORT DATA= WORK.Fncdc
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\Met\single files after sas\Fncdc.sav" 
            DBMS=SPSS REPLACE;
RUN;


PROC EXPORT DATA= WORK.Fwbdata
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\Met\single files after sas\Fwbdata.sav" 
            DBMS=SPSS REPLACE;
RUN;

PROC EXPORT DATA= WORK.Fwunderdata
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\Met\single files after sas\Fwunderdata.sav" 
            DBMS=SPSS REPLACE;
RUN;



libname merg 'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\Met\single files after sas\' ;





/*Create 1 dataset*/

data MET;
set merg.Fepa merg.fncdc merg.fwbdata merg.fwunderdata;
run;


data MET2;
set MET;
if tmin=. then delete;
run;



PROC EXPORT DATA= WORK.MET2 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\met_2_sav\met.sav" 
            DBMS=SPSS REPLACE;
RUN;
