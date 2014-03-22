

/*NCDC DATA*/

PROC IMPORT OUT= WORK.NCDC_xy 
            DATAFILE= "C:\Users\ekloog\Documents\Postdoc\~work\C.Data_ga
thering\1.Data sources\e.Climate data\weatherstation_NE_location.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


libname temp 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.1.Raw_data\Temp\original Svanklot data\$FINAL\' ;



 
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
merge temp.ncdc  Ncdc_xy (keep= stn  LAT_DD LONG_DD) ;
by stn;
run;

data ncdc_v2;
set  ncdc(rename=( LAT_DD=LATITUDE  LONG_DD=LONGITUDE));;
if temp=. then delete;
run;


data  Fncdc (where=(date>='01JAN2003'D and date<='31DEC2003'D )) ;
set  ncdc_v2;
run;


proc summary nway data= Fncdc;
class stn;
var LATITUDE LONGITUDE;
output out=aggncdc mean=y x;
run;


PROC EXPORT DATA= WORK.AGGNCDC 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\ncdc-wb cor\agg_ncdc.dbf" 
            DBMS=DBF REPLACE;
RUN;




/*WB AND WU DATA*/


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



/*Create 1 dataset*/

data MET;
set  Fwunderdata fwbdata;
run;



proc summary nway data= MET;
class site;
var LATITUDE LONGITUDE;
output out=aggncdc2 mean=y x;
run;


PROC EXPORT DATA= WORK.AGGNCDC2 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\ncdc-wb cor\agg_ncdc2.dbf" 
            DBMS=DBF REPLACE;
RUN;


/*WAIT FOR GIS JOINS AND REIMPORT KEY TABLE*/

PROC IMPORT OUT= WORK.wbwukey
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\ncdc-wb cor\agg_wbwu3.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


proc sort data = wbwukey; by site ;run;
proc sort data = MET; by site ;run;

data met2;
 merge met (in=a) wbwukey (keep= site bufid);
   by site ;
   if bufid=0 then delete;
   run; 




   PROC IMPORT OUT= WORK.ncdckey
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\2.Gather_data\ncdc-wb cor\agg_ncdc4.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;


proc sort data = ncdckey; by stn ;run;
proc sort data = Fncdc; by stn ;run;

data fncdc2;
 merge Fncdc (in=a) ncdckey (keep= stn bufid);
   by stn ;
   if bufid=0 then delete;
   run; 



   proc sort data = met2; by bufid date ;run;
proc sort data = Fncdc2; by bufid date ;run;

data final;
 merge  met2 (in=a) Fncdc2 (keep= bufid date temp);
   by bufid date ;
        run; 


data final2;
set final;
if temp=. then delete;
if temp_mean=. then delete;
run;

proc corr data=final2;
var temp temp_mean;
run;


PROC EXPORT DATA= final2 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\ncdc_wb_corr\cordata.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*RESULTS IN CORR OF r=0.98*/
