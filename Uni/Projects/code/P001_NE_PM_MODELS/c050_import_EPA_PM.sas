/* STARSreadPM25.sas
*
* Macro to read EPA PM25 data from raw data files
*/

libname temp "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN050_EPA_PM\";

filename PM252009 "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN050_EPA_PM\RD_501_88101_2009-0.txt" ;
filename PM252010 "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN050_EPA_PM\RD_501_88101_2010-0.txt" ;
filename PM252011 "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN050_EPA_PM\RD_501_88101_2011-0.txt" ;
                                

*BEGIN MACRO SBYEAR TO READ IN PM2.5 SPECIATION DATA BY YEAR** ;
%MACRO SBYEAR ;
%DO year=2009 %TO 2011 ; 
data S&year._1 ;
infile PM25&year dlm="|" lrecl=1000 TRUNCOVER DSD ;

*# RD|Action Code|State Code|County Code|Site ID|Parameter|POC|Sample Duration|Unit|
Method|Date|Start Time|Sample Value|Null Data Code|Sampling Frequency|Monitor Protocol (MP) ID|
Qualifier - 1|Qualifier - 2|Qualifier - 3|Qualifier - 4|Qualifier - 5|Qualifier - 6|
Qualifier - 7|Qualifier - 8|Qualifier - 9|Qualifier - 10|Alternate Method Detectable Limit|
Uncertainty ;
informat TransactionType $2.  ActionCode $1. StateCode $2. CountyCode $3. SiteID $4. 
ParameterCode $5. POC $2. SampleDuration $2. UnitCode $3. MethodCode $3. TextDate $8.
TextStartTime $5. SampleValue best12. NullDataCode $4. SamplingFrequency $3.
MonitorProtocolID $8.  Qualifier_1 Qualifier_2 Qualifier_3 Qualifier_4 Qualifier_5
Qualifier_6 Qualifier_7 Qualifier_8 Qualifier_9 Qualifier_10 $4. AlternateMethodLOD 
Uncertainty best12. ; 
input transactiontype @ ;
if TransactionType="RD" then input ActionCode StateCode CountyCode SiteID 
ParameterCode POC SampleDuration UnitCode MethodCode TextDate TextStartTime 
SampleValue NullDataCode SamplingFrequency MonitorProtocolID Qualifier_1 Qualifier_2
Qualifier_3 Qualifier_4 Qualifier_5 Qualifier_6 Qualifier_7 Qualifier_8 Qualifier_9
Qualifier_10 AlternateMethodLOD Uncertainty ; 
else delete ;
run ;
%END ;%MEND ;
%SBYEAR ;







*COMBINE YEARLY DATA INTO ONE DATASET** ;
data dailyPM25 ;
set  S2009_1 S2010_1 S2011_1 ; 
TextYear = substr(TextDate,1,4) ;
Year = input(TextYear,4.) ;
Textmonth = substr(TextDate,5,6) ;
month = input(Textmonth,4.) ;
Textday = substr(TextDate,7,8) ;
day = input(Textday,4.) ;
run ;

data dailyPM25; set dailyPM25;
tmm=substr(Textmonth,1,2) ;
month = input(tmm,4.) ;
date=mdy(month,day,year);
format date mmddyy8.;
drop month;
run;


data dailyPM25;set  dailyPM25;
if qualifier_1 in ('1','2','3','4','5','6','7','8','9','10') OR qualifier_1 in 
('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','MD','ND','SQ')
then delete;
	MonitorID = StateCode || "-" || CountyCode || "-" || SiteID || "-" || POC;
drop TextYear Textday Textmonth year day tmm ;
run;

proc sort data=dailyPM25;
by StateCode CountyCode MonitorID;
run;

data temp.dailyPM25;set dailyPM25;
drop qualifier_1 qualifier_2 qualifier_3 qualifier_4 qualifier_5 
qualifier_6 qualifier_7 qualifier_8 qualifier_9 qualifier_10;
run;

proc sort data=temp.dailyPM25;
by StateCode CountyCode MonitorID;
run;



data Dailypm25;
set Dailypm25;
sitecode=compress(StateCode||CountyCode||SiteID);
run; 


/*extract only NE*/


data Dailypm25_NE;
set Dailypm25;
where StateCode ="09" or StateCode="23" or StateCode="25" or StateCode="33" or StateCode="44" or StateCode="50";
run; 




PROC IMPORT OUT= WORK.sitexy
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pm_sites.dbf" 
			            DBMS=DBF   REPLACE;
						     GEDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;




PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_sitecode.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

option mprint;
%macro Year(year=);

data y&year;
 set Dailypm25_NE;
  Y = Year(date);
   if Y = &year;
run;

proc sort data = sitexy; by sitecode   ;run;
proc sort data = Y&year ; by sitecode ;run;

data Y&year.xy;
merge Y&year  (in=a) sitexy(in=b keep=sitecode long_pm lat_pm)  ;
  by sitecode;
    if a;
	run; 


data Y&year.xy(keep=sitecode long_pm lat_pm pm25 date);
set Y&year.xy;
pm25=SampleValue;
run; 

data Y&year.xy;
set Y&year.xy;
if pm25=. then delete;
if pm25 < 0.00001 then delete;
if long_pm=. then delete;
run; 

data pm.pm&year;
set Y&year.xy;
run; 

/*we need to aggreagte the sites to have only 24h means*/
/**** From 2009 - 2011 PM25 is collected Hourly!!! - WE take the mean! *****/
proc means data = pm.pm&year mean maxdec = 4;
 class Sitecode Date;
  var PM25 Long_PM Lat_PM;
   ods output Summary = pm.Summary(keep = Sitecode Date pm25_Mean Lat_PM_Mean Long_PM_Mean);
run;

data pm.pm&year;
 set pm.Summary;
  keep Sitecode Date long_pm lat_pm pm25;
   long_pm = Long_PM_Mean;
   lat_pm  = Lat_PM_Mean;
   pm25    = pm25_Mean;
run;



proc sort data = pmguid; by sitecode   ;run;
proc sort data = pm.pm&year ; by sitecode ;run;

data pm.pm&year;
merge pm.pm&year(in=a) pmguid (in=b keep=sitecode guid)  ;
  by sitecode;
    if a;
	run; 




PROC EXPORT DATA= pm.pm&year
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN008_mod3_corr\pmguidt&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


%mend year;

%year(year=2009);
%year(year=2010);
%year(year=2011);

