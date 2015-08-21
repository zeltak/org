


/*proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;*/


/* start individual cases*/


libname cc 'Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN008_cases\' ;

data cases ;
set xo.ap_counts;
drop pmnew--temp_fmayear;
run; 



libname xo 'Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\3.Analysis\AN001_CasesXover\' ;
libname full 'Y:\Projects\P042_Medicare_DVT\3.1.10.1.Raw_data\PM\' ;




data  poll_v3;
set full.fullpm;
run; 


/*options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;*/



/*<<<<<<<<<<<<<<START OF MACROS>>>>>>>>>>>>>>>*/


/*1)first macro to create controls*/


%macro makecontrol (daynum=);
  data control; set cases;
    length case time 3;
    case=0; time=2;
    if day ne &daynum; /* take all days in the stratum except the index day */
    matchday=&daynum;  /* keep track of the index day that created this data set */
    matchdow=weekday(date); /* in case you want to stratify on day of week too */
	
/*	uncomment for evey 3 days of controls and comment for every week*/
	*extract every third day for controls;
	test=day-&daynum;
	test2=mod(test,3);
	if test2=0;
	drop test test2;run;

  proc append data=control base=hazcontr; run;
%mend;* makecontrol;


/*2)second macro to create exposure data and case data*/


%macro createLumleyHazard (poll,cases);


/** create  exposure data **/
/* All exposure data (date, temp, PM, rhum, etc) should go into this data set */
   data exposures; set Poll_v3;
        day=day(date);
        month=month(date);
        year=year(date);
		date2=mdy(month,day,year);
		dow=weekday(date);
		format date date7.;
		keep date date2 guid pmnew--pmnewmaweek temp_f--temp_fmaweek dow   ;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race QID guid Avg_p_A65-- fcol_bin_m ;
		run;


proc sort data=cases; by QID  date ;run;
proc sort data=exposures; by  date;run;

   /* For both cases and exposures stuff */

   %let startDate='01MAR2000'D;
   %let dateInterval='MONTH';

   %let numDays=31;

   
   data exposures; set exposures;
     if date >= &startDate;
     stratum = intck(&dateInterval,&startDate, date); /* Number each stratum */
     day = datdif(intnx(&dateInterval,&startDate, stratum),date,'act/act')+1; /* Number each day within each stratum */
     matchday = day;
     dow = weekday(date);
	 run;

   
   data cases; set cases;
     if date >= &startDate;
     stratum = intck(&dateInterval,&startDate, date); /* Number each stratum */
     day = datdif(intnx(&dateInterval,&startDate, stratum),date,'act/act')+1; /* Number each day within each stratum */
     matchday = day;
     matchdow = weekday(date);
	 format date date7.;
	 run;

   
   /* proc means data=hazard; */
   	 proc sort data=cases;
     by stratum matchday;run;
     proc sort data=exposures;
     by stratum matchday;run;

   %do i=1 %to &numDays;
     %makecontrol(daynum=&i);
   %end;
  

/*   append cases to controls*/
   /*******/
     data hazard;  set  cases ;
     length case time 3; /*keeps the data set smaller*/
     case=1; time=1;
     matchday=day;
	 run;
	data hazard2; set hazard hazcontr;
	run;

	data hazard2; set hazard2;
	month=month(date);
	year=year(date);
	date2=mdy(month,matchday,year);
	*if date2 ne .;
	format date2 date7.;
	run;

   	 proc sort data=exposures;
     by guid date2;run;
     proc sort data=hazard2;
     by guid date2;
	 run;

     data hazard3; merge hazard2 exposures(drop=date);
     by guid date2;
     *if a=1 AND b=1; 
	 if case ne .;
	 run;


data xo.try_all_ap; set hazard3;
*dow=weekday(date2);
/*white=0;black=0;othrace=0;*/
/*if race2=1 then white=1; *white;*/
/*if race2=2 then black=1; *black;*/
/*if race2=3 then othrace=1; *other;*/
/*agepm=agecat*&pm;*/
/*sexpm=sex*&pm;*/
/*whitepm=white*&pm;*/
/*blackpm=black*&pm;*/
/*othracepm=othrace*&pm;*/
if dow=1 then wd1=1; else wd1=0;
if dow=2 then wd2=1; else wd2=0;
if dow=3 then wd3=1; else wd3=0;
if dow=4 then wd4=1; else wd4=0;
if dow=5 then wd5=1; else wd5=0;
if dow=6 then wd6=1; else wd6=0;
/*comment this below for evey 3 days and uncomment for every 1 week*/
/*if matchdow=dow;*/
*if aptmpm ne . and aptmpm1 ne . and mi ne . and &pm ne .;
run;

%mend createLumleyHazard;

/*^^^^end of macro^^^^^*/



/*Launcher for macro*/
%createLumleyHazard (poll,cases);


/*START OF PHREG*/

/*    proc sort data=try; by QID  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/

options nonotes nosource nosource2 ;



/*VVVV*/

proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\filename.log"; run;

proc printto log="nul:"; run;


data try_all_sub35;
set xo.try_all;
if pmnewma1 > 35 then delete;
run; 


	proc phreg data=xo.try_all_AP  nosummary;
    model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6;
    strata  QID  date;
	ods output ParameterEstimates=Tpmnewma1;
		run;



		/*V*/
	proc phreg data=xo.try_all  nosummary;
    model time*case(0) = pmnewma3 temp_fma3 sex age wd1--wd6;
    strata  QID  date;
	ods output ParameterEstimates=pmnewma3;
		run;
 


/*V*/

proc phreg data=xo.try_all nosummary;
    model time*case(0) = pmnew_l0 temp_f  wd1--wd6 age sex race;
    strata  QID  date;
	ods output ParameterEstimates=pmnew_l0;
	run;



/*add urb/rural and LU*/


PROC IMPORT OUT= x1
            DATAFILE= "Y:\Projects\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN002_Cases_guid_MIA\cases_DVT_clipped_XY.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

 data x1(rename=(guid_=guid ));
set x1;
date=input(adate,date9.);
format date mmddyy10.;
drop adate;
run; 
proc summary nway data=x1;
class guid;
var mon20;
output out=OUTPUTFILE1 mean=mon20;
run; 


PROC IMPORT OUT= WORK.pmguidma
            DATAFILE= "P:\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_emission.dbf" 
			            DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN; 

PROC IMPORT OUT= WORK.pmguidne
            DATAFILE= "P:\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_final.dbf" 
			            DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN; 




proc sql;
  create table pmguidma11 as
  select guid,p_open
  from pmguidma
quit;

proc sql;
  create table pmguidne11 as
  select guid,p_open
  from pmguidne
quit;

data po;
set pmguidma11 pmguidne11;
run; 


proc sort data = xo.try_all; by guid   ;run;
proc sort data = OUTPUTFILE1 ; by  guid  ;run;
proc sort data = po ; by  guid  ;run;

data xo.try_all_lu;
merge xo.try_all(in=a) OUTPUTFILE1 (in=b) po (in=c)  ;
  by  guid ;
    if a;
	run; 

data xo.try_all_lu;
set xo.try_all_lu;
m=month(date);
season=1;
if m=1 or m=2 or m=3 or m=10 or m=11 or m=12 then season=0;
po_bin=1;
if p_open < 63 then po_bin=0;
if mon20=0 then fmon20=1;
if mon20=1 then fmon20=0;
if po_bin=0 then fpo_bin=1;
if po_bin=1 then fpo_bin=0;
if inc_bin_m=0 then finc_bin_m=1;
if inc_bin_m=1 then finc_bin_m=0;
if col_bin_m=0 then fcol_bin_m=1;
if col_bin_m=1 then fcol_bin_m=0;
if sex=1 then bsex=0;
if sex=2 then bsex=1;
if bsex=0 then fbsex=1;
if bsex=1 then fbsex=0;
if season=0 then fseason=1;
if season=1 then fseason=0;
brace=1;
if race ne 1 then brace=0;
if brace=0 then fbrace=1;
if brace=1 then fbrace=0;
run; 

libname xor 'Y:\Projects\P042_Medicare_DVT\3.1.10.5.Results\xover\' ;




proc printto log="nul:"; run;


/*interact with % open space*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*po_bin po_bin ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_Os;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*po_bin fpo_bin ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_Osf;
run;




/*interact with urb-rural*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*mon20 mon20 ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_urb;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*fmon20 fmon20 ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_urbf;
run;

/*interact with income*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*inc_bin_m inc_bin_m ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_inc;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*finc_bin_m finc_bin_m ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_incf;
run;


/*interact with col*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0)= pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*col_bin_m col_bin_m ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_col_bin_m;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 sex age wd1--wd6 pmnewma1*fcol_bin_m fcol_bin_m ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_col_bin_mf;
run;


/*interact with sex*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 bsex age wd1--wd6 pmnewma1*bsex bsex ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_sex;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 bsex age wd1--wd6 pmnewma1*fbsex fbsex ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_fsex;
run;

data  try_all_lu;
set  xo.try_all_lu;
   doy=put (date,julian5.);
   doy2=substr(doy,3,3);
   sinetime=sin(2*constant('pi')*doy2/365.25);
   costime=cos(2*constant('pi')*doy2/365.25);
run; 


/*interact with sex2 */
proc phreg data=try_all_lu  nosummary;
model time*case(0) = pmnewma1 brace temp_fma1 age  wd1--wd6 pmnewma1*bsex bsex Avg_per_mi  Avg_pctcol sinetime costime ;
strata  QID  date;
ods output ParameterEstimates=pmnewma1_sex;
run;



/*interact with sex2 */
proc phreg data=try_all_lu  nosummary;
model time*case(0) = pmnewma3 brace temp_fma1 age  wd1--wd6 pmnewma3*fbsex fbsex season ;
strata  QID  date;
ods output ParameterEstimates=pmnewma1_fsex;
run;




/*interact with race*/
/*1 is white*/
proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma1 temp_fma1 bsex age wd1--wd6 pmnewma1*brace brace ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_race;
run;


proc phreg data=xo.try_all_lu  nosummary;
model time*case(0) = pmnewma3 temp_fma1 bsex age wd1--wd6 pmnewma1*fbrace fbrace ;
strata  QID  date;
ods output ParameterEstimates=xor.pmnewma1_fbrace;
run;

options notes source source2 MLOGIC MPRINT MRECALL SYMBOLGEN errors=0;


proc sort data = xo.try_all_lu; by  QID   ;run; 


proc summary nway data=xo.try_all_lu;
class CLASSVR;
var VAR1 VAR2;
output out=OUTPUTFILE mean=NEWNAME1 NEWNAME2;
run; 
