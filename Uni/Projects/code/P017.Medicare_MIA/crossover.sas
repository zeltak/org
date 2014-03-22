

libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


PROC IMPORT OUT= inc
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.1.Raw_data\SES\midatl_guid_cbg00wtdv2.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

data poll;
set poll.poll_lag_V5;
where date>='03Mar2000'D and date<='31Dec2006'D ; 
run;

data inc;
set inc(rename=( guid_=guid ));;
run; 

proc sort data=poll;
by guid;
run;

proc sort data=inc;
by guid;
run;

data poll_v3;
merge poll inc ;
by guid;
run;


PROC IMPORT OUT= WORK.urb
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\MIA_ur_guid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



data urb;
set urb;
guid2=input(guid, 8.); 
drop guid;
run;

data urb;
set urb;
guid=guid2; 
drop guid2;
run;


proc sort data = urb; by guid   ;run;
proc sort data = poll_v3 ; by guid ;run;

data poll_v3x;
merge poll_v3(in=a) urb (in=b keep=guid mon20 mon30)  ;
  by guid;
    if a;
	run; 


data poll_v3xy;
set poll_v3x;
if medhhin_wtd <  32281 then inc_bin_25 = 0;
else inc_bin_25=1;
if medhhin_wtd <  37137.5 then inc_bin_m = 0;
else inc_bin_m=1;
if medhhin_wtd < 45409 then inc_bin_75 = 0;
else inc_bin_75=1;
if pctbachorhigher_wtd < 9.7 then col_bin_25 = 0;
else col_bin_25=1;
if pctbachorhigher_wtd < 13.4 then col_bin_m = 0;
else col_bin_m=1;
if pctbachorhigher_wtd < 19.7 then col_bin_75 = 0;
else col_bin_75=1;
if pctlowinc_wtd < 16.7 then hs_bin_25 = 0;
else hs_bin_25=1;
if pctlowinc_wtd < 23.1 then hs_bin_m = 0;
else hs_bin_m=1;
if pctlowinc_wtd < 28.9 then hs_bin_75 = 0;
else hs_bin_75=1;
run;


/*within Xkm=1 (urban) far from mon=o (rural)*/
/*inc_bin=0 (low)high=1*/
/*collow=0 (low)high=1*/
/*hs_bin_m (high inc) income=0 low inc=1*/

data poll_v3(drop=pmnewmaweek--pmnewmayear temp_fmaweek--temp_fmayear);
set poll_v3xy;
if mon30=0 then fmon30=1;
if mon30=1 then fmon30=0;
if inc_bin_m=0 then finc_bin_m=1;
if inc_bin_m=1 then finc_bin_m=0;
if col_bin_m=0 then fcol_bin_m=1;
if col_bin_m=1 then fcol_bin_m=0;
if hs_bin_m=0 then fhs_bin_m=1;
if hs_bin_m=1 then fhs_bin_m=0;
run; 


libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN002_poll_set_final\' ;


data aod.poll_v3;
set poll_v3;
run; 

PROC IMPORT OUT= WORK.all
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid_MIA\ALL.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data all;
set all(rename=(dateadmi=date));
run;


data all2(where=(date>="01JAN2000"D and date<="31DEC2006"D )) ;;
set all;
format DATE  mmddyy8.;
run;

data aod.all4cases;
set all2;
run; 













libname xo 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.5.Results\xover\' ;


















 /*stroke*/
 /*stroke*/
 /*stroke*/
 /*stroke*/
 /*stroke*/
 /*stroke*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where stroke=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.stroke_fixed;
	run;








	













 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where cvd=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pmnew pmnewma3  pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


/*	proc phreg data=try nosummary;*/
/*    model time*case(0) = pmnewma1 temp_f  wd1--wd6;*/
/*    strata  hic  date;*/
/*	ods output ParameterEstimates=xo.cvd_fixed;*/
/*	run;*/
/**/
/**/
/**/



/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma3 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_fixed;
	run;



/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnew temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_fixed;
	run;















 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where resp=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pmnew pmnewma3 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


/*	proc phreg data=try nosummary;*/
/*    model time*case(0) = pmnewma1 temp_f  wd1--wd6;*/
/*    strata  hic  date;*/
/*	ods output ParameterEstimates=xo.resp_fixed;*/
/*	run;*/
/**/


proc phreg data=try nosummary;
    model time*case(0) = pmnewma3 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_fixed;
	run;


proc phreg data=try nosummary;
    model time*case(0) = pmnew temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_fixed;
	run;

	













 /*copd*/
 /*copd*/
 /*copd*/
 /*copd*/
 /*copd*/
 /*copd*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where copd=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.copd_fixed;
	run;



	













 /*ari*/
 /*ari*/
 /*ari*/
 /*ari*/
 /*ari*/
 /*ari*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where ari=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.ari_fixed;
	run;















 /*pneum*/
 /*pneum*/
 /*pneum*/
 /*pneum*/
 /*pneum*/
 /*pneum*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where pneum=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.pneum_fixed;
	run;



	













 /*mi*/
 /*mi*/
 /*mi*/
 /*mi*/
 /*mi*/
 /*mi*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where mi=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.mi_fixed;
	run;















 /*chf*/
 /*chf*/
 /*chf*/
 /*chf*/
 /*chf*/
 /*chf*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where chf=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.chf_fixed;
	run;















 /*diab*/
 /*diab*/
 /*diab*/
 /*diab*/
 /*diab*/
 /*diab*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where diab=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.diab_fixed;
	run;

















 /*ihd*/
 /*ihd*/
 /*ihd*/
 /*ihd*/
 /*ihd*/
 /*ihd*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where ihd=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.ihd_fixed;
	run;

















 /*strisc*/
 /*strisc*/
 /*strisc*/
 /*strisc*/
 /*strisc*/
 /*strisc*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where strisc=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.strisc_fixed;
	run;



	













 /*strhem*/
 /*strhem*/
 /*strhem*/
 /*strhem*/
 /*strhem*/
 /*strhem*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where strhem=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.strhem_fixed;
	run;








/*INTERACTIONS*/

	/*INTERACTIONS*/

			/*INTERACTIONS*/
	
					/*INTERACTIONS*/





/*INTERACTIONS*/

	/*INTERACTIONS*/

			/*INTERACTIONS*/
	
					/*INTERACTIONS*/



 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where cvd=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pmnewma3 temp_fma1 temp_fma3 mon30--fhs_bin_m;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
*dow=weekday(date2);
if race=1 then brace=1; *white;
if race ne 1 then brace=0; 
if sex=1 then bsex=0;
if sex=2 then bsex=1;
if bsex=0 then fbsex=1;
if bsex=1 then fbsex=0;
if brace=0 then fbrace=1;
if brace=1 then fbrace=0;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 mon30 pmnewma1*mon30 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_mon30;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fmon30 pmnewma1*fmon30 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_fmon30;
	run;





	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 inc_bin_m pmnewma1*inc_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_inc_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 finc_bin_m pmnewma1*finc_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_finc_bin_m;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 col_bin_m pmnewma1*col_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_col_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fcol_bin_m pmnewma1*fcol_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_fcol_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 hs_bin_m pmnewma1*hs_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_hs_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fhs_bin_m pmnewma1*fhs_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_fhs_bin_m;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 bsex pmnewma1*bsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_bsex;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fbsex pmnewma1*fbsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_fbsex;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 brace pmnewma1*brace temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_brace;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fbrace pmnewma1*fbrace temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_int_fbrace;
	run;











 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/
 /*resp*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where resp=1;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pmnewma3 temp_fma1 temp_fma3 mon30--fhs_bin_m;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid ;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
*dow=weekday(date2);
if race=1 then brace=1; *white;
if race ne 1 then brace=0; 
if sex=1 then bsex=0;
if sex=2 then bsex=1;
if bsex=0 then fbsex=1;
if bsex=1 then fbsex=0;
if brace=0 then fbrace=1;
if brace=1 then fbrace=0;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 mon30 pmnewma1*mon30 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_mon30;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fmon30 pmnewma1*fmon30 temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_fmon30;
	run;





	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 inc_bin_m pmnewma1*inc_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_inc_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 finc_bin_m pmnewma1*finc_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_finc_bin_m;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 col_bin_m pmnewma1*col_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_col_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fcol_bin_m pmnewma1*fcol_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_fcol_bin_m;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 hs_bin_m pmnewma1*hs_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_hs_bin_m;
	run;


proc means data=try n min max mean std nmiss;
var bsex; 
run; 

	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fhs_bin_m pmnewma1*fhs_bin_m temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_fhs_bin_m;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 bsex pmnewma1*bsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_bsex;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fbsex pmnewma1*fbsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_fbsex;
	run;



	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 brace pmnewma1*brace temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_brace;
	run;




	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 fbrace pmnewma1*fbrace temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_int_fbrace;
	run;
































/*new 10.12.2012*/



	


 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/
 /*cvd*/


proc datasets lib=work; delete all cases control Exposures hazard hazard2 hazard3 hazcontr mypar2 inc poll try ; run;


/* start individual cases*/


data cases;
set all2;
where cvd=1;
if sex=1 then bsex=0;
if sex=2 then bsex=1;
if bsex=0 then fbsex=1;
if bsex=1 then fbsex=0;
run; 

options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;



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
		keep date date2 guid pmnew temp_f dow  pmnewma1 pmnew pmnewma3  pop_65upest--medhhin_wtd;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
   		keep date x y sex age race hic guid bsex fbsex;
		run;


proc sort data=cases; by hic  date ;run;
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


data try; set hazard3;
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

/*    proc sort data=try; by hic  date;run;*/
/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


/*	proc phreg data=try nosummary;*/
/*    model time*case(0) = pmnewma1 temp_f  wd1--wd6;*/
/*    strata  hic  date;*/
/*	ods output ParameterEstimates=xo.cvd_fixed;*/
/*	run;*/
/**/
/**/
/**/



/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/

proc means data=try n min max mean std nmiss;
var ; 
run; 



options notes source source2 MLOGIC MPRINT MRECALL SYMBOLGEN errors=0;


	proc phreg data=try nosummary;
    model time*case(0) = pmnewma3 bsex pmnewma3*bsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_fixed;
	where se
	run;



/*	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);*/


	proc phreg data=try nosummary;
    model time*case(0) = pmnew fbsex pmnewma3*fbsex temp_f  wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.cvd_fixed;
	run;



