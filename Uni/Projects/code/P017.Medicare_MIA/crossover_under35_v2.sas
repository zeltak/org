proc printto; run;

libname poll "f:\Uni\Projects\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;



data poll_v3;
set aod.poll_v3_under35;
run; 


 



data all2;
set aod.all4cases;
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

/*data cvd_U_35;*/
/*set aod.try_cvd_controls;*/
/*where pmnew < 35;*/
/*run; */

proc printto log="nul:"; run;

	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f pct65upest pctnonwht_wtd pctlowinc_wtd pctbachorhigher_wtd medhhin_wtd wd1--wd6;
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


	proc printto log="nul:"; run;

	proc phreg data=try nosummary;
    model time*case(0) = pmnewma1 temp_f pct65upest pctnonwht_wtd pctlowinc_wtd pctbachorhigher_wtd medhhin_wtd wd1--wd6;
    strata  hic  date;
	ods output ParameterEstimates=xo.resp_fixed;
	run;












