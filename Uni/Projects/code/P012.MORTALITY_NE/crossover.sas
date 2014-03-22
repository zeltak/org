PROC IMPORT OUT= cases
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\3.Analysis\AN002_CaseXover_lpm\lpmccover.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 






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
   data exposures; set h.poll;
        day=day(date);
        month=month(date);
        year=year(date);
		date2=mdy(month,day,year);
		dow=weekday(date);
		format date date7.;
		keep date date2 guid pmnew tempc dow deltapm temp2d;
	run;


	/** create case data **/
	/* All case data (date, # of events, patient id, etc) should go into this data set */
data cases; set h.cases;
        day=day(date);
        month=month(date);
        year=year(date);
		date=mdy(month,day,year);
/*		if month in (5,6,7,8) and year=2000;*/
	keep date x y sex age puid guid education martial race;
run;


proc sort data=cases; by puid  date ;run;
proc sort data=exposures; by  date;run;

   /* For both cases and exposures stuff */

   %let startDate='01Jan2000'D;
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
/** create dummy for interacrions **/
/*
white=0;black=0;othrace=0;
if race2=1 then white=1; *white;
if race2=2 then black=1; *black;
if race2=3 then othrace=1; *other;
cold=0;hot=0;othseas=0;
if season=1 then cold=1; *cold;
if season=2 then hot=1; *hot;
if season=3 then othseas=1; *other;
*/
/** interactions **/
/*
agepm=agecat*&pm;
sexpm=sex*&pm;
whitepm=white*&pm;
blackpm=black*&pm;
othracepm=othrace*&pm;
hotpm=hot*&pm;
othseaspm=othseas*&pm;

typeadpm=typeadmi*&pm;
diabppm=diabp*&pm;
diabcpm=diabc*&pm;
copdppm=copdp*&pm;
copdcpm=copdc*&pm;
mippm=mip*&pm;
pneucpm=pneuc*&pm;
afibppm=afibp*&pm;
aricpm=aric*&pm;
*/
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



/*CHECKDATA*/


/*to check the data*/
/**/
/*proc sort data=try; by puid  date;run;*/
/**/
/*proc sort data=hazard3; by puid  date;run;*/
/*proc sort data=hazcontr; by puid  date;run;*/
/**/
/*data try;set try;*/
/*format date2 date7.;*/
/*run;*/
/*data exposures;set exposures;*/
/*format date2 date7.;*/
/*run;*/
/**/
/*proc sort data=exposures; by guid  date;run;*/
/*proc sort data=h.poll; by  date;run;*/
/**/
/*data exp; set h.poll;*/
/*d1=lag1(date);*/
/*run;*/
/*data exp; set exp;*/
/*if d1=date then two=1; else two=0;*/
/*run;*/
/*proc freq data=exp;*/
/*tables two/out=fr2;*/
/*run;*/




/*START OF PHREG*/

    proc sort data=try; by puid  date;run;
/** one var only **/
	ods output ParameterEstimates=mypar2 (keep= parameter Estimate  StdErr);


data try2;
set try;
if deltapm=. then delete;
run;



	proc phreg data=try2 nosummary;
    model time*case(0) = deltapm temp2d ;
    strata  puid  date;
	run;




	data h.final_3day;
	set try2;
	run;
