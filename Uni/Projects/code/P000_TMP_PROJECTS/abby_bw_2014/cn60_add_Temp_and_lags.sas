/*create yealy files of poll and temp*/

options mprint;
%macro import(year=);



libname metc 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data Met_Complete_&year;
set metc.metc&year;
run; 

 
libname poll 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;

data poll&year;
set poll.poll&year;
run; 

libname gstn 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guidstn;
set gstn.guid_stn_&year;
run; 


proc sort data = guidstn; by guid   ;run;
proc sort data = poll&year ; by guid ;run;

data poll&year.V2;
merge poll&year(in=a) guidstn (in=b keep=guid stn)  ;
  by guid;
    if a;
	run; 
 

	proc sort data = Met_Complete_&year; by stn date   ;run;
	proc sort data =  poll&year.V2 ; by stn date ;run;

	data poll&year.V3;
	merge poll&year.V2(in=a) Met_Complete_&year (in=b keep= stn date Temp_F ah_gm3_F)  ;
	  by stn date;
	    if a;
		run; 

PROC IMPORT OUT= grid&year(keep = guid date)
            DATAFILE= "f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_&year..csv" 
			            DBMS=CSV REPLACE;
						     GETNAMES=YES;
							      DATAROW=2; 
								  RUN;
								   
proc sort data = grid&year; by guid date;run;
proc sort data = poll&year.V3 ;   by guid date;run;

data poll&year.V3;
merge grid&year(in=a) poll&year.V3 (in=b)  ;
  by guid date;
  if a;
run;  

data poll&year.V3;
set poll&year.V3;
if pmnew=. then delete;
run; 




%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 
%import(year=2009); 
%import(year=2010); 
%import(year=2011); 




libname pt 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags' ;


data pt.polltemp;
set poll2000V3 poll2001V3 poll2002V3 poll2003V3 poll2004V3 poll2005V3 poll2006V3 poll2007V3 poll2008V3 poll2009V3 poll2010V3 poll2011V3;
run; 



/*LAGS #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

 
%macro makelags(fname,pol);

proc sort data = &fname; by guid date;run; 
 data &pol; set  &fname;by guid;
  &pol._l0=&pol;
%local i;
 %do i=0 %to 365;
  &pol._l%eval(&i+1)=lag1(&pol._l&i);
   if first.guid then &pol._l%eval(&i+1)=.;
 %end;
&pol.ma2 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.ma4 = mean(of &pol._l0 - &pol._l3);
 &pol.ma5 = mean(of &pol._l0 - &pol._l4);
 &pol.ma6 = mean(of &pol._l0 - &pol._l5);
 &pol.ma7 = mean(of &pol._l0 - &pol._l6);
 &pol.maweek = mean(of &pol._l0 - &pol._l6);
 &pol.ma10 = mean(of &pol._l0 - &pol._l9);
 &pol.ma2week = mean(of &pol._l0 - &pol._l13);
 &pol.ma3week = mean(of &pol._l0 - &pol._l20);
 &pol.ma4week = mean(of &pol._l0 - &pol._l27);
 &pol.ma2month = mean(of &pol._l0 - &pol._l59);
 &pol.ma3month = mean(of &pol._l0 - &pol._l89);
 &pol.ma6month = mean(of &pol._l0 - &pol._l182);
 &pol.ma1trim = mean(of &pol._l189 - &pol._l280);
 &pol.ma2trim = mean(of &pol._l105 - &pol._l189);
 &pol.ma3trim = mean(of &pol._l0 - &pol._l105);
 &pol.maLMP20w = mean(of &pol._l0 - &pol._l140);
 run;
%mend;

%makelags(pt.polltemp,pmnew);
%makelags(pt.polltemp,temp_f);
%makelags(pt.polltemp,ah_gm3_F);


/*0-12.99 weeks (1st trimester)*/
/*13 weeks-24.99 weeks (2nd trimester)*/
/*25 weeks-delivery (3rd trimester)*/
/*and LMP-first 20 weeks gestation */

libname lag 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\' ;






/**/
/*data lag.temp_f;*/
/*set temp_f;*/
/*run; */
/**/
/*data lag.ah_gm3_F;*/
/*set temp_f;*/
/*run; */
/**/


/*trim datasets*/

libname babby 'f:\Uni\Projects\3.1.0.TMP_PROJECTS\abby_BW\' ;



data babby.birth;
set lag.pmnew;
/* pm_ma1trim = mean(of pmnew_l189 - pmnew_l280);*/
/* pm_ma2trim = mean(of pmnew_l105 - pmnew_l189);*/
/* pm_ma3trim = mean(of pmnew_l0 - pmnew_l105);*/
 keep date guid long_AOD_X lat_AOD_x pmnew pmnew_l0 pmnew_l1--pmnew_l320 pmnewma1 
pmnewma3 pmnewmaweek pmnewma2week pmnewmamonth pmnewma3month  pmnewmabirth ;
run; 


 data babby.birth;
 set babby.birth;
  if lat_aod_x > 43 then delete;
 run; 

/*0-12.99 weeks (1st trimester)*/
/*13 weeks-24.99 weeks (2nd trimester)*/
/*25 weeks-delivery (3rd trimester)*/
/*and LMP-first 20 weeks gestation */




proc sort data = pmlag; by guid date   ;run;
proc sort data = templag ; by guid date  ;run;
proc sort data = ah_gm3_Flag ; by guid date  ;run;

data poll_lag;
merge pmlag(in=a) templag(in=b drop=long_AOD_X lat_AOD_x ) ah_gm3_Flag(in=c drop=long_AOD_X lat_AOD_x) ;
  by guid date ;
    if a;
	run; 


