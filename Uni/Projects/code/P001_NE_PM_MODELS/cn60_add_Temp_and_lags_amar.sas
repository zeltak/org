/*create yealy files of poll and temp*/

options mprint;
%macro import(year=);





libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data Met_Complete_&year;
set metc.metc&year;
run; 

 
libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN039_Final_poll_datasets\' ;

data poll&year;
set poll.poll&year;
run; 

libname gstn 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

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
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_&year..csv" 
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




libname pt 'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags' ;


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
 &pol.ma1 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
  &pol.ma4 = mean(of &pol._l0 - &pol._l3);
   &pol.ma5 = mean(of &pol._l0 - &pol._l4);
    &pol.ma6 = mean(of &pol._l0 - &pol._l5);
	 &pol.ma7 = mean(of &pol._l0 - &pol._l6);
	  &pol.ma8 = mean(of &pol._l0 - &pol._l7);
	   &pol.ma9 = mean(of &pol._l0 - &pol._l8);
	    &pol.ma10 = mean(of &pol._l0 - &pol._l9);
		 &pol.ma11 = mean(of &pol._l0 - &pol._l10);
		 &pol.ma12 = mean(of &pol._l0 - &pol._l11);
	     &pol.ma13 = mean(of &pol._l0 - &pol._l13);
		&pol.ma14 = mean(of &pol._l0 - &pol._l13);
		 &pol.ma15 = mean(of &pol._l0 - &pol._l14);
		 &pol.ma16 = mean(of &pol._l0 - &pol._l15);
		 &pol.ma17 = mean(of &pol._l0 - &pol._l16);
		 &pol.ma18 = mean(of &pol._l0 - &pol._l17);
		 &pol.ma19 = mean(of &pol._l0 - &pol._l18);
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.ma3week = mean(of &pol._l0 - &pol._l21);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.mabirth = mean(of &pol._l0 - &pol._l280);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(pt.polltemp,pmnew);
%makelags(pt.polltemp,temp_f);
/*%makelags(pt.polltemp,ah_gm3_F);*/


libname lag 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\' ;

/*data lag.pmnew;*/
/*set pmnew;*/
/*run; */
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

data pmlag;
set pmnew;
keep date guid long_AOD_X lat_AOD_x pmnew pmnew_l0 pmnew_l1 pmnew_l2 pmnew_l3  pmnewma1 pmnewma3--pmnewma19 pmnewmaweek pmnewma2week pmnewmamonth pmnewma3month  pmnewmabirth pmnewmayear;
run; 


data templag;
set temp_f;
keep date guid long_AOD_X lat_AOD_x temp_f temp_f_l0 temp_f_l1 temp_f_l2 temp_f_l3 temp_fma1 temp_fma3--temp_fma19 temp_fmaweek temp_fma2week temp_fmamonth temp_fma3month  temp_fmabirth temp_fmayear;
run; 

/*data ah_gm3_Flag;*/
/*set ah_gm3_F;*/
/*keep date guid long_AOD_X lat_AOD_x ah_gm3_F ah_gm3_F_l0 ah_gm3_F_l1 ah_gm3_F_l2 ah_gm3_F_l3 ah_gm3_Fma1 ah_gm3_Fma3 ah_gm3_Fmaweek ah_gm3_Fma2week ah_gm3_Fmamonth ah_gm3_Fma3month  ah_gm3_Fmabirth ah_gm3_Fmayear;*/
/*run; */
/**/




proc sort data = pmlag; by guid date   ;run;
proc sort data = templag ; by guid date  ;run;
/*proc sort data = ah_gm3_Flag ; by guid date  ;run;*/

data poll_lag;
merge pmlag(in=a) templag(in=b drop=long_AOD_X lat_AOD_x )  ;
  by guid date ;
    if a;
	run; 

/*export final trimmed lag file */

	data lag.poll_lag_v5a;
	set poll_lag;
	run; 
