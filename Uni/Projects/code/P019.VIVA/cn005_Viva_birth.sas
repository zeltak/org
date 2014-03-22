/**********************************************************************
*   PRODUCT:   averagepredbc_3_29_12.sas 
*   CREATOR:   sjmelly@hsph.harvard.edu 
*   DATE:      3/29/2012
*   Last EDIT: 4/12/2012
*   DESC:      Creates average exposures to black carbon based on daily predictions provided by Alexandros Gryparis
*              Predictions at first address from LMP - 180 days.  Will need to assume subjects were at enrollment address at time of conception
*              Average BC predictions were not calculated for addresses on Cape Cod and in Western MA
***********************************************************************/

options source2 linesize=78 error=max center number pageno=1;
options nomlogic nomprint nosymbolgen;


libname viv 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.1.Raw_data\VIVA data\' ;


data Vivadates_age3;
set viv.Vivadates_age3;
run; 

 
libname viva "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\";




/**/
/*libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\' ;*/
/**/
/**/
/**/
/* data poll;*/
/*set poll.poll_lag_v5  ;*/
/* run; */



 proc sort data =  DATA4; by aid   ;run;
 proc sort data = Vivadates_age3 ; by aid ;run;

 data Vivadates_age3_v2;
 merge Vivadates_age3(in=a)  DATA4 (in=b)  ;
   by aid;
if b;
run; 



	 data Vivadates_age3_v4;
	 set Vivadates_age3_v2;
	 if aid=. then delete;
	 rename date=datepred;
	 run; 

data Vivadates_age3_v4;
set Vivadates_age3_v4;
where date>=pm25predstart_date and date <=pm25predstop;
run; 





proc sort data = Vivadates_age3_v4;
by aid datepred;
run;








proc sort data = Vivadates_age3_v4; by aid datepred  ;run; 

proc means noprint data=Vivadates_age3_v4 n min max mean std nmiss;
by aid datepred; 
var pmnew;
output out=meanfile N(pmnew)=pmnewcnt;
run; 




/*Calculate average exposure during pregancy*/
data viva.vivaprenatal;
set Vivadates_age3_v4;
where datepred >= lmp_revi and datepred < deliv_da;
run;


proc sort data = viva.vivaprenatal;
by aid datepred ;
run;

proc means noprint data = viva.vivaprenatal;
by aid;
var pmnew ga;
output out = viva.vivaprenatalavg mean(pmnew) = prenatalpm min(ga) = ga  ;
run;

data viva.vivaprenatalavg (drop = _type_ );
set viva.vivaprenatalavg;
if _freq_ < 0.75 * ga then do;
prenatalpm = .;
end;
run;



/*Calculate average exposure over last month prior to birth*/

data viva.vivalstmth;
set Vivadates_age3_v4;
where datepred >= dob_minu and datepred < deliv_da;
run;

proc sort data = viva.vivalstmth;
by aid datepred;
run;

proc means noprint data = viva.vivalstmth;
by aid;
var pmnew;
output out = viva.vivalstmthavg mean(pmnew) = lstmthpm ;
run;

data viva.vivalstmthavg (drop = _type_ lstmthcntbcflag);
set viva.vivalstmthavg;
if _freq_ < 0.75 * 30 then do;
end;
run;



/*Calculate average exposure over year 1*/

data viva.vivayr1;
set Vivadates_age3_v4;
where datepred >= deliv_da and datepred < yr1;
run;

proc sort data = viva.vivayr1;
by aid datepred;
run;

proc means noprint data = viva.vivayr1;
by aid;
var pmnew;
output out = viva.vivayr1avg mean(pmnew) = yr1pm  ;
run;

data viva.vivayr1avg (drop = _type_ yr1cntbcflag);
set viva.vivayr1avg;

if _freq_ < 0.75 * 365 then do;

end;
run;

/*Calculate average exposure over year 2*/


data viva.vivayr2;
set Vivadates_age3_v4;
where datepred >= yr1 and datepred < yr2;
run;

proc sort data = viva.vivayr2;
by aid datepred;
run;

proc means noprint data = viva.vivayr2;
by aid;
var pmnew;
output out = viva.vivayr2avg mean(pmnew) = yr2pm ;
run;

data viva.vivayr2avg (drop = _type_ yr2cntbcflag);
set viva.vivayr2avg;
if _freq_ < 0.75 * 365 then do;
end;
run;




/*Calculate average exposure over year 3*/

data viva.vivayr3;
set Vivadates_age3_v4;
where datepred >= yr2 and datepred < yr3;
run;

proc sort data = viva.vivayr3;
by aid datepred;
run;

proc means noprint data = viva.vivayr3;
by aid;
var pmnew;
output out = viva.vivayr3avg mean(pmnew) = yr3pm;
run;

data viva.vivayr3avg (drop = _type_ yr3cntbcflag);
set viva.vivayr3avg;
if _freq_ < 0.75 * 365 then do;
end;
run;


/*Calculate average exposure over first trimester*/
data viva.vivatri1;
set Vivadates_age3_v4;
where datepred >= lmp_revi and datepred < lmp_plus;
run;

proc sort data = viva.vivatri1;
by aid datepred;
run;

proc means noprint data = viva.vivatri1;
by aid;
var pmnew;
output out = viva.vivatri1avg mean(pmnew) = tri1pm;
run;


/*if more than 25% of hourly data is missing for an exposure period set average to null*/

data viva.vivatri1avg (drop = _type_ tri1cntbcflag);
set viva.vivatri1avg;
if _freq_ < 0.75 * 93 then do;
end;
run;



/*Calculate average exposure over second trimester*/
data viva.vivatri2;
set Vivadates_age3_v4;
where datepred >= lmp_plus and datepred < lmp_plu0;
tri2lth = lmp_plu0 - lmp_plus;
run;

proc sort data = viva.vivatri2;
by aid datepred;
run;

proc means noprint data = viva.vivatri2;
by aid;
var pmnew tri2lth;
output out = viva.vivatri2avg mean(pmnew) = tri2pm min(tri2lth)=tri2lth;
run;

data viva.vivatri2avg (drop = _type_ tri2cntbcflag);
set viva.vivatri2avg;
if _freq_ < 0.75 * tri2lth then do;
end;
run;




/*Calculate average exposure over third trimester
A few subjects were born before the third trimester*/

data viva.vivatri3;
set Vivadates_age3_v4;
where datepred >= lmp_plu0 and datepred < deliv_da;
tri3lth = deliv_da - lmp_plu0;
run;

proc sort data = viva.vivatri3;
by aid datepred;
run;


proc means noprint data = viva.vivatri3;
by aid;
var pmnew tri3lth;
output out = viva.vivatri3avg mean(pmnew) = tri3pm min(tri3lth)=tri3lth;
run;

data viva.vivatri3avg (drop = _type_ tri3cntbcflag);
set viva.vivatri3avg;
if _freq_ < 0.75 * tri3lth then do;
end;
run;



/*Calculate average first 6 months exposure */
data viva.viva6mth;
set Vivadates_age3_v4;
where datepred >= deliv_da and datepred < dob_plus;
run;

proc sort data = viva.viva6mth;
by aid datepred;
run;

proc means noprint data = viva.viva6mth;
by aid;
var pmnew;
output out = viva.viva6mthavg mean(pmnew) = mth6pm;
run;

data viva.viva6mthavg (drop = _type_);
set viva.viva6mthavg;
if _freq_ < 0.75 * 183 then do;
end;
run;






data viva.vivabcestimates_4_13_12 (drop = _freq_ cntbcflag tri3cnt mth6cntbcflag);
merge viva.vivaprenatalavg viva.vivatri1avg viva.vivatri2avg viva.vivatri3avg viva.vivalstmthavg viva.viva6mthavg viva.vivayr1avg viva.vivayr2avg viva.vivayr3avg;
by aid;
run;

