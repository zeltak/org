
data xx;
set tncdc;
newvar=put(date, date9.);
Gender = substr(newvar,1,5);
run;
libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



PROC IMPORT OUT= WORK.maguid
            DATAFILE= "f:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN007_keytables\MAgrid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN ;


data maguid (keep=   glong glat);
set maguid;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 



options mprint;
%macro import(year=);

data t&year;
set mods.Fintmpc_&year;
if glong < -73.52 then delete;
if glong > -69.90 then delete;
if glat < 41.21 then delete;
if glat > 42.91 then delete;
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

data t2000;
set t2000;
drop badgrids;
run; 

data tall;
set t2000 t2001 t2002 t2003 t2004 t2005 t2006 t2007 t2008;
run; 

data tall   (drop = xnym ynym xnymx ynymx);;
set tall;

xnym=put(glong,Best12.);
   ynym=put(glat, Best12.);
   xnymx = xnym*10000;
   ynymx = ynym*10000;
   guid = compress(xnymx||ynymx);
run; 



/*LAGS #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

 
%macro makelags(fname,pol);

proc sort data = &fname; by glong glat date;run; 
 data &pol; set  &fname;by glong glat;
  &pol._l0=&pol;
%local i;
 %do i=0 %to 365;
  &pol._l%eval(&i+1)=lag1(&pol._l&i);
   if first.guid then &pol._l%eval(&i+1)=.;
 %end;
 &pol.ma1 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.ma3week = mean(of &pol._l0 - &pol._l21);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.mabirth = mean(of &pol._l0 - &pol._l280);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(tall,fintemp);

libname exp 'f:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN002_BW_exposure\' ;

 

data exp.templag;
set fintemp;
keep date--fintemp_l3 fintempma1--fintempmayear;
run; 


/*for cw data*/

 
libname cw 's:\ENVEPI\Itai\' ;

data cw;
set cw.b_ap_met_20130429;
keep date TEMP_C;
where date>='01Jan2000'D and date<='31Dec2008'D ; 
run; 




%macro makelags(fname,pol);

proc sort data = &fname; by  date;run; 
 data &pol; set  &fname;by date;
  &pol._l0=&pol;
%local i;
 %do i=0 %to 365;
  &pol._l%eval(&i+1)=lag1(&pol._l&i);
   if first.guid then &pol._l%eval(&i+1)=.;
 %end;
 &pol.ma1 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.mabirth = mean(of &pol._l0 - &pol._l280);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(cw,TEMP_C);

data exp.cwtc;
set TEMP_C;
keep date tempc--TEMP_C_l3 TEMP_Cma1--TEMP_Cmayear ;
run;  



libname met 'f:\Uni\Projects\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data met;
set met.metc2000 met.metc2001 met.metc2002 met.metc2003 met.metc2004 met.metc2005 met.metc2006 met.metc2007 met.metc2008;
if stn="725090" or stn="725095" or stn="725070" or stn="725080";
keep stn date temp_F;
run; 
 
data met;
set met;
tncdc= (5/9)*(Temp_F-32);
run; 





%macro makelags(fname,pol);

proc sort data = &fname; by  date;run; 
 data &pol; set  &fname;by date;
  &pol._l0=&pol;
%local i;
 %do i=0 %to 365;
  &pol._l%eval(&i+1)=lag1(&pol._l&i);
   if first.guid then &pol._l%eval(&i+1)=.;
 %end;
 &pol.ma1 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.mabirth = mean(of &pol._l0 - &pol._l280);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(met,tncdc);


data exp.tncdc;
set tncdc;
keep date stn tncdc--tncdc_l3 tncdcma1--tncdcmayear ;
run;  


/*create deviation variable*/

data devt;
set exp.templag;
keep guid date fintemp;
run; 

data devt;
set devt;
 if date = "29Feb2000"d then delete ;
  	 if date = "29Feb2004"d then delete ;
		 if date = "29Feb2008"d then delete ;
run; 


data devt2;
set devt;
newvar=put(date, date9.);
udate = substr(newvar,1,5);
run; 

proc summary nway data=devt2;
class udate guid;
var fintemp ;
output out=devfin2 mean(fintemp)=devtemp ;
run; 

data exp.devfin2;
set devfin2;
drop _type_ _freq_;
run; 
