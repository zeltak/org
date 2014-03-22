
libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

PROC IMPORT OUT= WORK.pmguid
            DATAFILE= "f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN030_tempe_address_guid\nas_temp_guid.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data pmguid;
set pmguid;
xnym=put(xx,Best12.);
   ynym=put(yy, Best12.);
   xnymx = xnym*10000;
   ynymx = ynym*10000;
   guid = compress(xnymx||ynymx);
run; 


options mprint;
%macro import(year=);

data t&year;
set mods.Fintmpc_&year;
if glong < -73.8 then delete;
if glong > -69.03 then delete;
if glat > 45.1 then delete;
if glat < 41.00 then delete;
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

proc sort data = tall; by guid   ;run;
proc sort data = pmguid ; by guid  ;run;

data DATA3;
merge tall(in=a) pmguid (in=b)  ;
  by guid ;
    if b;
	run; 


/*LAGS #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

libname exp 'f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN031_exposure\' ;

 
 
%macro makelags(fname,pol);

proc sort data = &fname; by glong glat date;run; 
 data exp.fintemp; set  &fname;by glong glat;
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
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(data3,fintemp);



data exp.templag;
set exp.fintemp;
keep date--fintemp_l14 fintempma1--fintempmayear;
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


data  exp.templag;
set  exp.templag;
dow=weekday(date);
   doy=put (date,julian5.);
   doy2=substr(doy,3,3);
   sinetime=sin(2*constant('pi')*doy2/365.25);
   costime=cos(2*constant('pi')*doy2/365.25);
   newvar=put(date, date9.);
udate = substr(newvar,1,5);
run; 



proc sort data = exp.devfin2; by udate guid  ;run;
proc sort data =  exp.templag ; by udate guid ;run;

data  exp.tempNAS;
merge  exp.templag(in=a) exp.devfin2 (in=b)  ;
  by udate guid;
    if a;
	run; 

data exp.tempNAS;
set exp.tempNAS;
findevtemp=fintemp-devtemp;
if glong=. then delete;
run;
