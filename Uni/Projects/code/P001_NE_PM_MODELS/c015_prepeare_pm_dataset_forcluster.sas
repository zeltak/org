libname PM  "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset";



data PM_All(keep = PM25 Long_PM Lat_PM SiteCode EPAcode Date); 
 set PM.PM2000 PM.PM2001 PM.PM2002 PM.PM2003 PM.PM2004 PM.PM2005 PM.PM2006 PM.PM2007 PM.PM2008;
  if pm25 <= 0 then delete;
run;

proc summary nway data=PM_All;
class sitecode date;
var PM25 Long_PM Lat_PM;
output out = PM_All(drop = _type_ _freq_) mean=PM25 Long_PM Lat_PM;
run;  



/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2000 1
31dec2008 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

proc sort data =  PM_All; by date; run;



proc freq data = PM_All; 
  table sitecode;
  ods output OneWayFreqs = OneWayFreqs;
run;

data OneWayFreqs(keep = sitecode);
 set OneWayFreqs; run;

/*** Create the list of Date ***/

data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set Daily;
     if _n_ = 1 then do;
        elenco = trim(left(date));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(date));
      elenco_new = elenco;
       call symputx("List",elenco_new);
      output;
     end;
run;

/*** Create combination of all dates for each PM station ***/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = Date Sitecode);
  if _N_ = 1 then set Daily&date;
 set Onewayfreqs;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

%mend full;

%full(List = &List);


proc sort data = PM_All;   by date sitecode; run;
proc sort data = Final;    by date sitecode; run;

data PM_All;
 merge PM_All (in=a) Final (in=b);   
  by date sitecode;
   if b;
run;

/**** Create Unique Sitecode XY ****/

data PM_XY(keep = Long_PM Lat_PM SiteCode); 
 set PM.PM2000 PM.PM2001 PM.PM2002 PM.PM2003 PM.PM2004 PM.PM2005 PM.PM2006 PM.PM2007 PM.PM2008;
  if pm25 < 0 then delete;
run;

proc summary nway data=PM_XY;
 class sitecode;
  var Long_PM Lat_PM;
   output out=PM_XY_out(drop=_type_ _freq_) mean=Long_PM Lat_PM;
run; 


proc sort data =  PM_XY_out; by Sitecode; run;

/*** We want to find the closest Weather station to each Weather station ***/

data Sitecode_Fake(keep = Sitecode_fake Lat_fake Long_fake);
 set PM_XY_out; 
   Sitecode_fake = Sitecode||"_Fake"; 
    Lat_fake  = Lat_PM;
    Long_fake = Long_PM;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new Sitecode);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set PM_XY_out;
     if _n_ = 1 then do;
        elenco = trim(left(Sitecode));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(Sitecode));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2000&ID; set PM_XY_out;
  where Sitecode = "&ID";
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2000&ID;
   set Sitecode_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_PM, Long_PM), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_PM data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2000&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;


/*** Create a full set for each Station ***/

proc sort data = Res_PM; by Sitecode; run; 

data Res_PM;
  set Res_PM;
  count + 1;
  by Sitecode;
  if first.Sitecode then count = 1;
run;

/**** Take only 7 closest Sitecode station to each Sitecode station ****/

data Res_PM_close1(keep = Sitecode Sitecode_fake1);
 set Res_PM;
  if count = 1;
   Sitecode_fake1 = Sitecode_fake;
run;

data Res_PM_close2(keep = Sitecode Sitecode_fake2);
 set Res_PM;
  if count = 2;
   Sitecode_fake2 = Sitecode_fake;
run;

data Res_PM_close3(keep = Sitecode Sitecode_fake3);
 set Res_PM;
  if count = 3;
   Sitecode_fake3 = Sitecode_fake;
run;

data Res_PM_close4(keep = Sitecode Sitecode_fake4);
 set Res_PM;
  if count = 4;
    Sitecode_fake4 = Sitecode_fake;
run;

data Res_PM_close5(keep = Sitecode Sitecode_fake5);
 set Res_PM;
  if count = 5;
      Sitecode_fake5 = Sitecode_fake;
run;

data Res_PM_close6(keep = Sitecode Sitecode_fake6);
 set Res_PM;
  if count = 6;
     Sitecode_fake6 = Sitecode_fake;
run;

data Res_PM_close7(keep = Sitecode Sitecode_fake7);
 set Res_PM;
  if count = 7;
     Sitecode_fake7 = Sitecode_fake;
run;


data Res_PM_close8(keep = Sitecode Sitecode_fake8);
 set Res_PM;
  if count = 8;
     Sitecode_fake8 = Sitecode_fake;
run;


data Res_PM_close9(keep = Sitecode Sitecode_fake9);
 set Res_PM;
  if count = 9;
     Sitecode_fake9 = Sitecode_fake;
run;


data Res_PM_close10(keep = Sitecode Sitecode_fake10);
 set Res_PM;
  if count = 10;
     Sitecode_fake10 = Sitecode_fake;
run;


data Res_PM_close11(keep = Sitecode Sitecode_fake11);
 set Res_PM;
  if count = 11;
     Sitecode_fake11 = Sitecode_fake;
run;


data Res_PM_close12(keep = Sitecode Sitecode_fake12);
 set Res_PM;
  if count = 12;
     Sitecode_fake12 = Sitecode_fake;
run;


data Res_PM_close13(keep = Sitecode Sitecode_fake13);
 set Res_PM;
  if count = 13;
     Sitecode_fake13 = Sitecode_fake;
run;

data Unique;
 merge Res_PM_close1 Res_PM_close2 Res_PM_close3 Res_PM_close4 Res_PM_close5 Res_PM_close6 Res_PM_close7
       Res_PM_close8 Res_PM_close9 Res_PM_close10 Res_PM_close11 Res_PM_close12 Res_PM_close13;
  by Sitecode;
run;




proc sort data = PM_All; by Sitecode; run;
proc sort data = Unique;  by Sitecode; run;

data PM_All_v1;
 merge PM_All Unique;   
 by Sitecode; 
run;

data PM_All_v1(drop = Sitecode_fake1--Sitecode_fake13); 
 set PM_All_v1;
    id_M1 = compress(Sitecode_fake1,'_Fake');
    id_M2 = compress(Sitecode_fake2,'_Fake');
    id_M3 = compress(Sitecode_fake3,'_Fake');
    id_M4 = compress(Sitecode_fake4,'_Fake');
    id_M5 = compress(Sitecode_fake5,'_Fake');
    id_M6 = compress(Sitecode_fake6,'_Fake');
    id_M7 = compress(Sitecode_fake7,'_Fake');
	id_M8 = compress(Sitecode_fake8,'_Fake');
	id_M9 = compress(Sitecode_fake9,'_Fake');
	id_M10 = compress(Sitecode_fake10,'_Fake');
	id_M11 = compress(Sitecode_fake11,'_Fake');
	id_M12 = compress(Sitecode_fake12,'_Fake');
	id_M13 = compress(Sitecode_fake13,'_Fake');
run;

/*** Create the dataset with the closest station by distance ***/

/*#1*/

data PM_Comp1(keep = id_M1 date PM25_1); 
 set PM_All_v1;
  id_M1 = Sitecode;
   PM25_1 = PM25;
run;

proc sort data = PM_All_v1;   by id_M1 date; run;
proc sort data = PM_Comp1;  by id_M1 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp1(in=b);
   by id_M1 date; 
    if a;
run;


/*#2*/

data PM_Comp2(keep = id_M2 date PM25_2); 
 set PM_All_v1;
  id_M2 = Sitecode;
   PM25_2 = PM25;
run;

proc sort data = PM_All_v1;   by id_M2 date; run;
proc sort data = PM_Comp2;  by id_M2 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp2(in=b);
   by id_M2 date; 
    if a;
run;

/*#3*/

data PM_Comp3(keep = id_M3 date PM25_3); 
 set PM_All_v1;
  id_M3 = Sitecode;
   PM25_3 = PM25;
run;

proc sort data = PM_All_v1;   by id_M3 date; run;
proc sort data = PM_Comp3;  by id_M3 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp3(in=b);
   by id_M3 date; 
    if a;
run;



data PM_Comp4(keep = id_M4 date PM25_4); 
 set PM_All_v1;
  id_M4 = Sitecode;
   PM25_4 = PM25;
run;

proc sort data = PM_All_v1;   by id_M4 date; run;
proc sort data = PM_Comp4;  by id_M4 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp4(in=b);
   by id_M4 date; 
    if a;
run;


data PM_Comp5(keep = id_M5 date PM25_5); 
 set PM_All_v1;
  id_M5 = Sitecode;
   PM25_5 = PM25;
run;

proc sort data = PM_All_v1;   by id_M5 date; run;
proc sort data = PM_Comp5;  by id_M5 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp5(in=b);
   by id_M5 date; 
    if a;
run;


data PM_Comp6(keep = id_M6 date PM25_6); 
 set PM_All_v1;
  id_M6 = Sitecode;
   PM25_6 = PM25;
run;

proc sort data = PM_All_v1;   by id_M6 date; run;
proc sort data = PM_Comp6;  by id_M6 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp6(in=b);
   by id_M6 date; 
    if a;
run;



data PM_Comp7(keep = id_M7 date PM25_7); 
 set PM_All_v1;
  id_M7 = Sitecode;
   PM25_7 = PM25;
run;

proc sort data = PM_All_v1;   by id_M7 date; run;
proc sort data = PM_Comp7;  by id_M7 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp7(in=b);
   by id_M7 date; 
    if a;
run;



data PM_Comp8(keep = id_M8 date PM25_8); 
 set PM_All_v1;
  id_M8 = Sitecode;
   PM25_8 = PM25;
run;

proc sort data = PM_All_v1;   by id_M8 date; run;
proc sort data = PM_Comp8;  by id_M8 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp8(in=b);
   by id_M8 date; 
    if a;
run;



data PM_Comp9(keep = id_M9 date PM25_9); 
 set PM_All_v1;
  id_M9 = Sitecode;
   PM25_9 = PM25;
run;

proc sort data = PM_All_v1;   by id_M9 date; run;
proc sort data = PM_Comp9;  by id_M9 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp9(in=b);
   by id_M9 date; 
    if a;
run;



data PM_Comp10(keep = id_M10 date PM25_10); 
 set PM_All_v1;
  id_M10 = Sitecode;
   PM25_10 = PM25;
run;

proc sort data = PM_All_v1;   by id_M10 date; run;
proc sort data = PM_Comp10;  by id_M10 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp10(in=b);
   by id_M10 date; 
    if a;
run;



data PM_Comp11(keep = id_M11 date PM25_11); 
 set PM_All_v1;
  id_M11 = Sitecode;
   PM25_11 = PM25;
run;

proc sort data = PM_All_v1;   by id_M11 date; run;
proc sort data = PM_Comp11;  by id_M11 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp11(in=b);
   by id_M11 date; 
    if a;
run;



data PM_Comp12(keep = id_M12 date PM25_12); 
 set PM_All_v1;
  id_M12 = Sitecode;
   PM25_12 = PM25;
run;

proc sort data = PM_All_v1;   by id_M12 date; run;
proc sort data = PM_Comp12;  by id_M12 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp12(in=b);
   by id_M12 date; 
    if a;
run;




data PM_Comp13(keep = id_M13 date PM25_13); 
 set PM_All_v1;
  id_M13 = Sitecode;
   PM25_13 = PM25;
run;

proc sort data = PM_All_v1;   by id_M13 date; run;
proc sort data = PM_Comp13;  by id_M13 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp13(in=b);
   by id_M13 date; 
    if a;
run;






data PM_All_complete;
 set PM_All_v1;
  if PM25_1 ne .             Then PM25__F = PM25_1;
  if PM25_1 = . 				Then PM25__F = PM25_2;
  if PM25_1 = . & PM25_2 = . 	Then PM25__F = PM25_3;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . 	Then PM25__F = PM25_4;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = .	Then PM25__F = PM25_5;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = .	Then PM25__F = PM25_6;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = .	Then PM25__F = PM25_7;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = .	Then PM25__F = PM25_8;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = .	Then PM25__F = PM25_9;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = .	Then PM25__F = PM25_10;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = .	Then PM25__F = PM25_11;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = .	Then PM25__F = PM25_12;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	Then PM25__F = PM25_13;
   run;



/*** Check no missing: ok ***/

proc means data = PM_All_complete nmiss n;
 var PM25__F  ;
run;


proc means data=PM_All_complete n min max;
var PM25__F; 
run; 




libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;


data Meanall (keep= date mpm_F reg_id);
set mpm2.mpm2000 mpm2.mpm2001 mpm2.mpm2002 mpm2.mpm2003 mpm2.mpm2004 mpm2.mpm2005 mpm2.mpm2006 mpm2.mpm2007 mpm2.mpm2008;
run; 


proc summary nway data=Meanall;
class date;
var mpm_F;
output out=Meanall mean=mpm_F ;
run; 

proc sort data = PM_All_complete; by date ;run;
proc sort data = Meanall ; by date ;run;

data PM_All_complete_full;
merge PM_All_complete(in=a) Meanall (in=b)  ;
  by date;
  if a;
run;  


data PM_All_complete_full_v2;
set PM_All_complete_full;
if pm25__F =. then pm25__F= mpm_f;
cluspm=pm25__F; 
run; 

/*Check no missing values remain*/
proc means data = PM_All_complete_full_v2 nmiss n;
 var cluspm  ;
run;

/*transpose data*/

proc sort data = PM_All_complete_full_v2; by date; run;

proc transpose data = PM_All_complete_full_v2 out=pmtranspose prefix=st_;
 by date;
  id sitecode;
run;

data pmtranspose ;
 set pmtranspose;
  where _NAME_ = "PM25__F";
run;


data pmtranspose (drop=_NAME_);
set pmtranspose;
run; 

/*export all years*/

PROC EXPORT DATA= WORK.pmtranspose 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN030_cluster\pmcluster.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;



/*export every 3 years years*/

data pmtranspose0002(where=(date>='01JAN2000'D and date<='31DEC2002'D )) ;;
set pmtranspose;
run; 

PROC EXPORT DATA= WORK.pmtranspose0002 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN030_cluster\pmcluster0002.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


data pmtranspose0305(where=(date>='01JAN2003'D and date<='31DEC2005'D )) ;;
set pmtranspose;
run; 

PROC EXPORT DATA= WORK.pmtranspose0305 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN030_cluster\pmcluster0305.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


data pmtranspose0608(where=(date>='01JAN2006'D and date<='31DEC2008'D )) ;;
set pmtranspose;
run; 

PROC EXPORT DATA= WORK.pmtranspose0608 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN030_cluster\pmcluster0608.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
