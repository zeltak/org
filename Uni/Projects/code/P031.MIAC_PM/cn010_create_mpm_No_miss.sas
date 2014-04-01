libname pm 'f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN001_PM_allyears\' ;



proc summary nway data=pm.all_Pm;
class sitecode date;
var PM25 Long_PM Lat_PM;
output out = PM_All(drop = _type_ _freq_) mean=PM25 Long_PM Lat_PM;
run;  



/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2003 1
31dec2011 1
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
 set pm.all_Pm;
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

/**** Take only 18 closest Sitecode station to each Sitecode station ****/

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


data Res_PM_close14(keep = Sitecode Sitecode_fake14);
 set Res_PM;
  if count = 14;
     Sitecode_fake14 = Sitecode_fake;
run;

data Res_PM_close15(keep = Sitecode Sitecode_fake15);
 set Res_PM;
  if count = 15;
     Sitecode_fake15 = Sitecode_fake;
run;

data Res_PM_close16(keep = Sitecode Sitecode_fake16);
 set Res_PM;
  if count = 16;
     Sitecode_fake16 = Sitecode_fake;
run;

data Res_PM_close17(keep = Sitecode Sitecode_fake17);
 set Res_PM;
  if count = 17;
     Sitecode_fake17 = Sitecode_fake;
run;

data Res_PM_close18(keep = Sitecode Sitecode_fake18);
 set Res_PM;
  if count = 18;
     Sitecode_fake18 = Sitecode_fake;
run;

data Res_PM_close19(keep = Sitecode Sitecode_fake19);
 set Res_PM;
  if count = 19;
     Sitecode_fake19 = Sitecode_fake;
run;


data Res_PM_close20(keep = Sitecode Sitecode_fake20);
 set Res_PM;
  if count = 20;
     Sitecode_fake20 = Sitecode_fake;
run;





data Unique;
 merge Res_PM_close1 Res_PM_close2 Res_PM_close3 Res_PM_close4 Res_PM_close5 Res_PM_close6 Res_PM_close7
       Res_PM_close8 Res_PM_close9 Res_PM_close10 Res_PM_close11 Res_PM_close12 Res_PM_close13 Res_PM_close14 Res_PM_close15
Res_PM_close16 Res_PM_close17 Res_PM_close18 Res_PM_close19 Res_PM_close20;
  by Sitecode;
run;




proc sort data = PM_All; by Sitecode; run;
proc sort data = Unique;  by Sitecode; run;

data PM_All_v1;
 merge PM_All Unique;   
 by Sitecode; 
run;

data PM_All_v1(drop = Sitecode_fake1--Sitecode_fake20); 
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
	id_M14 = compress(Sitecode_fake14,'_Fake');
	id_M15 = compress(Sitecode_fake15,'_Fake');
	id_M16 = compress(Sitecode_fake16,'_Fake');
	id_M17 = compress(Sitecode_fake17,'_Fake');
	id_M18 = compress(Sitecode_fake18,'_Fake');
	id_M19 = compress(Sitecode_fake19,'_Fake');
	id_M20 = compress(Sitecode_fake20,'_Fake');
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

data PM_Comp14(keep = id_M14 date PM25_14); 
 set PM_All_v1;
  id_M14 = Sitecode;
   PM25_14 = PM25;
run;

proc sort data = PM_All_v1;   by id_M14 date; run;
proc sort data = PM_Comp14;  by id_M14 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp14(in=b);
   by id_M14 date; 
    if a;
run;



data PM_Comp15(keep = id_M15 date PM25_15); 
 set PM_All_v1;
  id_M15 = Sitecode;
   PM25_15 = PM25;
run;

proc sort data = PM_All_v1;   by id_M15 date; run;
proc sort data = PM_Comp15;  by id_M15 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp15(in=b);
   by id_M15 date; 
    if a;
run;



data PM_Comp16(keep = id_M16 date PM25_16); 
 set PM_All_v1;
  id_M16 = Sitecode;
   PM25_16 = PM25;
run;

proc sort data = PM_All_v1;   by id_M16 date; run;
proc sort data = PM_Comp16;  by id_M16 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp16(in=b);
   by id_M16 date; 
    if a;
run;



data PM_Comp17(keep = id_M17 date PM25_17); 
 set PM_All_v1;
  id_M17 = Sitecode;
   PM25_17 = PM25;
run;

proc sort data = PM_All_v1;   by id_M17 date; run;
proc sort data = PM_Comp17;  by id_M17 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp17(in=b);
   by id_M17 date; 
    if a;
run;



data PM_Comp18(keep = id_M18 date PM25_18); 
 set PM_All_v1;
  id_M18 = Sitecode;
   PM25_18 = PM25;
run;

proc sort data = PM_All_v1;   by id_M18 date; run;
proc sort data = PM_Comp18;  by id_M18 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp18(in=b);
   by id_M18 date; 
    if a;
run;



data PM_Comp19(keep = id_M19 date PM25_19); 
 set PM_All_v1;
  id_M19 = Sitecode;
   PM25_19 = PM25;
run;

proc sort data = PM_All_v1;   by id_M19 date; run;
proc sort data = PM_Comp19;  by id_M19 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp19(in=b);
   by id_M19 date; 
    if a;
run;



data PM_Comp20(keep = id_M20 date PM25_20); 
 set PM_All_v1;
  id_M20 = Sitecode;
   PM25_20 = PM25;
run;

proc sort data = PM_All_v1;   by id_M20 date; run;
proc sort data = PM_Comp20;  by id_M20 date; run;

data PM_All_v1;
 merge PM_All_v1(in = a) PM_Comp20(in=b);
   by id_M20 date; 
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
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =. Then PM25__F = PM25_14;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  Then PM25__F = PM25_15;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  & PM25_15 =.  Then PM25__F = PM25_16;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  & PM25_15 =. & PM25_16 =. Then PM25__F = PM25_17;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  & PM25_15 =. & PM25_16 =.  & PM25_17 =.  Then PM25__F = PM25_18;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  & PM25_15 =. & PM25_16 =.  & PM25_17 =.    & PM25_18 =.   Then PM25__F = PM25_19;
  if PM25_1 = . & PM25_2 = . & PM25_3 = . & PM25_4 = . & PM25_5 = . & PM25_6 = . & PM25_7 = . & PM25_8 = . & PM25_9 = . & PM25_10 = . & PM25_11 = . & PM25_12 = .	 & PM25_13 =.  & PM25_14 =.  & PM25_15 =. & PM25_16 =.  & PM25_17 =.    & PM25_18 =.    & PM25_19 =.   Then PM25__F = PM25_20;
run;



/*** Check no missing: ok ***/

proc means data = PM_All_complete nmiss n;
 var PM25__F  ;
run;


proc means data=PM_All_complete n min max;
var PM25__F; 
run; 



data pm.PM_All_complete (rename=(PM25__F=PM25)); 
set PM_All_complete;
keep sitecode date  PM25__F;
run; 



 
