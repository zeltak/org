


libname met 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data met2009;
set met.met2009xy;
run; 


/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2009 1
31dec2009 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

proc sort data =  Met2009; by date; run;

/**** Griglia STN ****/

proc freq data = Met2009; 
  table STN;
  ods output OneWayFreqs = OneWayFreqs;
run;

data OneWayFreqs(keep = STN);
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

%put &list;


/*** Create combination of all dates for each Met station ***/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = Date STN);
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


proc sort data = Met2009;   by date STN; run;
proc sort data = Final;     by date STN; run;

data Weather_2009;
 merge Met2009 (in=a) Final (in=b);   
  by date STN;
   if b;
run;


/**** Create Unique STN XY ****/

proc sort data =  Met2009 nodupkey out = STN_XY (keep = STN Lat_met Long_Met); by STN; run;

/*** We want to find the closest Weather station to each Weather station ***/

data STN_Fake(keep = STN_fake Lat_fake Long_fake);
 set STN_XY; 
   STN_fake = STN||"_Fake"; 
    Lat_fake  = Lat_met;
    Long_fake = Long_met;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new STN);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set STN_XY;
     if _n_ = 1 then do;
        elenco = trim(left(STN));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(STN));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2009&ID; set STN_XY;
  where STN = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2009&ID;
   set STN_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_met, Long_met), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Met data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2009&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;





/*** Create a full set for each Station ***/

proc sort data = Res_Met; by STN; run; 

data Res_Met;
  set Res_Met;
  count + 1;
  by STN;
  if first.STN then count = 1;
run;

/**** Take only 7 closest STN station to each STN station ****/

data Res_Met_close1(keep = STN STN_fake1);
 set Res_Met;
  if count = 1;
   STN_fake1 = STN_fake;
run;

data Res_Met_close2(keep = STN STN_fake2);
 set Res_Met;
  if count = 2;
   STN_fake2 = STN_fake;
run;

data Res_Met_close3(keep = STN STN_fake3);
 set Res_Met;
  if count = 3;
   STN_fake3 = STN_fake;
run;

data Res_Met_close4(keep = STN STN_fake4);
 set Res_Met;
  if count = 4;
    STN_fake4 = STN_fake;
run;

data Res_Met_close5(keep = STN STN_fake5);
 set Res_Met;
  if count = 5;
      STN_fake5 = STN_fake;
run;

data Res_Met_close6(keep = STN STN_fake6);
 set Res_Met;
  if count = 6;
     STN_fake6 = STN_fake;
run;

data Res_Met_close7(keep = STN STN_fake7);
 set Res_Met;
  if count = 7;
     STN_fake7 = STN_fake;
run;

data Res_Met_close8(keep = STN STN_fake8);
 set Res_Met;
  if count = 8;
     STN_fake7 = STN_fake;
run;


data Unique;
 merge Res_Met_close1 Res_Met_close2 Res_Met_close3 Res_Met_close4 Res_Met_close5 Res_Met_close6 Res_Met_close7 Res_Met_close8;
  by STN;
run;

data Met_complete;
 set Weather_2009;
  keep STN date Temp WDSP SLP visib ah_gm3 lat_met Long_met;
run;





proc sort data = Met_complete; by STN; run;
proc sort data = Unique;       by STN; run;

data Met_complete;
 merge Met_complete Unique;   
 by STN; 
run;

data Met_complete(drop = STN_fake1--STN_fake7); 
 set Met_complete;
  if STN_fake1 = " " then delete;
    id_M1 = 1*substr(STN_fake1,3,10);
    id_M2 = 1*substr(STN_fake2,3,10);
    id_M3 = 1*substr(STN_fake3,3,10);
    id_M4 = 1*substr(STN_fake4,3,10);
    id_M5 = 1*substr(STN_fake5,3,10);
    id_M6 = 1*substr(STN_fake6,3,10);
    id_M7 = 1*substr(STN_fake7,3,10);
	id_M8 = 1*substr(STN_fake8,3,10);
run;



/*** Create the dataset with the closest station by distance ***/

data Met_Comp1(keep = id_M1 date Temp1 WDSP1 ah_gm31 slp1 visib1); 
 set Met_Complete;
  id_M1 = STN;
   Temp1 = Temp;
   WDSP1 = WDSP;
   ah_gm31 = ah_gm3;
   visib1  = visib;
   slp1=slp;
run;

proc sort data = Met_Complete;  by id_M1 date; run;
proc sort data = Met_Comp1;     by id_M1 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp1 (in=b);
   by id_M1 date; 
    if a;
run;




data Met_Comp2(keep = id_M2 date Temp2 WDSP2 ah_gm32 slp2 visib2); 
 set Met_Complete;
  id_M2 = STN;
   Temp2 = Temp;
   WDSP2 = WDSP;
   ah_gm32 = ah_gm3;
   visib2  = visib;
    slp2=slp;
run;

proc sort data = Met_Complete;  by id_M2 date; run;
proc sort data = Met_Comp2;     by id_M2 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp2 (in=b);
   by id_M2 date; 
    if a;
run;




data Met_Comp3(keep = id_M3 date Temp3 WDSP3 ah_gm33 visib3 slp3); 
 set Met_Complete;
  id_M3 = STN;
   Temp3 = Temp;
   WDSP3 = WDSP;
   ah_gm33 = ah_gm3;
   visib3  = visib;
    slp3=slp;
run;

proc sort data = Met_Complete;  by id_M3 date; run;
proc sort data = Met_Comp3;     by id_M3 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp3 (in=b);
   by id_M3 date; 
    if a;
run;



data Met_Comp4(keep = id_M4 date Temp4 WDSP4 ah_gm34 visib4 slp4); 
 set Met_Complete;
  id_M4 = STN;
   Temp4 = Temp;
   WDSP4 = WDSP;
   ah_gm34 = ah_gm3;
   visib4  = visib;
       slp4=slp;
run;

proc sort data = Met_Complete;  by id_M4 date; run;
proc sort data = Met_Comp4;     by id_M4 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp4 (in=b);
   by id_M4 date; 
    if a;
run;



data Met_Comp5(keep = id_M5 date Temp5 WDSP5 ah_gm35 visib5 slp5); 
 set Met_Complete;
  id_M5 = STN;
   Temp5 = Temp;
   WDSP5 = WDSP;
   ah_gm35 = ah_gm3;
   visib5  = visib;
       slp5=slp;
run;

proc sort data = Met_Complete;  by id_M5 date; run;
proc sort data = Met_Comp5;     by id_M5 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp5 (in=b);
   by id_M5 date; 
    if a;
run;

data Met_Comp6(keep = id_M6 date Temp6 WDSP6 ah_gm36 visib6 slp6); 
 set Met_Complete;
  id_M6 = STN;
   Temp6 = Temp;
   WDSP6 = WDSP;
   ah_gm36 = ah_gm3;
   visib6  = visib;
       slp6=slp;
run;

proc sort data = Met_Complete;  by id_M6 date; run;
proc sort data = Met_Comp6;     by id_M6 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp6 (in=b);
   by id_M6 date; 
    if a;
run;



data Met_Comp7(keep = id_M7 date Temp7 WDSP7 ah_gm37 visib7 slp7); 
 set Met_Complete;
  id_M7 = STN;
   Temp7 = Temp;
   WDSP7 = WDSP;
   ah_gm37 = ah_gm3;
   visib7  = visib;
       slp7=slp;
run;

proc sort data = Met_Complete;  by id_M7 date; run;
proc sort data = Met_Comp7;     by id_M7 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp7 (in=b);
   by id_M7 date; 
    if a;
run;

data Met_Comp8(keep = id_M8 date Temp8 WDSP8 ah_gm38 visib8 slp8); 
 set Met_Complete;
  id_M8 = STN;
   Temp8 = Temp;
   WDSP8 = WDSP;
   ah_gm38 = ah_gm3;
   visib8  = visib;
       slp8=slp;
run;

proc sort data = Met_Complete;  by id_M8 date; run;
proc sort data = Met_Comp8;     by id_M8 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp8 (in=b);
   by id_M8 date; 
    if a;
run;



data Met_Complete_2009;
 set Met_Complete;
  if Temp1 ne .             Then Temp_F = Temp1;
  if Temp1 = . 				Then Temp_F = Temp2;
  if Temp1 = . & Temp2 = . 	Then Temp_F = Temp3;
  if Temp1 = . & Temp2 = . & Temp3 = . 	Then Temp_F = Temp4;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = .	Then Temp_F = Temp5;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . 	Then Temp_F = Temp6;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . & Temp6 = . 	Then Temp_F = Temp7;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . & Temp6 = . & Temp7 = . 	Then Temp_F = Temp8;
  if wdsp1 ne .             Then wdsp_F = wdsp1;
  if wdsp1 = . 				Then wdsp_F = wdsp2;
  if wdsp1 = . & wdsp2 = . 	Then wdsp_F = wdsp3;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . 	Then wdsp_F = wdsp4;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = .	Then wdsp_F = wdsp5;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . 	Then wdsp_F = wdsp6;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . & wdsp6 = . & wdsp7 = .	Then wdsp_F = wdsp8;
  if visib1 ne .             Then visib_F = visib1;
  if visib1 = . 				Then visib_F = visib2;
  if visib1 = . & visib2 = . 	Then visib_F = visib3;
  if visib1 = . & visib2 = . & visib3 = . 	Then visib_F = visib4;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = .	Then visib_F = visib5;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . 	Then visib_F = visib6;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . & visib6 = .  & visib7 = .	Then visib_F = visib8;
  if ah_gm31 ne .             Then ah_gm3_F = ah_gm31;
  if ah_gm31 = . 				Then ah_gm3_F = ah_gm32;
  if ah_gm31 = . & ah_gm32 = . 	Then ah_gm3_F = ah_gm33;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . 	Then ah_gm3_F = ah_gm34;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = .	Then ah_gm3_F = ah_gm35;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . 	Then ah_gm3_F = ah_gm36;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . & ah_gm36 = . 	Then ah_gm3_F = ah_gm37;
  if slp1 ne .             Then slp_F = slp1;
  if slp1 = . 				Then slp_F = slp2;
  if slp1 = . & slp2 = . 	Then slp_F = slp3;
  if slp1 = . & slp2 = . & slp3 = . 	Then slp_F = slp4;
  if slp1 = . & slp2 = . & slp3 = . & slp4 = .	Then slp_F = slp5;
run;



/*** Check no missing: ok ***/

proc means data = Met_Complete_2009 nmiss;
 var Temp_F visib_F ah_gm3_F wdsp_F slp_F ;
run;

/********************************************************************************************************/

libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc.metc2009;
set Met_Complete_2009;
 *where temp_f ne .;
run; 

proc datasets lib=work kill; run;


/**** 2010 ***/


libname met 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data met2010;
set met.met2010xy;
run; 


/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2010 1
31dec2010 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

proc sort data =  Met2010; by date; run;

/**** Griglia STN ****/

proc freq data = Met2010; 
  table STN;
  ods output OneWayFreqs = OneWayFreqs;
run;

data OneWayFreqs(keep = STN);
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

%put &list;


/*** Create combination of all dates for each Met station ***/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = Date STN);
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


proc sort data = Met2010;   by date STN; run;
proc sort data = Final;     by date STN; run;

data Weather_2010;
 merge Met2010 (in=a) Final (in=b);   
  by date STN;
   if b;
run;


/**** Create Unique STN XY ****/

proc sort data =  Met2010 nodupkey out = STN_XY (keep = STN Lat_met Long_Met); by STN; run;

/*** We want to find the closest Weather station to each Weather station ***/

data STN_Fake(keep = STN_fake Lat_fake Long_fake);
 set STN_XY; 
   STN_fake = STN||"_Fake"; 
    Lat_fake  = Lat_met;
    Long_fake = Long_met;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new STN);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set STN_XY;
     if _n_ = 1 then do;
        elenco = trim(left(STN));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(STN));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2010&ID; set STN_XY;
  where STN = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2010&ID;
   set STN_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_met, Long_met), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Met data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2010&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;





/*** Create a full set for each Station ***/

proc sort data = Res_Met; by STN; run; 

data Res_Met;
  set Res_Met;
  count + 1;
  by STN;
  if first.STN then count = 1;
run;

/**** Take only 7 closest STN station to each STN station ****/

data Res_Met_close1(keep = STN STN_fake1);
 set Res_Met;
  if count = 1;
   STN_fake1 = STN_fake;
run;

data Res_Met_close2(keep = STN STN_fake2);
 set Res_Met;
  if count = 2;
   STN_fake2 = STN_fake;
run;

data Res_Met_close3(keep = STN STN_fake3);
 set Res_Met;
  if count = 3;
   STN_fake3 = STN_fake;
run;

data Res_Met_close4(keep = STN STN_fake4);
 set Res_Met;
  if count = 4;
    STN_fake4 = STN_fake;
run;

data Res_Met_close5(keep = STN STN_fake5);
 set Res_Met;
  if count = 5;
      STN_fake5 = STN_fake;
run;

data Res_Met_close6(keep = STN STN_fake6);
 set Res_Met;
  if count = 6;
     STN_fake6 = STN_fake;
run;

data Res_Met_close7(keep = STN STN_fake7);
 set Res_Met;
  if count = 7;
     STN_fake7 = STN_fake;
run;


data Unique;
 merge Res_Met_close1 Res_Met_close2 Res_Met_close3 Res_Met_close4 Res_Met_close5 Res_Met_close6 Res_Met_close7;
  by STN;
run;

data Met_complete;
 set Weather_2010;
  keep STN date Temp WDSP SLP visib ah_gm3 lat_met Long_met;
run;


proc sort data = Met_complete; by STN; run;
proc sort data = Unique;       by STN; run;

data Met_complete;
 merge Met_complete Unique;   
 by STN; 
run;

data Met_complete(drop = STN_fake1--STN_fake7); 
 set Met_complete;
  if STN_fake1 = " " then delete;
    id_M1 = 1*substr(STN_fake1,3,10);
    id_M2 = 1*substr(STN_fake2,3,10);
    id_M3 = 1*substr(STN_fake3,3,10);
    id_M4 = 1*substr(STN_fake4,3,10);
    id_M5 = 1*substr(STN_fake5,3,10);
    id_M6 = 1*substr(STN_fake6,3,10);
    id_M7 = 1*substr(STN_fake7,3,10);
run;



/*** Create the dataset with the closest station by distance ***/

data Met_Comp1(keep = id_M1 date Temp1 WDSP1 ah_gm31 slp1 visib1); 
 set Met_Complete;
  id_M1 = STN;
   Temp1 = Temp;
   WDSP1 = WDSP;
   ah_gm31 = ah_gm3;
   visib1  = visib;
   slp1=slp;
run;

proc sort data = Met_Complete;  by id_M1 date; run;
proc sort data = Met_Comp1;     by id_M1 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp1 (in=b);
   by id_M1 date; 
    if a;
run;

data Met_Comp2(keep = id_M2 date Temp2 WDSP2 ah_gm32 slp2 visib2); 
 set Met_Complete;
  id_M2 = STN;
   Temp2 = Temp;
   WDSP2 = WDSP;
   ah_gm32 = ah_gm3;
   visib2  = visib;
    slp2=slp;
run;

proc sort data = Met_Complete;  by id_M2 date; run;
proc sort data = Met_Comp2;     by id_M2 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp2 (in=b);
   by id_M2 date; 
    if a;
run;




data Met_Comp3(keep = id_M3 date Temp3 WDSP3 ah_gm33 visib3 slp3); 
 set Met_Complete;
  id_M3 = STN;
   Temp3 = Temp;
   WDSP3 = WDSP;
   ah_gm33 = ah_gm3;
   visib3  = visib;
    slp3=slp;
run;

proc sort data = Met_Complete;  by id_M3 date; run;
proc sort data = Met_Comp3;     by id_M3 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp3 (in=b);
   by id_M3 date; 
    if a;
run;



data Met_Comp4(keep = id_M4 date Temp4 WDSP4 ah_gm34 visib4 slp4); 
 set Met_Complete;
  id_M4 = STN;
   Temp4 = Temp;
   WDSP4 = WDSP;
   ah_gm34 = ah_gm3;
   visib4  = visib;
       slp4=slp;
run;

proc sort data = Met_Complete;  by id_M4 date; run;
proc sort data = Met_Comp4;     by id_M4 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp4 (in=b);
   by id_M4 date; 
    if a;
run;



data Met_Comp5(keep = id_M5 date Temp5 WDSP5 ah_gm35 visib5 slp5); 
 set Met_Complete;
  id_M5 = STN;
   Temp5 = Temp;
   WDSP5 = WDSP;
   ah_gm35 = ah_gm3;
   visib5  = visib;
       slp5=slp;
run;

proc sort data = Met_Complete;  by id_M5 date; run;
proc sort data = Met_Comp5;     by id_M5 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp5 (in=b);
   by id_M5 date; 
    if a;
run;

data Met_Comp6(keep = id_M6 date Temp6 WDSP6 ah_gm36 visib6 slp6); 
 set Met_Complete;
  id_M6 = STN;
   Temp6 = Temp;
   WDSP6 = WDSP;
   ah_gm36 = ah_gm3;
   visib6  = visib;
       slp6=slp;
run;

proc sort data = Met_Complete;  by id_M6 date; run;
proc sort data = Met_Comp6;     by id_M6 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp6 (in=b);
   by id_M6 date; 
    if a;
run;



data Met_Comp7(keep = id_M7 date Temp7 WDSP7 ah_gm37 visib7 slp7); 
 set Met_Complete;
  id_M7 = STN;
   Temp7 = Temp;
   WDSP7 = WDSP;
   ah_gm37 = ah_gm3;
   visib7  = visib;
       slp7=slp;
run;

proc sort data = Met_Complete;  by id_M7 date; run;
proc sort data = Met_Comp7;     by id_M7 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp7 (in=b);
   by id_M7 date; 
    if a;
run;





data Met_Complete_2010;
 set Met_Complete;
  if Temp1 ne .             Then Temp_F = Temp1;
  if Temp1 = . 				Then Temp_F = Temp2;
  if Temp1 = . & Temp2 = . 	Then Temp_F = Temp3;
  if Temp1 = . & Temp2 = . & Temp3 = . 	Then Temp_F = Temp4;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = .	Then Temp_F = Temp5;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . 	Then Temp_F = Temp6;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . & Temp6 = . 	Then Temp_F = Temp7;
  if wdsp1 ne .             Then wdsp_F = wdsp1;
  if wdsp1 = . 				Then wdsp_F = wdsp2;
  if wdsp1 = . & wdsp2 = . 	Then wdsp_F = wdsp3;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . 	Then wdsp_F = wdsp4;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = .	Then wdsp_F = wdsp5;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . 	Then wdsp_F = wdsp6;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . & wdsp6 = . 	Then wdsp_F = wdsp7;
  if visib1 ne .             Then visib_F = visib1;
  if visib1 = . 				Then visib_F = visib2;
  if visib1 = . & visib2 = . 	Then visib_F = visib3;
  if visib1 = . & visib2 = . & visib3 = . 	Then visib_F = visib4;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = .	Then visib_F = visib5;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . 	Then visib_F = visib6;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . & visib6 = . 	Then visib_F = visib7;
  if ah_gm31 ne .             Then ah_gm3_F = ah_gm31;
  if ah_gm31 = . 				Then ah_gm3_F = ah_gm32;
  if ah_gm31 = . & ah_gm32 = . 	Then ah_gm3_F = ah_gm33;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . 	Then ah_gm3_F = ah_gm34;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = .	Then ah_gm3_F = ah_gm35;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . 	Then ah_gm3_F = ah_gm36;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . & ah_gm36 = . 	Then ah_gm3_F = ah_gm37;
  if slp1 ne .             Then slp_F = slp1;
  if slp1 = . 				Then slp_F = slp2;
  if slp1 = . & slp2 = . 	Then slp_F = slp3;
  if slp1 = . & slp2 = . & slp3 = . 	Then slp_F = slp4;
  if slp1 = . & slp2 = . & slp3 = . & slp4 = .	Then slp_F = slp5;
run;



/*** Check no missing: ok ***/

proc means data = Met_Complete_2010 nmiss;
 var Temp_F visib_F ah_gm3_F wdsp_F slp_F ;
run;

/********************************************************************************************************/

libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc.metc2010;
set Met_Complete_2010;
 *where temp_f ne .;
run; 

proc datasets lib=work kill; run;


/**** 2011 ***/


libname met 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data met2011;
set met.met2011xy;
run; 


/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2011 1
31dec2011 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

proc sort data =  Met2011; by date; run;

/**** Griglia STN ****/

proc freq data = Met2011; 
  table STN;
  ods output OneWayFreqs = OneWayFreqs;
run;

data OneWayFreqs(keep = STN);
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

%put &list;


/*** Create combination of all dates for each Met station ***/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = Date STN);
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


proc sort data = Met2011;   by date STN; run;
proc sort data = Final;     by date STN; run;

data Weather_2011;
 merge Met2011 (in=a) Final (in=b);   
  by date STN;
   if b;
run;


/**** Create Unique STN XY ****/

proc sort data =  Met2011 nodupkey out = STN_XY (keep = STN Lat_met Long_Met); by STN; run;

/*** We want to find the closest Weather station to each Weather station ***/

data STN_Fake(keep = STN_fake Lat_fake Long_fake);
 set STN_XY; 
   STN_fake = STN||"_Fake"; 
    Lat_fake  = Lat_met;
    Long_fake = Long_met;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new STN);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set STN_XY;
     if _n_ = 1 then do;
        elenco = trim(left(STN));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(STN));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2011&ID; set STN_XY;
  where STN = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2011&ID;
   set STN_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_met, Long_met), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Met data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2011&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;





/*** Create a full set for each Station ***/

proc sort data = Res_Met; by STN; run; 

data Res_Met;
  set Res_Met;
  count + 1;
  by STN;
  if first.STN then count = 1;
run;

/**** Take only 7 closest STN station to each STN station ****/

data Res_Met_close1(keep = STN STN_fake1);
 set Res_Met;
  if count = 1;
   STN_fake1 = STN_fake;
run;

data Res_Met_close2(keep = STN STN_fake2);
 set Res_Met;
  if count = 2;
   STN_fake2 = STN_fake;
run;

data Res_Met_close3(keep = STN STN_fake3);
 set Res_Met;
  if count = 3;
   STN_fake3 = STN_fake;
run;

data Res_Met_close4(keep = STN STN_fake4);
 set Res_Met;
  if count = 4;
    STN_fake4 = STN_fake;
run;

data Res_Met_close5(keep = STN STN_fake5);
 set Res_Met;
  if count = 5;
      STN_fake5 = STN_fake;
run;

data Res_Met_close6(keep = STN STN_fake6);
 set Res_Met;
  if count = 6;
     STN_fake6 = STN_fake;
run;

data Res_Met_close7(keep = STN STN_fake7);
 set Res_Met;
  if count = 7;
     STN_fake7 = STN_fake;
run;


data Unique;
 merge Res_Met_close1 Res_Met_close2 Res_Met_close3 Res_Met_close4 Res_Met_close5 Res_Met_close6 Res_Met_close7;
  by STN;
run;

data Met_complete;
 set Weather_2011;
  keep STN date Temp WDSP SLP visib ah_gm3 lat_met Long_met;
run;


proc sort data = Met_complete; by STN; run;
proc sort data = Unique;       by STN; run;

data Met_complete;
 merge Met_complete Unique;   
 by STN; 
run;

data Met_complete(drop = STN_fake1--STN_fake7); 
 set Met_complete;
  if STN_fake1 = " " then delete;
    id_M1 = 1*substr(STN_fake1,3,10);
    id_M2 = 1*substr(STN_fake2,3,10);
    id_M3 = 1*substr(STN_fake3,3,10);
    id_M4 = 1*substr(STN_fake4,3,10);
    id_M5 = 1*substr(STN_fake5,3,10);
    id_M6 = 1*substr(STN_fake6,3,10);
    id_M7 = 1*substr(STN_fake7,3,10);
run;



/*** Create the dataset with the closest station by distance ***/

data Met_Comp1(keep = id_M1 date Temp1 WDSP1 ah_gm31 slp1 visib1); 
 set Met_Complete;
  id_M1 = STN;
   Temp1 = Temp;
   WDSP1 = WDSP;
   ah_gm31 = ah_gm3;
   visib1  = visib;
   slp1=slp;
run;

proc sort data = Met_Complete;  by id_M1 date; run;
proc sort data = Met_Comp1;     by id_M1 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp1 (in=b);
   by id_M1 date; 
    if a;
run;

data Met_Comp2(keep = id_M2 date Temp2 WDSP2 ah_gm32 slp2 visib2); 
 set Met_Complete;
  id_M2 = STN;
   Temp2 = Temp;
   WDSP2 = WDSP;
   ah_gm32 = ah_gm3;
   visib2  = visib;
    slp2=slp;
run;

proc sort data = Met_Complete;  by id_M2 date; run;
proc sort data = Met_Comp2;     by id_M2 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp2 (in=b);
   by id_M2 date; 
    if a;
run;




data Met_Comp3(keep = id_M3 date Temp3 WDSP3 ah_gm33 visib3 slp3); 
 set Met_Complete;
  id_M3 = STN;
   Temp3 = Temp;
   WDSP3 = WDSP;
   ah_gm33 = ah_gm3;
   visib3  = visib;
    slp3=slp;
run;

proc sort data = Met_Complete;  by id_M3 date; run;
proc sort data = Met_Comp3;     by id_M3 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp3 (in=b);
   by id_M3 date; 
    if a;
run;



data Met_Comp4(keep = id_M4 date Temp4 WDSP4 ah_gm34 visib4 slp4); 
 set Met_Complete;
  id_M4 = STN;
   Temp4 = Temp;
   WDSP4 = WDSP;
   ah_gm34 = ah_gm3;
   visib4  = visib;
       slp4=slp;
run;

proc sort data = Met_Complete;  by id_M4 date; run;
proc sort data = Met_Comp4;     by id_M4 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp4 (in=b);
   by id_M4 date; 
    if a;
run;



data Met_Comp5(keep = id_M5 date Temp5 WDSP5 ah_gm35 visib5 slp5); 
 set Met_Complete;
  id_M5 = STN;
   Temp5 = Temp;
   WDSP5 = WDSP;
   ah_gm35 = ah_gm3;
   visib5  = visib;
       slp5=slp;
run;

proc sort data = Met_Complete;  by id_M5 date; run;
proc sort data = Met_Comp5;     by id_M5 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp5 (in=b);
   by id_M5 date; 
    if a;
run;

data Met_Comp6(keep = id_M6 date Temp6 WDSP6 ah_gm36 visib6 slp6); 
 set Met_Complete;
  id_M6 = STN;
   Temp6 = Temp;
   WDSP6 = WDSP;
   ah_gm36 = ah_gm3;
   visib6  = visib;
       slp6=slp;
run;

proc sort data = Met_Complete;  by id_M6 date; run;
proc sort data = Met_Comp6;     by id_M6 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp6 (in=b);
   by id_M6 date; 
    if a;
run;



data Met_Comp7(keep = id_M7 date Temp7 WDSP7 ah_gm37 visib7 slp7); 
 set Met_Complete;
  id_M7 = STN;
   Temp7 = Temp;
   WDSP7 = WDSP;
   ah_gm37 = ah_gm3;
   visib7  = visib;
       slp7=slp;
run;

proc sort data = Met_Complete;  by id_M7 date; run;
proc sort data = Met_Comp7;     by id_M7 date; run;

data Met_Complete;
 merge Met_Complete (in = a) Met_Comp7 (in=b);
   by id_M7 date; 
    if a;
run;





data Met_Complete_2011;
 set Met_Complete;
  if Temp1 ne .             Then Temp_F = Temp1;
  if Temp1 = . 				Then Temp_F = Temp2;
  if Temp1 = . & Temp2 = . 	Then Temp_F = Temp3;
  if Temp1 = . & Temp2 = . & Temp3 = . 	Then Temp_F = Temp4;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = .	Then Temp_F = Temp5;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . 	Then Temp_F = Temp6;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = . & Temp5 = . & Temp6 = . 	Then Temp_F = Temp7;
  if wdsp1 ne .             Then wdsp_F = wdsp1;
  if wdsp1 = . 				Then wdsp_F = wdsp2;
  if wdsp1 = . & wdsp2 = . 	Then wdsp_F = wdsp3;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . 	Then wdsp_F = wdsp4;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = .	Then wdsp_F = wdsp5;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . 	Then wdsp_F = wdsp6;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = . & wdsp5 = . & wdsp6 = . 	Then wdsp_F = wdsp7;
  if visib1 ne .             Then visib_F = visib1;
  if visib1 = . 				Then visib_F = visib2;
  if visib1 = . & visib2 = . 	Then visib_F = visib3;
  if visib1 = . & visib2 = . & visib3 = . 	Then visib_F = visib4;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = .	Then visib_F = visib5;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . 	Then visib_F = visib6;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = . & visib5 = . & visib6 = . 	Then visib_F = visib7;
  if ah_gm31 ne .             Then ah_gm3_F = ah_gm31;
  if ah_gm31 = . 				Then ah_gm3_F = ah_gm32;
  if ah_gm31 = . & ah_gm32 = . 	Then ah_gm3_F = ah_gm33;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . 	Then ah_gm3_F = ah_gm34;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = .	Then ah_gm3_F = ah_gm35;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . 	Then ah_gm3_F = ah_gm36;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = . & ah_gm35 = . & ah_gm36 = . 	Then ah_gm3_F = ah_gm37;
  if slp1 ne .             Then slp_F = slp1;
  if slp1 = . 				Then slp_F = slp2;
  if slp1 = . & slp2 = . 	Then slp_F = slp3;
  if slp1 = . & slp2 = . & slp3 = . 	Then slp_F = slp4;
  if slp1 = . & slp2 = . & slp3 = . & slp4 = .	Then slp_F = slp5;
run;



/*** Check no missing: ok ***/

proc means data = Met_Complete_2011 nmiss;
 var Temp_F visib_F ah_gm3_F wdsp_F slp_F ;
run;

/********************************************************************************************************/

libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc.metc2011;
set Met_Complete_2011;
 *where temp_f ne .;
run; 

proc datasets lib=work kill; run;

