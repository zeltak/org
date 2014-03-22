libname aodpm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN003_PM_AOD_Combined\' ;





data aodpm2007;
set aodpm.t2007;
run; 


/*CREATE GUID AND SEASON */


/*copy x and y coordinantes from numeric to character (text) variables*/

   data aodpm2007_v3  ;
      set aodpm2007  ;
      xnym=put(Long_aod,6.2); 
	  ynym=put(Lat_aod,6.2); 
	  run;

	  data  aodpm2007_v4 ;
      set  aodpm2007_v3 ;
      xnymx = xnym*-100; 
	  ynymx = ynym*100; 
	  run;

/*concentrate (compress) both x and y variables into one ID*/

      data  aodpm2007_v5;
      set  aodpm2007_v4;
      guid=compress(xnymx||ynymx);
	  run;

/*convert if from text to numeric*/

      data aodpm2007_v6;
      set  aodpm2007_v5;
	  drop  xnymx ynymx xnym ynym;
	  guid2=input(guid, 8.); 
      drop guid;
  	  run;


	  data aodpm2007;
      set  aodpm2007_v6;
	  guid=guid2; 
      drop guid2;
  	  run;

libname met 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data met2007;
set met.met2007xy;
run; 


/**** Create Complete Time Series *****/

data seriesj;
 input Date date9. Value;
  format Date date9.;
cards;
01jan2007 1
31dec2007 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

proc sort data =  Met2007; by date; run;

/**** Griglia STN ****/

proc freq data = Met2007; 
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


proc sort data = Met2007;   by date STN; run;
proc sort data = Final;     by date STN; run;

data Weather_2007;
 merge Met2007 (in=a) Final (in=b);   
  by date STN;
   if b;
run;



/**** Create Unique STN XY ****/

proc sort data =  Met2007 nodupkey out = STN_XY (keep = STN Lat_met Long_Met); by STN; run;

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

data st2007&ID; set STN_XY;
  where STN = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set STN_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_met, Long_met), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Met data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

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
 set Weather_2007;
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




data Met_Complete_2007;
 set Met_Complete;
  if Temp1 ne .             Then Temp_F = Temp1;
  if Temp1 = . 				Then Temp_F = Temp2;
  if Temp1 = . & Temp2 = . 	Then Temp_F = Temp3;
  if Temp1 = . & Temp2 = . & Temp3 = . 	Then Temp_F = Temp4;
  if Temp1 = . & Temp2 = . & Temp3 = . & Temp4 = .	Then Temp_F = Temp5;
  if wdsp1 ne .             Then wdsp_F = wdsp1;
  if wdsp1 = . 				Then wdsp_F = wdsp2;
  if wdsp1 = . & wdsp2 = . 	Then wdsp_F = wdsp3;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . 	Then wdsp_F = wdsp4;
  if wdsp1 = . & wdsp2 = . & wdsp3 = . & wdsp4 = .	Then wdsp_F = wdsp5;
  if visib1 ne .             Then visib_F = visib1;
  if visib1 = . 				Then visib_F = visib2;
  if visib1 = . & visib2 = . 	Then visib_F = visib3;
  if visib1 = . & visib2 = . & visib3 = . 	Then visib_F = visib4;
  if visib1 = . & visib2 = . & visib3 = . & visib4 = .	Then visib_F = visib5;
  if ah_gm31 ne .             Then ah_gm3_F = ah_gm31;
  if ah_gm31 = . 				Then ah_gm3_F = ah_gm32;
  if ah_gm31 = . & ah_gm32 = . 	Then ah_gm3_F = ah_gm33;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . 	Then ah_gm3_F = ah_gm34;
  if ah_gm31 = . & ah_gm32 = . & ah_gm33 = . & ah_gm34 = .	Then ah_gm3_F = ah_gm35;
  if slp1 ne .             Then slp_F = slp1;
  if slp1 = . 				Then slp_F = slp2;
  if slp1 = . & slp2 = . 	Then slp_F = slp3;
  if slp1 = . & slp2 = . & slp3 = . 	Then slp_F = slp4;
  if slp1 = . & slp2 = . & slp3 = . & slp4 = .	Then slp_F = slp5;
run;



/*** Check no missing: ok ***/

proc means data = Met_Complete_2007 nmiss;
 var Temp_F visib_F ah_gm3_F wdsp_F slp_F ;
run;




libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc.metc2007;
set Met_Complete_2007;
run; 

 

proc sort data = Aodpm2007         nodupkey out=Site_AODPM(keep=Sitecode Long_PM Lat_PM); by sitecode; run;



proc sort data = Met_complete_2007 nodupkey out=STN_2007(keep=STN Long_Met Lat_Met);     by Long_Met Lat_Met; run;

data STN_2007;
set STN_2007;
 if Lat_met = . then delete;
run;


/*calculating distance from pm to met stations*/

%macro Temp;

data id_elenco(keep = elenco elenco_new Sitecode);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set Site_AODPM;
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

data st2007&ID; set Site_AODPM;
  where Sitecode = "&ID";
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set STN_2007;
    Distance = round(geodist(Lat_PM,Long_PM,Lat_Met,Long_Met), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_AOD_PM_MET data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;


/*sort by the shortest distance in each sitecode first and give it a count*/

proc sort data = Res_aod_pm_met; by sitecode distance; run;


data Res_aod_pm_met;
  set Res_aod_pm_met;
  count + 1;
  by Sitecode;
  if first.sitecode then count = 1;
run;

data Res_aod_pm_met2;
  set Res_aod_pm_met;
  if count ne  1 then delete;
run;



/*MERGE SITECODE TO STN IN AODPM DATA*/

proc sort data = aodpm2007; by sitecode   ;run;
proc sort data = Res_aod_pm_met2 ; by sitecode ;run;

data aodpm2007_v2;
merge aodpm2007 (in=a) Res_aod_pm_met2 (in=b keep = sitecode stn)   ;
  by sitecode ;
    if a;
	run; 


/*MERGE SITECODE TO STN IN AODPM DATA*/




proc sort data = aodpm2007_v2; by stn date   ;run;
proc sort data = Met_complete_2007 ; by stn date  ;run;

data aodpm2007_v3;
merge aodpm2007_v2 (in=a) Met_complete_2007 (in=b keep = stn date temp_f wdsp_f visib_f ah_gm3_F )   ;
  by stn date ;
    if a;
	run; 




/*PART 3: JOIN LU DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */ 


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_emission.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



/*PART 3: JOIN LU DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */ 



/*join to LU data*/



proc sort data= lu;
by guid;
run;

proc sort data= Aodpm2007_v3;
by guid;
run;


data Aodpm2007_v4;
merge Aodpm2007_v3(in=a) lu (in=b keep=guid pop_sqkm elev A1_dist_km p_urban p_open area_pm point_pm population ) ;
by guid;
if a;
run;


/*NOTE THE DATASET WILL SHRINK IN HALF DUE TO THE UNCLIPPED AOD DATASET (LU IS CLIPPED)*/

data Aodpm2007_v5;
set Aodpm2007_v4;
if pm25=. then delete;
if elev=. then delete;
run;




PROC IMPORT OUT= WORK.guidreg
            DATAFILE="c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = Aodpm2007_v5; by guid   ;run;
proc sort data = guidreg ; by guid ;run;

data Aodpm2007_v6;
merge Aodpm2007_v5(in=a) guidreg (in=b keep=guid reg_id)  ;
  by guid;
    if a;
	run; 



/*exports*/

libname aodx 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN006_J_AODPM_METLU\' ;

data aodx.mod1_2007_prew;
set Aodpm2007_v6 ;
run;

/*save as csv for R*/

PROC EXPORT DATA= WORK.Aodpm2007_v6
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t2007.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  






/*JOIN MET DATA TO GUID*/






/*import script cn001 output*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.all_aod;
run;


/*create grid by sorting by x,y and removing duplicates */


proc sort data = all_aod_00_10 nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 


/*clip grid*/



/*clipd grid*/

proc sort data = grid; by guid   ;run;
proc sort data = lu ; by guid ;run;

data grid(keep=guid lat_aod long_aod);
merge grid (in=a) lu (in=b)  ;
  by guid;
    if b;
	run; 



proc sort data = Met_complete_2007 nodupkey out=STN_2007(keep=STN Long_Met Lat_Met);     by Long_Met Lat_Met; run;

data STN_2007;
set STN_2007;
 if Lat_met = . then delete;
run;

/*adjust to own study area , this splits the dataset to 2 for computational reasons*/

data Grid1;
 set Grid;
  if guid <= 71054295;
run;

data Grid2;
 set Grid;
  if guid > 71054295;
run;


%macro Temp(grid=);

data id_elenco(keep = elenco elenco_new guid);
  length elenco $ 32767. elenco_new $ 32767. ;
   retain elenco_new;
   set grid&grid;
     if _n_ = 1 then do;
        elenco = trim(left(guid));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(guid));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%put &Type;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);


data st2007&ID; set grid;
  where guid = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set STN_2007;
    Distance = round(geodist(Lat_aod,Long_aod,Lat_Met,Long_Met), 0.001);
run;

dm "out;clear;log;clear;";

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_guid&grid._met data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;


%let i=%eval(&i+1);
%end;



%mend Temp;

%Temp(grid = 1);

%Temp(grid = 2);


data Res_guid_met;
set Res_guid1_met Res_guid2_met;
run; 



/*sort by the shortest distance in each sitecode first and give it a count*/

proc sort data = Res_guid_met; by guid distance; run;



data Res_guid_met2;
  set Res_guid_met;
  count + 1;
  by guid;
  if first.guid then count = 1;
run;

data Res_guid_met3;
  set Res_guid_met2;
  if count ne  1 then delete;
run;


libname guid_stn 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn.guid_stn_2007;
set Res_guid_met3 ;
run; 

/* */
/*proc datasets lib=work kill; run;*/
