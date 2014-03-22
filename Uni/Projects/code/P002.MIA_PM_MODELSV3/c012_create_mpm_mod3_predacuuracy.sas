/*2000*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2000;
set pm.pm2000;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2000 ***/

proc freq data = pm2000;
 table SiteCode;
  ods output onewayfreqs = Monitors_2000(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2000
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2000;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2000_&stp;
 merge pm2000 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2000_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2000v2;
merge pm2000_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2000v2;
 class reg_id date;
   var pm25;
      output out = Mean_2000_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2000 Pm2000v2 Monitors_2000 Pm2000_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 1
31dec2000 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 2
31dec2000 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 3
31dec2000 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 4
31dec2000 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 5
31dec2000 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 6
31dec2000 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2000 7
31dec2000 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2000_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2000_v1;
merge Mean_2000_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2000_v1 ; by reg_id;run;

data Mean_2000_v2;
merge Mean_2000_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2000_v2; by date reg_id; run;


data Mean_2000_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2000_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2000_v3_1(keep = id_M1 date mpm1); 
 set Mean_2000_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2000_v3;    by id_M1 date; run;
proc sort data =  Mean_2000_v3_1;  by id_M1 date; run;

data Mean_2000_v3;
 merge Mean_2000_v3(in = a) Mean_2000_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2000_v3_2(keep = id_M2 date mpm2); 
 set Mean_2000_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2000_v3;    by id_M2 date; run;
proc sort data =  Mean_2000_v3_2;  by id_M2 date; run;

data Mean_2000_v3;
 merge Mean_2000_v3(in = a) Mean_2000_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2000_v3_3(keep = id_M3 date mpm3); 
 set Mean_2000_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2000_v3;    by id_M3 date; run;
proc sort data =  Mean_2000_v3_3;  by id_M3 date; run;

data Mean_2000_v3;
 merge Mean_2000_v3(in = a) Mean_2000_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2000_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2000_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2000_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2000_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2000_v1 Mean_2000_v2 Mean_2000_v3 Mean_2000_v3_1 Mean_2000_v3_2 Mean_2000_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2000_&stp; 
 set Mean_2000_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2001*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2001;
set pm.pm2001;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2001 ***/

proc freq data = pm2001;
 table SiteCode;
  ods output onewayfreqs = Monitors_2001(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2001
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2001;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2001_&stp;
 merge pm2001 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2001_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2001v2;
merge pm2001_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2001v2;
 class reg_id date;
   var pm25;
      output out = Mean_2001_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2001 Pm2001v2 Monitors_2001 Pm2001_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 1
31dec2001 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 2
31dec2001 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 3
31dec2001 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 4
31dec2001 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 5
31dec2001 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 6
31dec2001 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2001 7
31dec2001 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2001_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2001_v1;
merge Mean_2001_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2001_v1 ; by reg_id;run;

data Mean_2001_v2;
merge Mean_2001_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2001_v2; by date reg_id; run;


data Mean_2001_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2001_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2001_v3_1(keep = id_M1 date mpm1); 
 set Mean_2001_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2001_v3;    by id_M1 date; run;
proc sort data =  Mean_2001_v3_1;  by id_M1 date; run;

data Mean_2001_v3;
 merge Mean_2001_v3(in = a) Mean_2001_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2001_v3_2(keep = id_M2 date mpm2); 
 set Mean_2001_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2001_v3;    by id_M2 date; run;
proc sort data =  Mean_2001_v3_2;  by id_M2 date; run;

data Mean_2001_v3;
 merge Mean_2001_v3(in = a) Mean_2001_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2001_v3_3(keep = id_M3 date mpm3); 
 set Mean_2001_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2001_v3;    by id_M3 date; run;
proc sort data =  Mean_2001_v3_3;  by id_M3 date; run;

data Mean_2001_v3;
 merge Mean_2001_v3(in = a) Mean_2001_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2001_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2001_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2001_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2001_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2001_v1 Mean_2001_v2 Mean_2001_v3 Mean_2001_v3_1 Mean_2001_v3_2 Mean_2001_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2001_&stp; 
 set Mean_2001_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2002*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2002;
set pm.pm2002;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2002 ***/

proc freq data = pm2002;
 table SiteCode;
  ods output onewayfreqs = Monitors_2002(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2002
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2002;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2002_&stp;
 merge pm2002 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2002_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2002v2;
merge pm2002_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2002v2;
 class reg_id date;
   var pm25;
      output out = Mean_2002_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2002 Pm2002v2 Monitors_2002 Pm2002_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 1
31dec2002 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 2
31dec2002 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 3
31dec2002 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 4
31dec2002 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 5
31dec2002 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 6
31dec2002 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2002 7
31dec2002 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2002_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2002_v1;
merge Mean_2002_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2002_v1 ; by reg_id;run;

data Mean_2002_v2;
merge Mean_2002_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2002_v2; by date reg_id; run;


data Mean_2002_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2002_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2002_v3_1(keep = id_M1 date mpm1); 
 set Mean_2002_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2002_v3;    by id_M1 date; run;
proc sort data =  Mean_2002_v3_1;  by id_M1 date; run;

data Mean_2002_v3;
 merge Mean_2002_v3(in = a) Mean_2002_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2002_v3_2(keep = id_M2 date mpm2); 
 set Mean_2002_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2002_v3;    by id_M2 date; run;
proc sort data =  Mean_2002_v3_2;  by id_M2 date; run;

data Mean_2002_v3;
 merge Mean_2002_v3(in = a) Mean_2002_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2002_v3_3(keep = id_M3 date mpm3); 
 set Mean_2002_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2002_v3;    by id_M3 date; run;
proc sort data =  Mean_2002_v3_3;  by id_M3 date; run;

data Mean_2002_v3;
 merge Mean_2002_v3(in = a) Mean_2002_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2002_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2002_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2002_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2002_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2002_v1 Mean_2002_v2 Mean_2002_v3 Mean_2002_v3_1 Mean_2002_v3_2 Mean_2002_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2002_&stp; 
 set Mean_2002_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2003*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2003;
set pm.pm2003;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2003 ***/

proc freq data = pm2003;
 table SiteCode;
  ods output onewayfreqs = Monitors_2003(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2003
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2003;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2003_&stp;
 merge pm2003 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2003_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2003v2;
merge pm2003_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2003v2;
 class reg_id date;
   var pm25;
      output out = Mean_2003_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2003 Pm2003v2 Monitors_2003 Pm2003_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 1
31dec2003 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 2
31dec2003 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 3
31dec2003 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 4
31dec2003 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 5
31dec2003 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 6
31dec2003 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2003 7
31dec2003 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2003_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2003_v1;
merge Mean_2003_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2003_v1 ; by reg_id;run;

data Mean_2003_v2;
merge Mean_2003_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2003_v2; by date reg_id; run;


data Mean_2003_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2003_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2003_v3_1(keep = id_M1 date mpm1); 
 set Mean_2003_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2003_v3;    by id_M1 date; run;
proc sort data =  Mean_2003_v3_1;  by id_M1 date; run;

data Mean_2003_v3;
 merge Mean_2003_v3(in = a) Mean_2003_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2003_v3_2(keep = id_M2 date mpm2); 
 set Mean_2003_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2003_v3;    by id_M2 date; run;
proc sort data =  Mean_2003_v3_2;  by id_M2 date; run;

data Mean_2003_v3;
 merge Mean_2003_v3(in = a) Mean_2003_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2003_v3_3(keep = id_M3 date mpm3); 
 set Mean_2003_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2003_v3;    by id_M3 date; run;
proc sort data =  Mean_2003_v3_3;  by id_M3 date; run;

data Mean_2003_v3;
 merge Mean_2003_v3(in = a) Mean_2003_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2003_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2003_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2003_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2003_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2003_v1 Mean_2003_v2 Mean_2003_v3 Mean_2003_v3_1 Mean_2003_v3_2 Mean_2003_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2003_&stp; 
 set Mean_2003_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2004*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2004;
set pm.pm2004;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2004 ***/

proc freq data = pm2004;
 table SiteCode;
  ods output onewayfreqs = Monitors_2004(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2004
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2004;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2004_&stp;
 merge pm2004 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2004_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2004v2;
merge pm2004_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2004v2;
 class reg_id date;
   var pm25;
      output out = Mean_2004_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2004 Pm2004v2 Monitors_2004 Pm2004_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 1
31dec2004 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 2
31dec2004 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 3
31dec2004 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 4
31dec2004 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 5
31dec2004 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 6
31dec2004 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2004 7
31dec2004 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2004_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2004_v1;
merge Mean_2004_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2004_v1 ; by reg_id;run;

data Mean_2004_v2;
merge Mean_2004_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2004_v2; by date reg_id; run;


data Mean_2004_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2004_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2004_v3_1(keep = id_M1 date mpm1); 
 set Mean_2004_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2004_v3;    by id_M1 date; run;
proc sort data =  Mean_2004_v3_1;  by id_M1 date; run;

data Mean_2004_v3;
 merge Mean_2004_v3(in = a) Mean_2004_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2004_v3_2(keep = id_M2 date mpm2); 
 set Mean_2004_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2004_v3;    by id_M2 date; run;
proc sort data =  Mean_2004_v3_2;  by id_M2 date; run;

data Mean_2004_v3;
 merge Mean_2004_v3(in = a) Mean_2004_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2004_v3_3(keep = id_M3 date mpm3); 
 set Mean_2004_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2004_v3;    by id_M3 date; run;
proc sort data =  Mean_2004_v3_3;  by id_M3 date; run;

data Mean_2004_v3;
 merge Mean_2004_v3(in = a) Mean_2004_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2004_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2004_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2004_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2004_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2004_v1 Mean_2004_v2 Mean_2004_v3 Mean_2004_v3_1 Mean_2004_v3_2 Mean_2004_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2004_&stp; 
 set Mean_2004_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2005*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2005;
set pm.pm2005;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2005 ***/

proc freq data = pm2005;
 table SiteCode;
  ods output onewayfreqs = Monitors_2005(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2005
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2005;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2005_&stp;
 merge pm2005 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2005_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2005v2;
merge pm2005_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2005v2;
 class reg_id date;
   var pm25;
      output out = Mean_2005_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2005 Pm2005v2 Monitors_2005 Pm2005_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 1
31dec2005 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 2
31dec2005 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 3
31dec2005 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 4
31dec2005 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 5
31dec2005 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 6
31dec2005 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2005 7
31dec2005 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2005_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2005_v1;
merge Mean_2005_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2005_v1 ; by reg_id;run;

data Mean_2005_v2;
merge Mean_2005_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2005_v2; by date reg_id; run;


data Mean_2005_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2005_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2005_v3_1(keep = id_M1 date mpm1); 
 set Mean_2005_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2005_v3;    by id_M1 date; run;
proc sort data =  Mean_2005_v3_1;  by id_M1 date; run;

data Mean_2005_v3;
 merge Mean_2005_v3(in = a) Mean_2005_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2005_v3_2(keep = id_M2 date mpm2); 
 set Mean_2005_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2005_v3;    by id_M2 date; run;
proc sort data =  Mean_2005_v3_2;  by id_M2 date; run;

data Mean_2005_v3;
 merge Mean_2005_v3(in = a) Mean_2005_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2005_v3_3(keep = id_M3 date mpm3); 
 set Mean_2005_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2005_v3;    by id_M3 date; run;
proc sort data =  Mean_2005_v3_3;  by id_M3 date; run;

data Mean_2005_v3;
 merge Mean_2005_v3(in = a) Mean_2005_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2005_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2005_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2005_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2005_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2005_v1 Mean_2005_v2 Mean_2005_v3 Mean_2005_v3_1 Mean_2005_v3_2 Mean_2005_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2005_&stp; 
 set Mean_2005_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2006*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2006;
set pm.pm2006;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2006 ***/

proc freq data = pm2006;
 table SiteCode;
  ods output onewayfreqs = Monitors_2006(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2006
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2006;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2006_&stp;
 merge pm2006 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2006_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2006v2;
merge pm2006_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2006v2;
 class reg_id date;
   var pm25;
      output out = Mean_2006_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2006 Pm2006v2 Monitors_2006 Pm2006_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 1
31dec2006 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 2
31dec2006 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 3
31dec2006 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 4
31dec2006 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 5
31dec2006 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 6
31dec2006 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2006 7
31dec2006 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2006_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2006_v1;
merge Mean_2006_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2006_v1 ; by reg_id;run;

data Mean_2006_v2;
merge Mean_2006_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2006_v2; by date reg_id; run;


data Mean_2006_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2006_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2006_v3_1(keep = id_M1 date mpm1); 
 set Mean_2006_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2006_v3;    by id_M1 date; run;
proc sort data =  Mean_2006_v3_1;  by id_M1 date; run;

data Mean_2006_v3;
 merge Mean_2006_v3(in = a) Mean_2006_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2006_v3_2(keep = id_M2 date mpm2); 
 set Mean_2006_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2006_v3;    by id_M2 date; run;
proc sort data =  Mean_2006_v3_2;  by id_M2 date; run;

data Mean_2006_v3;
 merge Mean_2006_v3(in = a) Mean_2006_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2006_v3_3(keep = id_M3 date mpm3); 
 set Mean_2006_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2006_v3;    by id_M3 date; run;
proc sort data =  Mean_2006_v3_3;  by id_M3 date; run;

data Mean_2006_v3;
 merge Mean_2006_v3(in = a) Mean_2006_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2006_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2006_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2006_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2006_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2006_v1 Mean_2006_v2 Mean_2006_v3 Mean_2006_v3_1 Mean_2006_v3_2 Mean_2006_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2006_&stp; 
 set Mean_2006_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2007*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2007;
set pm.pm2007;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2007 ***/

proc freq data = pm2007;
 table SiteCode;
  ods output onewayfreqs = Monitors_2007(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2007
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2007;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2007_&stp;
 merge pm2007 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2007_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2007v2;
merge pm2007_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2007v2;
 class reg_id date;
   var pm25;
      output out = Mean_2007_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2007 Pm2007v2 Monitors_2007 Pm2007_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 1
31dec2007 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 2
31dec2007 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 3
31dec2007 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 4
31dec2007 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 5
31dec2007 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 6
31dec2007 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2007 7
31dec2007 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2007_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2007_v1;
merge Mean_2007_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2007&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2007&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2007&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2007_v1 ; by reg_id;run;

data Mean_2007_v2;
merge Mean_2007_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2007_v2; by date reg_id; run;


data Mean_2007_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2007_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2007_v3_1(keep = id_M1 date mpm1); 
 set Mean_2007_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2007_v3;    by id_M1 date; run;
proc sort data =  Mean_2007_v3_1;  by id_M1 date; run;

data Mean_2007_v3;
 merge Mean_2007_v3(in = a) Mean_2007_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2007_v3_2(keep = id_M2 date mpm2); 
 set Mean_2007_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2007_v3;    by id_M2 date; run;
proc sort data =  Mean_2007_v3_2;  by id_M2 date; run;

data Mean_2007_v3;
 merge Mean_2007_v3(in = a) Mean_2007_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2007_v3_3(keep = id_M3 date mpm3); 
 set Mean_2007_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2007_v3;    by id_M3 date; run;
proc sort data =  Mean_2007_v3_3;  by id_M3 date; run;

data Mean_2007_v3;
 merge Mean_2007_v3(in = a) Mean_2007_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2007_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2007_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2007_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2007_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2007_v1 Mean_2007_v2 Mean_2007_v3 Mean_2007_v3_1 Mean_2007_v3_2 Mean_2007_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2007_&stp; 
 set Mean_2007_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


/*2008*/

/*create a full complete 365 days for all mpm for every region*/

%macro step(stp = );

PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2008;
set pm.pm2008;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 


/*** All monitors 2008 ***/

proc freq data = pm2008;
 table SiteCode;
  ods output onewayfreqs = Monitors_2008(keep = Sitecode);
run;
quit;

PROC SURVEYSELECT data = Monitors_2008
  outall out = Step_&stp samprate = 0.1;
run;

proc sort data = pm2008;     by sitecode; run;
proc sort data = Step_&stp;  by sitecode; run;

data pm2008_&stp;
 merge pm2008 Step_&stp;
  by sitecode;
  if Selected = 1 then delete;
run;

proc sort data = pm2008_&stp; by sitecode ;run;
proc sort data = pmreg;       by sitecode ;run;

data pm2008v2;
merge pm2008_&stp(in=a) pmreg(in=b keep=sitecode reg_id);
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2008v2;
 class reg_id date;
   var pm25;
      output out = Mean_2008_&Stp(drop = _Type_ _freq_) mean=mpm;
run; 

proc datasets lib = work; 
 delete Step_&stp Pm2008 Pm2008v2 Monitors_2008 Pm2008_&stp; 
run;

%mend Step;



%Step(stp=s1);
%Step(stp=s2);
%Step(stp=s3);
%Step(stp=s4);
%Step(stp=s5);
%Step(stp=s6);
%Step(stp=s7);
%Step(stp=s8);
%Step(stp=s9);
%Step(stp=s10);


/*create series of full days for each region*/

/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 1
31dec2008 1
run;

proc expand data = seriesj_1 out=daily1 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 2 ****/

data seriesj_2;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 2
31dec2008 2
run;

proc expand data = seriesj_2 out=daily2 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 3 ****/

data seriesj_3;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 3
31dec2008 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 4 ****/

data seriesj_4;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 4
31dec2008 4
run;

proc expand data = seriesj_4 out=daily4 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 5 ****/

data seriesj_5;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 5
31dec2008 5
run;

proc expand data = seriesj_5 out=daily5 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 6 ****/

data seriesj_6;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 6
31dec2008 6
run;

proc expand data = seriesj_6 out=daily6 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


/**** 7 ****/

data seriesj_7;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2008 7
31dec2008 7
run;

proc expand data = seriesj_7 out=daily7 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3 daily4 daily5 daily6 daily7 ;
run; 

proc datasets lib = work; 
 delete daily1 daily2 daily3 daily4 daily5 daily6 daily7
        Seriesj_1 Seriesj_2 Seriesj_3 Seriesj_4 Seriesj_5 Seriesj_6 Seriesj_7; 
run;


%macro step(stp=);

proc sort data = Mean_2008_&stp; by date reg_id ;run;
proc sort data = daily;          by date reg_id  ;run;

data Mean_2008_v1;
merge Mean_2008_&stp(in=a) daily(in=b)  ;
  by date reg_id ;
    if b;
run; 


/*** We want to find the closest Region to each Region ***/

data Regxy;
 set Regxy;
  keep reg_id Lat_reg Long_reg;
run;

data Region_Fake(keep = reg_id_fake Lat_fake Long_fake);
 set regxy; 
   reg_id_fake = Reg_id||"_Fake"; 
    Lat_fake  = Lat_reg;
    Long_fake = Long_reg;
run;

%let Type =;
%let ID =;

%macro Temp;

data id_elenco(keep = elenco elenco_new reg_id);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set regxy;
     if _n_ = 1 then do;
        elenco = trim(left(reg_id));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(reg_id));
      elenco_new = elenco;
       call symputx("Type",elenco_new);
      output;
     end;
run;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data st2008&ID; set regXY;
  where reg_id = &ID;
run;

data Dist_Met&ID;
  if _N_ = 1 then set st2008&ID;
   set Region_Fake;
    Distance = round(geodist(Lat_fake, Long_fake, Lat_reg, Long_reg), 0.001);
run;

proc sort data = Dist_Met&ID; by distance; run;

proc append base = Res_Reg data = Dist_Met&ID force;
run;

proc datasets lib=work; delete id_elenco Dist_Met&ID st2008&ID; run;

%let i=%eval(&i+1);
%end;

%mend Temp;

%Temp;

proc sort data = Res_reg; by reg_id distance; run;



/*** Create a full set for each Station ***/

proc sort data = Res_reg; by reg_id; run; 

data Res_reg;
  set Res_reg;
  count + 1;
  by Reg_id;
  if first.Reg_id then count = 1;
run;



/**** Take only 3 closest Region for each Region ****/

data Res_reg1(keep = reg_id reg1_id_fake);
 set Res_reg;
  if count = 1;
   reg1_id_fake = reg_id_fake;
run;

data Res_reg2(keep = reg_id reg2_id_fake);
 set Res_reg;
  if count = 2;
    reg2_id_fake = reg_id_fake;
run;

data Res_reg3(keep = reg_id reg3_id_fake);
 set Res_reg;
  if count = 3;
      reg3_id_fake = reg_id_fake;
run;

data Unique;
 merge Res_reg1 Res_reg2 Res_reg3;
  by Reg_id;
run;

proc sort data = Unique;        by reg_id;run;
proc sort data = Mean_2008_v1 ; by reg_id;run;

data Mean_2008_v2;
merge Mean_2008_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2008_v2; by date reg_id; run;


data Mean_2008_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2008_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2008_v3_1(keep = id_M1 date mpm1); 
 set Mean_2008_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2008_v3;    by id_M1 date; run;
proc sort data =  Mean_2008_v3_1;  by id_M1 date; run;

data Mean_2008_v3;
 merge Mean_2008_v3(in = a) Mean_2008_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2008_v3_2(keep = id_M2 date mpm2); 
 set Mean_2008_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2008_v3;    by id_M2 date; run;
proc sort data =  Mean_2008_v3_2;  by id_M2 date; run;

data Mean_2008_v3;
 merge Mean_2008_v3(in = a) Mean_2008_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2008_v3_3(keep = id_M3 date mpm3); 
 set Mean_2008_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2008_v3;    by id_M3 date; run;
proc sort data =  Mean_2008_v3_3;  by id_M3 date; run;

data Mean_2008_v3;
 merge Mean_2008_v3(in = a) Mean_2008_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2008_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2008_&stp._Final(keep = reg_id date mpm_F);
 set Mean_2008_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;


/*** Check no missing: ok ***/

proc means data = Mean_2008_&stp._Final nmiss;
 var mpm_F ;
run;

proc datasets lib = work; 
 delete Mean_2008_v1 Mean_2008_v2 Mean_2008_v3 Mean_2008_v3_1 Mean_2008_v3_2 Mean_2008_v3_3 
        Res_reg1 Res_reg2 Res_reg3 Unique Region_Fake Res_reg; 
run;

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear_val\' ;

%macro step(stp=);

data mpm2.mpm2008_&stp; 
 set Mean_2008_&stp._final;
run; 

%mend step;

%step(stp=s1);
%step(stp=s2);
%step(stp=s3);
%step(stp=s4);
%step(stp=s5);
%step(stp=s6);
%step(stp=s7);
%step(stp=s8);
%step(stp=s9);
%step(stp=s10);


proc datasets lib=work kill; run; 


