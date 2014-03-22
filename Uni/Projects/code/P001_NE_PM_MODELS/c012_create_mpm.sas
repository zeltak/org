/*2000*/


/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2000;
set pm.pm2000;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 

proc sort data = pm2000; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2000v2;
merge pm2000(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2000v2;
 class reg_id date;
   var pm25;
      output out=Mean_2000(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2000; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2000_v1;
merge Mean_2000(in=a) daily (in=b)  ;
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


data Mean_2000_v4;
 set Mean_2000_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2000_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2000;
set Mean_2000_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2000
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2000_m2_pred.dbf" 
		               DBMS=DBF REPLACE;
					        GETDELETED=NO;
							RUN; 



proc sort data = Mean_2000_v4; by date reg_id   ;run;
proc sort data = mod2pred2000 ; by date reg_id ;run;

data mod2pred2000_V2;
merge mod2pred2000(in=a) Mean_2000_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
	run; 


	data mod2pred2000_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
	set mod2pred2000_V2;
	m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


	run; 

	proc means data= mod2pred2000_V3 n min max mean std nmiss;
	var ; 
	run; 



PROC EXPORT DATA= WORK.mod2pred2000_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2000_m2_pred_mpm.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2000;
set fullg.y2000 ;
run; 

 proc univariate data=Fgrid2000;
var reg_id;
run;
 


proc sort data = Mean_2000_v4; by date reg_id   ;run;
proc sort data = fgrid2000 ;   by date reg_id ;run;

data fgrid2000_V2;
merge fgrid2000(in=a) Mean_2000_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
	run; 


data fgrid2000_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2000_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 




PROC EXPORT DATA= WORK.fgrid2000_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2000.csv" 
           DBMS=CSV REPLACE;
	     PUTNAMES=YES;
 RUN;

proc means data=fgrid2000_V3 n min max mean std nmiss;
var ; 
run; 
 

proc datasets lib=work kill; run;



/*2001*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2001;
set pm.pm2001;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2001; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2001v2;
merge pm2001(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2001v2;
 class reg_id date;
   var pm25;
      output out=Mean_2001(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2001; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2001_v1;
merge Mean_2001(in=a) daily (in=b)  ;
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


data Mean_2001_v4;
 set Mean_2001_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2001_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2001;
set Mean_2001_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2001
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2001_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2001_v4; by date reg_id   ;run;
proc sort data = mod2pred2001 ; by date reg_id ;run;

data mod2pred2001_V2;
merge mod2pred2001(in=a) Mean_2001_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2001_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2001_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2001_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2001_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2001;
set fullg.y2001 ;
run; 

 


proc sort data = Mean_2001_v4; by date reg_id   ;run;
proc sort data = fgrid2001 ;   by date reg_id ;run;

data fgrid2001_V2;
merge fgrid2001(in=a) Mean_2001_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2001_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2001_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2001_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2001.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2002*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2002;
set pm.pm2002;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2002; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2002v2;
merge pm2002(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2002v2;
 class reg_id date;
   var pm25;
      output out=Mean_2002(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2002; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2002_v1;
merge Mean_2002(in=a) daily (in=b)  ;
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


data Mean_2002_v4;
 set Mean_2002_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2002_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2002;
set Mean_2002_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2002
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2002_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2002_v4; by date reg_id   ;run;
proc sort data = mod2pred2002 ; by date reg_id ;run;

data mod2pred2002_V2;
merge mod2pred2002(in=a) Mean_2002_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2002_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2002_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2002_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2002_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2002;
set fullg.y2002 ;
run; 

 


proc sort data = Mean_2002_v4; by date reg_id   ;run;
proc sort data = fgrid2002 ;   by date reg_id ;run;

data fgrid2002_V2;
merge fgrid2002(in=a) Mean_2002_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2002_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2002_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2002_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2002.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2003*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2003;
set pm.pm2003;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2003; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2003v2;
merge pm2003(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2003v2;
 class reg_id date;
   var pm25;
      output out=Mean_2003(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2003; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2003_v1;
merge Mean_2003(in=a) daily (in=b)  ;
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


data Mean_2003_v4;
 set Mean_2003_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2003_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2003;
set Mean_2003_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2003
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2003_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2003_v4; by date reg_id   ;run;
proc sort data = mod2pred2003 ; by date reg_id ;run;

data mod2pred2003_V2;
merge mod2pred2003(in=a) Mean_2003_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2003_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2003_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2003_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2003_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2003;
set fullg.y2003 ;
run; 

 


proc sort data = Mean_2003_v4; by date reg_id   ;run;
proc sort data = fgrid2003 ;   by date reg_id ;run;

data fgrid2003_V2;
merge fgrid2003(in=a) Mean_2003_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2003_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2003_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2003_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2003.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2004*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2004;
set pm.pm2004;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2004; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2004v2;
merge pm2004(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2004v2;
 class reg_id date;
   var pm25;
      output out=Mean_2004(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2004; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2004_v1;
merge Mean_2004(in=a) daily (in=b)  ;
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


data Mean_2004_v4;
 set Mean_2004_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2004_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2004;
set Mean_2004_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2004
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2004_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2004_v4; by date reg_id   ;run;
proc sort data = mod2pred2004 ; by date reg_id ;run;

data mod2pred2004_V2;
merge mod2pred2004(in=a) Mean_2004_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2004_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2004_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2004_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2004_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2004;
set fullg.y2004 ;
run; 

 


proc sort data = Mean_2004_v4; by date reg_id   ;run;
proc sort data = fgrid2004 ;   by date reg_id ;run;

data fgrid2004_V2;
merge fgrid2004(in=a) Mean_2004_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2004_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2004_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2004_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2004.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2005*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2005;
set pm.pm2005;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

ods trace on;
proc freq data= pm2005;
table date / list;
 ods output OneWayFreqs = OneWayFreqs;
run;
ods trace off;

proc sort data = pm2005; by sitecode ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2005v2;
merge pm2005(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2005v2;
 class reg_id date;
   var pm25;
      output out=Mean_2005(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2005; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2005_v1;
merge Mean_2005(in=a) daily (in=b)  ;
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


proc sort data = Pm2005; by date   ;run; 


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


data Mean_2005_v4;
 set Mean_2005_v3;
  if mpm1 ne .            Then mpm_F = mpm1;
  if mpm1 = .             Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
run;


/*UNIQUE FOR 2005 SINCE JAN 18TH DOSENT HAVE ANY READING, WE AVERAGE THE DAY BEFORE AND AFTER*/

data Mean_2005_v4;
 set Mean_2005_v4;
  if date = '18jan2005'd and reg_id = 1 then mpm_f = mean(7.7,9.67);
  if date = '18jan2005'd and reg_id = 2 then mpm_f = mean(7.7,15.38);
  if date = '18jan2005'd and reg_id = 3 then mpm_f = mean(10.1,15.16);
run;


/*** Check no missing: ok ***/

proc means data = Mean_2005_v4 nmiss;
 var mpm_F ;
run;

proc sort data = Mean_2005_v4; by mpm_F   ;run; 




libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2005;
set Mean_2005_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2005
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2005_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2005_v4; by date reg_id   ;run;
proc sort data = mod2pred2005 ; by date reg_id ;run;

data mod2pred2005_V2;
merge mod2pred2005(in=a) Mean_2005_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2005_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2005_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2005_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2005_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2005;
set fullg.y2005 ;
run; 

 


proc sort data = Mean_2005_v4; by date reg_id   ;run;
proc sort data = fgrid2005 ;   by date reg_id ;run;

data fgrid2005_V2;
merge fgrid2005(in=a) Mean_2005_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2005_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2005_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2005_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2005.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2006*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2006;
set pm.pm2006;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2006; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2006v2;
merge pm2006(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2006v2;
 class reg_id date;
   var pm25;
      output out=Mean_2006(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2006; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2006_v1;
merge Mean_2006(in=a) daily (in=b)  ;
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


data Mean_2006_v4;
 set Mean_2006_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2006_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2006;
set Mean_2006_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2006
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2006_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2006_v4; by date reg_id   ;run;
proc sort data = mod2pred2006 ; by date reg_id ;run;

data mod2pred2006_V2;
merge mod2pred2006(in=a) Mean_2006_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2006_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2006_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2006_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2006_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2006;
set fullg.y2006 ;
run; 

 


proc sort data = Mean_2006_v4; by date reg_id   ;run;
proc sort data = fgrid2006 ;   by date reg_id ;run;

data fgrid2006_V2;
merge fgrid2006(in=a) Mean_2006_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2006_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2006_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2006_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2006.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2007*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2007;
set pm.pm2007;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2007; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2007v2;
merge pm2007(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2007v2;
 class reg_id date;
   var pm25;
      output out=Mean_2007(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2007; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2007_v1;
merge Mean_2007(in=a) daily (in=b)  ;
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


data Mean_2007_v4;
 set Mean_2007_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2007_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2007;
set Mean_2007_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2007
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2007_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2007_v4; by date reg_id   ;run;
proc sort data = mod2pred2007 ; by date reg_id ;run;

data mod2pred2007_V2;
merge mod2pred2007(in=a) Mean_2007_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2007_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2007_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2007_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2007_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2007;
set fullg.y2007 ;
run; 

 


proc sort data = Mean_2007_v4; by date reg_id   ;run;
proc sort data = fgrid2007 ;   by date reg_id ;run;

data fgrid2007_V2;
merge fgrid2007(in=a) Mean_2007_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2007_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2007_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2007_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2007.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;



/*2008*/




/*create a full complete 365 days for all mpm for every region*/




PROC IMPORT OUT= WORK.pmreg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\pmcode_region.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 


PROC IMPORT OUT= WORK.regXY
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\reg_centroids_XY.dbf" 
                  DBMS=DBF  REPLACE;
                 GETDELETED=NO;
               RUN; 



libname pm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset\' ;

data pm2008;
set pm.pm2008;
if pm25 < 0 then delete;
if pm25 > 100 then delete;
run; 

proc sort data = pm2008; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2008v2;
merge pm2008(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
  run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2008v2;
 class reg_id date;
   var pm25;
      output out=Mean_2008(drop = _Type_ _freq_) mean=mpm;
run; 


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


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2008; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2008_v1;
merge Mean_2008(in=a) daily (in=b)  ;
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


data Mean_2008_v4;
 set Mean_2008_v3;
  if mpm1 ne .                 Then mpm_F = mpm1;
  if mpm1 = .         Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = .  Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2008_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2008;
set Mean_2008_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2008
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2008_m2_pred.dbf" 
                   DBMS=DBF REPLACE;
                  GETDELETED=NO;
              RUN; 



proc sort data = Mean_2008_v4; by date reg_id   ;run;
proc sort data = mod2pred2008 ; by date reg_id ;run;

data mod2pred2008_V2;
merge mod2pred2008(in=a) Mean_2008_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


  data mod2pred2008_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
  set mod2pred2008_V2;
  m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;


  run; 


PROC EXPORT DATA= WORK.mod2pred2008_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2008_m2_pred_mpm.csv" 
                  DBMS=CSV REPLACE;
                 PUTNAMES=YES;
               RUN;
                


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2008;
set fullg.y2008 ;
run; 

 


proc sort data = Mean_2008_v4; by date reg_id   ;run;
proc sort data = fgrid2008 ;   by date reg_id ;run;

data fgrid2008_V2;
merge fgrid2008(in=a) Mean_2008_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
  run; 


data fgrid2008_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2008_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 


PROC EXPORT DATA= WORK.fgrid2008_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2008.csv" 
           DBMS=CSV REPLACE;
       PUTNAMES=YES;
 RUN;
                  

proc datasets lib=work kill; run;


