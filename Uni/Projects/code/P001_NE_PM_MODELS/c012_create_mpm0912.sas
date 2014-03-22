/*2010*/


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

data pm2010;
set pm.pm2010;
 if pm25 < 0 then delete;
 if pm25 > 100 then delete;
run; 

proc sort data = pm2010; by sitecode   ;run;
proc sort data = pmreg ; by sitecode ;run;

data pm2010v2;
merge pm2010(in=a) pmreg (in=b keep=sitecode reg_id)  ;
  by sitecode;
    if a;
	run; 


/*create mpm per region including missing days*/

proc summary nway data=pm2010v2;
 class reg_id date;
   var pm25;
      output out=Mean_2010(drop = _Type_ _freq_) mean=mpm;
run; 


/*create series of full days for each region*/



/**** 1 ****/

data seriesj_1;
 input Date date9. reg_id;
  format Date date9.;
datalines;
01jan2010 1
31dec2010 1
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
01jan2010 2
31dec2010 2
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
01jan2010 3
31dec2010 3
run;

proc expand data = seriesj_3 out=daily3 to=day method=step;
  convert reg_id = reg_id;
  id date;
run;


data daily;
set daily1 daily2 daily3  ;
run; 

proc sort data = Mean_2010; by date reg_id ;run;
proc sort data = daily;     by date reg_id  ;run;

data Mean_2010_v1;
merge Mean_2010(in=a) daily (in=b)  ;
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
proc sort data = Mean_2010_v1 ; by reg_id;run;

data Mean_2010_v2;
merge Mean_2010_v1(in=a) Unique (in=b)  ;
  by reg_id;
run;

proc sort data= Mean_2010_v2; by date reg_id; run;


data Mean_2010_v3(drop = reg1_id_fake--reg3_id_fake); 
 set Mean_2010_v2;
    id_M1 = 1*substr(reg1_id_fake,3,10);
    id_M2 = 1*substr(reg2_id_fake,3,10);
    id_M3 = 1*substr(reg3_id_fake,3,10);
run;


/*** Create the dataset with the closest station by distance ***/

/*** 1 ***/

data Mean_2010_v3_1(keep = id_M1 date mpm1); 
 set Mean_2010_v3;
  id_M1 = reg_id;
     mpm1=mpm;
run;

proc sort data =  Mean_2010_v3;    by id_M1 date; run;
proc sort data =  Mean_2010_v3_1;  by id_M1 date; run;

data Mean_2010_v3;
 merge Mean_2010_v3(in = a) Mean_2010_v3_1(in=b);
   by id_M1 date; 
    if a;
run;


/*** 2 ***/

data Mean_2010_v3_2(keep = id_M2 date mpm2); 
 set Mean_2010_v3;
  id_M2 = reg_id;
     mpm2=mpm;
run;

proc sort data =  Mean_2010_v3;    by id_M2 date; run;
proc sort data =  Mean_2010_v3_2;  by id_M2 date; run;

data Mean_2010_v3;
 merge Mean_2010_v3(in = a) Mean_2010_v3_2(in=b);
   by id_M2 date; 
    if a;
run;

/*** 3 ***/

data Mean_2010_v3_3(keep = id_M3 date mpm3); 
 set Mean_2010_v3;
  id_M3 = reg_id;
     mpm3=mpm;
run;

proc sort data =  Mean_2010_v3;    by id_M3 date; run;
proc sort data =  Mean_2010_v3_3;  by id_M3 date; run;

data Mean_2010_v3;
 merge Mean_2010_v3(in = a) Mean_2010_v3_3(in=b);
   by id_M3 date; 
    if a;
run;

proc sort data= Mean_2010_v3; by date reg_id; run;

/*create a full dataset where missing mpm days in regions get the mpm from the closest region*/


data Mean_2010_v4;
 set Mean_2010_v3;
  if mpm1 ne .              Then mpm_F = mpm1;
  if mpm1 = . 				Then mpm_F = mpm2;
  if mpm1 = . & mpm2 = . 	Then mpm_F = mpm3;
  run;



/*** Check no missing: ok ***/

proc means data = Mean_2010_v4 nmiss;
 var mpm_F ;
run;

libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

data mpm2.mpm2010;
set Mean_2010_v4;
run; 

 



/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mode2 predictions*/



PROC IMPORT OUT= WORK.mod2pred2010
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2010_m2_pred.dbf" 
		               DBMS=DBF REPLACE;
					        GETDELETED=NO;
							RUN; 



proc sort data = Mean_2010_v4; by date reg_id   ;run;
proc sort data = mod2pred2010 ; by date reg_id ;run;

data mod2pred2010_V2;
merge mod2pred2010(in=a) Mean_2010_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
	run; 


	data mod2pred2010_V3 (keep=guid Lat_AOD Long_AOD mpm_F bimon pred pred_si date );
	set mod2pred2010_V2;
	m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;
	run; 

	proc means data= mod2pred2010_V3 n min max mean std nmiss;
	var ; 
	run; 

symbol1 v=dot h=0.5 w=0.5 c=blue;
proc gplot data=mod2pred2010_V3;
Title "TITLE";
  plot pred*mpm_f/ grid;
     label VAR1 = "LABLE1";
	       label VAR2 = "LABLE2";
		   run; 
		   quit; 

PROC EXPORT DATA= WORK.mod2pred2010_V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN004_mod2pred\T2010_m2_pred_mpm.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


/*#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/*assign mpm for mod3 predictions*/


libname fullg 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;

data fgrid2010;
set fullg.y2010 ;
run; 

 proc univariate data=Fgrid2010;
var reg_id;
run;
 


proc sort data = Mean_2010_v4; by date reg_id   ;run;
proc sort data = fgrid2010 ;   by date reg_id ;run;

data fgrid2010_V2;
merge fgrid2010(in=a) Mean_2010_v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
	run; 


data fgrid2010_V3 (keep=guid Long_AOD Lat_AOD mpm_F bimon date);
set fgrid2010_V2;
 m = month(date); 
  if (m=1 or m=2) then bimon=1; 
  if (m=3 or m=4) then bimon=2;
  if (m=5 or m=6) then bimon=3;
  if (m=7 or m=8) then bimon=4;
  if (m=9 or m=10) then bimon=5;
  if (m=11 or m=12) then bimon=6;
run; 




PROC EXPORT DATA= WORK.fgrid2010_V3
     OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\fullgrid_mpm_2010.csv" 
           DBMS=CSV REPLACE;
	     PUTNAMES=YES;
 RUN;

proc means data=fgrid2010_V3 n min max mean std nmiss;
var ; 
run; 
 

proc datasets lib=work kill; run;

