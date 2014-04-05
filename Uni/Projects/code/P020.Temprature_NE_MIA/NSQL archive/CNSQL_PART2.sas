libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2001 (keep= date test_ave  glong glat);
set mods.Final60kmet2001;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2001_pred (keep= date pred glong glat fishid);
set mods.Mod2_2001_pred;
     run; 


proc sql;
  create table Mod2_2001_predV3  as
   select *
    from Mod2_2001_pred left join Final60kmet2001
     on Mod2_2001_pred.glong = Final60kmet2001.glong and Mod2_2001_pred.glat = Final60kmet2001.glat and Mod2_2001_pred.date = Final60kmet2001.date ;
run;

data mods.Mod2_2001_predV3;
set Mod2_2001_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2001_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2001&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2001     DATA=mods.solutionf2001&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2001; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2001_3;
merge mods.solutionf2001(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2001_3;
set solutionf2001_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2001 (drop=effect estimate StdErr);
set solutionf2001_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2001 (drop=effect estimate StdErr);
set solutionf2001_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2001 ; by glong glat   ;run;
proc sort data = slope2001 ; by glong glat ;run;

data fs;
merge Intercept2001(in=a) slope2001(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2001 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2001all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2001x  (drop=xx yy);
set mod3_2001;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2001 as
   select *
    from mod3_2001x left join grid 
     on mod3_2001x.glong = grid.glong and mod3_2001x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2001;
set Mod3_2001;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2001V3 as
   select *
    from Mod3_2001 left join Final60kmet2001 
     on Mod3_2001.glong = Final60kmet2001.glong and Mod3_2001.glat = Final60kmet2001.glat and Mod3_2001.date = Final60kmet2001.date;
run;
 
proc sql;
  create table mod3_2001fs as
   select *
    from Mod3_2001V3 left join fs 
     on Mod3_2001V3.glong = fs.glong and Mod3_2001V3.glat = fs.glat;
run;
 

data mod3_2001fs_pred;
 set mod3_2001fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2001fs_pred;
set mod3_2001fs_pred;
run; 

/*proc means data=mod3_2001fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2001fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2001 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2001
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2001.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2001 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2001.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2001
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2001.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2001; by station   ;run;
proc sort data = stn_closest_XXYY_2001 ; by station ;run;

data m1_met_2001;
merge m1_met_2001(in=a) stn_closest_XXYY_2001 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2001 (drop= xx yy);
set m1_met_2001;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2001 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2001.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2001  (drop= xx yy);*/
/*set guid_st_2001 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2001fs_pred left join guid_st_2001*/
/*     on Mod3_2001fs_pred.glong = guid_st_2001.glong and Mod3_2001fs_pred.glat = guid_st_2001.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2001 left join Mod3_2001fs_pred
     on m1_met_2001.glong = Mod3_2001fs_pred.glong and m1_met_2001.glat = Mod3_2001fs_pred.glat and  m1_met_2001.date = Mod3_2001fs_pred.date;
run;
 

title 'Correlations for mod3 t2001';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2001;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2002 (keep= date test_ave  glong glat);
set mods.Final60kmet2002;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2002_pred (keep= date pred glong glat fishid);
set mods.Mod2_2002_pred;
     run; 


proc sql;
  create table Mod2_2002_predV3  as
   select *
    from Mod2_2002_pred left join Final60kmet2002
     on Mod2_2002_pred.glong = Final60kmet2002.glong and Mod2_2002_pred.glat = Final60kmet2002.glat and Mod2_2002_pred.date = Final60kmet2002.date ;
run;

data mods.Mod2_2002_predV3;
set Mod2_2002_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2002_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2002&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2002     DATA=mods.solutionf2002&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2002; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2002_3;
merge mods.solutionf2002(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2002_3;
set solutionf2002_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2002 (drop=effect estimate StdErr);
set solutionf2002_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2002 (drop=effect estimate StdErr);
set solutionf2002_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2002 ; by glong glat   ;run;
proc sort data = slope2002 ; by glong glat ;run;

data fs;
merge Intercept2002(in=a) slope2002(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2002 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2002all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2002x  (drop=xx yy);
set mod3_2002;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2002 as
   select *
    from mod3_2002x left join grid 
     on mod3_2002x.glong = grid.glong and mod3_2002x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2002;
set Mod3_2002;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2002V3 as
   select *
    from Mod3_2002 left join Final60kmet2002 
     on Mod3_2002.glong = Final60kmet2002.glong and Mod3_2002.glat = Final60kmet2002.glat and Mod3_2002.date = Final60kmet2002.date;
run;
 
proc sql;
  create table mod3_2002fs as
   select *
    from Mod3_2002V3 left join fs 
     on Mod3_2002V3.glong = fs.glong and Mod3_2002V3.glat = fs.glat;
run;
 

data mod3_2002fs_pred;
 set mod3_2002fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2002fs_pred;
set mod3_2002fs_pred;
run; 

/*proc means data=mod3_2002fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2002fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2002 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2002
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2002.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2002 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2002.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2002
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2002.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2002; by station   ;run;
proc sort data = stn_closest_XXYY_2002 ; by station ;run;

data m1_met_2002;
merge m1_met_2002(in=a) stn_closest_XXYY_2002 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2002 (drop= xx yy);
set m1_met_2002;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2002 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2002.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2002  (drop= xx yy);*/
/*set guid_st_2002 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2002fs_pred left join guid_st_2002*/
/*     on Mod3_2002fs_pred.glong = guid_st_2002.glong and Mod3_2002fs_pred.glat = guid_st_2002.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2002 left join Mod3_2002fs_pred
     on m1_met_2002.glong = Mod3_2002fs_pred.glong and m1_met_2002.glat = Mod3_2002fs_pred.glat and  m1_met_2002.date = Mod3_2002fs_pred.date;
run;
 

title 'Correlations for mod3 t2002';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2002;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;





libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2003 (keep= date test_ave  glong glat);
set mods.Final60kmet2003;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2003_pred (keep= date pred glong glat fishid);
set mods.Mod2_2003_pred;
     run; 


proc sql;
  create table Mod2_2003_predV3  as
   select *
    from Mod2_2003_pred left join Final60kmet2003
     on Mod2_2003_pred.glong = Final60kmet2003.glong and Mod2_2003_pred.glat = Final60kmet2003.glat and Mod2_2003_pred.date = Final60kmet2003.date ;
run;

data mods.Mod2_2003_predV3;
set Mod2_2003_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2003_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2003&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2003     DATA=mods.solutionf2003&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2003; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2003_3;
merge mods.solutionf2003(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2003_3;
set solutionf2003_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2003 (drop=effect estimate StdErr);
set solutionf2003_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2003 (drop=effect estimate StdErr);
set solutionf2003_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2003 ; by glong glat   ;run;
proc sort data = slope2003 ; by glong glat ;run;

data fs;
merge Intercept2003(in=a) slope2003(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2003 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2003all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2003x  (drop=xx yy);
set mod3_2003;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2003 as
   select *
    from mod3_2003x left join grid 
     on mod3_2003x.glong = grid.glong and mod3_2003x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2003;
set Mod3_2003;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2003V3 as
   select *
    from Mod3_2003 left join Final60kmet2003 
     on Mod3_2003.glong = Final60kmet2003.glong and Mod3_2003.glat = Final60kmet2003.glat and Mod3_2003.date = Final60kmet2003.date;
run;
 
proc sql;
  create table mod3_2003fs as
   select *
    from Mod3_2003V3 left join fs 
     on Mod3_2003V3.glong = fs.glong and Mod3_2003V3.glat = fs.glat;
run;
 

data mod3_2003fs_pred;
 set mod3_2003fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2003fs_pred;
set mod3_2003fs_pred;
run; 

/*proc means data=mod3_2003fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2003fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2003 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2003
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2003 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2003; by station   ;run;
proc sort data = stn_closest_XXYY_2003 ; by station ;run;

data m1_met_2003;
merge m1_met_2003(in=a) stn_closest_XXYY_2003 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2003 (drop= xx yy);
set m1_met_2003;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2003 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2003.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2003  (drop= xx yy);*/
/*set guid_st_2003 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2003fs_pred left join guid_st_2003*/
/*     on Mod3_2003fs_pred.glong = guid_st_2003.glong and Mod3_2003fs_pred.glat = guid_st_2003.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2003 left join Mod3_2003fs_pred
     on m1_met_2003.glong = Mod3_2003fs_pred.glong and m1_met_2003.glat = Mod3_2003fs_pred.glat and  m1_met_2003.date = Mod3_2003fs_pred.date;
run;
 

title 'Correlations for mod3 t2003';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2003;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;
















libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2004 (keep= date test_ave  glong glat);
set mods.Final60kmet2004;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2004_pred (keep= date pred glong glat fishid);
set mods.Mod2_2004_pred;
     run; 


proc sql;
  create table Mod2_2004_predV3  as
   select *
    from Mod2_2004_pred left join Final60kmet2004
     on Mod2_2004_pred.glong = Final60kmet2004.glong and Mod2_2004_pred.glat = Final60kmet2004.glat and Mod2_2004_pred.date = Final60kmet2004.date ;
run;

data mods.Mod2_2004_predV3;
set Mod2_2004_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2004_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2004&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2004     DATA=mods.solutionf2004&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2004; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2004_3;
merge mods.solutionf2004(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2004_3;
set solutionf2004_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2004 (drop=effect estimate StdErr);
set solutionf2004_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2004 (drop=effect estimate StdErr);
set solutionf2004_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2004 ; by glong glat   ;run;
proc sort data = slope2004 ; by glong glat ;run;

data fs;
merge Intercept2004(in=a) slope2004(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2004 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2004all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2004x  (drop=xx yy);
set mod3_2004;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2004 as
   select *
    from mod3_2004x left join grid 
     on mod3_2004x.glong = grid.glong and mod3_2004x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2004;
set Mod3_2004;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2004V3 as
   select *
    from Mod3_2004 left join Final60kmet2004 
     on Mod3_2004.glong = Final60kmet2004.glong and Mod3_2004.glat = Final60kmet2004.glat and Mod3_2004.date = Final60kmet2004.date;
run;
 
proc sql;
  create table mod3_2004fs as
   select *
    from Mod3_2004V3 left join fs 
     on Mod3_2004V3.glong = fs.glong and Mod3_2004V3.glat = fs.glat;
run;
 

data mod3_2004fs_pred;
 set mod3_2004fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2004fs_pred;
set mod3_2004fs_pred;
run; 

/*proc means data=mod3_2004fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2004fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2004 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2004
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2004.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2004 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2004
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2004; by station   ;run;
proc sort data = stn_closest_XXYY_2004 ; by station ;run;

data m1_met_2004;
merge m1_met_2004(in=a) stn_closest_XXYY_2004 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2004 (drop= xx yy);
set m1_met_2004;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2004 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2004.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2004  (drop= xx yy);*/
/*set guid_st_2004 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2004fs_pred left join guid_st_2004*/
/*     on Mod3_2004fs_pred.glong = guid_st_2004.glong and Mod3_2004fs_pred.glat = guid_st_2004.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2004 left join Mod3_2004fs_pred
     on m1_met_2004.glong = Mod3_2004fs_pred.glong and m1_met_2004.glat = Mod3_2004fs_pred.glat and  m1_met_2004.date = Mod3_2004fs_pred.date;
run;
 

title 'Correlations for mod3 t2004';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2004;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;






libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2005 (keep= date test_ave  glong glat);
set mods.Final60kmet2005;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2005_pred (keep= date pred glong glat fishid);
set mods.Mod2_2005_pred;
     run; 


proc sql;
  create table Mod2_2005_predV3  as
   select *
    from Mod2_2005_pred left join Final60kmet2005
     on Mod2_2005_pred.glong = Final60kmet2005.glong and Mod2_2005_pred.glat = Final60kmet2005.glat and Mod2_2005_pred.date = Final60kmet2005.date ;
run;

data mods.Mod2_2005_predV3;
set Mod2_2005_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2005_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2005&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2005     DATA=mods.solutionf2005&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2005; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2005_3;
merge mods.solutionf2005(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2005_3;
set solutionf2005_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2005 (drop=effect estimate StdErr);
set solutionf2005_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2005 (drop=effect estimate StdErr);
set solutionf2005_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2005 ; by glong glat   ;run;
proc sort data = slope2005 ; by glong glat ;run;

data fs;
merge Intercept2005(in=a) slope2005(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2005 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2005all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2005x  (drop=xx yy);
set mod3_2005;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2005 as
   select *
    from mod3_2005x left join grid 
     on mod3_2005x.glong = grid.glong and mod3_2005x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2005;
set Mod3_2005;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2005V3 as
   select *
    from Mod3_2005 left join Final60kmet2005 
     on Mod3_2005.glong = Final60kmet2005.glong and Mod3_2005.glat = Final60kmet2005.glat and Mod3_2005.date = Final60kmet2005.date;
run;
 
proc sql;
  create table mod3_2005fs as
   select *
    from Mod3_2005V3 left join fs 
     on Mod3_2005V3.glong = fs.glong and Mod3_2005V3.glat = fs.glat;
run;
 

data mod3_2005fs_pred;
 set mod3_2005fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2005fs_pred;
set mod3_2005fs_pred;
run; 

/*proc means data=mod3_2005fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2005fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2005 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2005
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2005.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2005 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2005; by station   ;run;
proc sort data = stn_closest_XXYY_2005 ; by station ;run;

data m1_met_2005;
merge m1_met_2005(in=a) stn_closest_XXYY_2005 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2005 (drop= xx yy);
set m1_met_2005;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2005 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2005.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2005  (drop= xx yy);*/
/*set guid_st_2005 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2005fs_pred left join guid_st_2005*/
/*     on Mod3_2005fs_pred.glong = guid_st_2005.glong and Mod3_2005fs_pred.glat = guid_st_2005.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2005 left join Mod3_2005fs_pred
     on m1_met_2005.glong = Mod3_2005fs_pred.glong and m1_met_2005.glat = Mod3_2005fs_pred.glat and  m1_met_2005.date = Mod3_2005fs_pred.date;
run;
 

title 'Correlations for mod3 t2005';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2005;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;







libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2006 (keep= date test_ave  glong glat);
set mods.Final60kmet2006;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2006_pred (keep= date pred glong glat fishid);
set mods.Mod2_2006_pred;
     run; 


proc sql;
  create table Mod2_2006_predV3  as
   select *
    from Mod2_2006_pred left join Final60kmet2006
     on Mod2_2006_pred.glong = Final60kmet2006.glong and Mod2_2006_pred.glat = Final60kmet2006.glat and Mod2_2006_pred.date = Final60kmet2006.date ;
run;

data mods.Mod2_2006_predV3;
set Mod2_2006_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2006_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2006&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2006     DATA=mods.solutionf2006&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2006; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2006_3;
merge mods.solutionf2006(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2006_3;
set solutionf2006_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2006 (drop=effect estimate StdErr);
set solutionf2006_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2006 (drop=effect estimate StdErr);
set solutionf2006_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2006 ; by glong glat   ;run;
proc sort data = slope2006 ; by glong glat ;run;

data fs;
merge Intercept2006(in=a) slope2006(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2006 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2006all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2006x  (drop=xx yy);
set mod3_2006;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2006 as
   select *
    from mod3_2006x left join grid 
     on mod3_2006x.glong = grid.glong and mod3_2006x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2006;
set Mod3_2006;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2006V3 as
   select *
    from Mod3_2006 left join Final60kmet2006 
     on Mod3_2006.glong = Final60kmet2006.glong and Mod3_2006.glat = Final60kmet2006.glat and Mod3_2006.date = Final60kmet2006.date;
run;
 
proc sql;
  create table mod3_2006fs as
   select *
    from Mod3_2006V3 left join fs 
     on Mod3_2006V3.glong = fs.glong and Mod3_2006V3.glat = fs.glat;
run;
 

data mod3_2006fs_pred;
 set mod3_2006fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2006fs_pred;
set mod3_2006fs_pred;
run; 

/*proc means data=mod3_2006fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2006fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2006 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2006
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2006.dbf" 
			            DBMS=DBF REPLACE;
						RUN;


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2006 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\stn_closest_XXYY_2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = m1_met_2006; by station   ;run;
proc sort data = stn_closest_XXYY_2006 ; by station ;run;

data m1_met_2006;
merge m1_met_2006(in=a) stn_closest_XXYY_2006 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2006 (drop= xx yy);
set m1_met_2006;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*fix station*/

/*PROC IMPORT OUT= guid_st_2006 (keep= xx yy station)*/
/*            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2006.dbf" */
/*			            DMS=DBF   REPLACE;*/
/*						     GETDELETED=NO;*/
/*							 RUN; */
/**/
/*data guid_st_2006  (drop= xx yy);*/
/*set guid_st_2006 ;*/
/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/
/*run; */
/**/
/**/
/*proc sql;*/
/*  create table m3_cor as*/
/*   select **/
/*    from Mod3_2006fs_pred left join guid_st_2006*/
/*     on Mod3_2006fs_pred.glong = guid_st_2006.glong and Mod3_2006fs_pred.glat = guid_st_2006.glat ;*/
/*run;*/
/* */


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2006 left join Mod3_2006fs_pred
     on m1_met_2006.glong = Mod3_2006fs_pred.glong and m1_met_2006.glat = Mod3_2006fs_pred.glat and  m1_met_2006.date = Mod3_2006fs_pred.date;
run;
 

title 'Correlations for mod3 t2006';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2006;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;




libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2007 (keep= date test_ave  glong glat);
set mods.Final60kmet2007;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2007_pred (keep= date pred glong glat fishid);
set mods.Mod2_2007_pred;
     run; 


proc sql;
  create table Mod2_2007_predV3  as
   select *
    from Mod2_2007_pred left join Final60kmet2007
     on Mod2_2007_pred.glong = Final60kmet2007.glong and Mod2_2007_pred.glat = Final60kmet2007.glat and Mod2_2007_pred.date = Final60kmet2007.date ;
run;

data mods.Mod2_2007_predV3;
set Mod2_2007_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2007_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2007&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2007     DATA=mods.solutionf2007&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2007; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2007_3;
merge mods.solutionf2007(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2007_3;
set solutionf2007_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2007 (drop=effect estimate StdErr);
set solutionf2007_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2007 (drop=effect estimate StdErr);
set solutionf2007_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2007 ; by glong glat   ;run;
proc sort data = slope2007 ; by glong glat ;run;

data fs;
merge Intercept2007(in=a) slope2007(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2007 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2007all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2007x  (drop=xx yy);
set mod3_2007;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2007 as
   select *
    from mod3_2007x left join grid 
     on mod3_2007x.glong = grid.glong and mod3_2007x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2007;
set Mod3_2007;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2007V3 as
   select *
    from Mod3_2007 left join Final60kmet2007 
     on Mod3_2007.glong = Final60kmet2007.glong and Mod3_2007.glat = Final60kmet2007.glat and Mod3_2007.date = Final60kmet2007.date;
run;
 
proc sql;
  create table mod3_2007fs as
   select *
    from Mod3_2007V3 left join fs 
     on Mod3_2007V3.glong = fs.glong and Mod3_2007V3.glat = fs.glat;
run;
 

data mod3_2007fs_pred;
 set mod3_2007fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2007fs_pred;
set mod3_2007fs_pred;
run; 

/*proc means data=mod3_2007fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2007fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2007 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2007
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2007.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;


libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2008 (keep= date test_ave  glong glat);
set mods.Final60kmet2008;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2008_pred (keep= date pred glong glat fishid);
set mods.Mod2_2008_pred;
     run; 


proc sql;
  create table Mod2_2008_predV3  as
   select *
    from Mod2_2008_pred left join Final60kmet2008
     on Mod2_2008_pred.glong = Final60kmet2008.glong and Mod2_2008_pred.glat = Final60kmet2008.glat and Mod2_2008_pred.date = Final60kmet2008.date ;
run;

data mods.Mod2_2008_predV3;
set Mod2_2008_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2008_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2008&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2008     DATA=mods.solutionf2008&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2008; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2008_3;
merge mods.solutionf2008(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2008_3;
set solutionf2008_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2008 (drop=effect estimate StdErr);
set solutionf2008_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2008 (drop=effect estimate StdErr);
set solutionf2008_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2008 ; by glong glat   ;run;
proc sort data = slope2008 ; by glong glat ;run;

data fs;
merge Intercept2008(in=a) slope2008(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2008 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2008all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2008x  (drop=xx yy);
set mod3_2008;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2008 as
   select *
    from mod3_2008x left join grid 
     on mod3_2008x.glong = grid.glong and mod3_2008x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2008;
set Mod3_2008;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2008V3 as
   select *
    from Mod3_2008 left join Final60kmet2008 
     on Mod3_2008.glong = Final60kmet2008.glong and Mod3_2008.glat = Final60kmet2008.glat and Mod3_2008.date = Final60kmet2008.date;
run;
 
proc sql;
  create table mod3_2008fs as
   select *
    from Mod3_2008V3 left join fs 
     on Mod3_2008V3.glong = fs.glong and Mod3_2008V3.glat = fs.glat;
run;
 

data mod3_2008fs_pred;
 set mod3_2008fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2008fs_pred;
set mod3_2008fs_pred;
run; 

/*proc means data=mod3_2008fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2008fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2008 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2008
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;



libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2009 (keep= date test_ave  glong glat);
set mods.Final60kmet2009;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2009_pred (keep= date pred glong glat fishid);
set mods.Mod2_2009_pred;
     run; 


proc sql;
  create table Mod2_2009_predV3  as
   select *
    from Mod2_2009_pred left join Final60kmet2009
     on Mod2_2009_pred.glong = Final60kmet2009.glong and Mod2_2009_pred.glat = Final60kmet2009.glat and Mod2_2009_pred.date = Final60kmet2009.date ;
run;

data mods.Mod2_2009_predV3;
set Mod2_2009_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2009_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2009&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2009     DATA=mods.solutionf2009&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2009; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2009_3;
merge mods.solutionf2009(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2009_3;
set solutionf2009_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2009 (drop=effect estimate StdErr);
set solutionf2009_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2009 (drop=effect estimate StdErr);
set solutionf2009_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2009 ; by glong glat   ;run;
proc sort data = slope2009 ; by glong glat ;run;

data fs;
merge Intercept2009(in=a) slope2009(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2009 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2009all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2009x  (drop=xx yy);
set mod3_2009;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2009 as
   select *
    from mod3_2009x left join grid 
     on mod3_2009x.glong = grid.glong and mod3_2009x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2009;
set Mod3_2009;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2009V3 as
   select *
    from Mod3_2009 left join Final60kmet2009 
     on Mod3_2009.glong = Final60kmet2009.glong and Mod3_2009.glat = Final60kmet2009.glat and Mod3_2009.date = Final60kmet2009.date;
run;
 
proc sql;
  create table mod3_2009fs as
   select *
    from Mod3_2009V3 left join fs 
     on Mod3_2009V3.glong = fs.glong and Mod3_2009V3.glat = fs.glat;
run;
 

data mod3_2009fs_pred;
 set mod3_2009fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2009fs_pred;
set mod3_2009fs_pred;
run; 

/*proc means data=mod3_2009fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2009fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2009 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2009
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2009.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;


libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2010 (keep= date test_ave  glong glat);
set mods.Final60kmet2010;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2010_pred (keep= date pred glong glat fishid);
set mods.Mod2_2010_pred;
     run; 


proc sql;
  create table Mod2_2010_predV3  as
   select *
    from Mod2_2010_pred left join Final60kmet2010
     on Mod2_2010_pred.glong = Final60kmet2010.glong and Mod2_2010_pred.glat = Final60kmet2010.glat and Mod2_2010_pred.date = Final60kmet2010.date ;
run;

data mods.Mod2_2010_predV3;
set Mod2_2010_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2010_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2010&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2010     DATA=mods.solutionf2010&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2010; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2010_3;
merge mods.solutionf2010(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2010_3;
set solutionf2010_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2010 (drop=effect estimate StdErr);
set solutionf2010_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2010 (drop=effect estimate StdErr);
set solutionf2010_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2010 ; by glong glat   ;run;
proc sort data = slope2010 ; by glong glat ;run;

data fs;
merge Intercept2010(in=a) slope2010(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2010 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2010all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2010x  (drop=xx yy);
set mod3_2010;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2010 as
   select *
    from mod3_2010x left join grid 
     on mod3_2010x.glong = grid.glong and mod3_2010x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2010;
set Mod3_2010;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2010V3 as
   select *
    from Mod3_2010 left join Final60kmet2010 
     on Mod3_2010.glong = Final60kmet2010.glong and Mod3_2010.glat = Final60kmet2010.glat and Mod3_2010.date = Final60kmet2010.date;
run;
 
proc sql;
  create table mod3_2010fs as
   select *
    from Mod3_2010V3 left join fs 
     on Mod3_2010V3.glong = fs.glong and Mod3_2010V3.glat = fs.glat;
run;
 

data mod3_2010fs_pred;
 set mod3_2010fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2010fs_pred;
set mod3_2010fs_pred;
run; 

/*proc means data=mod3_2010fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2010fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2010 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2010
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2010.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;






libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*END model that works*/
/*END model that works*/
/*END model that works*/


data Final60kmet2011 (keep= date test_ave  glong glat);
set mods.Final60kmet2011;
     glong= round(xx,0.00001);
     glat= round(yy,0.00001);
run; 

data Mod2_2011_pred (keep= date pred glong glat fishid);
set mods.Mod2_2011_pred;
     run; 


proc sql;
  create table Mod2_2011_predV3  as
   select *
    from Mod2_2011_pred left join Final60kmet2011
     on Mod2_2011_pred.glong = Final60kmet2011.glong and Mod2_2011_pred.glat = Final60kmet2011.glat and Mod2_2011_pred.date = Final60kmet2011.date ;
run;

data mods.Mod2_2011_predV3;
set Mod2_2011_predV3 ;
if Test_Ave =. then delete;
run; 

ods listing close;*to suppress the output printing;

/*note there may be mising due to small areas being joined*/

%macro Region;
 
%do i=1 %to 80;
 
data data&i;
  set mods.Mod2_2011_predV3;
where fishid = &i;
run;

proc mixed data= data&i  method=reml;
model pred =  Test_Ave /s ;
ods output solutionf = mods.solutionf2011&i ;
by glong glat;
run;
quit;


%end;
 
%mend;
 
%Region;



%macro Region;
 
%do i=1 %to 80;

PROC APPEND BASE=mods.solutionf2011     DATA=mods.solutionf2011&i;
RUN;


%end;
 
%mend;
 
%Region;


/*-----------------------------------------------------------*/
/*mod3 initial regression and extract slopes and intercepts*/
/*-----------------------------------------------------------*/

PROC IMPORT OUT= grid
                            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf"
						    DBMS=DBF   REPLACE;
						    GETDELETED=NO;
							RUN;



data grid (drop=xx yy);
set grid;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

Proc sort data = mods.solutionf2011; by glong glat   ;run;
proc sort data = grid ; by glong glat ;run;


data solutionf2011_3;
merge mods.solutionf2011(in=a ) grid (in=b)  ;
  by glong glat;
    if a;
	run; 

data solutionf2011_3;
set solutionf2011_3;
keep StdErr effect guid estimate glong glat;
run; 


data slope2011 (drop=effect estimate StdErr);
set solutionf2011_3;
where effect="TEST_AVE";
slope_tempc=estimate;
StdErr_tempc=StdErr;
run;



data intercept2011 (drop=effect estimate StdErr);
set solutionf2011_3;
where effect="Intercept";
slope_inter=estimate;
StdErr_inter=StdErr;
run;





proc sort data = Intercept2011 ; by glong glat   ;run;
proc sort data = slope2011 ; by glong glat ;run;

data fs;
merge Intercept2011(in=a) slope2011(in=b)  ;
  by glong glat;
    if a;
	run; 

data fs (keep=newinter newslope glong glat);
set fs;
newinter=slope_inter;
newslope=slope_tempc;
run; 





/*-----------------------------------------------------------*/
/*import full grid for every grid/day combo */
/*-----------------------------------------------------------*/

PROC IMPORT OUT= mod3_2011 (drop=tempc ws guid)
  DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2011all.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
	    GUESSINGROWS=500000;
		RUN;
	
data mod3_2011x  (drop=xx yy);
set mod3_2011;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run;

proc sql;
  create table Mod3_2011 as
   select *
    from mod3_2011x left join grid 
     on mod3_2011x.glong = grid.glong and mod3_2011x.glat = grid.glat;
run;
 
/*delete water and outside map points*/
		
data Mod3_2011;
set Mod3_2011;
if near_water=1 then delete;
if near_water=. then delete;
run; 

		 


proc sql;
  create table Mod3_2011V3 as
   select *
    from Mod3_2011 left join Final60kmet2011 
     on Mod3_2011.glong = Final60kmet2011.glong and Mod3_2011.glat = Final60kmet2011.glat and Mod3_2011.date = Final60kmet2011.date;
run;
 
proc sql;
  create table mod3_2011fs as
   select *
    from Mod3_2011V3 left join fs 
     on Mod3_2011V3.glong = fs.glong and Mod3_2011V3.glat = fs.glat;
run;
 

data mod3_2011fs_pred;
 set mod3_2011fs;
  pred_m3 = newinter +  TEST_AVE*newslope;
/*  drop station;*/
run;

data mods.mod3_2011fs_pred;
set mod3_2011fs_pred;
run; 

/*proc means data=mod3_2011fs_pred n min max mean std nmiss;*/
/*var ; */
/*run; */

proc summary nway data=mod3_2011fs_pred;
 class glat glong;
 var pred_m3;
 output out=out2011 mean=pred_m3;
 run; 

/*#yearly map*/

PROC EXPORT DATA= out2011
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt2011.dbf" 
			            DBMS=DBF REPLACE;
						RUN;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

proc datasets lib=work kill nolist memtype=data;
quit;

