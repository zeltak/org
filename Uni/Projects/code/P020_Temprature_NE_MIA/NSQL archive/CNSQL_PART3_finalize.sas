/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;




/*1.CREATE LONG TERM PRED3 MAP*/

Options mprint;
%macro import(year=);

PROC IMPORT OUT= t&year
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\lt&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

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


Data tall;
set t2000  t2001 t2002 t2003 t2004 t2005 t2006 t2007 t2008 t2009 t2010 t2011;
run; 


Proc summary nway data=tall;
class glong glat;
var pred_m3;
output out=OUTPUTFILE mean=pred_m3;
run; 



PROC EXPORT DATA= OUTPUTFILE
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_002_longterm_maps\ltALL.dbf" 
			            DBMS=DBF REPLACE;
						RUN;




/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2000fs_pred;
set mods.Mod3_2000fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2000;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2000_predv3 left join mod1
     on Mod2_2000_predv3.glong = mod1.glong and Mod2_2000_predv3.glat = mod1.glat and Mod2_2000_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2000fs_pred left join AS2
     on Mod3_2000fs_pred.glong = AS2.glong and Mod3_2000fs_pred.glat = AS2.glat and Mod3_2000fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2000 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2001fs_pred;
set mods.Mod3_2001fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2001;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2001_predv3 left join mod1
     on Mod2_2001_predv3.glong = mod1.glong and Mod2_2001_predv3.glat = mod1.glat and Mod2_2001_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2001fs_pred left join AS2
     on Mod3_2001fs_pred.glong = AS2.glong and Mod3_2001fs_pred.glat = AS2.glat and Mod3_2001fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2001 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2002fs_pred;
set mods.Mod3_2002fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2002;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2002_predv3 left join mod1
     on Mod2_2002_predv3.glong = mod1.glong and Mod2_2002_predv3.glat = mod1.glat and Mod2_2002_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2002fs_pred left join AS2
     on Mod3_2002fs_pred.glong = AS2.glong and Mod3_2002fs_pred.glat = AS2.glat and Mod3_2002fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2002 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2003fs_pred;
set mods.Mod3_2003fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2003;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2003_predv3 left join mod1
     on Mod2_2003_predv3.glong = mod1.glong and Mod2_2003_predv3.glat = mod1.glat and Mod2_2003_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2003fs_pred left join AS2
     on Mod3_2003fs_pred.glong = AS2.glong and Mod3_2003fs_pred.glat = AS2.glat and Mod3_2003fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2003 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2004fs_pred;
set mods.Mod3_2004fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2004;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2004_predv3 left join mod1
     on Mod2_2004_predv3.glong = mod1.glong and Mod2_2004_predv3.glat = mod1.glat and Mod2_2004_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2004fs_pred left join AS2
     on Mod3_2004fs_pred.glong = AS2.glong and Mod3_2004fs_pred.glat = AS2.glat and Mod3_2004fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2004 (keep=glong glat date fintemp);
set as4;
run; 



/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2005fs_pred;
set mods.Mod3_2005fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2005;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2005_predv3 left join mod1
     on Mod2_2005_predv3.glong = mod1.glong and Mod2_2005_predv3.glat = mod1.glat and Mod2_2005_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2005fs_pred left join AS2
     on Mod3_2005fs_pred.glong = AS2.glong and Mod3_2005fs_pred.glat = AS2.glat and Mod3_2005fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2005 (keep=glong glat date fintemp);
set as4;
run; 



/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2006fs_pred;
set mods.Mod3_2006fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2006;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2006_predv3 left join mod1
     on Mod2_2006_predv3.glong = mod1.glong and Mod2_2006_predv3.glat = mod1.glat and Mod2_2006_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2006fs_pred left join AS2
     on Mod3_2006fs_pred.glong = AS2.glong and Mod3_2006fs_pred.glat = AS2.glat and Mod3_2006fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2006 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2007fs_pred;
set mods.Mod3_2007fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2007;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2007_predv3 left join mod1
     on Mod2_2007_predv3.glong = mod1.glong and Mod2_2007_predv3.glat = mod1.glat and Mod2_2007_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2007fs_pred left join AS2
     on Mod3_2007fs_pred.glong = AS2.glong and Mod3_2007fs_pred.glat = AS2.glat and Mod3_2007fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2007 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2008fs_pred;
set mods.Mod3_2008fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2008;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2008_predv3 left join mod1
     on Mod2_2008_predv3.glong = mod1.glong and Mod2_2008_predv3.glat = mod1.glat and Mod2_2008_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2008fs_pred left join AS2
     on Mod3_2008fs_pred.glong = AS2.glong and Mod3_2008fs_pred.glat = AS2.glat and Mod3_2008fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2008 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2009fs_pred;
set mods.Mod3_2009fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2009;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2009_predv3 left join mod1
     on Mod2_2009_predv3.glong = mod1.glong and Mod2_2009_predv3.glat = mod1.glat and Mod2_2009_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2009fs_pred left join AS2
     on Mod3_2009fs_pred.glong = AS2.glong and Mod3_2009fs_pred.glat = AS2.glat and Mod3_2009fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2009 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2010fs_pred;
set mods.Mod3_2010fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2010;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2010_predv3 left join mod1
     on Mod2_2010_predv3.glong = mod1.glong and Mod2_2010_predv3.glat = mod1.glat and Mod2_2010_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2010fs_pred left join AS2
     on Mod3_2010fs_pred.glong = AS2.glong and Mod3_2010fs_pred.glat = AS2.glat and Mod3_2010fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2010 (keep=glong glat date fintemp);
set as4;
run; 

/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;



/*2.CREATE USED PRED MAP*/

Data mods.Mod3_2011fs_pred;
set mods.Mod3_2011fs_pred;
keep date glong glat test_ave pred_m3;
run; 

Data mod1;
set mods.Mod1_2011;
keep date glong glat tempc;
run; 


proc sql;
  create table AS2  as
   select *
    from mods.Mod2_2011_predv3 left join mod1
     on Mod2_2011_predv3.glong = mod1.glong and Mod2_2011_predv3.glat = mod1.glat and Mod2_2011_predv3.date = mod1.date ;
run;


Proc means data=AS2 n min max mean std nmiss;
var ; 
run; 


proc sql;
  create table AS3  as
   select *
    from mods.Mod3_2011fs_pred left join AS2
     on Mod3_2011fs_pred.glong = AS2.glong and Mod3_2011fs_pred.glat = AS2.glat and Mod3_2011fs_pred.date = AS2.date ;
run;


data AS4;
set AS3;
fintemp = pred_m3;
if pred ne . then fintemp=pred;
if tempc ne . then fintemp=pred;
run;


Data mods.Fintmpc_2011 (keep=glong glat date fintemp);
set as4;
run; 




 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2003 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2003; by station  dist;

data stn_closest_XXYY_2003; set stn_closest_XXYY_2003; by station  dist;
if first.station;
run;

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


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2003 left join mods.Mod3_2003fs_pred
     on m1_met_2003.glong = Mod3_2003fs_pred.glong and m1_met_2003.glat = Mod3_2003fs_pred.glat and  m1_met_2003.date = Mod3_2003fs_pred.date;
run;
 

title 'Correlations for mod3 t2003';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2003;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

/*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2003 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2003; by station  dist;

data stn_closest_XXYY_2003; set stn_closest_XXYY_2003; by station  dist;
if first.station;
run;

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


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2003 left join mods.Mod3_2003fs_pred
     on m1_met_2003.glong = Mod3_2003fs_pred.glong and m1_met_2003.glat = Mod3_2003fs_pred.glat and  m1_met_2003.date = Mod3_2003fs_pred.date;
run;
 

title 'Correlations for mod3 t2003';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2003;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2005 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2005; by station  dist;

data stn_closest_XXYY_2005; set stn_closest_XXYY_2005; by station  dist;
if first.station;
run;

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


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2005 left join mods.Mod3_2005fs_pred
     on m1_met_2005.glong = Mod3_2005fs_pred.glong and m1_met_2005.glat = Mod3_2005fs_pred.glat and  m1_met_2005.date = Mod3_2005fs_pred.date;
run;
 

title 'Correlations for mod3 t2005';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2005;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2006 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2006; by station  dist;

data stn_closest_XXYY_2006; set stn_closest_XXYY_2006; by station  dist;
if first.station;
run;

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


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2006 left join mods.Mod3_2006fs_pred
     on m1_met_2006.glong = Mod3_2006fs_pred.glong and m1_met_2006.glat = Mod3_2006fs_pred.glat and  m1_met_2006.date = Mod3_2006fs_pred.date;
run;
 

title 'Correlations for mod3 t2006';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2006;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2007 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2007; by station  dist;

data stn_closest_XXYY_2007; set stn_closest_XXYY_2007; by station  dist;
if first.station;
run;

proc sort data = m1_met_2007; by station   ;run;
proc sort data = stn_closest_XXYY_2007 ; by station ;run;

data m1_met_2007;
merge m1_met_2007(in=a) stn_closest_XXYY_2007 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2007 (drop= xx yy);
set m1_met_2007;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2007 left join mods.Mod3_2007fs_pred
     on m1_met_2007.glong = Mod3_2007fs_pred.glong and m1_met_2007.glat = Mod3_2007fs_pred.glat and  m1_met_2007.date = Mod3_2007fs_pred.date;
run;
 

title 'Correlations for mod3 t2007';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2007;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2008 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2008; by station  dist;

data stn_closest_XXYY_2008; set stn_closest_XXYY_2008; by station  dist;
if first.station;
run;

proc sort data = m1_met_2008; by station   ;run;
proc sort data = stn_closest_XXYY_2008 ; by station ;run;

data m1_met_2008;
merge m1_met_2008(in=a) stn_closest_XXYY_2008 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2008 (drop= xx yy);
set m1_met_2008;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2008 left join mods.Mod3_2008fs_pred
     on m1_met_2008.glong = Mod3_2008fs_pred.glong and m1_met_2008.glat = Mod3_2008fs_pred.glat and  m1_met_2008.date = Mod3_2008fs_pred.date;
run;
 

title 'Correlations for mod3 t2008';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2008;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2009 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2009; by station  dist;

data stn_closest_XXYY_2009; set stn_closest_XXYY_2009; by station  dist;
if first.station;
run;

proc sort data = m1_met_2009; by station   ;run;
proc sort data = stn_closest_XXYY_2009 ; by station ;run;

data m1_met_2009;
merge m1_met_2009(in=a) stn_closest_XXYY_2009 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2009 (drop= xx yy);
set m1_met_2009;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2009 left join mods.Mod3_2009fs_pred
     on m1_met_2009.glong = Mod3_2009fs_pred.glong and m1_met_2009.glat = Mod3_2009fs_pred.glat and  m1_met_2009.date = Mod3_2009fs_pred.date;
run;
 

title 'Correlations for mod3 t2009';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2009;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/


 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2010 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2010; by station  dist;

data stn_closest_XXYY_2010; set stn_closest_XXYY_2010; by station  dist;
if first.station;
run;

proc sort data = m1_met_2010; by station   ;run;
proc sort data = stn_closest_XXYY_2010 ; by station ;run;

data m1_met_2010;
merge m1_met_2010(in=a) stn_closest_XXYY_2010 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2010 (drop= xx yy);
set m1_met_2010;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2010 left join mods.Mod3_2010fs_pred
     on m1_met_2010.glong = Mod3_2010fs_pred.glong and m1_met_2010.glat = Mod3_2010fs_pred.glat and  m1_met_2010.date = Mod3_2010fs_pred.date;
run;
 

title 'Correlations for mod3 t2010';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2010;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/

 /*-----------------------------------------------------------*/
/*check R2*/
/*-----------------------------------------------------------*/



PROC IMPORT OUT= m1_met_2011 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= stn_closest_XXYY_2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2011; by station  dist;

data stn_closest_XXYY_2011; set stn_closest_XXYY_2011; by station  dist;
if first.station;
run;

proc sort data = m1_met_2011; by station   ;run;
proc sort data = stn_closest_XXYY_2011 ; by station ;run;

data m1_met_2011;
merge m1_met_2011(in=a) stn_closest_XXYY_2011 (in=b)  ;
  by station;
    if a;
	run; 

data m1_met_2011 (drop= xx yy);
set m1_met_2011;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 


/*LIBRARIES*/

libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;


proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2011 left join mods.Mod3_2011fs_pred
     on m1_met_2011.glong = Mod3_2011fs_pred.glong and m1_met_2011.glat = Mod3_2011fs_pred.glat and  m1_met_2011.date = Mod3_2011fs_pred.date;
run;
 

title 'Correlations for mod3 t2011';
proc corr data =  m3_cor_resxy  nomiss outp=mods.CorrOutp2011;
 var tempc pred_m3;
 run;

/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
/*CLEAN WORKSPACE*/
