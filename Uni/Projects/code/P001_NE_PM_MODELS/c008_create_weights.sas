
/*import script cn001 outout*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.all_aod;
run;


/*create grid by sorting by x,y and removing duplicates */


proc sort data = all_aod_00_10 nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 


/*clip grid*/


PROC IMPORT OUT= WORK.lu
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN004_LU_full_dataset\lu_emission.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



proc sort data = grid; by guid   ;run;
proc sort data = lu ; by guid ;run;

data grid(keep=guid lat_aod long_aod);
merge grid (in=a) lu (in=b)  ;
  by guid;
    if b;
	run; 


/*export for mod3 uniq grid*/

PROC EXPORT DATA= WORK.grid
           OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\uniq_grid.csv" 
		               DBMS=CSV REPLACE;
					        PUTNAMES=YES;
							RUN;
							 


/*	clip all aod*/


proc sort data = all_aod_00_10; by guid   ;run;
proc sort data = lu ; by guid ;run;

data all_aod_00_10 (keep=guid aod date lat_aod long_aod);
merge all_aod_00_10 (in=a) lu (in=b)  ;
  by guid;
    if b;
	run; 






/**** Create Data ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2000 1
31/12/2008 1
run;

/*creates the completed time series for above range*/
/*the output file is 'daily'*/

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

/*create a list of dates for cycle-first type macro*/

data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set Daily;
     if _n_ = 1 then do;
        elenco = trim(left(Date));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(Date));
      elenco_new = elenco;
       call symputx("Lista",elenco_new);
      output;
     end;
run;

%put &lista;

/*clear editor*/
DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; 
/*clear log*/
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


/*launch the macro*/

%put &Lista;

/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Daily;
  where date = &date;
run;

data Daily&date(keep = date guid Long_aod Lat_aod);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%mend full;

%full(List = &Lista);






/*MERGES*/


/*1)merge final (full empty grid) with the allaod0010 file with all AOD values*/






proc sort data= All_aod_00_10;
by guid date;
run;


proc sort data= final;
by guid date;
run;



data all_aod_merged;
merge final (in=a) All_aod_00_10 (in=b) ;
by guid date;
if a;
run;



data all_aod_merged_v2;
set all_aod_merged;
if aod = . then obs=0;
if aod ne . then obs=1;
run;





/*#2)Add land use data*/


/*NOTE: AFTER JONING TO LU YOU WILL GET MUCH MORE OBSR SINCE VALUES THAT ARE OUTSIDE THE TRUE STUDY REGION WILL NOT GET
ANY LU AND WILL BE DELETED*/

PROC IMPORT OUT= WORK.reg
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data= lu;
by guid;
run;

proc sort data= reg;
by guid;
run;


proc sort data= all_aod_merged_v2;
by guid;
run;


data all_aod_merged_v3;
merge  all_aod_merged_v2 lu reg (keep=guid reg_id reg_name)  ;
by guid;
run;


data all_aod_merged_v4;
set all_aod_merged_v3;
if elev =. then delete;
run;





/*divide all file into files based on year*/
option mprint;
%macro Year(year=);

data y&year;
 set all_aod_merged_v4;
  Y = Year(date);
   if Y = &year;
run;

%mend year;

%year(year=2000);
%year(year=2001);
%year(year=2002);
%year(year=2003);
%year(year=2004);
%year(year=2005);
%year(year=2006);
%year(year=2007);
%year(year=2008);



/*export for full files before adding mpm*/

libname mod3 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;


%macro mod3expo(year=);

data mod3.y&year;
 set y&year;
  keep date guid long_aod lat_aod reg_id reg_name;
   run;

%mend mod3expo;

%mod3expo(year=2000);
%mod3expo(year=2001);
%mod3expo(year=2002);
%mod3expo(year=2003);
%mod3expo(year=2004);
%mod3expo(year=2005);
%mod3expo(year=2006);
%mod3expo(year=2007);
%mod3expo(year=2008);






/*JOIN MET DATA*/

/*Year 2000*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2000;
set  metc.metc2000;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2000;
set  metguid.guid_stn_2000;
run;



proc sort data = y2000; by guid  ;run;
proc sort data = guid_stn_2000 ; by guid ;run;

data y2000_v5;
merge y2000(in=a) guid_stn_2000 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2000_v5; by stn date   ;run;
	proc sort data = metc2000 ; by stn date ;run;


/*for weights*/

data y2000_v6 (keep= date guid obs elev  temp_f m)  ;
	merge y2000_v5(in=a) metc2000(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 



/*for mod2*/

data y2000_v7  ;
	merge y2000_v5(in=a) metc2000(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2000_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2000.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2000_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2000.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






/*JOIN MET DATA*/

/*Year 2001*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2001;
set  metc.metc2001;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2001;
set  metguid.guid_stn_2001;
run;



proc sort data = y2001; by guid  ;run;
proc sort data = guid_stn_2001 ; by guid ;run;

data y2001_v5;
merge y2001(in=a) guid_stn_2001 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2001_v5; by stn date   ;run;
	proc sort data = metc2001 ; by stn date ;run;


/*for weights*/

data y2001_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2001_v5(in=a) metc2001(in=b)  ;
	   by stn date;
	    if a;
		m = month(date);
        if temp_f=. then delete; 
		run; 

/*for mod2*/

data y2001_v7  ;
	merge y2001_v5(in=a) metc2001(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2001_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2001.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2001_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2001.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






/*JOIN MET DATA*/

/*Year 2002*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2002;
set  metc.metc2002;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2002;
set  metguid.guid_stn_2002;
run;



proc sort data = y2002; by guid  ;run;
proc sort data = guid_stn_2002 ; by guid ;run;

data y2002_v5;
merge y2002(in=a) guid_stn_2002 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2002_v5; by stn date   ;run;
	proc sort data = metc2002 ; by stn date ;run;


/*for weights*/

data y2002_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2002_v5(in=a) metc2002(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2002_v7  ;
	merge y2002_v5(in=a) metc2002(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2002_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2002.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2002_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2002.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






/*JOIN MET DATA*/

/*Year 2003*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2003;
set  metc.metc2003;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2003;
set  metguid.guid_stn_2003;
run;



proc sort data = y2003; by guid  ;run;
proc sort data = guid_stn_2003 ; by guid ;run;

data y2003_v5;
merge y2003(in=a) guid_stn_2003 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2003_v5; by stn date   ;run;
	proc sort data = metc2003 ; by stn date ;run;


/*for weights*/

data y2003_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2003_v5(in=a) metc2003(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2003_v7  ;
	merge y2003_v5(in=a) metc2003(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2003_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2003.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2003_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2003.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;






/*JOIN MET DATA*/

/*Year 2004*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2004;
set  metc.metc2004;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2004;
set  metguid.guid_stn_2004;
run;



proc sort data = y2004; by guid  ;run;
proc sort data = guid_stn_2004 ; by guid ;run;

data y2004_v5;
merge y2004(in=a) guid_stn_2004 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2004_v5; by stn date   ;run;
	proc sort data = metc2004 ; by stn date ;run;


/*for weights*/

data y2004_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2004_v5(in=a) metc2004(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2004_v7  ;
	merge y2004_v5(in=a) metc2004(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2004_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2004.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2004_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2004.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;







/*JOIN MET DATA*/

/*Year 2005*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2005;
set  metc.metc2005;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2005;
set  metguid.guid_stn_2005;
run;



proc sort data = y2005; by guid  ;run;
proc sort data = guid_stn_2005 ; by guid ;run;

data y2005_v5;
merge y2005(in=a) guid_stn_2005 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2005_v5; by stn date   ;run;
	proc sort data = metc2005 ; by stn date ;run;


/*for weights*/

data y2005_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2005_v5(in=a) metc2005(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2005_v7  ;
	merge y2005_v5(in=a) metc2005(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2005_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2005.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2005_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2005.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;







/*JOIN MET DATA*/

/*Year 2006*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2006;
set  metc.metc2006;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2006;
set  metguid.guid_stn_2006;
run;



proc sort data = y2006; by guid  ;run;
proc sort data = guid_stn_2006 ; by guid ;run;

data y2006_v5;
merge y2006(in=a) guid_stn_2006 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2006_v5; by stn date   ;run;
	proc sort data = metc2006 ; by stn date ;run;


/*for weights*/

data y2006_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2006_v5(in=a) metc2006(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2006_v7  ;
	merge y2006_v5(in=a) metc2006(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2006_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2006.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2006_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2006.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;







/*JOIN MET DATA*/

/*Year 2007*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2007;
set  metc.metc2007;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2007;
set  metguid.guid_stn_2007;
run;



proc sort data = y2007; by guid  ;run;
proc sort data = guid_stn_2007 ; by guid ;run;

data y2007_v5;
merge y2007(in=a) guid_stn_2007 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2007_v5; by stn date   ;run;
	proc sort data = metc2007 ; by stn date ;run;


/*for weights*/

data y2007_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2007_v5(in=a) metc2007(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2007_v7  ;
	merge y2007_v5(in=a) metc2007(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2007_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2007.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2007_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2007.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




/*JOIN MET DATA*/

/*Year 2008*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2008;
set  metc.metc2008;
run;


libname metguid 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\' ;

data guid_stn_2008;
set  metguid.guid_stn_2008;
run;



proc sort data = y2008; by guid  ;run;
proc sort data = guid_stn_2008 ; by guid ;run;

data y2008_v5;
merge y2008(in=a) guid_stn_2008 (in=b keep=guid stn)  ;
  by  guid;
    if a;
	run; 


	proc sort data = y2008_v5; by stn date   ;run;
	proc sort data = metc2008 ; by stn date ;run;


/*for weights*/

data y2008_v6 (keep= date guid obs elev slp_f temp_f m)  ;
	merge y2008_v5(in=a) metc2008(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2008_v7  ;
	merge y2008_v5(in=a) metc2008(in=b)  ;
	   by stn date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2008_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2008.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2008_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2008.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




