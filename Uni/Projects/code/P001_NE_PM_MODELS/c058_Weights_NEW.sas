
/*import script cn001 outout*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.aod_2009  aod.aod_2010 aod.aod_2011;
run;

/*proc freq data=all_aod_00_10;*/
/*table guid*date / list;*/
/*run; */

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
01/01/2009 1
31/12/2011 1
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
/*DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; */
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


%year(year=2009);
%year(year=2010);
%year(year=2011);



/*export for full files before adding mpm*/

libname mod3 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN011_mod3_pre_mpm\' ;


%macro mod3expo(year=);

data mod3.y&year;
 set y&year;
  keep date guid long_aod lat_aod reg_id reg_name;
   run;

%mend mod3expo;

%mod3expo(year=2009);
%mod3expo(year=2010);
%mod3expo(year=2011);







/*JOIN MET DATA*/

/*Year 2009*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2009_v2;
set  metc.metc2009_v2;
run;





	proc sort data = y2009; by guid date   ;run;
	proc sort data = metc2009_v2 ; by guid date ;run;


/*for weights*/

data y2009_v6 (keep= date guid obs elev  temp_f m)  ;
	merge y2009(in=a) metc2009_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2009_v7  ;
	merge y2009(in=a) metc2009_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2009_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2009.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2009_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2009.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




/*JOIN MET DATA*/

/*Year 2010*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2010_v2;
set  metc.metc2010_v2;
run;





	proc sort data = y2010; by guid date   ;run;
	proc sort data = metc2010_v2 ; by guid date ;run;


/*for weights*/

data y2010_v6 (keep= date guid obs elev  temp_f m)  ;
	merge y2010(in=a) metc2010_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2010_v7  ;
	merge y2010(in=a) metc2010_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2010_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2010.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2010_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2010.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;



/*JOIN MET DATA*/

/*Year 2011*/


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2011_v2;
set  metc.metc2011_v2;
run;





	proc sort data = y2011; by guid date   ;run;
	proc sort data = metc2011_v2 ; by guid date ;run;


/*for weights*/

data y2011_v6 (keep= date guid obs elev  temp_f m)  ;
	merge y2011(in=a) metc2011_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if temp_f=. then delete;
		run; 

/*for mod2*/

data y2011_v7  ;
	merge y2011(in=a) metc2011_v2(in=b)  ;
	   by guid date;
	    if a;
		m = month(date); 
		if aod=. then delete;
		if temp_f=. then delete;
		run; 


PROC EXPORT DATA= WORK.Y2011_v6 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN009_Weights\y2011.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC EXPORT DATA= WORK.Y2011_v7 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN003_mod2\y2011.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


