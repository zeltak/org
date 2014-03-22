libname aodpm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN003_PM_AOD_Combined\' ;


data aodpm2010;
set aodpm.t2009;
run; 


/*CREATE GUID AND SEASON */


/*copy x and y coordinantes from numeric to character (text) variables*/

   data aodpm2009_v3  ;
      set aodpm2009  ;
      xnym=put(Long_aod,6.2); 
	  ynym=put(Lat_aod,6.2); 
	  run;

	  data  aodpm2009_v4 ;
      set  aodpm2009_v3 ;
      xnymx = xnym*-100; 
	  ynymx = ynym*100; 
	  run;

/*concentrate (compress) both x and y variables into one ID*/

      data  aodpm2009_v5;
      set  aodpm2009_v4;
      guid=compress(xnymx||ynymx);
	  run;

/*convert if from text to numeric*/

      data aodpm2009_v6;
      set  aodpm2009_v5;
	  drop  xnymx ynymx xnym ynym;
	  guid2=input(guid, 8.); 
      drop guid;
  	  run;


	  data aodpm2009;
      set  aodpm2009_v6;
	  guid=guid2; 
      drop guid2;
  	  run;

/*****************************/


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

proc sort data= Aodpm2009;
by guid;
run;


data Aodpm2009_v6;
merge Aodpm2009(in=a) lu (in=b keep=guid pop_sqkm elev A1_dist_km p_urban p_open area_pm point_pm population ) ;
by guid;
if a;
run;


/*****************************************************************************************************************/
/*CREATE A GRID OF THE ARAE*/

/*import script cn001 outout*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.aod_2009  aod.aod_2010 aod.aod_2011;
run;

/* No dupkey in all AOD available */


/*create grid by sorting by x,y and removing duplicates */


proc sort data = all_aod_00_10 nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 



/*export for mod3 uniq grid*/

PROC EXPORT DATA= WORK.grid
           OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\uniq_grid.csv" 
		               DBMS=CSV REPLACE;
					        PUTNAMES=YES;
							RUN;
							 


/*	clip all aod only for NE */


proc sort data = all_aod_00_10; by guid ;run;
proc sort data = lu;            by guid ;run;

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








/*****************************************************************************************************************/

data final2009 (where=(date>="01JAN2009"D and date<="31DEC2009"D )) ;
set final;
run; 


PROC IMPORT OUT= WORK.guidstn(keep = guid stn)
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_stn_0911.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



proc sort data = final2009; by guid   ;run;
proc sort data = guidstn ; by guid ;run;

data guid2009;
merge final2009(in=a) guidstn (in=b)  ;
  by guid ;
    if a;
	run; 


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2009;
set  metc.metc2009;
run;



proc sort data = metc2009;  by stn date;run;
proc sort data = guid2009 ; by stn date;run;

data metc2009_v2;
merge metc2009(in=a keep=stn temp_f wdsp_f visib_f date ah_gm3_F lat_met long_met) guid2009 (in=b)  ;
  by stn date;
    if b;
	run; 


data metc.metc2009_v2;
set metc2009_v2;
run; 

/*join mod1 files with common met data*/


proc sort data = Aodpm2009_v6; by guid date ;run;
proc sort data = metc2009_v2 ; by guid date ;run;

data out2009_3;
merge Aodpm2009_v6(in=a) metc2009_v2 (in=b)  ;
  by guid date;
    if a;
	run; 

PROC EXPORT DATA= WORK.out2009_3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t2009.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


proc datasets lib=work kill; run;


libname aodpm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN003_PM_AOD_Combined\' ;


data aodpm2010;
set aodpm.t2010;
run; 


/*CREATE GUID AND SEASON */


/*copy x and y coordinantes from numeric to character (text) variables*/

   data aodpm2010_v3  ;
      set aodpm2010  ;
      xnym=put(Long_aod,6.2); 
	  ynym=put(Lat_aod,6.2); 
	  run;

	  data  aodpm2010_v4 ;
      set  aodpm2010_v3 ;
      xnymx = xnym*-100; 
	  ynymx = ynym*100; 
	  run;

/*concentrate (compress) both x and y variables into one ID*/

      data  aodpm2010_v5;
      set  aodpm2010_v4;
      guid=compress(xnymx||ynymx);
	  run;

/*convert if from text to numeric*/

      data aodpm2010_v6;
      set  aodpm2010_v5;
	  drop  xnymx ynymx xnym ynym;
	  guid2=input(guid, 8.); 
      drop guid;
  	  run;


	  data aodpm2010;
      set  aodpm2010_v6;
	  guid=guid2; 
      drop guid2;
  	  run;

/*****************************/


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

proc sort data= Aodpm2010;
by guid;
run;


data Aodpm2010_v6;
merge Aodpm2010(in=a) lu (in=b keep=guid pop_sqkm elev A1_dist_km p_urban p_open area_pm point_pm population ) ;
by guid;
if a;
run;


/*****************************************************************************************************************/
/*CREATE A GRID OF THE ARAE*/

/*import script cn001 outout*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.aod_2010  aod.aod_2010 aod.aod_2011;
run;

/* No dupkey in all AOD available */


/*create grid by sorting by x,y and removing duplicates */


proc sort data = all_aod_00_10 nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 



/*export for mod3 uniq grid*/

PROC EXPORT DATA= WORK.grid
           OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\uniq_grid.csv" 
		               DBMS=CSV REPLACE;
					        PUTNAMES=YES;
							RUN;
							 


/*	clip all aod only for NE */


proc sort data = all_aod_00_10; by guid ;run;
proc sort data = lu;            by guid ;run;

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
01/01/2010 1
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


/*****************************************************************************************************************/

data final2010 (where=(date>="01JAN2010"D and date<="31DEC2010"D )) ;
set final;
run; 


PROC IMPORT OUT= WORK.guidstn(keep = guid stn)
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_stn_0911.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



proc sort data = final2010; by guid   ;run;
proc sort data = guidstn ; by guid ;run;

data guid2010;
merge final2010(in=a) guidstn (in=b)  ;
  by guid ;
    if a;
	run; 


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2010;
set  metc.metc2010;
run;



proc sort data = metc2010;  by stn date;run;
proc sort data = guid2010 ; by stn date;run;

data metc2010_v2;
merge metc2010(in=a keep=stn temp_f wdsp_f visib_f date ah_gm3_F lat_met long_met) guid2010 (in=b)  ;
  by stn date;
    if b;
	run; 


data metc.metc2010_v2;
set metc2010_v2;
run; 

/*join mod1 files with common met data*/


proc sort data = Aodpm2010_v6; by guid date ;run;
proc sort data = metc2010_v2 ; by guid date ;run;

data out2010_3;
merge Aodpm2010_v6(in=a) metc2010_v2 (in=b)  ;
  by guid date;
    if a;
	run; 

PROC EXPORT DATA= WORK.out2010_3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t2010.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;



proc datasets lib=work kill; run;



libname aodpm 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN003_PM_AOD_Combined\' ;


data aodpm2011;
set aodpm.t2011;
run; 


/*CREATE GUID AND SEASON */


/*copy x and y coordinantes from numeric to character (text) variables*/

   data aodpm2011_v3  ;
      set aodpm2011  ;
      xnym=put(Long_aod,6.2); 
	  ynym=put(Lat_aod,6.2); 
	  run;

	  data  aodpm2011_v4 ;
      set  aodpm2011_v3 ;
      xnymx = xnym*-100; 
	  ynymx = ynym*100; 
	  run;

/*concentrate (compress) both x and y variables into one ID*/

      data  aodpm2011_v5;
      set  aodpm2011_v4;
      guid=compress(xnymx||ynymx);
	  run;

/*convert if from text to numeric*/

      data aodpm2011_v6;
      set  aodpm2011_v5;
	  drop  xnymx ynymx xnym ynym;
	  guid2=input(guid, 8.); 
      drop guid;
  	  run;


	  data aodpm2011;
      set  aodpm2011_v6;
	  guid=guid2; 
      drop guid2;
  	  run;

/*****************************/


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

proc sort data= Aodpm2011;
by guid;
run;


data Aodpm2011_v6;
merge Aodpm2011(in=a) lu (in=b keep=guid pop_sqkm elev A1_dist_km p_urban p_open area_pm point_pm population ) ;
by guid;
if a;
run;


/*****************************************************************************************************************/
/*CREATE A GRID OF THE ARAE*/

/*import script cn001 outout*/
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data all_aod_00_10;
set aod.aod_2011  aod.aod_2011 aod.aod_2011;
run;

/* No dupkey in all AOD available */


/*create grid by sorting by x,y and removing duplicates */


proc sort data = all_aod_00_10 nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 



/*export for mod3 uniq grid*/

PROC EXPORT DATA= WORK.grid
           OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN005_mod3\uniq_grid.csv" 
		               DBMS=CSV REPLACE;
					        PUTNAMES=YES;
							RUN;
							 


/*	clip all aod only for NE */


proc sort data = all_aod_00_10; by guid ;run;
proc sort data = lu;            by guid ;run;

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
01/01/2011 1
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


/*****************************************************************************************************************/

data final2011 (where=(date>="01JAN2011"D and date<="31DEC2011"D )) ;
set final;
run; 


PROC IMPORT OUT= WORK.guidstn(keep = guid stn)
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_stn_0911.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



proc sort data = final2011; by guid   ;run;
proc sort data = guidstn ; by guid ;run;

data guid2011;
merge final2011(in=a) guidstn (in=b)  ;
  by guid ;
    if a;
	run; 


libname metc 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN005_MET_full_dataset\' ;

data metc2011;
set  metc.metc2011;
run;



proc sort data = metc2011;  by stn date;run;
proc sort data = guid2011 ; by stn date;run;

data metc2011_v2;
merge metc2011(in=a keep=stn temp_f wdsp_f visib_f date ah_gm3_F lat_met long_met) guid2011 (in=b)  ;
  by stn date;
    if b;
	run; 


data metc.metc2011_v2;
set metc2011_v2;
run; 

/*join mod1 files with common met data*/


proc sort data = Aodpm2011_v6; by guid date ;run;
proc sort data = metc2011_v2 ; by guid date ;run;

data out2011_3;
merge Aodpm2011_v6(in=a) metc2011_v2 (in=b)  ;
  by guid date;
    if a;
	run; 

PROC EXPORT DATA= WORK.out2011_3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN001_mod1\t2011.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

