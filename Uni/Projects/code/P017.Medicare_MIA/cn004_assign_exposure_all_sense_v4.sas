
libname poll "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN040_Lags\" ;


PROC IMPORT OUT= inc
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.1.Raw_data\SES\midatl_guid_cbg00wtdv2.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 

data poll;
set poll.poll_lag_V5;
where date>='01Jun2000'D and date<='31Dec2006'D ; 
run;

data inc;
set inc(rename=( guid_=guid ));;
run; 

proc sort data=poll;
by guid;
run;

proc sort data=inc;
by guid;
run;

data poll_v3;
merge poll inc ;
by guid;
run;

PROC IMPORT OUT= WORK.regguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\guid_region.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc sort data = regguid; by guid   ;run;
proc sort data = poll_v3  ; by guid ;run;

data poll_v3;
merge poll_v3(in=a) regguid (in=b keep=guid reg_id)  ;
  by guid;
    if a;
	run; 

data poll_v3;
set poll_v3;
if guid=. then delete;
run; 

PROC IMPORT OUT= WORK.zipguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\MIA_zipcode_guid.dbf" 
			            DBMS=DBF  REPLACE;
						     GETDELETED=NO;
							 RUN; 

data zipguid;
set zipguid(rename=( zip=zipcode ));;;
run; 



/*#create full datasest*/



PROC IMPORT OUT= WORK.maguid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\aodgridfull_clip.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data maguid(drop=x y objectid);
set maguid;
long_aod=x;
lat_aod=y;
run; 

proc sort data = maguid nodupkey Out = grid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 


/**** Create Data ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/03/2000 1
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

options notes source source2 ; *re-instate LOG WINDOW printing;


data ffx (where=(date<="31DEC2006"D )) ;
set final;
guid2=input(guid, 8.); 
drop guid;
run;
data final2;
set ffx;
guid=guid2; 
drop guid2;
run; 



PROC IMPORT OUT= WORK.all
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid_MIA\ALL.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


data all;
set all(rename=(dateadmi=date));
run;


proc summary data = all  sum nway;
 class guid date;
   var copd--strhem;
   output out = Summary         
								 sum(copd)=    copd
                        		 sum(ari) =    ari
                                 sum(pneum) =  pneum
                                 sum(mi)    =  mi   
                                 sum(chf)   =  chf  
								 sum(diab)   =  diab  
                                 sum(resp)  =  resp 
                                 sum(cvd)   =  cvd  
                                 sum(ihd)   =  ihd  
                                 sum(stroke) = stroke
                                 sum(strisc) = strisc
                                 sum(strhem) = strhem;
                             
run;
quit;


proc sort data = final2         ; by guid date; run;
proc sort data = summary;         by guid date; run;

data TimeSeries;
  merge summary (in=a) final2  (in=b);
   by guid date;
run;





data TimeSeries2;
set TimeSeries;
if long_aod=. then delete;
run;


data nomiss(drop=i);                                                    
  set TimeSeries2;                                                            
  array testmiss(*) copd--strhem;                                            
  do i = 1 to dim(testmiss);                                              
    if testmiss(i)=. then testmiss(i)=0;                                    
  end;                                                                    
run;

/*add day of the week and clean*/

data nomiss(drop = _TYPE_ _FREQ_);
 set nomiss;
  dow=weekday(date);
run;



proc sort data=poll_v3;
by guid date;
run;

proc sort data=nomiss;
by guid date;
run;


data times;
merge nomiss (in=a)   poll_v3 (in=b);
by guid date;
  if a;
run;

data times4;
set times;
if pmnew=. then delete;
format date JULIAN.;
run;

/*get meanpm for each guid for 7 year period*/


proc summary data=times4;
class guid;
var pmnewma1 ;
output out=new mean=mpmguid;
run;

proc sort data=times4;
by guid;
run;

proc sort data=new;
by guid ;
run;

data times5;
merge times4   new ;
by guid ;
run;


data times6;
set times5;
if guid=. then delete;
deltapm=pmnewma1-mpmguid;
run;

data times7;
set times6;
if medhhin_wtd <  32281 then inc_bin_25 = 0;
else inc_bin_25=1;
if medhhin_wtd <  37137.5 then inc_bin_m = 0;
else inc_bin_m=1;
if medhhin_wtd < 45409 then inc_bin_75 = 0;
else inc_bin_75=1;
if pctnonwht_wtd < 1. then min_bin_25 = 0;
else min_bin_25=1;
if pctnonwht_wtd < 2.7 then min_bin_m = 0;
else min_bin_m=1;
if pctnonwht_wtd < 8.5 then min_bin_75 = 0;
else min_bin_75=1;
if  pct65upest < 11.8 then age_bin_25 = 0;
else age_bin_25=1;
if  pct65upest < 14.1 then age_bin_m = 0;
else age_bin_m=1;
if  pct65upest < 16.6 then age_bin_75 = 0;
else age_bin_75=1;
if pctbachorhigher_wtd < 9.7 then col_bin_25 = 0;
else col_bin_25=1;
if pctbachorhigher_wtd < 13.4 then col_bin_m = 0;
else col_bin_m=1;
if pctbachorhigher_wtd < 19.7 then col_bin_75 = 0;
else col_bin_75=1;
if pctlowinc_wtd < 16.7 then hs_bin_25 = 0;
else hs_bin_25=1;
if pctlowinc_wtd < 23.1 then hs_bin_m = 0;
else hs_bin_m=1;
if pctlowinc_wtd < 28.9 then hs_bin_75 = 0;
else hs_bin_75=1;
run;

/*for splines*/
data short;
set times7;
keep guid date;
run;


PROC EXPORT DATA= WORK.short
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\short_0006.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


/*export to r*/


PROC EXPORT DATA= WORK.times7
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\all_0006.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

/*Models*/


/**/
/*PROC IMPORT OUT= times7*/
/*  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\all_0006.csv" */
/*    DBMS=CSV REPLACE;*/
/*	  GETNAMES=YES;*/
/*	    DATAROW=2; */
/*		RUN;*/

PROC IMPORT OUT= spline
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\3.Analysis\AN001_R_files_bycase\spline.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
	

proc sort data = times7; by guid date   ;run;
proc sort data = spline ; by guid date ;run;

data DATA3;
merge times7(in=a) spline (in=b)  ;
  by guid date;
    if a;
	run; 


proc means data=DATA3 n min max mean std nmiss sum;
var cvd; 
run; 


/*analysis*/

/*cvd*/

	proc glimmix data=DATA3  ;
      model cvd = deltapm mpmguid dow temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (8.2524)/  hold=1; 
run;

	proc glimmix data=DATA3  ;
      model cvd =  pmnewma1 dow temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (  9.4295)/  hold=1; 
run;









proc glimmix data=DATA3  ;
      model copd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (5.513)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model ari = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms ( 2.624)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model pneum = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (6.2448)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model cvd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (9.4684)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model resp = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (8.2172)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model mi = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms ( 5.3460)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model chf = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (6.5738)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model diab = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (3.803)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model ihd = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (7.2090)/  hold=1; 
run;


proc glimmix data=DATA3  ;
      model stroke = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (5.9051)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model strisc = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (4.4072)/  hold=1; 
run;

proc glimmix data=DATA3  ;
      model strhem = pmnewma1 pmnewmayear temp_fma1 pctnonwht_wtd pctbachorhigher_wtd pct65upest pctlowinc_wtd x1--x35 /s dist=poisson link=log  ;
   random intercept / subject=guid ;
   parms (2.253)/  hold=1; 
run;
