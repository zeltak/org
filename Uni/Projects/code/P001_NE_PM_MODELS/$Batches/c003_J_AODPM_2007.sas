
/*** import AOD and PM datafile ***/

libname AOD "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset";
libname PM  "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN002_PM_full_dataset";


/**** Year: 2007 ****/

data PM_2007(keep = PM25 Long_PM Lat_PM SiteCode EPAcode Date); 
 set PM.PM2007;
  where  '01jan2007'd <= date <= '31dec2007'd;  
  if pm25 < 0 then delete;
run;

data AOD_2007(keep = Long_AOD Lat_AOD AOD Date);
 set AOD.All_aod;
  where  '01jan2007'd <= date <= '31dec2007'd;  
run;

/*** Number of SiteCode -- Number of Dates available, Year:2007 ***/

proc sort data = PM_2007 nodupkey out = PM_ID_2007(keep=SiteCode);  by SiteCode; run; 
proc sort data = PM_2007 nodupkey out = PM_Date2007(keep=Date);     by Date;     run;

proc sort data = AOD_2007  nodupkey out = AOD_Date2007(keep=Date); by Date; run;   

/*** Merge by Common Date ***/

data Common_Date; merge PM_Date2007(in = a) AOD_Date2007(in = b); by date; if a and b; run;

/*** Create a list of Dates ***/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Common_Date;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("List",list_new);
      output;
     end;
run;

DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; 
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%put &List;

PROC DATASETS LIBRARY=work; delete id_list Aod_date2007 Pm_date2007 Date_comuni; RUN;

/***** Macro AOD *********/

options mprint;

%let Type =;
%let DateList =;
%let Cycle =;
%let ID =;

%macro AOD_DATA(DateList = );

%let j=1;

%do %while (%scan(&DateList,&j) ne);
 %let Cycle = %scan(&DateList,&j);

%macro AOD(date = );

data PM_01_&Date;   set PM_2007;   where date = &Date; run;
data AOD_01_&Date;  set AOD_2007;  where date = &Date; run;

/*** Here I create a list of SiteCode ***/

data id_list(keep = list list_new SiteCode);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set PM_01_&Date;
     if _n_ = 1 then do;
        list = trim(left(SiteCode));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(SiteCode));
      list_new = list;
       call symputx("Type",list_new);
      output;
     end;
run;

%put &Type;

%let i=1;

%do %while (%scan(&Type,&i) ne );
 %let ID = %scan(&Type,&i);

data St_PM&ID&Date;
 set PM_01_&Date;
  where SiteCode = "&ID"; /**** Drop " if sitecode is numeric *****/
run;

data distance&ID&Date;
  if _N_ = 1 then set St_PM&ID&Date;
 set AOD_01_&Date;
run;

data distance&ID&Date;
 set distance&ID&Date;
   Distance= round(geodist(Lat_AOD, Long_AOD, Lat_PM, Long_PM), 0.001);
   if Distance > 13 then AOD = .; /*** Cut off point of 12 Km ***/
run;

proc sort data = distance&ID&Date; by distance; run;

data distance&ID&Date;
 set distance&ID&Date;
   if _N_ > = 2 then delete;
run;

proc append base = Results data = distance&ID&Date force;
run;

proc datasets lib=work; delete id_list St_PM&ID&Date distance&ID&Date;run;

%let i=%eval(&i+1);
%end;

DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; 
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

proc datasets lib=work; delete AOD_01_&Cycle PM_01_&Cycle;run;

%mend AOD;

%AOD(date = &Cycle);

%let j=%eval(&j+1);
%end;

%mend AOD_DATA;

%AOD_DATA(DateList = &List);



data resultsv2(where=(date>='01JAN2007'D and date<='31DEC2007'D ));
set results;
if aod=. then delete;
run; 






proc sort data =  resultsv2; by date sitecode   ;run;

libname aod3 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN003_PM_AOD_Combined\' ;

 

data aod3.T2007;
set resultsv2 ;
run;	



