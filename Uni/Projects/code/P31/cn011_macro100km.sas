proc printto log="nul:"; run;



libname mods 'f:\Uni\Projects\p031_MIAC_PM\3.Work\3.Analysis\AN_001_mods\' ;

libname pm 'f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN001_PM_allyears\' ;


options mprint;
%macro import(year=);

data Pmsite;
set pm.PM_All_complete;
c=year(date);
run; 

data Pmsite&year;
set Pmsite;
where c=&year;
run; 
						 

%MEND ;

/*%import(year=2000);*/
/*%import(year=2001);*/
/*%import(year=2002);*/
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);
%import(year=2009);
%import(year=2010);
%import(year=2011);
%import(year=2012);


PROC IMPORT OUT= near_table_met (keep=long_aod lat_aod sitecode guid)
            DATAFILE= "f:\Uni\Projects\p031_MIAC_PM\3.Work\2.Gather_data\FN015_withinkm\km100.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
 

*---------------------------2003------------------------------*/
/*---------------------------2003------------------------------*/
/*---------------------------2003------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2003 1
31/12/2003 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2003;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2003 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);



proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;




/*---------------------------2004------------------------------*/
/*---------------------------2004------------------------------*/
/*---------------------------2004------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2004 1
31/12/2004 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2004;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2004 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;

/*---------------------------2005------------------------------*/
/*---------------------------2005------------------------------*/
/*---------------------------2005------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2005 1
31/12/2005 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2005;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2005 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;


/*---------------------------2006------------------------------*/
/*---------------------------2006------------------------------*/
/*---------------------------2006------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2006 1
31/12/2006 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2006;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2006 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;


/*---------------------------2007------------------------------*/
/*---------------------------2007------------------------------*/
/*---------------------------2007------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2007 1
31/12/2007 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2007;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2007 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;


/*---------------------------2008------------------------------*/
/*---------------------------2008------------------------------*/
/*---------------------------2008------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2008 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2008;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2008 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;


/*---------------------------2009------------------------------*/
/*---------------------------2009------------------------------*/
/*---------------------------2009------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2009 1
31/12/2009 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2009;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2009 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;


/*---------------------------2010------------------------------*/
/*---------------------------2010------------------------------*/
/*---------------------------2010------------------------------*/


/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2010 1
31/12/2010 1
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2010;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2010 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;



/*---------------------------2011------------------------------*/
/*---------------------------2011------------------------------*/
/*---------------------------2011------------------------------*/


/**** Create the date list ****/ 
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


proc sort data = near_table_met; by SiteCode; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Pmsite2011;
  where date = &date;
run;

proc sort data      = Daily&date;                  by SiteCode; run;
proc transpose data = Daily&date  out = Transpose; by SiteCode; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "PM25";
run;

data DATA3;
 merge near_table_met(in=a) Transpose(in=b);
  by SiteCode;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class long_aod lat_aod;
  var Col1;
   output out = meanout mean(col1) = meanpm100k;  
run; 
quit;

data day&date;
 set meanout;
 keep date long_aod lat_aod guid meanpm100k;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final_100kmet2011 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);




proc datasets lib=work; delete Daily id_elenco meanout seriesj; run;



