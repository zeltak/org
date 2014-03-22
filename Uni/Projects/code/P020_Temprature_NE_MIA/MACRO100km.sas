
libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;
proc printto log="nul:"; run;



/*---------------------------2000------------------------------*/
/*---------------------------2000------------------------------*/
/*---------------------------2000------------------------------*/


PROC IMPORT OUT= met2000
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2000.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2000met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2000.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2000 1
31/12/2000 1
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


proc sort data = Near_table_2000met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2000;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2000met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2000 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);








/*---------------------------2001------------------------------*/
/*---------------------------2001------------------------------*/
/*---------------------------2001------------------------------*/


PROC IMPORT OUT= met2001
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2001.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2001met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2001.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2001 1
31/12/2001 1
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


proc sort data = Near_table_2001met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2001;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2001met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2001 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2002------------------------------*/
/*---------------------------2002------------------------------*/
/*---------------------------2002------------------------------*/


PROC IMPORT OUT= met2002
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2002.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2002met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2002.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




/**** Create the date list ****/ 
/*creates the complete time series range*/

data seriesj;
 input date ddmmyy10. Value;
  format date ddmmyy10.;
cards;
01/01/2002 1
31/12/2002 1
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


proc sort data = Near_table_2002met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2002;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2002met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2002 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2003------------------------------*/
/*---------------------------2003------------------------------*/
/*---------------------------2003------------------------------*/


PROC IMPORT OUT= met2003
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2003met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2003.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2003met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2003;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2003met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2003 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2004------------------------------*/
/*---------------------------2004------------------------------*/
/*---------------------------2004------------------------------*/


PROC IMPORT OUT= met2004
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2004met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2004met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2004;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2004met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2004 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2005------------------------------*/
/*---------------------------2005------------------------------*/
/*---------------------------2005------------------------------*/


PROC IMPORT OUT= met2005
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2005met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2005.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2005met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2005;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2005met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2005 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2006------------------------------*/
/*---------------------------2006------------------------------*/
/*---------------------------2006------------------------------*/


PROC IMPORT OUT= met2006
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2006met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2006.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2006met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2006;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2006met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2006 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2007------------------------------*/
/*---------------------------2007------------------------------*/
/*---------------------------2007------------------------------*/


PROC IMPORT OUT= met2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2007met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2007met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2007;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2007met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2007 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2008------------------------------*/
/*---------------------------2008------------------------------*/
/*---------------------------2008------------------------------*/


PROC IMPORT OUT= met2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2008met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2008met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2008;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2008met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2008 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2009------------------------------*/
/*---------------------------2009------------------------------*/
/*---------------------------2009------------------------------*/


PROC IMPORT OUT= met2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2009met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2009met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2009;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2009met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2009 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2010------------------------------*/
/*---------------------------2010------------------------------*/
/*---------------------------2010------------------------------*/


PROC IMPORT OUT= met2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2010met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2010met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2010;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2010met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2010 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);







/*---------------------------2011------------------------------*/
/*---------------------------2011------------------------------*/
/*---------------------------2011------------------------------*/


PROC IMPORT OUT= met2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= near_table_2011met(keep = station xx yy)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN015_withinkm\km2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 




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


proc sort data = Near_table_2011met; by station; run;






/*use the macro variable created in cycle*/
/*The output is called 'Final'*/

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let date = %scan(&List,&j);

data Daily&date;
 set Met2011;
  where date = &date;
run;

proc sort data      = Daily&date;                  by station; run;
proc transpose data = Daily&date  out = Transpose; by station; run;

data Transpose(drop = _NAME_ _label_);
 set Transpose;
  if _NAME_ = "tempc";
run;

data DATA3;
 merge Near_table_2011met(in=a) Transpose(in=b);
  by station;
  if a;
run; 

data DATA3;
 set DATA3;
  if col1 = . then delete;
run;

proc summary nway data=DATA3;
 class xx yy;
  var Col1;
   output out = meanout mean(col1) = TEST_AVE;  
run; 
quit;

data day&date;
 set meanout;
 keep date xx yy Test_Ave;
  date = &date;
  format date date9.;
run;

proc append base = mods.Final60kmet2011 data = day&date force;
run;

proc datasets lib=work; delete Daily&date  day&date DATA3 Transpose; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &Lista);

