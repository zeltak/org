
/*proc printto log="nul:"; run;*/
ods listing close;*to suppress the output printing;
 proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\Archive\fg2007.log"; run;



PROC IMPORT OUT= grid
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 






/**** Create Data ****/ 
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


/*create full grid*/

/*create a list of dates for cycle-first type macro*/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Daily;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("list",list_new);
      output;
     end;
run;

%put &list;


/*launch the macro*/

%put &list;

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

data Daily&date(keep = date xx yy);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_list Daily&date; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &list);



PROC IMPORT OUT= WORK.key_full2007
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2007.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;  


proc sort data = key_full2007; by xx yy   ;run;

proc sort data =  final; by xx yy   ;run;

data mod2_2007_s1;
merge final (in=a) key_full2007 (in=b keep=xx yy guid  station)  ;
  by xx yy ;
    if a;
	run; 

data mod2_2007_s1 (drop=stn);
set mod2_2007_s1;
 stn     = compress(left(station));
 station = stn;
 if station = "KNYMT.S" then station = "KNYMT_S";
run; 


PROC EXPORT DATA= mod2_2007_s1
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2007all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*** Export ***/

proc datasets lib=work kill; run; quit;





/*proc printto log="nul:"; run;*/
ods listing close;*to suppress the output printing;
 proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\Archive\fg2008.log"; run;



PROC IMPORT OUT= grid
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 






/**** Create Data ****/ 
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


/*create full grid*/

/*create a list of dates for cycle-first type macro*/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Daily;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("list",list_new);
      output;
     end;
run;

%put &list;


/*launch the macro*/

%put &list;

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

data Daily&date(keep = date xx yy);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_list Daily&date; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &list);



PROC IMPORT OUT= WORK.key_full2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;  


proc sort data = key_full2008; by xx yy   ;run;

proc sort data =  final; by xx yy   ;run;

data mod2_2008_s1;
merge final (in=a) key_full2008 (in=b keep=xx yy guid  station)  ;
  by xx yy ;
    if a;
	run; 

data mod2_2008_s1 (drop=stn);
set mod2_2008_s1;
 stn     = compress(left(station));
 station = stn;
 if station = "KNYMT.S" then station = "KNYMT_S";
run; 


PROC EXPORT DATA= mod2_2008_s1
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2008all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*** Export ***/

proc datasets lib=work kill; run; quit;




/*proc printto log="nul:"; run;*/
ods listing close;*to suppress the output printing;
 proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\Archive\fg2009.log"; run;



PROC IMPORT OUT= grid
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 






/**** Create Data ****/ 
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


/*create full grid*/

/*create a list of dates for cycle-first type macro*/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Daily;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("list",list_new);
      output;
     end;
run;

%put &list;


/*launch the macro*/

%put &list;

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

data Daily&date(keep = date xx yy);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_list Daily&date; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &list);



PROC IMPORT OUT= WORK.key_full2009
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2009.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;  


proc sort data = key_full2009; by xx yy   ;run;

proc sort data =  final; by xx yy   ;run;

data mod2_2009_s1;
merge final (in=a) key_full2009 (in=b keep=xx yy guid  station)  ;
  by xx yy ;
    if a;
	run; 

data mod2_2009_s1 (drop=stn);
set mod2_2009_s1;
 stn     = compress(left(station));
 station = stn;
 if station = "KNYMT.S" then station = "KNYMT_S";
run; 


PROC EXPORT DATA= mod2_2009_s1
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2009all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*** Export ***/

proc datasets lib=work kill; run; quit;






/*proc printto log="nul:"; run;*/
ods listing close;*to suppress the output printing;
 proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\Archive\fg2010.log"; run;



PROC IMPORT OUT= grid
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 






/**** Create Data ****/ 
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


/*create full grid*/

/*create a list of dates for cycle-first type macro*/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Daily;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("list",list_new);
      output;
     end;
run;

%put &list;


/*launch the macro*/

%put &list;

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

data Daily&date(keep = date xx yy);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_list Daily&date; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &list);



PROC IMPORT OUT= WORK.key_full2010
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2010.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;  


proc sort data = key_full2010; by xx yy   ;run;

proc sort data =  final; by xx yy   ;run;

data mod2_2010_s1;
merge final (in=a) key_full2010 (in=b keep=xx yy guid  station)  ;
  by xx yy ;
    if a;
	run; 

data mod2_2010_s1 (drop=stn);
set mod2_2010_s1;
 stn     = compress(left(station));
 station = stn;
 if station = "KNYMT.S" then station = "KNYMT_S";
run; 


PROC EXPORT DATA= mod2_2010_s1
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2010all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*** Export ***/

proc datasets lib=work kill; run; quit;











/*proc printto log="nul:"; run;*/
ods listing close;*to suppress the output printing;
 proc printto log="f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\Archive\fg2011.log"; run;



PROC IMPORT OUT= grid
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\FINAL_ALL_GRID.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 






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


/*create full grid*/

/*create a list of dates for cycle-first type macro*/

data id_list(keep = list list_new date);
  length list $ 30000. list_new $ 30000. ;
   retain list_new;
   set Daily;
     if _n_ = 1 then do;
        list = trim(left(Date));
        list_new = list;
                     output;
     end;
     if _n_ > 1 then do;
      list = trim(left(list_new))||" " || trim(left(Date));
      list_new = list;
       call symputx("list",list_new);
      output;
     end;
run;

%put &list;


/*launch the macro*/

%put &list;

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

data Daily&date(keep = date xx yy);
  if _N_ = 1 then set Daily&date;
 set grid;
run;

proc append base = Final data = Daily&date force;
run;

proc datasets lib=work; delete id_list Daily&date; run;

%let j=%eval(&j+1);
%end;


%mend full;

%full(List = &list);



PROC IMPORT OUT= WORK.key_full2011
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2011.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN;  


proc sort data = key_full2011; by xx yy   ;run;

proc sort data =  final; by xx yy   ;run;

data mod2_2011_s1;
merge final (in=a) key_full2011 (in=b keep=xx yy guid  station)  ;
  by xx yy ;
    if a;
	run; 

data mod2_2011_s1 (drop=stn);
set mod2_2011_s1;
 stn     = compress(left(station));
 station = stn;
 if station = "KNYMT.S" then station = "KNYMT_S";
run; 


PROC EXPORT DATA= mod2_2011_s1
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN011_mod3_files\mod3_2011all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  

/*** Export ***/

proc datasets lib=work kill; run; quit;




























