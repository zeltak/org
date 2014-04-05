/*ALL LOG WHEN THE SCRIPS RUNS*/

proc printto log="nul:"; run;



/*START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2000\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2000\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2000 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2000
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2000.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 






/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2001\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2001\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2001 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2001
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2001.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 








/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2002\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2002\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2002 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2002
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2002.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 




/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2003\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2003\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2003 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2003
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2004\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2004\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2004 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2004
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2004.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2005\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2005\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2005 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2005
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2005.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2006\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2006\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2006 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2006
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2006.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 




/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2007\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2007\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2007 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2007
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2007.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 




/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2008\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2008\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2008 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2008
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2009\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2009\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2009 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2009
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2009.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2010\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2010\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2010 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;

PROC EXPORT DATA= work.lst2010
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2010.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2011\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;
options noquotelenmax;

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_ST\2011\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;



%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;
proc sort data = &filename._4 ; by x y ;run;
proc sort data = &filename._8 ; by x y ;run;

data &filename;
 merge &filename._0 &filename._4 &filename._8;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 &filename._4 &filename._8; run;

data &filename (drop= &filename._0 &filename._4 &filename._8 day_st_kelvin night_st_kelvin dst_faren nst_faren)  ;
 set &filename;
  day_st_kelvin   = (&filename._0)*0.02;
  night_st_kelvin = (&filename._4)*0.02;
  emis_scale = (&filename._8)*0.002+0.49;
  dst_faren  =(((day_st_kelvin-273) * 1.8)+32);
  nst_faren  =(((night_st_kelvin-273) * 1.8)+32);
  DTc = (5/9)*(dst_faren-32);
  NTc = (5/9)*(nst_faren-32);
  DTckin=  dtc/(emis_scale**0.25);
  NTckin=  ntc/(emis_scale**0.25);
  run; 


/*append every iteration to oned file*/
proc append base=LST2011 data = &filename;
run;

%end; 

%mend;

%MergeData;


proc printto; run;

options notes source source2;

PROC EXPORT DATA= work.lst2011
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2011.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/**/
/*proc freq data=lst8;*/
/*table x*y / list;*/
/*ods output list = list8; */
/*run; */
/**/
/**/
/*data list_check(keep = X Y Frequency);*/
/* set list8;*/
/*  if Frequency = 1;*/
/*run;*/
/**/




/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/


options mprint;
%macro import(year=);



PROC IMPORT OUT= WORK.lst
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


/*#fix clip offset*/
data lst_fix;
set lst;
xx=x+0.003191407;
yy=y+0.00374356;
run;  

PROC EXPORT DATA= lst_fix
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

%MEND ;

/*%import(year=2000);*/
/*%import(year=2001);*/
%import(year=2002);
%import(year=2003);
/*%import(year=2004);*/
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 
%import(year=2009);
%import(year=2010);
%import(year=2011); 




/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/



PROC IMPORT OUT= WORK.lst
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN005_MODIS_yearly\lst2004.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


/*#fix clip offset*/
data lst_fix;
set lst;
xx=x+0.003191407;
yy=y+0.00374356;
run;  

 

proc sort data = lst_fix nodupkey out=grid; by xx yy; run;quit;





data gridx (drop= x1 y2);
      set grid;
	  x1 = xx*-1000000000;
	  y2 = yy*1000000000;
      guid=compress(x1||y2);
	   run;
	


/*data gridy (drop= x1 y2);*/
/*      set grid;*/
/*	  x3 = xx*-1;*/
/*	  x4 = round(x3,0.001);*/
/*      y4 = round(yy,0.001);*/
/*	  x1 = x4*-1000;*/
/*	  y2 = y4*1000;*/
/*      guid=compress(x1||y2);*/
/*	   run;*/



/*to create guid as numeric instead of character*/




PROC EXPORT DATA= gridx
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\LST_fullgrid2004.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
	
